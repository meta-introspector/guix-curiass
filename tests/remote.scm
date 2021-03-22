;;; remote.scm -- test the remote building mechanism
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
;;;
;;; This file is part of Cuirass.
;;;
;;; Cuirass is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Cuirass is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Cuirass.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (cuirass database)
             (cuirass specification)
             (gnu packages base)
             (guix build utils)
             (guix channels)
             (guix derivations)
             (guix gexp)
             (guix monads)
             (guix packages)
             (guix store)
             (tests common)
             (squee)
             (srfi srfi-64)
             (ice-9 match)
             (ice-9 threads))

(define server
  (make-parameter #f))

(define worker
  (make-parameter #f))

(define (start-worker)
  (worker
   (match (primitive-fork)
     (0
      (setenv "REQUEST_PERIOD" "1")
      (execlp "cuirass" "cuirass" "remote-worker"
              "--server=127.0.0.1:5555"
              "--private-key=tests/signing-key.sec"
              "--public-key=tests/signing-key.pub"))
     (pid pid))))

(define (stop-worker)
  (let ((worker (worker)))
    (kill worker SIGINT)
    (waitpid worker)))

(define (start-server)
  (server
   (match (primitive-fork)
     (0
      (mkdir-p "tests/cache")
      (execlp "cuirass" "cuirass" "remote-server"
              (string-append "--database=" (%package-database))
              "--cache=tests/cache"
              "--private-key=tests/signing-key.sec"
              "--public-key=tests/signing-key.pub"))
     (pid pid))))

(define (stop-server)
  (let ((server (server)))
    (kill server SIGINT)
    (waitpid server)))

(define* (dummy-drv #:optional sleep)
  (with-store store
    (derivation-file-name
     (run-with-store store
       (let ((exp #~(begin
                      (when #$sleep
                        (sleep #$sleep))
                      (mkdir #$output))))
         (gexp->derivation "foo" exp))))))

(define drv
  (dummy-drv))

(define drv-with-timeout
  (dummy-drv 2))

(define* (make-build #:key
                     drv
                     output
                     (timeout 0))
  `((#:derivation . ,drv)
    (#:eval-id . 1)
    (#:job-name . "fake-job")
    (#:system . "x86_64-linux")
    (#:nix-name . "fake-1.0")
    (#:log . "unused so far")
    (#:status . ,(build-status scheduled))
    (#:outputs . (("out" . ,output)))
    (#:timestamp . 1501347493)
    (#:timeout . ,timeout)))

(test-group-with-cleanup "remote"
  (test-assert "db-init"
    (begin
      (test-init-db!)
      #t))

  (test-assert "fill-db"
    (let ((build build)
          (spec
           (specification
            (name "guix")
            (build 'hello)))
          (checkouts
           (list
            (checkout->channel-instance "dir1"
                                        #:name 'guix
                                        #:url "url1"
                                        #:commit "fakesha1"))))
      (db-add-or-update-specification spec)
      (db-add-evaluation "guix" checkouts
                         #:timestamp 1501347493)
      (db-add-build (make-build #:drv drv
                                #:output "fake-1"))))

  (test-assert "remote-server"
    (begin
      (start-server)
      #t))

  (test-assert "remote-worker"
    (begin
      (start-worker)
      #t))

  (test-assert "build done"
    (retry
     (lambda ()
       (eq? (assq-ref (db-get-build drv) #:status)
            (build-status succeeded)))
     #:times 10
     #:delay 1))

  (test-assert "build timeout"
    (begin
      (db-add-build (make-build #:drv drv-with-timeout
                                #:output "fake-2"
                                #:timeout 1))
      (retry
       (lambda ()
         (eq? (assq-ref (db-get-build drv-with-timeout) #:status)
              (build-status failed)))
       #:times 10
       #:delay 1)))

  (test-assert "worker restart"
    (begin
      (stop-worker)
      (start-worker)
      (db-update-build-status! drv (build-status scheduled))
      (retry
       (lambda ()
         (eq? (assq-ref (db-get-build drv) #:status)
              (build-status succeeded)))
       #:times 10
       #:delay 1)))

  (test-assert "clean-up"
    (begin
      (stop-worker)
      (stop-server))))
