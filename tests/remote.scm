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
             (avahi)
             (avahi client)
             (squee)
             (srfi srfi-34)
             (srfi srfi-64)
             (ice-9 match)
             (ice-9 threads))

(define server
  (make-parameter #f))

(define worker
  (make-parameter #f))

(define spawn?
  (if (defined? 'spawn)                           ;introduced in Guile 3.0.9
      (@ (guile) spawn)
      (lambda* (program arguments #:key (search-path? #f))
        (match (primitive-fork)
          (0
           (apply (if search-path? execlp execl) program arguments))
          (pid
           pid)))))

(define (start-worker)
  (setenv "REQUEST_PERIOD" "1")
  (setenv "CUIRASS_LOGGING_LEVEL" "debug")
  (worker (spawn "cuirass"
                 '("cuirass" "remote-worker"
                   "--server=127.0.0.1:5555"
                   "--private-key=tests/signing-key.sec"
                   "--public-key=tests/signing-key.pub")
                 #:search-path? #t)))

(define (stop-worker)
  (let ((worker (worker)))
    (kill worker SIGINT)
    (waitpid worker)))

(define (start-server)
  (mkdir-p "tests/cache")
  (setenv "CUIRASS_LOGGING_LEVEL" "debug")
  (server (spawn "cuirass"
                 (list "cuirass" "remote-server"
                       (string-append "--database=" (%package-database))
                       "--cache=tests/cache"
                       "--private-key=tests/signing-key.sec"
                       "--public-key=tests/signing-key.pub")
                 #:search-path? #t)))

(define (stop-server)
  (let ((server (server)))
    (kill server SIGINT)
    (waitpid server)))

(define* (dummy-drv #:optional sleep)
  (with-store store
    (derivation-file-name
     (run-with-store store
       ;; Add a nonce to make sure a new derivation is built each time we run
       ;; the tests.
       (let ((exp #~(let ((nonce (list #$(car (gettimeofday))
                                       #$(getpid))))
                      (when #$sleep
                        (sleep #$sleep))
                      (mkdir #$output))))
         (gexp->derivation "foo" exp))))))

(define drv
  (delay (dummy-drv)))

(define drv-with-timeout
  (delay (dummy-drv 2)))

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

(define guix-daemon-running?
  (let ((result (delay (guard (c ((store-connection-error? c) #f))
                         (with-store store
                           #t)))))
    (lambda ()
      "Return true if guix-daemon is running."
      (force result))))

(define avahi-daemon-running?
  (let ((result (delay
                  (catch 'avahi-error
                    (lambda ()
                      (let* ((poll (make-simple-poll))
                             (client (make-client (simple-poll poll)
                                                  (list
                                                   client-flag/ignore-user-config)
                                                  (const #t))))
                        (client? client)))
                    (const #f)))))
    (lambda ()
      "Return true if avahi-daemon is running."
      (force result))))

(test-group-with-cleanup "remote"
  (test-assert "db-init"
    (begin
      (test-init-db!)
      #t))

  ;; The remaining tests require guix-daemon to be running.
  (test-skip (if (and (guix-daemon-running?) (avahi-daemon-running?)) 0 100))

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
      (db-add-build (make-build #:drv (force drv)
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
       (eq? (assq-ref (db-get-build (force drv)) #:status)
            (build-status succeeded)))
     #:times 10
     #:delay 1))

  (test-assert "build timeout"
    (begin
      (db-add-build (make-build #:drv (force drv-with-timeout)
                                #:output "fake-2"
                                #:timeout 1))
      (retry
       (lambda ()
         (eq? (assq-ref (db-get-build (force drv-with-timeout)) #:status)
              (build-status failed)))
       #:times 10
       #:delay 1)))

  (test-assert "worker restart"
    (begin
      (stop-worker)
      (start-worker)
      (db-update-build-status! (force drv) (build-status scheduled))
      (retry
       (lambda ()
         (eq? (assq-ref (db-get-build (force drv)) #:status)
              (build-status succeeded)))
       #:times 10
       #:delay 1)))

  (test-assert "clean-up"
    (begin
      (stop-worker)
      (stop-server))))
