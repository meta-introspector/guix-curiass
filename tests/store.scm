;;; store.scm -- Helpers to deal with the store.
;;; Copyright © 2024 Ludovic Courtès <ludo@gnu.org>
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

(define-module (tests store)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module ((gnu packages bootstrap) #:select (%bootstrap-guile))
  #:use-module (cuirass store)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-71))

(define %seed
  (logxor (cdr (gettimeofday))
          (car (gettimeofday))
          (cdr (gettimeofday))))

(define %state
  (seed->random-state %seed))

(define (trivial-thing)
  (let ((nonce (random 1e6 %state)))
    (computed-file "trivial-thing"
                   #~(begin
                       (set-port-encoding! (current-output-port) "UTF-8")

                       (display "starting!\n") (force-output)
                       (display "lambda: λ\n") (force-output)

                       ;; Sleep to give the 'current-read-waiter' a chance to
                       ;; be invoked.
                       (sleep 1)

                       #$nonce
                       (mkdir #$output)
                       (display "done\n"))
                   #:guile %bootstrap-guile)))

(define (trivial-derivation store)
  (run-with-store store
    (lower-object (trivial-thing))))


(test-begin "store")

(test-equal "build-derivations&, non-fiber"
  '(build-succeeded build-started)
  (with-store store
    (set-build-options store #:print-build-trace #t)
    (let* ((drv (trivial-derivation store))
           (port finish (build-derivations& store (list drv)))
           (events (process-build-log port
                                      (lambda (event result)
                                        (cons (car event) result))
                                      '())))
      (close-port port)
      (finish)
      events)))

(test-equal "build-derivations&, fiberized and non-blocking"
  '(build-succeeded build-started)

  ;; This test used to crash: 'build-derivations&' spawns a non-fiber thread
  ;; so we need to make sure it uses a blocking store connection or it would
  ;; end up invoking the Fibers scheduler (via 'current-read-waiter' & co.),
  ;; which would crash.  See <https://issues.guix.gnu.org/68237>.
  (run-fibers
   (lambda ()
     (define channel
       (make-channel))

     (spawn-fiber
      (lambda ()
        ;; XXX: We cannot use 'with-store/non-blocking' upfront because
        ;; packages use promises, and 'force' is a continuation barrier as of
        ;; Guile 3.0.9.
        (with-store store
          (with-store/non-blocking store2
            (set-build-options store #:print-build-trace #t)
            (set-build-options store2 #:print-build-trace #t)
            (let* ((drv (trivial-derivation store))
                   (port finish (build-derivations& store2 (list drv)))
                   (events (process-build-log port
                                              (lambda (event result)
                                                (cons (car event) result))
                                              '())))
              (close-port port)
              (finish)
              (put-message channel events))))))

     (get-message channel))))

(test-end "store")
