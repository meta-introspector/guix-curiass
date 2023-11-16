;;; utils.scm -- tests for (cuirass utils) module
;;; Copyright © 2023 Ludovic Courtès <ludo@gnu.org>
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

(use-modules (cuirass utils)
             (cuirass logging)
             (fibers)
             (fibers channels)
             (srfi srfi-1)
             (srfi srfi-64)
             (system base compile))

;; Enable debugging output.
(current-logging-level 'debug)

(test-begin "utils")

(define (resource-pool-test resources consumers)
  (lambda ()
    (define channel
      (make-channel))

    (spawn-fiber
     (lambda ()
       (define pool (make-resource-pool (iota resources)))
       (for-each (lambda (rank)
                   (spawn-fiber
                    (lambda ()
                      (with-resource-from-pool pool resource
                        (put-message channel rank)
                        #t)))
                   #t)
                 (iota consumers))))

    (sort (map (lambda _
                 (get-message channel))
               (iota consumers))
          <)))

(test-equal "resource pool, no contention"
  (iota 10)
  (run-fibers (resource-pool-test 10 10)
              ;; XXX: Disable preemption.  With Fibers 1.3.1, we get random
              ;; deadlocks on completion (when the 'sort' call above has
              ;; completed) with the default #:hz value.
              #:hz 0))

(test-equal "resource pool, contention"
  (iota 100)
  (run-fibers (resource-pool-test 10 100)))

(test-equal "resource pool, exception thrown"
  42
  ;; This test used to hang: 'raise-exception' is written in C and a
  ;; continuation barrier as of Guile 3.0.9, and a call to 'put-message' from
  ;; the exception handler would lead to "Attempt to suspend fiber within
  ;; continuation barrier".  See <https://issues.guix.gnu.org/67041>.
  (compile
   '(begin
      (use-modules (fibers)
                   (cuirass utils))
      (run-fibers
       (lambda ()
         (define pool (make-resource-pool (iota 10)))
         (catch 'doh!
           (lambda ()
             (with-resource-from-pool pool x
               (throw 'doh!)))
           (const 42)))))
   #:to 'value))

(test-end)
