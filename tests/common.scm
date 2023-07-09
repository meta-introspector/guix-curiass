;;; common.scm -- Common test helpers.
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

(define-module (tests common)
  #:use-module (cuirass database)
  #:use-module (cuirass parameters)
  #:use-module (cuirass utils)
  #:use-module ((fibers scheduler) #:select (current-scheduler))
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (%db
            retry
            test-init-db!))

(define %db
  (make-parameter #f))

(define (pg-tmp)
  "Start a temporary PostgreSQL instance using ephemeralpg."
  ;; Destroy the database right after disconnection.
  (let* ((pipe (open-input-pipe "pg_tmp -w 1"))
         (uri (read-string pipe)))
    (close-pipe pipe)
    uri))

(define* (retry f #:key times delay)
  (let loop ((attempt 1))
    (let ((result (f)))
      (cond
       (result result)
       (else
        (if (>= attempt times)
            #f
            (begin
              (if (current-scheduler)
                  ((@ (fibers) sleep) delay)
                  (sleep delay))
              (loop (+ 1 attempt)))))))))

(define (test-init-db!)
  "Initialize the test database."
  (%create-database? #t)
  (%package-database (pg-tmp))
  (%db (db-open)))
