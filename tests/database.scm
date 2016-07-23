;;;; database.scm - tests for (cuirass database) module
;;;
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;;
;;; This file is part of Cuirass.
;;;
;;; Cuirass is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Cuirass is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Cuirass.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (cuirass database)
             (srfi srfi-64))

(define* (make-dummy-job #:optional (name "foo"))
  `((#:name . ,name)
    (#:derivation . ,(string-append name ".drv"))
    (#:specification 0)))

(define %db
  ;; Global Slot for a database object.
  (make-parameter #t))

(define %id
  ;; Global Slot for a job ID in the database.
  (make-parameter #t))

(parameterize
    ((%package-database
      ;; Use an empty and temporary database for the tests.
      (string-append (getcwd) "/" (number->string (getpid)) "-tmp.db")))
  (dynamic-wind
    (const #t)
    (λ ()
      (test-assert "db-init"
        (%db (db-init)))

      (test-assert "sqlite-exec"
        (begin
          (sqlite-exec (%db) "\
INSERT INTO Evaluations (derivation, job_name, specification)\
  VALUES ('drv1', 'job1', 1);")
          (sqlite-exec (%db) "\
INSERT INTO Evaluations (derivation, job_name, specification)\
  VALUES ('drv2', 'job2', 2);")
          (sqlite-exec (%db) "\
INSERT INTO Evaluations (derivation, job_name, specification)\
  VALUES ('drv3', 'job3', 3);")
          (sqlite-exec (%db) "SELECT * FROM Evaluations;")))

      (test-assert "db-add-evaluation"
        (let* ((job (make-dummy-job))
               (key (assq-ref job #:derivation)))
          (db-add-evaluation (%db) job)
          (%id key)))

      (test-assert "db-get-evaluation"
        (db-get-evaluation (%db) (%id)))

      (test-expect-fail "db-add-build-log")
      ;; XXX: 'Builds' database table is not implemented yet.
      (test-equal "db-add-build-log"
        "foo log"
        (let ((job (acons #:id (%id) (make-dummy-job)))
              (log-column 3))
          (db-add-build-log (%db) job "foo log")
          (vector-ref (db-get-evaluation (%db) (%id)) log-column)))

      (test-assert "db-close"
        (db-close (%db))))
    (λ ()
      (delete-file (%package-database)))))
