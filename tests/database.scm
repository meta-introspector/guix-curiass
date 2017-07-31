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

(define example-spec
  '((#:name . "guix")
    (#:url . "git://git.savannah.gnu.org/guix.git")
    (#:load-path . ".")
    (#:file . "/tmp/gnu-system.scm")
    (#:proc . hydra-jobs)
    (#:arguments (subset . "hello"))
    (#:branch . "master")
    (#:tag . #f)
    (#:commit . #f)
    (#:no-compile? . #f)))

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

(define database-name
  ;; Use an empty and temporary database for the tests.
  (string-append (getcwd) "/" (number->string (getpid)) "-tmp.db"))

(test-group-with-cleanup "database"
  (test-assert "db-init"
    (%db (db-init database-name)))

  (test-assert "sqlite-exec"
    (begin
      (sqlite-exec (%db) "\
INSERT INTO Evaluations (specification, revision) VALUES (1, 1);")
      (sqlite-exec (%db) "\
INSERT INTO Evaluations (specification, revision) VALUES (2, 2);")
      (sqlite-exec (%db) "\
INSERT INTO Evaluations (specification, revision) VALUES (3, 3);")
      (sqlite-exec (%db) "SELECT * FROM Evaluations;")))

  (test-equal "db-add-specification"
    example-spec
    (begin
      (db-add-specification (%db) example-spec)
      (car (db-get-specifications (%db)))))

  (test-assert "db-add-derivation"
    (let* ((job (make-dummy-job))
           (key (assq-ref job #:derivation)))
      (db-add-derivation (%db) job)
      (%id key)))

  (test-assert "db-get-derivation"
    (db-get-derivation (%db) (%id)))

  (test-assert "db-add-build"
    (let ((build `((#:derivation . "/foo.drv")
                   (#:eval-id . 42)
                   (#:log . "log")
                   (#:outputs . (("foo" . "/foo"))))))
      (db-add-build (%db) build)

      ;; This should be idempotent, see <https://bugs.gnu.org/28094>.
      (db-add-build (%db) build)))

  (test-assert "db-close"
    (db-close (%db)))

  (delete-file database-name))
