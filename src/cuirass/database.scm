;;;; database.scm - store evaluation and build results
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

(define-module (cuirass database)
  #:use-module (cuirass base)
  #:use-module (cuirass job)
  #:use-module (sqlite3)
  #:export (db-init
            db-open
            db-close
            db-add-evaluation
            db-get-evaluation
            db-delete-evaluation))

(define (sqlite-exec db sql)
  "Wrap 'sqlite-prepare', 'sqlite-step', and 'sqlite-finalize'."
  (let ((stmt (sqlite-prepare db sql)))
    (sqlite-step stmt)
    (sqlite-finalize stmt)))

(define (db-init db-name)
  "Open database contained in DB-NAME, to store or read jobs and builds
informations.  DB-NAME must be a string.  SCHEMA must be some SQL statements
initialize the database.  Return a database object."
  (when (file-exists? db-name)
    (format (current-error-port) "Removing leftover database ~a~%" db-name)
    (delete-file db-name))
  (let* ((db (sqlite-open db-name (logior SQLITE_OPEN_CREATE
                                          SQLITE_OPEN_READWRITE))))
    (for-each (λ (sql) (sqlite-exec db sql))
              '("PRAGMA foreign_keys=OFF;"
                "BEGIN TRANSACTION;"
                "COMMIT;"
                "
CREATE TABLE job_spec (
  name            text not null,
  url             text not null,
  branch          text not null,
  file            text not null,
  proc            text not null,
  arguments       text not null,
  primary key (name)
);"
                "
CREATE TABLE build (
  id              integer primary key autoincrement not null,
  job_spec        text not null,
  drv             text not null,
  output          text
  -- foreign key (job_spec) references job_spec(name)
);"))
    db))

(define (db-open filename)
  "Open database contained in FILENAME, to store or read jobs and builds
informations.  Return a database object.  FILENAME must be a string corresponding
to a valid file name."
  (sqlite-open filename SQLITE_OPEN_READWRITE))

(define (db-close db)
  "Close database object DB."
  (sqlite-close db))

(define (db-add-evaluation db job)
  "Store a derivation result in database DB and return its ID."
   (sqlite-exec
    db
    (format #f "insert into build (job_spec, drv) values ('~A', '~A');"
            (job-name job)
            (job-derivation job)))
   (let* ((stmt (sqlite-prepare db "select last_insert_rowid() from build;"))
          (res  (sqlite-step stmt)))
     (sqlite-finalize stmt)
     (vector-ref res 0)))

(define (db-get-evaluation db id)
  "Retrieve a job in database DB which corresponds to ID."
  (let* ((stmt (sqlite-prepare
                db
                (format #f "select * from build where id=~A;" id)))
         (res  (sqlite-step stmt)))
    (sqlite-finalize stmt)
    res))

(define (db-delete-evaluation db id)
  "Delete a job in database DB which corresponds to ID."
  (sqlite-exec db
               (format #f "delete from build where id=~A;" id)))

;; (define (db-add-build db id)
;;   "Store a build result corresponding to ID in database DB.")
