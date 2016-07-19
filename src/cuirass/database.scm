;;; database.scm -- store evaluation and build results
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
  #:use-module (cuirass config)
  #:use-module (ice-9 rdelim)
  #:use-module (sqlite3)
  #:export (;; Procedures.
            db-init
            db-open
            db-close
            db-add-evaluation
            db-get-evaluation
            db-delete-evaluation
            db-add-build-log
            read-sql-file
            ;; Parameters.
            %package-database
            ;; Macros.
            with-database))

(define (sqlite-exec db sql)
  "Wrap 'sqlite-prepare', 'sqlite-step', and 'sqlite-finalize'."
  (let ((stmt (sqlite-prepare db sql)))
    (sqlite-step stmt)
    (sqlite-finalize stmt)))

(define %package-database
  ;; Define to the database file name of this package.
  (make-parameter (string-append %localstatedir "/" %package ".db")))

(define %package-schema-file
  ;; Define to the database schema file of this package.
  (make-parameter (string-append (or (getenv "CUIRASS_DATADIR")
                                     (string-append %datadir "/" %package))
                                 "/schema.sql")))

(define (read-sql-file file-name)
  "Return a list of string containing SQL instructions from FILE-NAME."
  (call-with-input-file file-name
    (λ (port)
      (let loop ((insts '()))
        (let ((inst (read-delimited ";" port 'concat)))
          (if (or (eof-object? inst)
                  ;; Don't cons the spaces after the last instructions.
                  (string-every char-whitespace? inst))
              (reverse! insts)
              (loop (cons inst insts))))))))

(define (db-init)
  "Open the database to store and read jobs and builds informations.  Return a
database object."
  (let ((db-name (%package-database)))
    (when (file-exists? db-name)
      (format (current-error-port) "Removing leftover database ~a~%" db-name)
      (delete-file db-name))
    (let ((db (sqlite-open db-name (logior SQLITE_OPEN_CREATE
                                           SQLITE_OPEN_READWRITE))))
      (for-each (λ (sql) (sqlite-exec db sql))
                (read-sql-file (%package-schema-file)))
      db)))

(define (db-open)
  "Open database to store or read jobs and builds informations.  Return a
database object."
  (sqlite-open (%package-database) SQLITE_OPEN_READWRITE))

(define (db-close db)
  "Close database object DB."
  (sqlite-close db))

(define (db-add-evaluation db job)
  "Store a derivation result in database DB and return its ID."
  (sqlite-exec
   db
   (format #f "insert into build (job_spec, drv) values ('~A', '~A');"
           (assq-ref job #:job-name)
           (assq-ref job #:derivation)))
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

(define-syntax-rule (with-database db body ...)
  "Run BODY with a connection to the database which is bound to DB in BODY."
  (let ((db (db-init)))
    (dynamic-wind
      (const #t)
      (λ () body ...)
      (λ () (db-close db)))))

(define* (read-quoted-string #:optional port)
  "Read all of the characters out of PORT and return them as a SQL quoted
string."
  (let loop ((chars '()))
    (let ((char (read-char port)))
      (cond ((eof-object? char) (list->string (reverse! chars)))
            ((char=? char #\')  (loop (cons* char char chars)))
            (else (loop (cons char chars)))))))

(define (db-add-build-log db job log)
  "Store a build LOG corresponding to JOB in database DB."
  (let ((id   (assq-ref job #:id))
        (log* (cond ((string? log) log)
                    ((port? log)
                     (seek log 0 SEEK_SET)
                     (read-quoted-string log))
                    (else #f))))
    (sqlite-exec db
                 (format #f "update build set log='~A' where id=~A;"
                         log* id))))
