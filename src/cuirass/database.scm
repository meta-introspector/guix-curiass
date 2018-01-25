;;; database.scm -- store evaluation and build results
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (cuirass database)
  #:use-module (cuirass config)
  #:use-module (cuirass utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (sqlite3)
  #:export (;; Procedures.
            assq-refs
            db-init
            db-open
            db-close
            db-add-specification
            db-get-specifications
            db-add-stamp
            db-get-stamp
            db-add-evaluation
            db-add-derivation
            db-get-derivation
            build-status
            db-add-build
            db-update-build-status!
            db-get-build
            db-get-builds
            read-sql-file
            read-quoted-string
            sqlite-exec
            ;; Parameters.
            %package-database
            %package-schema-file
            ;; Macros.
            with-database))

(define (%sqlite-exec db sql)
  (let* ((stmt (sqlite-prepare db sql))
         (res  (let loop ((res '()))
                 (let ((row (sqlite-step stmt)))
                   (if (not row)
                       (reverse! res)
                       (loop (cons row res)))))))
    (sqlite-finalize stmt)
    res))

(define-syntax sqlite-exec
  ;; Note: Making it a macro so -Wformat can do its job.
  (lambda (s)
    "Wrap 'sqlite-prepare', 'sqlite-step', and 'sqlite-finalize'.  Send to given
SQL statement to DB.  FMT and ARGS are passed to 'format'."
    (syntax-case s ()
      ((_ db fmt args ...)
       #'(%sqlite-exec db (format #f fmt args ...)))
      (id
       (identifier? #'id)
       #'(lambda (db fmt . args)
           (%sqlite-exec db (apply format #f fmt args)))))))

(define %package-database
  ;; Define to the database file name of this package.
  (make-parameter (string-append %localstatedir "/run/" %package
                                 "/" %package ".db")))

(define %package-schema-file
  ;; Define to the database schema file of this package.
  (make-parameter (string-append (or (getenv "CUIRASS_DATADIR")
                                     (string-append %datadir "/" %package))
                                 "/schema.sql")))

(define (read-sql-file file-name)
  "Return a list of string containing SQL instructions from FILE-NAME."
  (call-with-input-file file-name
    (lambda (port)
      (let loop ((insts '()))
        (let ((inst (read-delimited ";" port 'concat)))
          (if (or (eof-object? inst)
                  ;; Don't cons the spaces after the last instructions.
                  (string-every char-whitespace? inst))
              (reverse! insts)
              (loop (cons inst insts))))))))

(define* (db-init #:optional (db-name (%package-database))
                  #:key (schema (%package-schema-file)))
  "Open the database to store and read jobs and builds informations.  Return a
database object."
  (when (file-exists? db-name)
    (format (current-error-port) "Removing leftover database ~a~%" db-name)
    (delete-file db-name))
  (let ((db (sqlite-open db-name (logior SQLITE_OPEN_CREATE
                                         SQLITE_OPEN_READWRITE))))
    (for-each (lambda (sql) (sqlite-exec db sql))
              (read-sql-file schema))
    db))

(define* (db-open #:optional (db (%package-database)))
  "Open database to store or read jobs and builds informations.  Return a
database object."
  (if (file-exists? db)
      (sqlite-open db SQLITE_OPEN_READWRITE)
      (db-init db)))

(define (db-close db)
  "Close database object DB."
  (sqlite-close db))

(define* (assq-refs alst keys #:optional default-value)
  (map (lambda (key) (or (assq-ref alst key) default-value))
       keys))

(define (last-insert-rowid db)
  (vector-ref (car (sqlite-exec db "SELECT last_insert_rowid();"))
              0))

(define (db-add-specification db spec)
  "Store specification SPEC in database DB and return its ID."
  (apply sqlite-exec db "\
INSERT OR IGNORE INTO Specifications (repo_name, url, load_path, file, \
                  proc, arguments, branch, tag, revision, no_compile_p) \
  VALUES ('~A', '~A', '~A', '~A', '~S', '~S', '~A', '~A', '~A', ~A);"
         (append
          (assq-refs spec '(#:name #:url #:load-path #:file #:proc #:arguments))
          (assq-refs spec '(#:branch #:tag #:commit) "NULL")
          (list (if (assq-ref spec #:no-compile?) "1" "0"))))
  (last-insert-rowid db))

(define (db-get-specifications db)
  (let loop ((rows  (sqlite-exec db "SELECT * FROM Specifications;"))
             (specs '()))
    (match rows
      (() specs)
      ((#(name url load-path file proc args branch tag rev no-compile?)
        . rest)
       (loop rest
             (cons `((#:name . ,name)
                     (#:url . ,url)
                     (#:load-path . ,load-path)
                     (#:file . ,file)
                     (#:proc . ,(with-input-from-string proc read))
                     (#:arguments . ,(with-input-from-string args read))
                     (#:branch . ,branch)
                     (#:tag . ,(if (string=? tag "NULL") #f tag))
                     (#:commit . ,(if (string=? rev "NULL") #f rev))
                     (#:no-compile? . ,(positive? no-compile?)))
                   specs))))))

(define (db-add-derivation db job)
  "Store a derivation result in database DB and return its ID."
  (sqlite-exec db "\
INSERT OR IGNORE INTO Derivations (derivation, job_name, system, nix_name, evaluation)\
  VALUES ('~A', '~A', '~A', '~A', '~A');"
               (assq-ref job #:derivation)
               (assq-ref job #:job-name)
               (assq-ref job #:system)
               (assq-ref job #:nix-name)
               (assq-ref job #:eval-id)))

(define (db-get-derivation db id)
  "Retrieve a job in database DB which corresponds to ID."
  (car (sqlite-exec db "SELECT * FROM Derivations WHERE derivation='~A';" id)))

(define (db-add-evaluation db eval)
  (sqlite-exec db "\
INSERT INTO Evaluations (specification, revision) VALUES ('~A', '~A');"
               (assq-ref eval #:specification)
               (assq-ref eval #:revision))
  (last-insert-rowid db))

(define-syntax-rule (with-database db body ...)
  "Run BODY with a connection to the database which is bound to DB in BODY."
  ;; XXX: We don't install an unwind handler to play well with delimited
  ;; continuations and fibers.  But as a consequence, we leak DB when BODY
  ;; raises an exception.
  (let* ((db (db-open))
         (result (begin body ...)))
    (db-close db)
    result))

(define* (read-quoted-string #:optional (port (current-input-port)))
  "Read all of the characters out of PORT and return them as a SQL quoted
string."
  (let loop ((chars '()))
    (let ((char (read-char port)))
      (cond ((eof-object? char) (list->string (reverse! chars)))
            ((char=? char #\')  (loop (cons* char char chars)))
            (else (loop (cons char chars)))))))

;; Extended error codes (see <sqlite3.h>).
;; XXX: This should be defined by (sqlite3).
(define SQLITE_CONSTRAINT 19)
(define SQLITE_CONSTRAINT_PRIMARYKEY
  (logior SQLITE_CONSTRAINT (ash 6 8)))

(define-enumeration build-status
  ;; Build status as expected by Hydra's API.  Note: the negative values are
  ;; Cuirass' own extensions.
  (scheduled        -2)
  (started          -1)
  (succeeded         0)
  (failed            1)
  (failed-dependency 2)
  (failed-other      3)
  (canceled          4))

(define (db-add-build db build)
  "Store BUILD in database DB. BUILD eventual outputs are stored
in the OUTPUTS table."
  (let* ((build-exec
          (sqlite-exec db "\
INSERT INTO Builds (derivation, evaluation, log, status, timestamp, starttime, stoptime)\
  VALUES ('~A', '~A', '~A', '~A', '~A', '~A', '~A');"
                       (assq-ref build #:derivation)
                       (assq-ref build #:eval-id)
                       (assq-ref build #:log)
                       (or (assq-ref build #:status)
                           (build-status scheduled))
                       (or (assq-ref build #:timestamp) 0)
                       (or (assq-ref build #:starttime) 0)
                       (or (assq-ref build #:stoptime) 0)))
         (build-id (last-insert-rowid db)))
    (for-each (lambda (output)
                (match output
                  ((name . path)
                   (sqlite-exec db "\
INSERT INTO Outputs (build, name, path) VALUES ('~A', '~A', '~A');"
                                build-id name path))))
              (assq-ref build #:outputs))
    build-id))

(define* (db-update-build-status! db drv status #:key log-file)
  "Update DB so that DRV's status is STATUS.  This also updates the
'starttime' or 'stoptime' fields.  If LOG-FILE is true, record it as the build
log file for DRV."
  (define now
    (time-second (current-time time-utc)))

  (if (= status (build-status started))
      (sqlite-exec db "UPDATE Builds SET starttime='~A', status='~A' \
WHERE derivation='~A';"
                   now status drv)
      (sqlite-exec db "UPDATE Builds SET stoptime='~A', \
status='~A'~@[, log='~A'~] WHERE derivation='~A';"
                   now status log-file drv)))

(define (db-get-outputs db build-id)
  "Retrieve the OUTPUTS of the build identified by BUILD-ID in DB database."
  (let loop ((rows
              (sqlite-exec db "SELECT name, path FROM Outputs WHERE build='~A';"
                           build-id))
             (outputs '()))
    (match rows
      (() outputs)
      ((#(name path)
        . rest)
       (loop rest
             (cons `(,name . ((#:path . ,path)))
                   outputs))))))

(define db-build-request "\
SELECT Builds.id, Builds.timestamp, Builds.starttime, Builds.stoptime, Builds.log, Builds.status, Builds.derivation,\
Derivations.job_name, Derivations.system, Derivations.nix_name,\
Specifications.repo_name, Specifications.branch \
FROM Builds \
INNER JOIN Derivations ON Builds.derivation = Derivations.derivation and Builds.evaluation = Derivations.evaluation \
INNER JOIN Evaluations ON Derivations.evaluation = Evaluations.id \
INNER JOIN Specifications ON Evaluations.specification = Specifications.repo_name")

(define (db-format-build db build)
  (match build
    (#(id timestamp starttime stoptime log status derivation job-name system
          nix-name repo-name branch)
     `((#:id         . ,id)
       (#:timestamp  . ,timestamp)
       (#:starttime  . ,starttime)
       (#:stoptime   . ,stoptime)
       (#:log        . ,log)
       (#:status     . ,status)
       (#:derivation . ,derivation)
       (#:job-name   . ,job-name)
       (#:system     . ,system)
       (#:nix-name   . ,nix-name)
       (#:repo-name  . ,repo-name)
       (#:outputs    . ,(db-get-outputs db id))
       (#:branch     . ,branch)))))

(define (db-get-build db id)
  "Retrieve a build in database DB which corresponds to ID."
  (let ((res (sqlite-exec db (string-append db-build-request
                                            " WHERE Builds.id='~A';") id)))
    (match res
      ((build)
       (db-format-build db build))
      (() #f))))

(define (db-get-builds db filters)
  "Retrieve all builds in database DB which are matched by given FILTERS.
FILTERS is an assoc list which possible keys are 'project | 'jobset | 'job |
'system | 'nr | 'order | 'status."

  (define (format-where-clause filters)
    (let ((where-clause
           (filter-map
            (lambda (param)
              (match param
                (('project project)
                 (format #f "Specifications.repo_name='~A'" project))
                (('jobset jobset)
                 (format #f "Specifications.branch='~A'" jobset))
                (('job job)
                 (format #f "Derivations.job_name='~A'" job))
                (('system system)
                 (format #f "Derivations.system='~A'" system))
                (('status 'done)
                 "Builds.status >= 0")
                (('status 'pending)
                 "Builds.status < 0")
                (_ #f)))
            filters)))
      (if (> (length where-clause) 0)
          (string-append
           "WHERE "
           (string-join where-clause " AND "))
          "")))

  (define (format-order-clause filters)
    (or (any (match-lambda
               (('order 'build-id)
                "ORDER BY Builds.id ASC")
               (('order 'decreasing-build-id)
                "ORDER BY Builds.id DESC")
               (_ #f))
             filters)
        "ORDER BY Builds.id DESC"))               ;default order

  (define (format-limit-clause filters)
    (or (any (match-lambda
               (('nr number)
                (format #f "LIMIT '~A'" number))
               (_ #f))
             filters)
        ""))

  (let loop ((rows
              (sqlite-exec db (string-append
                               db-build-request
                               " "
                               (format-where-clause filters)
                               " "
                               (format-order-clause filters)
                               " "
                               (format-limit-clause filters)
                               ";")))
             (outputs '()))
    (match rows
      (()
       (reverse outputs))
      ((row . rest)
       (loop rest
             (cons (db-format-build db row) outputs))))))

(define (db-get-stamp db spec)
  "Return a stamp corresponding to specification SPEC in database DB."
  (let ((res (sqlite-exec db "SELECT * FROM Stamps WHERE specification='~A';"
                          (assq-ref spec #:name))))
    (match res
      (() "")
      ((#(spec commit)) commit))))

(define (db-add-stamp db spec commit)
  "Associate stamp COMMIT to specification SPEC in database DB."
  (if (string-null? (db-get-stamp db spec))
      (sqlite-exec db "\
INSERT INTO Stamps (specification, stamp) VALUES ('~A', '~A');"
                   (assq-ref spec #:name)
                   commit)
      (sqlite-exec db "\
UPDATE Stamps SET stamp='~A' WHERE specification='~A';"
                   commit
                   (assq-ref spec #:name))))
