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

(define (%sqlite-exec db sql . args)
  "Evaluate the given SQL query with the given ARGS.  Return the list of
rows."
  (define (normalize arg)
    ;; Turn ARG into a string, unless it's a primitive SQL datatype.
    (if (or (null? arg) (pair? arg) (vector? arg))
        (object->string arg)
        arg))

  (let ((stmt (sqlite-prepare db sql #:cache? #t)))
    (for-each (lambda (arg index)
                (sqlite-bind stmt index (normalize arg)))
              args (iota (length args) 1))
    (let ((result (sqlite-fold-right cons '() stmt)))
      (sqlite-finalize stmt)
      result)))

(define-syntax sqlite-exec/bind
  (lambda (s)
    ;; Expand to an '%sqlite-exec' call where the query string has
    ;; interspersed question marks and the argument list is separate.
    (define (string-literal? s)
      (string? (syntax->datum s)))

    (syntax-case s ()
      ((_ db (bindings ...) tail str arg rest ...)
       #'(sqlite-exec/bind db
                           (bindings ... (str arg))
                           tail
                           rest ...))
      ((_ db (bindings ...) tail str)
       #'(sqlite-exec/bind db (bindings ...) str))
      ((_ db ((strings args) ...) tail)
       (and (every string-literal? #'(strings ...))
            (string-literal? #'tail))
       ;; Optimized case: only string literals.
       (with-syntax ((query (string-join
                             (append (syntax->datum #'(strings ...))
                                     (list (syntax->datum #'tail)))
                             "? ")))
         #'(%sqlite-exec db query args ...)))
      ((_ db ((strings args) ...) tail)
       ;; Fallback case: some of the strings aren't literals.
       #'(%sqlite-exec db (string-join (list strings ... tail) "? ")
                       args ...)))))

(define-syntax-rule (sqlite-exec db query args ...)
  "Execute the specific QUERY with the given ARGS.  Uses of 'sqlite-exec'
typically look like this:

  (sqlite-exec db \"SELECT * FROM Foo WHERE x = \"
                  x \"AND Y=\" y \";\")

References to variables 'x' and 'y' here are replaced by question marks in the
SQL query, and then 'sqlite-bind' is used to bind them.

This ensures that (1) SQL injection is impossible, and (2) the number of
question marks matches the number of arguments to bind."
  (sqlite-exec/bind db () "" query args ...))

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

(define (wal-mode db)
  "Turn DB in \"write-ahead log\" mode and return it."
  (sqlite-exec db "PRAGMA journal_mode=WAL;")
  db)

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
  ;; Use "write-ahead log" mode because it improves concurrency and should
  ;; avoid SQLITE_LOCKED errors when we have several readers:
  ;; <https://www.sqlite.org/wal.html>.
  (wal-mode (if (file-exists? db)
                (sqlite-open db SQLITE_OPEN_READWRITE)
                (db-init db))))

(define (db-close db)
  "Close database object DB."
  (sqlite-close db))

(define (last-insert-rowid db)
  (vector-ref (car (sqlite-exec db "SELECT last_insert_rowid();"))
              0))

(define (db-add-specification db spec)
  "Store specification SPEC in database DB and return its ID."
  (sqlite-exec db "\
INSERT OR IGNORE INTO Specifications (repo_name, url, load_path, file, \
                  proc, arguments, branch, tag, revision, no_compile_p) \
  VALUES ("
               (assq-ref spec #:name) ", "
               (assq-ref spec #:url) ", "
               (assq-ref spec #:load-path) ", "
               (assq-ref spec #:file) ", "
               (symbol->string (assq-ref spec #:proc)) ", "
               (assq-ref spec #:arguments) ", "
               (assq-ref spec #:branch) ", "
               (assq-ref spec #:tag) ", "
               (assq-ref spec #:commit) ", "
               (if (assq-ref spec #:no-compile?) 1 0)
               ");")
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
                     (#:tag . ,(match tag
                                 ("NULL" #f)
                                 (_      tag)))
                     (#:commit . ,(match rev
                                    ("NULL" #f)
                                    (_      rev)))
                     (#:no-compile? . ,(positive? no-compile?)))
                   specs))))))

(define (db-add-derivation db job)
  "Store a derivation result in database DB and return its ID."
  (sqlite-exec db "\
INSERT INTO Derivations (derivation, job_name, system, nix_name, evaluation)\
  VALUES ("
               (assq-ref job #:derivation) ", "
               (assq-ref job #:job-name) ", "
               (assq-ref job #:system) ", "
               (assq-ref job #:nix-name) ", "
               (assq-ref job #:eval-id) ");")
  (last-insert-rowid db))

(define (db-get-derivation db id)
  "Retrieve a job in database DB which corresponds to ID."
  (car (sqlite-exec db "SELECT * FROM Derivations WHERE derivation=" id ";")))

(define (db-add-evaluation db eval)
  (sqlite-exec db "\
INSERT INTO Evaluations (specification, revision) VALUES ("
               (assq-ref eval #:specification) ", "
               (assq-ref eval #:revision) ");")
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
  VALUES ("
                       (assq-ref build #:derivation) ", "
                       (assq-ref build #:eval-id) ", "
                       (assq-ref build #:log) ", "
                       (or (assq-ref build #:status)
                           (build-status scheduled)) ", "
                       (or (assq-ref build #:timestamp) 0) ", "
                       (or (assq-ref build #:starttime) 0) ", "
                       (or (assq-ref build #:stoptime) 0) ");"))
         (build-id (last-insert-rowid db)))
    (for-each (lambda (output)
                (match output
                  ((name . path)
                   (sqlite-exec db "\
INSERT INTO Outputs (build, name, path) VALUES ("
                                build-id ", " name ", " path ");"))))
              (assq-ref build #:outputs))
    build-id))

(define* (db-update-build-status! db drv status #:key log-file)
  "Update DB so that DRV's status is STATUS.  This also updates the
'starttime' or 'stoptime' fields.  If LOG-FILE is true, record it as the build
log file for DRV."
  (define now
    (time-second (current-time time-utc)))

  (if (= status (build-status started))
      (sqlite-exec db "UPDATE Builds SET starttime=" now ", status="
                   status "WHERE derivation=" drv ";")

      ;; Update only if we're switching to a different status; otherwise leave
      ;; things unchanged.  This ensures that 'stoptime' remains valid and
      ;; doesn't change every time we mark DRV as 'succeeded' several times in
      ;; a row, for instance.
      (if log-file
          (sqlite-exec db "UPDATE Builds SET stoptime=" now
                       ", status=" status ", log=" log-file
                       "WHERE derivation=" drv "AND status != " status ";")
          (sqlite-exec db "UPDATE Builds SET stoptime=" now
                       ", status=" status
                       "WHERE derivation=" drv " AND status != " status ";"))))

(define (db-get-outputs db build-id)
  "Retrieve the OUTPUTS of the build identified by BUILD-ID in DB database."
  (let loop ((rows
              (sqlite-exec db "SELECT name, path FROM Outputs WHERE build="
                           build-id ";"))
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
                                            " WHERE Builds.id=")
                          id ";")))
    (match res
      ((build)
       (db-format-build db build))
      (() #f))))

(define (db-get-builds db filters)
  "Retrieve all builds in database DB which are matched by given FILTERS.
FILTERS is an assoc list which possible keys are 'project | 'jobset | 'job |
'system | 'nr | 'order | 'status."

  (define (clauses->query+arguments clauses)
    ;; Given CLAUSES, return two values: a SQL query string, and a list of
    ;; arguments to bind.  Each element of CLAUSES must be either a string, or
    ;; a (SQL ARGUMENT) tuple, where SQL is a query fragment and ARGUMENT is
    ;; the argument to be bound for that fragment.
    (let loop ((clauses   clauses)
               (query     '())
               (arguments '()))
      (match clauses
        (()
         (values (string-concatenate-reverse query)
                 (reverse arguments)))
        (((? string? clause) . rest)
         (loop rest
               (cons clause query)
               arguments))
        ((((? string? clause) argument) . rest)
         (loop rest
               (cons clause query)
               (cons argument arguments))))))

  (define (where-clauses filters)
    (match (filter-map (match-lambda
                         (('project project)
                          (list "Specifications.repo_name=?" project))
                         (('jobset jobset)
                          (list "Specifications.branch=?" jobset))
                         (('job job)
                          (list "Derivations.job_name=?" job))
                         (('system system)
                          (list "Derivations.system=?" system))
                         (('status 'done)
                          "Builds.status >= 0")
                         (('status 'pending)
                          "Builds.status < 0")
                         (_ #f))
                       filters)
      (()
       '(""))
      ((clause)
       (list "WHERE " clause))
      ((clause0 rest ...)
       (cons* "WHERE " clause0
              (fold-right (lambda (clause result)
                            `(" AND " ,clause ,@result))
                          '()
                          rest)))))

  (define (order-clause filters)
    (or (any (match-lambda
               (('order 'build-id)
                "ORDER BY Builds.id ASC")
               (('order 'decreasing-build-id)
                "ORDER BY Builds.id DESC")
               (('order 'finish-time)
                "ORDER BY Builds.stoptime DESC")
               (('order 'start-time)
                "ORDER BY Builds.start DESC")
               (('order 'submission-time)
                "ORDER BY Builds.timestamp DESC")
               (('order 'status+submission-time)
                ;; With this order, builds in 'running' state (-1) appear
                ;; before those in 'scheduled' state (-2).
                "ORDER BY Builds.status DESC, Builds.timestamp DESC")
               (_ #f))
             filters)
        "ORDER BY Builds.id DESC"))               ;default order

  (define (limit-clause filters)
    (or (any (match-lambda
               (('nr number)
                (list "LIMIT ?" number))
               (_ #f))
             filters)
        ""))

  (call-with-values
      (lambda ()
        (clauses->query+arguments (append (list db-build-request " ")
                                          (where-clauses filters) '(" ")
                                          (list (order-clause filters) " ")
                                          (list (limit-clause filters) " "))))
    (lambda (sql arguments)
      (let loop ((rows    (apply %sqlite-exec db sql arguments))
                 (outputs '()))
        (match rows
          (()
           (reverse outputs))
          ((row . rest)
           (loop rest
                 (cons (db-format-build db row) outputs))))))))

(define (db-get-stamp db spec)
  "Return a stamp corresponding to specification SPEC in database DB."
  (let ((res (sqlite-exec db "SELECT * FROM Stamps WHERE specification="
                          (assq-ref spec #:name) ";")))
    (match res
      (() "")
      ((#(spec commit)) commit))))

(define (db-add-stamp db spec commit)
  "Associate stamp COMMIT to specification SPEC in database DB."
  (if (string-null? (db-get-stamp db spec))
      (sqlite-exec db "\
INSERT INTO Stamps (specification, stamp) VALUES ("
                   (assq-ref spec #:name) ", " commit ");")
      (sqlite-exec db "UPDATE Stamps SET stamp=" commit
                   "WHERE specification=" (assq-ref spec #:name) ";")))
