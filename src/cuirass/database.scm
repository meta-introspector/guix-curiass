;;; database.scm -- store evaluation and build results
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Tatiana Sholokhova <tanja201396@gmail.com>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (sqlite3)
  #:export (;; Procedures.
            db-init
            db-open
            db-close
            db-add-specification
            db-remove-specification
            db-get-specifications
            db-add-evaluation
            db-set-evaluations-done
            db-set-evaluation-done
            db-get-pending-derivations
            build-status
            db-add-build
            db-update-build-status!
            db-get-output
            db-get-build
            db-get-builds
            db-get-builds-by-search
            db-get-builds-min
            db-get-builds-max
            db-get-builds-query-min
            db-get-builds-query-max
            db-add-event
            db-get-events
            db-delete-events-with-ids-<=-to
            db-get-evaluations
            db-get-evaluations-build-summary
            db-get-evaluations-id-min
            db-get-evaluations-id-max
            db-get-evaluation-specification
            db-get-evaluation-summary
            read-sql-file
            read-quoted-string
            sqlite-exec
            ;; Parameters.
            %package-database
            %package-schema-file
            %db-channel
            %record-events?
            ;; Macros.
            with-db-worker-thread
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

(define-syntax catch-sqlite-error
  (syntax-rules (on =>)
    "Run EXP..., catching SQLite error and handling the given code as
specified."
    ((_ exp ... (on error => handle ...))
     (catch 'sqlite-error
       (lambda ()
         exp ...)
       (lambda (key who code message . rest)
         (if (= code error)
             (begin handle ...)
             (apply throw key who code message rest)))))))

(define %package-database
  ;; Define to the database file name of this package.
  (make-parameter (string-append %localstatedir "/lib/" %package
                                 "/" %package ".db")))

(define %package-schema-file
  ;; Define to the database schema file of this package.
  (make-parameter (string-append (or (getenv "CUIRASS_DATADIR")
                                     (string-append %datadir "/" %package))
                                 "/schema.sql")))

(define %package-sql-dir
  ;; Define to the directory containing the SQL files.
  (make-parameter (string-append (or (getenv "CUIRASS_DATADIR")
                                     (string-append %datadir "/" %package))
                                 "/sql")))

(define %db-channel
  (make-parameter #f))

(define %record-events?
  (make-parameter #f))

(define-syntax-rule (with-db-worker-thread db exp ...)
  "Evaluate EXP... in the critical section corresponding to %DB-CHANNEL.
DB is bound to the argument of that critical section: the database
connection."
  (call-with-worker-thread (%db-channel)
                           (lambda (db) exp ...)))

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

(define (set-db-options db)
  "Set various options for DB and return it."

  ;; Turn DB in "write-ahead log" mode and return it.
  (sqlite-exec db "PRAGMA journal_mode=WAL;")

  ;; Install a busy handler such that, when the database is locked, sqlite
  ;; retries until 30 seconds have passed, at which point it gives up and
  ;; throws SQLITE_BUSY.  This is useful when we have several fibers or
  ;; threads accessing the database concurrently.
  ;;(sqlite-busy-timeout db (* 30 1000))
  (sqlite-exec db "PRAGMA busy_timeout = 30000;")

  db)

(define (db-load db schema)
  "Evaluate the file SCHEMA, which may contain SQL queries, into DB."
  (for-each (cut sqlite-exec db <>)
            (read-sql-file schema)))

(define (db-schema-version db)
  (vector-ref (car (sqlite-exec db "PRAGMA user_version;")) 0))

(define (db-set-schema-version db version)
  (sqlite-exec db (format #f "PRAGMA user_version = ~d;" version)))

(define (latest-db-schema-version)
  "Return the version to which the schema should be upgraded, based on the
upgrade-n.sql files, or 0 if there are no such files."
  (reduce max 0
          (map (compose string->number (cut match:substring <> 1))
               (filter-map (cut string-match "^upgrade-([0-9]+)\\.sql$" <>)
                           (or (scandir (%package-sql-dir)) '())))))

(define* (db-init #:optional (db-name (%package-database))
                  #:key (schema (%package-schema-file)))
  "Open the database to store and read jobs and builds informations.  Return a
database object."
  (when (file-exists? db-name)
    (format (current-error-port) "Removing leftover database ~a~%" db-name)
    (delete-file db-name))
  (let ((db (sqlite-open db-name (logior SQLITE_OPEN_CREATE
                                         SQLITE_OPEN_READWRITE
                                         SQLITE_OPEN_NOMUTEX))))
    (db-load db schema)
    (db-set-schema-version db (latest-db-schema-version))
    db))

(define (schema-upgrade-file version)
  "Return the file containing the SQL instructions that upgrade the schema
from VERSION-1 to VERSION."
  (in-vicinity (%package-sql-dir) (format #f "upgrade-~a.sql" version)))

(define (db-upgrade db)
  "Upgrade database DB based on its current version and the available
upgrade-n.sql files."
  (for-each (lambda (version)
              (db-load db (schema-upgrade-file version))
              (db-set-schema-version db version))
            (let ((current (db-schema-version db)))
              (iota (- (latest-db-schema-version) current) (1+ current))))
  db)

(define* (db-open #:optional (db (%package-database)))
  "Open database to store or read jobs and builds informations.  Return a
database object."
  ;; Use "write-ahead log" mode because it improves concurrency and should
  ;; avoid SQLITE_LOCKED errors when we have several readers:
  ;; <https://www.sqlite.org/wal.html>.

  ;; SQLITE_OPEN_NOMUTEX disables mutexing on database connection and prepared
  ;; statement objects, thus making us responsible for serializing access to
  ;; database connections and prepared statements.
  (set-db-options (if (file-exists? db)
                      (db-upgrade
                       (sqlite-open db (logior SQLITE_OPEN_READWRITE
                                               SQLITE_OPEN_NOMUTEX)))
                      (db-init db))))

(define (db-close db)
  "Close database object DB."
  (sqlite-close db))

(define (last-insert-rowid db)
  (vector-ref (car (sqlite-exec db "SELECT last_insert_rowid();"))
              0))

(define (changes-count db)
  "The number of database rows that were changed or inserted or deleted by the
most recently completed INSERT, DELETE, or UPDATE statement."
  (vector-ref (car (sqlite-exec db "SELECT changes();"))
              0))

(define (expect-one-row rows)
  "Several SQL queries expect one result, or zero if not found.  This gets rid
of the list, and returns #f when there is no result."
  (match rows
    ((row) row)
    (() #f)))

(define (db-add-input spec-name input)
  (with-db-worker-thread db
    (sqlite-exec db "\
INSERT OR IGNORE INTO Inputs (specification, name, url, load_path, branch, \
tag, revision, no_compile_p) VALUES ("
                 spec-name ", "
                 (assq-ref input #:name) ", "
                 (assq-ref input #:url) ", "
                 (assq-ref input #:load-path) ", "
                 (assq-ref input #:branch) ", "
                 (assq-ref input #:tag) ", "
                 (assq-ref input #:commit) ", "
                 (if (assq-ref input #:no-compile?) 1 0) ");")))

(define (db-add-checkout spec-name eval-id checkout)
  "Insert CHECKOUT associated with SPEC-NAME and EVAL-ID.  If a checkout with
the same revision already exists for SPEC-NAME, return #f."
  (with-db-worker-thread db
    (catch-sqlite-error
     (sqlite-exec db "\
INSERT INTO Checkouts (specification, revision, evaluation, input,
directory) VALUES ("
                  spec-name ", "
                  (assq-ref checkout #:commit) ", "
                  eval-id ", "
                  (assq-ref checkout #:input) ", "
                  (assq-ref checkout #:directory) ");")
     (last-insert-rowid db)

     ;; If we get a unique-constraint-failed error, that means we have
     ;; already inserted the same checkout.  That happens for each input
     ;; that doesn't change between two evaluations.
     (on SQLITE_CONSTRAINT_PRIMARYKEY => #f))))

(define (db-add-specification spec)
  "Store SPEC in database the database.  SPEC inputs are stored in the INPUTS
table."
  (with-db-worker-thread db
    (sqlite-exec db "\
INSERT OR IGNORE INTO Specifications (name, load_path_inputs, \
package_path_inputs, proc_input, proc_file, proc, proc_args) \
  VALUES ("
                 (assq-ref spec #:name) ", "
                 (assq-ref spec #:load-path-inputs) ", "
                 (assq-ref spec #:package-path-inputs) ", "
                 (assq-ref spec #:proc-input) ", "
                 (assq-ref spec #:proc-file) ", "
                 (symbol->string (assq-ref spec #:proc)) ", "
                 (assq-ref spec #:proc-args) ");")
    (let ((spec-id (last-insert-rowid db)))
      (for-each (lambda (input)
                  (db-add-input (assq-ref spec #:name) input))
                (assq-ref spec #:inputs))
      spec-id)))

(define (db-remove-specification name)
  "Remove the specification matching NAME from the database and its inputs."
  (with-db-worker-thread db
    (sqlite-exec db "BEGIN TRANSACTION;")
    (sqlite-exec db "\
DELETE FROM Inputs WHERE specification=" name ";")
    (sqlite-exec db "\
DELETE FROM Specifications WHERE name=" name ";")
    (sqlite-exec db "COMMIT;")))

(define (db-get-inputs spec-name)
  (with-db-worker-thread db
    (let loop ((rows (sqlite-exec
                      db "SELECT * FROM Inputs WHERE specification="
                      spec-name ";"))
               (inputs '()))
      (match rows
        (() inputs)
        ((#(specification name url load-path branch tag revision no-compile-p)
           . rest)
         (loop rest
               (cons `((#:name . ,name)
                       (#:url . ,url)
                       (#:load-path . ,load-path)
                       (#:branch . ,branch)
                       (#:tag . ,tag)
                       (#:commit . ,revision)
                       (#:no-compile? . ,(positive? no-compile-p)))
                     inputs)))))))

(define (db-get-specifications)
  (with-db-worker-thread db
    (let loop ((rows  (sqlite-exec db "SELECT * FROM Specifications ORDER BY name DESC;"))
               (specs '()))
      (match rows
        (() specs)
        ((#(name load-path-inputs package-path-inputs proc-input proc-file proc
                 proc-args)
           . rest)
         (loop rest
               (cons `((#:name . ,name)
                       (#:load-path-inputs .
                                           ,(with-input-from-string load-path-inputs read))
                       (#:package-path-inputs .
                                              ,(with-input-from-string package-path-inputs read))
                       (#:proc-input . ,proc-input)
                       (#:proc-file . ,proc-file)
                       (#:proc . ,(with-input-from-string proc read))
                       (#:proc-args . ,(with-input-from-string proc-args read))
                       (#:inputs . ,(db-get-inputs name)))
                     specs)))))))

(define (db-add-evaluation spec-name checkouts)
  "Add a new evaluation for SPEC-NAME only if one of the CHECKOUTS is new.
Otherwise, return #f."
  (with-db-worker-thread db
    (sqlite-exec db "BEGIN TRANSACTION;")
    (sqlite-exec db "INSERT INTO Evaluations (specification, in_progress)
VALUES (" spec-name ", true);")
    (let* ((eval-id (last-insert-rowid db))
           (new-checkouts (filter-map
                           (cut db-add-checkout spec-name eval-id <>)
                           checkouts)))
      (if (null? new-checkouts)
          (begin (sqlite-exec db "ROLLBACK;")
                 #f)
          (begin (db-add-event 'evaluation
                               (time-second (current-time time-utc))
                               `((#:evaluation    . ,eval-id)
                                 (#:specification . ,spec-name)
                                 (#:in_progress   . #t)))
                 (sqlite-exec db "COMMIT;")
                 eval-id)))))

(define (db-set-evaluations-done)
  (with-db-worker-thread db
    (sqlite-exec db "UPDATE Evaluations SET in_progress = false;")))

(define (db-set-evaluation-done eval-id)
  (with-db-worker-thread db
    (sqlite-exec db "UPDATE Evaluations SET in_progress = false
WHERE id = " eval-id ";")
    (db-add-event 'evaluation
                  (time-second (current-time time-utc))
                  `((#:evaluation  . ,eval-id)
                    (#:in_progress . #f)))))

(define-syntax-rule (with-database body ...)
  "Run BODY with %DB-CHANNEL being dynamically bound to a channel providing a
worker thread that allows database operations to run without intefering with
fibers."
  (parameterize ((%db-channel (make-worker-thread-channel
                               (lambda ()
                                 (list (db-open))))))
    body ...))

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
(define SQLITE_CONSTRAINT_UNIQUE
  (logior SQLITE_CONSTRAINT (ash 8 8)))

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

(define (db-add-output derivation output)
  "Insert OUTPUT associated with DERIVATION.  If an output with the same path
already exists, return #f."
  (with-db-worker-thread db
    (catch-sqlite-error
     (match output
       ((name . path)
        (sqlite-exec db "\
INSERT INTO Outputs (derivation, name, path) VALUES ("
                     derivation ", " name ", " path ");")))
     (last-insert-rowid db)

     ;; If we get a unique-constraint-failed error, that means we have
     ;; already inserted the same output.  That happens with fixed-output
     ;; derivations.
     (on SQLITE_CONSTRAINT_PRIMARYKEY => #f))))

(define (db-add-build build)
  "Store BUILD in database the database only if one of its outputs is new.
Return #f otherwise.  BUILD outputs are stored in the OUTPUTS table."
  (with-db-worker-thread db
    (catch-sqlite-error
     (sqlite-exec db "BEGIN TRANSACTION;")
     (sqlite-exec db "
INSERT INTO Builds (derivation, evaluation, job_name, system, nix_name, log,
status, timestamp, starttime, stoptime)
VALUES ("
                  (assq-ref build #:derivation) ", "
                  (assq-ref build #:eval-id) ", "
                  (assq-ref build #:job-name) ", "
                  (assq-ref build #:system) ", "
                  (assq-ref build #:nix-name) ", "
                  (assq-ref build #:log) ", "
                  (or (assq-ref build #:status)
                      (build-status scheduled)) ", "
                  (or (assq-ref build #:timestamp) 0) ", "
                  (or (assq-ref build #:starttime) 0) ", "
                  (or (assq-ref build #:stoptime) 0) ");")
     (let* ((derivation (assq-ref build #:derivation))
            (outputs (assq-ref build #:outputs))
            (new-outputs (filter-map (cut db-add-output derivation <>)
                                     outputs)))
       (if (null? new-outputs)
           (begin (sqlite-exec db "ROLLBACK;")
                  #f)
           (begin (db-add-event 'build
                                (assq-ref build #:timestamp)
                                `((#:derivation . ,(assq-ref build #:derivation))
                                  ;; TODO Ideally this would use the value
                                  ;; from build, with a default of scheduled,
                                  ;; but it's hard to convert to the symbol,
                                  ;; so just hard code scheduled for now.
                                  (#:event       . scheduled)))
                  (sqlite-exec db "COMMIT;")
                  derivation)))

     ;; If we get a unique-constraint-failed error, that means we have
     ;; already inserted the same build.  That happens when several jobs
     ;; produce the same derivation, and we can ignore it.
     (on SQLITE_CONSTRAINT_UNIQUE
         =>
         (sqlite-exec db "ROLLBACK;") #f))))

(define* (db-update-build-status! drv status #:key log-file)
  "Update the database so that DRV's status is STATUS.  This also updates the
'starttime' or 'stoptime' fields.  If LOG-FILE is true, record it as the build
log file for DRV."
  (define now
    (time-second (current-time time-utc)))

  (define status-names
    `((,(build-status succeeded)         . "succeeded")
      (,(build-status failed)            . "failed")
      (,(build-status failed-dependency) . "failed (dependency)")
      (,(build-status failed-other)      . "failed (other)")
      (,(build-status canceled)          . "canceled")))

  (with-db-worker-thread db
    (if (= status (build-status started))
        (begin
          (sqlite-exec db "UPDATE Builds SET starttime=" now ", status="
                       status "WHERE derivation=" drv ";")
          (db-add-event 'build
                        now
                        `((#:derivation . ,drv)
                          (#:event      . started))))

        ;; Update only if we're switching to a different status; otherwise
        ;; leave things unchanged.  This ensures that 'stoptime' remains valid
        ;; and doesn't change every time we mark DRV as 'succeeded' several
        ;; times in a row, for instance.
        (begin
          (if log-file
              (sqlite-exec db "UPDATE Builds SET stoptime=" now
                           ", status=" status ", log=" log-file
                           "WHERE derivation=" drv "AND status != " status ";")
              (sqlite-exec db "UPDATE Builds SET stoptime=" now
                           ", status=" status
                           "WHERE derivation=" drv " AND status != " status
                           ";"))
          (when (positive? (changes-count db))
            (db-add-event 'build
                          now
                          `((#:derivation . ,drv)
                            (#:event      . ,(assq-ref status-names
                                                       status)))))))))

(define (db-get-output path)
  "Retrieve the OUTPUT for PATH."
  (with-db-worker-thread db
    ;; There isn't a unique index on path, but because Cuirass avoids adding
    ;; derivations which introduce the same outputs, there should only be one
    ;; result.
    (match (sqlite-exec db "SELECT derivation, name FROM Outputs
WHERE path =" path "
LIMIT 1;")
      (() #f)
      ((#(derivation name))
       `((#:derivation . ,derivation)
         (#:name . ,name))))))

(define (db-get-outputs derivation)
  "Retrieve the OUTPUTS of the build identified by DERIVATION in the
database."
  (with-db-worker-thread db
    (let loop ((rows
                (sqlite-exec db "SELECT name, path FROM Outputs
WHERE derivation =" derivation ";"))
               (outputs '()))
      (match rows
        (() outputs)
        ((#(name path)
           . rest)
         (loop rest
               (cons `(,name . ((#:path . ,path)))
                     outputs)))))))

(define (filters->order filters)
  (match (assq 'order filters)
    (('order . 'build-id) "rowid ASC")
    (('order . 'decreasing-build-id) "rowid DESC")
    (('order . 'finish-time) "stoptime DESC")
    (('order . 'finish-time+build-id) "stoptime DESC, rowid DESC")
    (('order . 'start-time) "starttime DESC")
    (('order . 'submission-time) "timestamp DESC")
    ;; With this order, builds in 'running' state (-1) appear
    ;; before those in 'scheduled' state (-2).
    (('order . 'status+submission-time) "status DESC, timestamp DESC")
    (_ "rowid DESC")))

(define (query->bind-arguments query-string)
  "Return a list of keys to query strings by parsing QUERY-STRING."
  (let ((args (append-map
               (lambda (token)
                 (match (string-split token #\:)
                   (("system" system)
                    `(#:system ,system))
                   (("spec" spec)
                    `(#:spec ,spec))
                   ((_ invalid) '())    ; ignore
                   ((query)
                    `(#:query
                      ,(fold
                        (lambda (transform val)
                          (match transform
                            ((pred modify-true modify-false)
                             ((if (pred val) modify-true modify-false) val))))
                        query
                        ;; Process special characters ^ and $.
                        (list (list (cut string-prefix? "^" <>)
                                    (cut string-drop <> 1)
                                    (cut string-append "%" <>))
                              (list (cut string-suffix? "$" <>)
                                    (cut string-drop-right <> 1)
                                    (cut string-append <> "%"))))))))
               (string-tokenize query-string))))
    ;; Normalize arguments
    (fold (lambda (key acc)
            (if (member key acc)
                acc
                (append (list key #f) acc)))
          args '(#:spec #:system))))

(define (db-get-builds-by-search filters)
  "Retrieve all builds in the database which are matched by given FILTERS.
FILTERS is an assoc list whose possible keys are the symbols query,
border-low-id, border-high-id, and nr."
  (with-db-worker-thread db
    (let* ((stmt-text (format #f "SELECT * FROM (
SELECT Builds.rowid, Builds.timestamp, Builds.starttime,
Builds.stoptime, Builds.log, Builds.status, Builds.job_name, Builds.system,
Builds.nix_name, Specifications.name
FROM Builds
INNER JOIN Evaluations ON Builds.evaluation = Evaluations.id
INNER JOIN Specifications ON Evaluations.specification = Specifications.name
WHERE (Builds.nix_name LIKE :query)
AND (:spec IS NULL
 OR (Specifications.name = :spec))
AND (:system IS NULL
 OR (Builds.system = :system))
AND (:borderlowid IS NULL
 OR (:borderlowid < Builds.rowid))
AND (:borderhighid IS NULL
 OR (:borderhighid > Builds.rowid))
ORDER BY
CASE WHEN :borderlowid IS NULL THEN Builds.rowid
                               ELSE -Builds.rowid
END DESC
LIMIT :nr)
ORDER BY rowid DESC;"))
           (stmt (sqlite-prepare db stmt-text #:cache? #t)))
      (apply sqlite-bind-arguments
             stmt
             (append (list
                      #:borderlowid (assq-ref filters 'border-low-id)
                      #:borderhighid (assq-ref filters 'border-high-id)
                      #:nr (match (assq-ref filters 'nr)
                             (#f -1)
                             (x x)))
                     (query->bind-arguments (assq-ref filters 'query))))
      (sqlite-reset stmt)
      (let loop ((rows (sqlite-fold-right cons '() stmt))
                 (builds '()))
        (match rows
          (() (reverse builds))
          ((#(id timestamp starttime stoptime log status job-name
                 system nix-name specification) . rest)
           (loop rest
                 (cons `((#:id . ,id)
                         (#:timestamp . ,timestamp)
                         (#:starttime . ,starttime)
                         (#:stoptime . ,stoptime)
                         (#:log . ,log)
                         (#:status . ,status)
                         (#:job-name . ,job-name)
                         (#:system . ,system)
                         (#:nix-name . ,nix-name)
                         (#:specification . ,specification))
                       builds))))))))

(define (db-get-builds filters)
  "Retrieve all builds in the database which are matched by given FILTERS.
FILTERS is an assoc list whose possible keys are 'derivation | 'id | 'jobset |
'job | 'system | 'nr | 'order | 'status | 'evaluation."
  (with-db-worker-thread db
    (let* ((order (filters->order filters))
           (stmt-text (format #f "SELECT * FROM (
SELECT Builds.derivation, Builds.rowid, Builds.timestamp, Builds.starttime,
Builds.stoptime, Builds.log, Builds.status, Builds.job_name, Builds.system,
Builds.nix_name, Specifications.name
FROM Builds
INNER JOIN Evaluations ON Builds.evaluation = Evaluations.id
INNER JOIN Specifications ON Evaluations.specification = Specifications.name
WHERE (:id IS NULL OR (:id = Builds.rowid))
AND (:derivation IS NULL OR (:derivation = Builds.derivation))
AND (:jobset IS NULL OR (:jobset = Specifications.name))
AND (:job IS NULL OR (:job = Builds.job_name))
AND (:system IS NULL OR (:system = Builds.system))
AND (:evaluation IS NULL OR (:evaluation = Builds.evaluation))
AND (:status IS NULL OR (:status = 'done' AND Builds.status >= 0)
                     OR (:status = 'pending' AND Builds.status < 0)
                     OR (:status = 'succeeded' AND Builds.status = 0)
                     OR (:status = 'failed' AND Builds.status > 0))
AND (:borderlowtime IS NULL OR :borderlowid IS NULL
 OR ((:borderlowtime, :borderlowid) < (Builds.stoptime, Builds.rowid)))
AND (:borderhightime IS NULL OR :borderhighid IS NULL
 OR ((:borderhightime, :borderhighid) > (Builds.stoptime, Builds.rowid)))
ORDER BY
CASE WHEN :borderlowtime IS NULL
       OR :borderlowid IS NULL THEN Builds.stoptime
                               ELSE -Builds.stoptime
END DESC,
CASE WHEN :borderlowtime IS NULL
       OR :borderlowid IS NULL THEN Builds.rowid
                               ELSE -Builds.rowid
END DESC
LIMIT :nr)
ORDER BY ~a, rowid ASC;" order))
           (stmt (sqlite-prepare db stmt-text #:cache? #t)))
      (sqlite-bind-arguments
       stmt
       #:derivation (assq-ref filters 'derivation)
       #:id (assq-ref filters 'id)
       #:jobset (assq-ref filters 'jobset)
       #:job (assq-ref filters 'job)
       #:evaluation (assq-ref filters 'evaluation)
       #:system (assq-ref filters 'system)
       #:status (and=> (assq-ref filters 'status) object->string)
       #:borderlowid (assq-ref filters 'border-low-id)
       #:borderhighid (assq-ref filters 'border-high-id)
       #:borderlowtime (assq-ref filters 'border-low-time)
       #:borderhightime (assq-ref filters 'border-high-time)
       #:nr (match (assq-ref filters 'nr)
              (#f -1)
              (x x)))
      (sqlite-reset stmt)
      (let loop ((rows (sqlite-fold-right cons '() stmt))
                 (builds '()))
        (match rows
          (() (reverse builds))
          ((#(derivation id timestamp starttime stoptime log status job-name
                         system nix-name specification) . rest)
           (loop rest
                 (cons `((#:derivation . ,derivation)
                         (#:id . ,id)
                         (#:timestamp . ,timestamp)
                         (#:starttime . ,starttime)
                         (#:stoptime . ,stoptime)
                         (#:log . ,log)
                         (#:status . ,status)
                         (#:job-name . ,job-name)
                         (#:system . ,system)
                         (#:nix-name . ,nix-name)
                         (#:specification . ,specification)
                         (#:outputs . ,(db-get-outputs derivation)))
                       builds))))))))

(define (db-get-build derivation-or-id)
  "Retrieve a build in the database which corresponds to DERIVATION-OR-ID."
  (with-db-worker-thread db
    (let ((key (if (number? derivation-or-id) 'id 'derivation)))
      (expect-one-row (db-get-builds `((,key . ,derivation-or-id)))))))

(define (db-add-event type timestamp details)
  (when (%record-events?)
    (with-db-worker-thread db
      (sqlite-exec db "\
INSERT INTO Events (type, timestamp, event_json) VALUES ("
                   (symbol->string type) ", "
                   timestamp ", "
                   (object->json-string details)
                   ");")
      #t)))

(define (db-get-events filters)
  (with-db-worker-thread db
    (let* ((stmt-text "\
SELECT Events.id,
       Events.type,
       Events.timestamp,
       Events.event_json
FROM Events
WHERE (:type IS NULL OR (:type = Events.type))
  AND (:borderlowtime IS NULL OR
       :borderlowid IS NULL OR
       ((:borderlowtime, :borderlowid) <
        (Events.timestamp, Events.id)))
  AND (:borderhightime IS NULL OR
       :borderhighid IS NULL OR
       ((:borderhightime, :borderhighid) >
        (Events.timestamp, Events.id)))
ORDER BY Events.id ASC
LIMIT :nr;")
           (stmt (sqlite-prepare db stmt-text #:cache? #t)))
      (sqlite-bind-arguments
       stmt
       #:type (and=> (assq-ref filters 'type)
                     symbol->string)
       #:nr (match (assq-ref filters 'nr)
              (#f -1)
              (x x)))
      (sqlite-reset stmt)
      (let loop ((rows (sqlite-fold-right cons '() stmt))
                 (events '()))
        (match rows
          (() (reverse events))
          ((#(id type timestamp event_json) . rest)
           (loop rest
                 (cons `((#:id . ,id)
                         (#:type . ,type)
                         (#:timestamp . ,timestamp)
                         (#:event_json . ,event_json))
                       events))))))))

(define (db-delete-events-with-ids-<=-to id)
  (with-db-worker-thread db
    (sqlite-exec
     db
     "DELETE FROM Events WHERE id <= " id ";")))

(define (db-get-pending-derivations)
  "Return the list of derivation file names corresponding to pending builds in
the database.  The returned list is guaranteed to not have any duplicates."
  (with-db-worker-thread db
    (map (match-lambda (#(drv) drv))
         (sqlite-exec db "
SELECT derivation FROM Builds WHERE Builds.status < 0;"))))

(define (db-get-checkouts eval-id)
  (with-db-worker-thread db
    (let loop ((rows (sqlite-exec
                      db "SELECT revision, input, directory FROM Checkouts
WHERE evaluation =" eval-id ";"))
               (checkouts '()))
      (match rows
        (() checkouts)
        ((#(revision input directory)
           . rest)
         (loop rest
               (cons `((#:commit . ,revision)
                       (#:input . ,input)
                       (#:directory . ,directory))
                     checkouts)))))))

(define (db-get-evaluations limit)
  (with-db-worker-thread db
    (let loop ((rows  (sqlite-exec db "SELECT id, specification, in_progress
FROM Evaluations ORDER BY id DESC LIMIT " limit ";"))
               (evaluations '()))
      (match rows
        (() (reverse evaluations))
        ((#(id specification in-progress)
           . rest)
         (loop rest
               (cons `((#:id . ,id)
                       (#:specification . ,specification)
                       (#:in-progress . ,in-progress)
                       (#:checkouts . ,(db-get-checkouts id)))
                     evaluations)))))))

(define (db-get-evaluations-build-summary spec limit border-low border-high)
  (with-db-worker-thread db
    (let loop ((rows (sqlite-exec db "
SELECT E.id, E.in_progress, B.succeeded, B.failed, B.scheduled
FROM
(SELECT id, in_progress
FROM Evaluations
WHERE (specification=" spec ")
AND (" border-low "IS NULL OR (id >" border-low "))
AND (" border-high "IS NULL OR (id <" border-high "))
ORDER BY CASE WHEN " border-low "IS NULL THEN id ELSE -id END DESC
LIMIT " limit ") E
LEFT JOIN
(SELECT rowid, evaluation, SUM(status=0) as succeeded,
SUM(status>0) as failed, SUM(status<0) as scheduled
FROM Builds
GROUP BY evaluation) B
ON B.evaluation=E.id
ORDER BY E.id ASC;"))
               (evaluations '()))
      (match rows
        (() evaluations)
        ((#(id in-progress succeeded failed scheduled) . rest)
         (loop rest
               (cons `((#:id . ,id)
                       (#:in-progress . ,in-progress)
                       (#:checkouts . ,(db-get-checkouts id))
                       (#:succeeded . ,(or succeeded 0))
                       (#:failed . ,(or failed 0))
                       (#:scheduled . ,(or scheduled 0)))
                     evaluations)))))))

(define (db-get-evaluations-id-min spec)
  "Return the min id of evaluations for the given specification SPEC."
  (with-db-worker-thread db
    (let ((rows (sqlite-exec db "
SELECT MIN(id) FROM Evaluations
WHERE specification=" spec)))
      (and=> (expect-one-row rows) (cut vector-ref <> 0)))))

(define (db-get-evaluations-id-max spec)
  "Return the max id of evaluations for the given specification SPEC."
  (with-db-worker-thread db
    (let ((rows (sqlite-exec db "
SELECT MAX(id) FROM Evaluations
WHERE specification=" spec)))
      (and=> (expect-one-row rows) (cut vector-ref <> 0)))))

(define (db-get-evaluation-summary id)
  (with-db-worker-thread db
    (let ((rows (sqlite-exec db "
SELECT E.id, E.in_progress, B.total, B.succeeded, B.failed, B.scheduled
FROM
 (SELECT id, in_progress
FROM Evaluations
WHERE (id=" id ")) E
LEFT JOIN
 (SELECT rowid, evaluation, SUM(status=0) as succeeded,
SUM(status>0) as failed, SUM(status<0) as scheduled, SUM(status>-100) as total
FROM Builds
GROUP BY evaluation) B
ON B.evaluation=E.id
ORDER BY E.id ASC;")))
      (and=> (expect-one-row rows)
             (match-lambda
               (#(id in-progress total succeeded failed scheduled)
                `((#:id . ,id)
                  (#:in-progress . ,in-progress)
                  (#:total . ,(or total 0))
                  (#:succeeded . ,(or succeeded 0))
                  (#:failed . ,(or failed 0))
                  (#:scheduled . ,(or scheduled 0)))))))))

(define (db-get-builds-query-min query)
  "Return the smallest build row identifier matching QUERY."
  (with-db-worker-thread db
    (let* ((stmt-text "SELECT MIN(Builds.rowid) FROM Builds
INNER JOIN Evaluations ON Builds.evaluation = Evaluations.id
INNER JOIN Specifications ON Evaluations.specification = Specifications.name
WHERE (Builds.nix_name LIKE :query)
AND (:spec IS NULL
 OR (Specifications.name = :spec))
AND (:system IS NULL
 OR (Builds.system = :system));")
           (stmt (sqlite-prepare db stmt-text #:cache? #t)))
      (apply sqlite-bind-arguments stmt
             (query->bind-arguments query))
      (sqlite-reset stmt)
      (let ((rows (sqlite-fold-right cons '() stmt)))
        (sqlite-finalize stmt)
        (and=> (expect-one-row rows) vector->list)))))

(define (db-get-builds-query-max query)
  "Return the largest build row identifier matching QUERY."
  (with-db-worker-thread db
    (let* ((stmt-text "SELECT MAX(Builds.rowid) FROM Builds
INNER JOIN Evaluations ON Builds.evaluation = Evaluations.id
INNER JOIN Specifications ON Evaluations.specification = Specifications.name
WHERE (Builds.nix_name LIKE :query)
AND (:spec IS NULL
 OR (Specifications.name = :spec))
AND (:system IS NULL
 OR (Builds.system = :system));")
           (stmt (sqlite-prepare db stmt-text #:cache? #t)))
      (apply sqlite-bind-arguments stmt
             (query->bind-arguments query))
      (sqlite-reset stmt)
      (let ((rows (sqlite-fold-right cons '() stmt)))
        (sqlite-finalize stmt)
        (and=> (expect-one-row rows) vector->list)))))

(define (db-get-builds-min eval status)
  "Return the min build (stoptime, rowid) pair for the given evaluation EVAL
and STATUS."
  (with-db-worker-thread db
    (let ((rows (sqlite-exec db "
SELECT stoptime, MIN(rowid) FROM
(SELECT rowid, stoptime FROM Builds
WHERE evaluation=" eval "
AND stoptime = (SELECT MIN(stoptime)
FROM Builds
WHERE evaluation = " eval "
AND (" status " IS NULL OR (" status " = 'pending'
                            AND Builds.status < 0)
                        OR (" status " = 'succeeded'
                            AND Builds.status = 0)
                        OR (" status " = 'failed'
                            AND Builds.status > 0))))")))
      (and=> (expect-one-row rows) vector->list))))

(define (db-get-builds-max eval status)
  "Return the max build (stoptime, rowid) pair for the given evaluation EVAL
and STATUS."
  (with-db-worker-thread db
    (let ((rows (sqlite-exec db "
SELECT stoptime, MAX(rowid) FROM
(SELECT rowid, stoptime FROM Builds
WHERE evaluation=" eval " AND
stoptime = (SELECT MAX(stoptime)
FROM Builds
WHERE evaluation = " eval "
AND (" status " IS NULL OR (" status " = 'pending'
                            AND Builds.status < 0)
                        OR (" status " = 'succeeded'
                            AND Builds.status = 0)
                        OR (" status " = 'failed'
                            AND Builds.status > 0))))")))
      (and=> (expect-one-row rows) vector->list))))

(define (db-get-evaluation-specification eval)
  "Return specification of evaluation with id EVAL."
  (with-db-worker-thread db
    (let ((rows (sqlite-exec db "
SELECT specification FROM Evaluations
WHERE id = " eval)))
      (and=> (expect-one-row rows) (cut vector-ref <> 0)))))
