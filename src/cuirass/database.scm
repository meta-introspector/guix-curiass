;;; database.scm -- store evaluation and build results
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Tatiana Sholokhova <tanja201396@gmail.com>
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
            db-get-specifications
            db-add-stamp
            db-get-stamp
            db-add-evaluation
            db-add-derivation
            db-get-derivation
            db-get-pending-derivations
            build-status
            db-add-build
            db-update-build-status!
            db-get-build
            db-get-builds
            db-get-builds-min
            db-get-builds-max
            db-get-evaluations
            db-get-evaluations-build-summary
            db-get-evaluations-id-min
            db-get-evaluations-id-max
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

(define (db-add-input db spec-name input)
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
               (if (assq-ref input #:no-compile?) 1 0) ");")
  (last-insert-rowid db))

(define (db-add-specification db spec)
  "Store SPEC in database DB. SPEC inputs are stored in the INPUTS table."
  (sqlite-exec db "\
INSERT OR IGNORE INTO Specifications (name, load_path_inputs, \
package_path_inputs, proc_input, proc_file, proc, proc_args) \
  VALUES ("
               (assq-ref spec #:name) ", "
               (assq-ref spec #:load-path-inputs) ", "
               (assq-ref spec #:package-path-inputs)", "
               (assq-ref spec #:proc-input) ", "
               (assq-ref spec #:proc-file) ", "
               (symbol->string (assq-ref spec #:proc)) ", "
               (assq-ref spec #:proc-args) ");")
  (let ((spec-id (last-insert-rowid db)))
    (for-each (lambda (input)
                (db-add-input db (assq-ref spec #:name) input))
              (assq-ref spec #:inputs))
    spec-id))

(define (db-get-inputs db spec-name)
  (let loop ((rows (sqlite-exec db "SELECT * FROM Inputs WHERE specification="
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
                   inputs))))))

(define (db-get-specifications db)
  (let loop ((rows  (sqlite-exec db "SELECT * FROM Specifications;"))
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
                     (#:inputs . ,(db-get-inputs db name)))
                   specs))))))

(define (db-add-derivation db job)
  "Store a derivation result in database DB and return its ID."
  (catch 'sqlite-error
    (lambda ()
      (sqlite-exec db "\
INSERT INTO Derivations (derivation, job_name, system, nix_name, evaluation)\
  VALUES ("
                   (assq-ref job #:derivation) ", "
                   (assq-ref job #:job-name) ", "
                   (assq-ref job #:system) ", "
                   (assq-ref job #:nix-name) ", "
                   (assq-ref job #:eval-id) ");")
      (last-insert-rowid db))
    (lambda (key who code message . rest)
      ;; If we get a unique-constraint-failed error, that means we have
      ;; already inserted the same (derivation,eval-id) tuple.  That happens
      ;; when several jobs produce the same derivation, and we can ignore it.
      (if (= code SQLITE_CONSTRAINT_PRIMARYKEY)
          (sqlite-exec db "SELECT * FROM Derivations WHERE derivation="
                       (assq-ref job #:derivation) ";")
          (apply throw key who code rest)))))

(define (db-get-derivation db id)
  "Retrieve a job in database DB which corresponds to ID."
  (car (sqlite-exec db "SELECT * FROM Derivations WHERE derivation=" id ";")))

(define (db-add-evaluation db eval)
  (sqlite-exec db "\
INSERT INTO Evaluations (specification, commits) VALUES ("
               (assq-ref eval #:specification) ", "
               (string-join (assq-ref eval #:commits)) ");")
  (last-insert-rowid db))

(define-syntax-rule (with-database db body ...)
  "Run BODY with a connection to the database which is bound to DB in BODY."
  ;; XXX: We don't install an unwind handler to play well with delimited
  ;; continuations and fibers.  But as a consequence, we leak DB when BODY
  ;; raises an exception.
  (let ((db (db-open)))
    (unwind-protect body ... (db-close db))))

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

(define (db-get-builds db filters)
  "Retrieve all builds in database DB which are matched by given FILTERS.
FILTERS is an assoc list whose possible keys are 'id | 'jobset | 'job |
'system | 'nr | 'order | 'status | 'evaluation."

  (define (format-output name path)
    `(,name . ((#:path . ,path))))

  (define (cons-output name path rest)
    "If NAME and PATH are both not #f, cons them to REST.
Otherwise return REST unchanged."
    (if (and (not name) (not path))
        rest
        (cons (format-output name path) rest)))

  (define (collect-outputs repeated-builds-id repeated-row outputs rows)
    "Given rows somewhat like
1 'a 'b  2 'x
^ 'c 'd  2 'x
| ^^^^^  ^^^^
| group  ++++- group headers
| detail
+------------ group id

return rows somewhat like

1 2 'x '((a b) (c d))

.

As a special case, if the group detail is #f #f, ignore it.
This is made specifically to support LEFT JOINs.

Assumes that if group id stays the same the group headers stay the same."
    (define (finish-group)
      (match repeated-row
        (#(timestamp starttime stoptime log status derivation job-name system
                     nix-name specification)
         `((#:id         . ,repeated-builds-id)
           (#:timestamp  . ,timestamp)
           (#:starttime  . ,starttime)
           (#:stoptime   . ,stoptime)
           (#:log        . ,log)
           (#:status     . ,status)
           (#:derivation . ,derivation)
           (#:job-name   . ,job-name)
           (#:system     . ,system)
           (#:nix-name   . ,nix-name)
           (#:specification . ,specification)
           (#:outputs    . ,outputs)))))

    (define (same-group? builds-id)
      (= builds-id repeated-builds-id))

    (match rows
      (() (list (finish-group)))
      ((#((? same-group? x-builds-id) x-output-name x-output-path other-cells ...) . rest)
       ;; Accumulate group members of current group.
       (let ((outputs (cons-output x-output-name x-output-path outputs)))
         (collect-outputs repeated-builds-id repeated-row outputs rest)))
      ((#(x-builds-id x-output-name x-output-path other-cells ...) . rest)
       (cons (finish-group)                       ;finish current group

             ;; Start new group.
             (let* ((outputs (cons-output x-output-name x-output-path '()))
                    (x-repeated-row (list->vector other-cells)))
               (collect-outputs x-builds-id x-repeated-row outputs rest))))))

  (define (group-outputs rows)
    (match rows
      (() '())
      ((#(x-builds-id x-output-name x-output-path other-cells ...) . rest)
       (let ((x-repeated-row (list->vector other-cells)))
         (collect-outputs x-builds-id x-repeated-row '() rows)))))

  (let* ((order (match (assq 'order filters)
                  (('order . 'build-id) "id ASC")
                  (('order . 'decreasing-build-id) "id DESC")
                  (('order . 'finish-time) "stoptime DESC")
                  (('order . 'finish-time+build-id) "stoptime DESC, id DESC")
                  (('order . 'start-time) "starttime DESC")
                  (('order . 'submission-time) "timestamp DESC")
                  (('order . 'status+submission-time)
                   ;; With this order, builds in 'running' state (-1) appear
                   ;; before those in 'scheduled' state (-2).
                   "status DESC, timestamp DESC")
                  (_ "id DESC")))
         (stmt-text (format #f "SELECT * FROM (
SELECT Builds.id, Outputs.name, Outputs.path, Builds.timestamp,
Builds.starttime, Builds.stoptime, Builds.log, Builds.status,
Builds.derivation, Derivations.job_name, Derivations.system,
Derivations.nix_name,Specifications.name
FROM Builds
INNER JOIN Derivations ON Builds.derivation = Derivations.derivation
AND Builds.evaluation = Derivations.evaluation
INNER JOIN Evaluations ON Derivations.evaluation = Evaluations.id
INNER JOIN Specifications ON Evaluations.specification = Specifications.name
LEFT JOIN Outputs ON Outputs.build = Builds.id
WHERE (:id IS NULL OR (:id = Builds.id))
AND (:jobset IS NULL OR (:jobset = Specifications.name))
AND (:job IS NULL OR (:job = Derivations.job_name))
AND (:system IS NULL OR (:system = Derivations.system))
AND (:evaluation IS NULL OR (:evaluation = Builds.evaluation))
AND (:status IS NULL OR (:status = 'done' AND Builds.status >= 0)
                     OR (:status = 'pending' AND Builds.status < 0))
AND (:borderlowtime IS NULL OR :borderlowid IS NULL
 OR ((:borderlowtime, :borderlowid) < (Builds.stoptime, Builds.id)))
AND (:borderhightime IS NULL OR :borderhighid IS NULL
 OR ((:borderhightime, :borderhighid) > (Builds.stoptime, Builds.id)))
ORDER BY
CASE WHEN :borderlowtime IS NULL
       OR :borderlowid IS NULL THEN Builds.stoptime
                               ELSE -Builds.stoptime
END DESC,
CASE WHEN :borderlowtime IS NULL
       OR :borderlowid IS NULL THEN Builds.id
                               ELSE -Builds.id
END DESC
LIMIT :nr)
ORDER BY ~a, id ASC;" order))
         (stmt (sqlite-prepare db stmt-text #:cache? #t)))
    (sqlite-bind-arguments
     stmt
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
    (group-outputs (sqlite-fold-right cons '() stmt))))

(define (db-get-build db id)
  "Retrieve a build in database DB which corresponds to ID."
  (match (db-get-builds db `((id . ,id)))
    ((build)
     build)
    (() #f)))

(define (db-get-pending-derivations db)
  "Return the list of derivation file names corresponding to pending builds in
DB.  The returned list is guaranteed to not have any duplicates."
  ;; This is of course much more efficient than calling 'delete-duplicates' on
  ;; a list of results obtained without DISTINCT, both in space and time.
  ;;
  ;; Here we use a subquery so that sqlite can use two indexes instead of
  ;; creating a "TEMP B-TREE" when doing a single flat query, as "EXPLAIN
  ;; QUERY PLAN" shows.
  (map (match-lambda (#(drv) drv))
       (sqlite-exec db "
SELECT DISTINCT derivation FROM (
  SELECT Derivations.derivation FROM Derivations INNER JOIN Builds
  WHERE Derivations.derivation = Builds.derivation AND Builds.status < 0
);")))

(define (db-get-stamp db spec)
  "Return a stamp corresponding to specification SPEC in database DB."
  (let ((res (sqlite-exec db "SELECT * FROM Stamps WHERE specification="
                          (assq-ref spec #:name) ";")))
    (match res
      (() #f)
      ((#(spec stamp)) stamp))))

(define (db-add-stamp db spec stamp)
  "Associate STAMP to specification SPEC in database DB."
  (if (db-get-stamp db spec)
      (sqlite-exec db "UPDATE Stamps SET stamp=" stamp
                   "WHERE specification=" (assq-ref spec #:name) ";")
      (sqlite-exec db "\
INSERT INTO Stamps (specification, stamp) VALUES ("
                   (assq-ref spec #:name) ", " stamp ");")))

(define (db-get-evaluations db limit)
  (let loop ((rows  (sqlite-exec db "SELECT id, specification, commits
FROM Evaluations ORDER BY id DESC LIMIT " limit ";"))
             (evaluations '()))
    (match rows
      (() (reverse evaluations))
      ((#(id specification commits)
        . rest)
       (loop rest
             (cons `((#:id . ,id)
                     (#:specification . ,specification)
                     (#:commits . ,(string-tokenize commits)))
                   evaluations))))))

(define (db-get-evaluations-build-summary db spec limit border-low border-high)
  (let loop ((rows (sqlite-exec db "
SELECT E.id, E.commits, B.succeeded, B.failed, B.scheduled
FROM
(SELECT id, commits
FROM Evaluations
WHERE (specification=" spec ")
AND (" border-low "IS NULL OR (id >" border-low "))
AND (" border-high "IS NULL OR (id <" border-high "))
ORDER BY CASE WHEN " border-low "IS NULL THEN id ELSE -id END DESC
LIMIT " limit ") E
LEFT JOIN
(SELECT id, evaluation, SUM(status=0) as succeeded,
SUM(status>0) as failed, SUM(status<0) as scheduled
FROM Builds
GROUP BY evaluation) B
ON B.evaluation=E.id
ORDER BY E.id ASC;"))
             (evaluations '()))
    (match rows
      (() evaluations)
      ((#(id commits succeeded failed scheduled) . rest)
       (loop rest
             (cons `((#:id . ,id)
                     (#:commits . ,commits)
                     (#:succeeded . ,(or succeeded 0))
                     (#:failed . ,(or failed 0))
                     (#:scheduled . ,(or scheduled 0)))
                   evaluations))))))

(define (db-get-evaluations-id-min db spec)
  "Return the min id of evaluations for the given specification SPEC."
  (let ((rows (sqlite-exec db "
SELECT MIN(id) FROM Evaluations
WHERE specification=" spec)))
    (vector-ref (car rows) 0)))

(define (db-get-evaluations-id-max db spec)
  "Return the max id of evaluations for the given specification SPEC."
  (let ((rows (sqlite-exec db "
SELECT MAX(id) FROM Evaluations
WHERE specification=" spec)))
    (vector-ref (car rows) 0)))

(define (db-get-builds-min db eval)
  "Return the min build (stoptime, id) pair for
   the given evaluation EVAL."
  (let ((rows (sqlite-exec db "
SELECT stoptime, MIN(id) FROM
(SELECT id, stoptime FROM Builds
WHERE evaluation=" eval " AND
stoptime = (SELECT MIN(stoptime)
FROM Builds WHERE evaluation=" eval "))")))
    (vector->list (car rows))))

(define (db-get-builds-max db eval)
  "Return the max build (stoptime, id) pair for
   the given evaluation EVAL."
  (let ((rows (sqlite-exec db "
SELECT stoptime, MAX(id) FROM
(SELECT id, stoptime FROM Builds
WHERE evaluation=" eval " AND
stoptime = (SELECT MAX(stoptime)
FROM Builds WHERE evaluation=" eval "))")))
    (vector->list (car rows))))
