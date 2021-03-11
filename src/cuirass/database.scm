;;; database.scm -- store evaluation and build results
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Tatiana Sholokhova <tanja201396@gmail.com>
;;; Copyright © 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (cuirass logging)
  #:use-module (cuirass config)
  #:use-module (cuirass notification)
  #:use-module (cuirass parameters)
  #:use-module (cuirass remote)
  #:use-module (cuirass specification)
  #:use-module (cuirass utils)
  #:use-module (guix channels)
  #:use-module (squee)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (;; Procedures.
            db-init
            db-open
            db-close
            exec-query/bind-params
            expect-one-row
            read-sql-file
            db-add-checkout
            db-add-specification
            db-remove-specification
            db-get-specification
            db-get-specifications
            evaluation-status
            db-add-evaluation
            db-abort-pending-evaluations
            db-set-evaluation-status
            db-set-evaluation-time
            build-status
            build-weather
            db-add-output
            db-add-build
            db-add-build-product
            db-get-output
            db-get-outputs
            db-get-time-since-previous-build
            db-get-build-percentage
            db-register-builds
            db-update-build-status!
            db-update-build-worker!
            db-restart-build!
            db-restart-evaluation!
            db-retry-evaluation!
            db-cancel-pending-builds!
            db-get-build-products
            db-get-builds-by-search
            db-get-builds
            db-get-build
            db-get-events
            db-delete-events-with-ids-<=-to
            db-get-pending-derivations
            db-get-checkouts
            db-get-evaluation
            db-get-evaluations
            db-get-evaluations-build-summary
            db-get-evaluations-id-min
            db-get-evaluations-id-max
            db-get-evaluation-summary
            db-get-builds-query-min
            db-get-builds-query-max
            db-get-builds-min
            db-get-builds-max
            db-get-evaluation-specification
            db-get-build-product-path
            db-push-notification
            db-pop-notification
            db-add-or-update-worker
            db-get-worker
            db-get-workers
            db-remove-unresponsive-workers
            db-clear-workers
            db-clear-build-queue
            ;; Parameters.
            %create-database?
            %package-database
            %package-schema-file
            %db-channel
            ;; Macros.
            exec-query/bind
            with-database
            with-db-worker-thread
            with-transaction))

;; Maximum priority for a Build or Specification.
(define max-priority 9)

(define (%exec-query db query args)
  (exec-query db query args))

(define (normalize obj)
  (if (string? obj)
      obj
      (and obj (object->string obj))))

(define-syntax %exec-query/bind
  (lambda (s)
    ;; Expand to an 'exec-query' call where the query string has
    ;; interspersed question marks and the argument list is separate.
    (define (string-literal? s)
      (string? (syntax->datum s)))

    (define (interleave a b)
      (if (null? b)
          (list (car a))
          `(,(car a) ,(car b) ,@(interleave (cdr a) (cdr b)))))

    (define (interleave-arguments str)
      (string-join
       (interleave str
                   (map (lambda (i)
                          (string-append "$"
                                         (number->string (1+ i))))
                        (iota (1- (length str)))))
       " "))

    (syntax-case s ()
      ((_ db (bindings ...) tail str arg rest ...)
       #'(%exec-query/bind db
                           (bindings ... (str arg))
                           tail
                           rest ...))
      ((_ db (bindings ...) tail str)
       #'(%exec-query/bind db (bindings ...) str))
      ((_ db ((strings args) ...) tail)
       ;; Optimized case: only string literals.
       (with-syntax ((query
                      (interleave-arguments
                       (append (syntax->datum #'(strings ...))
                               (list (syntax->datum #'tail))))))
         #'(%exec-query db query (map normalize (list args ...))))))))

(define-syntax-rule (exec-query/bind db query args ...)
  "Execute the specific QUERY with the given ARGS.  Uses of 'exec-query/bind'
typically look like this:

  (exec-query/bind db \"SELECT * FROM Foo WHERE x = \" x \"AND Y=\" y \";\")

References to variables 'x' and 'y' here are replaced by $1 and $2 in the
SQL query.

This ensures that (1) SQL injection is impossible, and (2) the number of
parameters matches the number of arguments to bind."
  (%exec-query/bind db () "" query args ...))

(define (exec-query/bind-params db query params)
  (define param-regex
    (make-regexp ":[a-zA-Z]+"))

  (define (argument-indexes arguments)
    (let loop ((res '())
               (bindings '())
               (counter 1)
               (arguments arguments))
      (if (null? arguments)
          (reverse res)
          (let* ((arg (car arguments))
                 (index (assoc-ref bindings arg)))
            (if index
                (loop (cons index res)
                      bindings
                      counter
                      (cdr arguments))
                (loop (cons counter res)
                      `((,arg . ,counter) ,@bindings)
                      (1+ counter)
                      (cdr arguments)))))))

  (let* ((args
          (reverse
           (fold-matches param-regex query
                         '() (lambda (m p)
                               (cons (match:substring m) p)))))
         (indexes (argument-indexes args))
         (proc (lambda (m)
                 (let ((index (car indexes)))
                   (set! indexes (cdr indexes))
                   (string-append "$" (number->string index)))))
         (query (regexp-substitute/global #f param-regex query
                                          'pre proc 'post))
         (params (map (lambda (arg)
                        (let ((symbol
                               (symbol->keyword
                                (string->symbol (substring arg 1)))))
                          (assoc-ref params symbol)))
                      (delete-duplicates args))))
    (exec-query db query (map normalize params))))

(define %create-database?
  (make-parameter #f))

(define %package-database
  (make-parameter #f))

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

(define-syntax-rule (with-database body ...)
  "Run BODY with %DB-CHANNEL being dynamically bound to a channel providing a
worker thread that allows database operations to run without interfering with
fibers."
  (parameterize ((%db-channel
                  (make-worker-thread-channel
                   (lambda ()
                     (list (db-open)))
                   #:parallelism
                   (min (current-processor-count) 8))))
    body ...))

(define-syntax-rule (with-db-worker-thread db exp ...)
  "Evaluate EXP... in the critical section corresponding to %DB-CHANNEL.
DB is bound to the argument of that critical section: the database connection."
  (let ((send-timeout 2)
        (receive-timeout 5)
        (caller-name (frame-procedure-name
                      (stack-ref (make-stack #t) 1))))
    (call-with-worker-thread
     (%db-channel)
     (lambda (db) exp ...)
     #:send-timeout send-timeout
     #:send-timeout-proc
     (lambda ()
       (log-message
        (format #f "No available database workers for ~a seconds."
                (number->string send-timeout))))
     #:receive-timeout receive-timeout
     #:receive-timeout-proc
     (lambda ()
       (log-message
        (format #f "Database worker unresponsive for ~a seconds (~a)."
                (number->string receive-timeout)
                caller-name))))))

(define-syntax-rule (with-transaction exp ...)
  "Evalute EXP within an SQL transaction."
  (with-db-worker-thread db
    (exec-query db "BEGIN TRANSACTION;")
    exp ...
    (exec-query db "COMMIT;")))

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

(define (expect-one-row rows)
  "Several SQL queries expect one result, or zero if not found.  This gets rid
of the list, and returns #f when there is no result."
  (match rows
    ((row) row)
    (() #f)))

(define (db-load db schema)
  "Evaluate the file SCHEMA, which may contain SQL queries, into DB."
  (for-each (cut exec-query db <>)
            (read-sql-file schema)))

(define (db-schema-version db)
  (catch 'psql-query-error
    (lambda ()
      (match (expect-one-row
              (exec-query db "SELECT version FROM SchemaVersion"))
        ((version) (string->number version))))
    (lambda _ #f)))

(define (db-set-schema-version db version)
  (exec-query db "DELETE FROM SchemaVersion")
  (exec-query/bind db "INSERT INTO SchemaVersion (version) VALUES
 (" version ")"))

(define (latest-db-schema-version)
  "Return the version to which the schema should be upgraded, based on the
upgrade-n.sql files, or 0 if there are no such files."
  (reduce max 0
          (map (compose string->number (cut match:substring <> 1))
               (filter-map (cut string-match "^upgrade-([0-9]+)\\.sql$" <>)
                           (or (scandir (%package-sql-dir)) '())))))

(define* (db-init db
                  #:key
                  (schema (%package-schema-file)))
  "Open the database to store and read jobs and builds informations.  Return a
database object."
  (db-load db schema)
  (db-set-schema-version db (latest-db-schema-version))
  db)

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

(define* (db-open #:key
                  (database (%package-database)))
  "Open database to store or read jobs and builds informations.  Return a
database object."
  (unless database
    (%package-database
     (format #f "dbname=~a host=~a"
             (%cuirass-database) (%cuirass-host))))

  (let ((db (connect-to-postgres-paramstring (%package-database))))
    (when (%create-database?)
      (match (db-schema-version db)
        (#f
         (db-init db))
        (else
         (db-upgrade db))))
    db))

(define (db-close db)
  "Close database object DB."
  (pg-conn-finish db))

(define* (db-add-checkout spec-name eval-id instance
                          #:key timestamp)
  "Insert INSTANCE associated with SPEC-NAME and EVAL-ID.  If a checkout with
the same revision already exists for SPEC-NAME, return #f."
  (let ((channel (channel-instance-channel instance)))
    (with-db-worker-thread db
      (match (expect-one-row
              (exec-query/bind db "\
INSERT INTO Checkouts (specification, revision, evaluation, channel,
directory, timestamp) VALUES ("
                               spec-name ", "
                               (channel-instance-commit instance) ", "
                               eval-id ", "
                               (symbol->string (channel-name channel)) ", "
                               (channel-instance-checkout instance) ", "
                               (or timestamp 0) ")
ON CONFLICT ON CONSTRAINT checkouts_pkey DO NOTHING
RETURNING (specification, revision);"))
        (x x)
        (() #f)))))

(define (db-add-specification spec)
  "Store SPEC in database."
  (with-db-worker-thread db
    (match (expect-one-row
            (exec-query/bind db "\
INSERT INTO Specifications (name, build, channels, \
build_outputs, notifications, priority, systems) \
  VALUES ("
                             (specification-name spec) ", "
                             (specification-build spec) ", "
                             (map channel->sexp
                                  (specification-channels spec)) ", "
                             (map build-output->sexp
                                  (specification-build-outputs spec)) ", "
                             (map notification->sexp
                                  (specification-notifications spec)) ", "
                             (specification-priority spec) ", "
                             (specification-systems spec) ")
ON CONFLICT ON CONSTRAINT specifications_pkey DO NOTHING
RETURNING name;"))
      ((name) name)
      (else #f))))

(define (db-remove-specification name)
  "Remove the specification matching NAME from the database."
  (with-db-worker-thread db
    (exec-query/bind db "\
DELETE FROM Specifications WHERE name=" name ";")))

(define (db-get-specification name)
  "Retrieve a specification in the database with the given NAME."
  (expect-one-row (db-get-specifications name)))

(define* (db-get-specifications #:optional name)
  (with-db-worker-thread db
    (let loop
        ((rows  (if name
                    (exec-query/bind db "
SELECT name, build, channels, build_outputs, notifications,\
priority, systems FROM Specifications WHERE name =" name ";")
                    (exec-query db "
SELECT name, build, channels, build_outputs, notifications,\
priority, systems FROM Specifications ORDER BY name ASC;")))
         (specs '()))
      (match rows
        (() (reverse specs))
        (((name build channels build-outputs notifications priority systems)
          . rest)
         (loop rest
               (cons (specification
                      (name name)
                      (build (with-input-from-string build read))
                      (channels
                       (map sexp->channel*
                            (with-input-from-string channels read)))
                      (build-outputs
                       (map sexp->build-output
                            (with-input-from-string build-outputs read)))
                      (notifications
                       (map sexp->notification
                            (with-input-from-string notifications read)))
                      (priority (string->number priority))
                      (systems (with-input-from-string systems read)))
                     specs)))))))

(define-enumeration evaluation-status
  (started          -1)
  (succeeded         0)
  (failed            1)
  (aborted           2))

(define* (db-add-evaluation spec-name instances
                            #:key
                            (checkouttime 0)
                            (evaltime 0)
                            timestamp)
  "Add a new evaluation for SPEC-NAME only if one of the INSTANCES is new.
Otherwise, return #f."
  (define now
    (or timestamp (time-second (current-time time-utc))))

  (with-db-worker-thread db
    (exec-query db "BEGIN TRANSACTION;")
    (let* ((eval-id
            (match (expect-one-row
                    (exec-query/bind db "\
INSERT INTO Evaluations (specification, status, timestamp,
checkouttime, evaltime)
VALUES (" spec-name "," (evaluation-status started) ","
now "," checkouttime "," evaltime ")
RETURNING id;"))
              ((id) (string->number id))))
           (new-instances (filter-map
                           (lambda (instance)
                             (db-add-checkout spec-name eval-id instance
                                              #:timestamp timestamp))
                           instances)))
      (if (null? new-instances)
          (begin (exec-query db "ROLLBACK;")
                 #f)
          (begin (exec-query db "COMMIT;")
                 eval-id)))))

(define (db-abort-pending-evaluations)
  (with-db-worker-thread db
    (exec-query/bind db "UPDATE Evaluations SET status =
" (evaluation-status aborted) " WHERE status = "
(evaluation-status started))))

(define (db-set-evaluation-status eval-id status)
  (with-db-worker-thread db
    (exec-query/bind db "UPDATE Evaluations SET status =
" status " WHERE id = " eval-id ";")))

(define (db-set-evaluation-time eval-id)
  (define now
    (time-second (current-time time-utc)))

  (with-db-worker-thread db
    (exec-query/bind db "UPDATE Evaluations SET evaltime = " now
                     "WHERE id = " eval-id ";")))

(define-enumeration build-status
  ;; Build status as expected by Hydra's API.  Note: the negative values are
  ;; Cuirass' own extensions.
  (submitted        -3)
  (scheduled        -2)
  (started          -1)
  (succeeded         0)
  (failed            1)
  (failed-dependency 2)
  (failed-other      3)
  (canceled          4))

(define-enumeration build-weather
  (unknown          -1)
  (new-success       0)
  (new-failure       1)
  (still-succeeding  2)
  (still-failing     3))

(define (build-status->weather status last-status)
  (cond
   ((or (< status 0) (not last-status))
    (build-weather unknown))
   ((and (= status 0) (> last-status 0))
    (build-weather new-success))
   ((and (> status 0) (= last-status 0))
    (build-weather new-failure))
   ((and (= status 0) (= last-status 0))
    (build-weather still-succeeding))
   ((and (> status 0) (> last-status 0))
    (build-weather still-failing))))

(define (db-add-output derivation output)
  "Insert OUTPUT associated with DERIVATION."
  (with-db-worker-thread db
    (match output
      ((name . path)
       (exec-query/bind db "\
INSERT INTO Outputs (derivation, name, path) VALUES ("
                        derivation ", " name ", " path ")
ON CONFLICT ON CONSTRAINT outputs_pkey DO NOTHING;")))))

(define (db-add-build build)
  "Store BUILD in database the database only if one of its outputs is new.
Return #f otherwise.  BUILD outputs are stored in the OUTPUTS table."
  (with-db-worker-thread db
    (exec-query/bind db "
INSERT INTO Builds (derivation, evaluation, job_name, system, nix_name, log,
status, priority, max_silent, timeout, timestamp, starttime, stoptime)
VALUES ("
                     (assq-ref build #:derivation) ", "
                     (assq-ref build #:eval-id) ", "
                     (assq-ref build #:job-name) ", "
                     (assq-ref build #:system) ", "
                     (assq-ref build #:nix-name) ", "
                     (assq-ref build #:log) ", "
                     (or (assq-ref build #:status)
                         (build-status scheduled)) ", "
                         (or (assq-ref build #:priority) max-priority) ", "
                         (or (assq-ref build #:max-silent) 0) ", "
                         (or (assq-ref build #:timeout) 0) ", "
                         (or (assq-ref build #:timestamp) 0) ", "
                         (or (assq-ref build #:starttime) 0) ", "
                         (or (assq-ref build #:stoptime) 0) ")
ON CONFLICT ON CONSTRAINT builds_derivation_key DO NOTHING;"))
  (let* ((derivation (assq-ref build #:derivation))
         (outputs (assq-ref build #:outputs))
         (new-outputs (filter-map (cut db-add-output derivation <>)
                                  outputs)))
    derivation))

(define (db-add-build-product product)
  "Insert PRODUCT into BuildProducts table."
  (with-db-worker-thread db
    (exec-query/bind db "\
INSERT INTO BuildProducts (build, type, file_size, checksum,
path) VALUES ("
                     (assq-ref product #:build) ", "
                     (assq-ref product #:type) ", "
                     (assq-ref product #:file-size) ", "
                     (assq-ref product #:checksum) ", "
                     (assq-ref product #:path) ")
ON CONFLICT ON CONSTRAINT buildproducts_pkey DO NOTHING;")))

(define (db-get-output path)
  "Retrieve the OUTPUT for PATH."
  (with-db-worker-thread db
    (match (exec-query/bind db "SELECT derivation, name FROM Outputs
WHERE path =" path "
LIMIT 1;")
      (() #f)
      (((derivation name))
       `((#:derivation . ,derivation)
         (#:name . ,name))))))

(define (db-get-outputs derivation)
  "Retrieve the OUTPUTS of the build identified by DERIVATION in the
database."
  (with-db-worker-thread db
    (let loop ((rows
                (exec-query/bind db "SELECT name, path FROM Outputs
WHERE derivation =" derivation ";"))
               (outputs '()))
      (match rows
        (() (reverse outputs))
        (((name path)
          . rest)
         (loop rest
               (cons `(,name . ((#:path . ,path)))
                     outputs)))))))

(define (db-get-time-since-previous-build job-name specification)
  "Return the time difference in seconds between the current time and the
registration time of the last build for JOB-NAME and SPECIFICATION."
  (with-db-worker-thread db
    (match (expect-one-row
            (exec-query/bind db "
SELECT extract(epoch from now())::int - Builds.timestamp FROM Builds
INNER JOIN Evaluations on Builds.evaluation = Evaluations.id
WHERE job_name  = " job-name "AND specification = " specification
"ORDER BY Builds.timestamp DESC LIMIT 1"))
      ((time)
       (string->number time))
      (else #f))))

(define (db-get-build-percentage build-id)
  "Return the build completion percentage for BUILD-ID."
  (with-db-worker-thread db
    (match (expect-one-row
            (exec-query/bind db "
SELECT LEAST(duration::float/last_duration * 100, 100)::int AS percentage FROM
(SELECT (extract(epoch from now())::int - starttime) as duration,
last_build.duration AS last_duration FROM builds,
(SELECT GREATEST((stoptime - starttime), 1) AS duration FROM Builds
WHERE job_name IN
(SELECT job_name from Builds WHERE id = " build-id ")
AND status >= 0
ORDER BY Builds.timestamp DESC LIMIT 1) last_build
where id = " build-id ") d;
"))
      ((time)
       (string->number time))
      (else #f))))

(define (db-register-builds jobs eval-id specification)
  (define (new-outputs? outputs)
    (let ((new-outputs
           (filter-map (match-lambda
                         ((name . path)
                          (let ((drv (db-get-output path)))
                            (and (not drv) path))))
                       outputs)))
      (not (null? new-outputs))))

  (define (build-priority priority)
    (let ((spec-priority (specification-priority specification)))
      (+ (* spec-priority 10) priority)))

  (define (register job)
    (let* ((drv        (assq-ref job #:derivation))
           (job-name   (assq-ref job #:job-name))
           (system     (assq-ref job #:system))
           (nix-name   (assq-ref job #:nix-name))
           (log        (assq-ref job #:log))
           (period     (assq-ref job #:period))
           (priority   (or (assq-ref job #:priority) max-priority))
           (max-silent (assq-ref job #:max-silent-time))
           (timeout    (assq-ref job #:timeout))
           (outputs    (assq-ref job #:outputs))
           (cur-time   (time-second (current-time time-utc))))
      (and (new-outputs? outputs)
           (let ((build `((#:derivation . ,drv)
                          (#:eval-id . ,eval-id)
                          (#:job-name . ,job-name)
                          (#:system . ,system)
                          (#:nix-name . ,nix-name)

                          ;; XXX: We'd leave LOG to #f (i.e., NULL) but that
                          ;; currently violates the non-NULL constraint.
                          (#:log . ,(or log ""))

                          (#:status . ,(build-status scheduled))
                          (#:priority . ,(build-priority priority))
                          (#:max-silent . ,max-silent)
                          (#:timeout . ,timeout)
                          (#:outputs . ,outputs)
                          (#:timestamp . ,cur-time)
                          (#:starttime . 0)
                          (#:stoptime . 0))))
             (if period
                 (let* ((spec (specification-name specification))
                        (time
                         (db-get-time-since-previous-build job-name spec))
                        (add-build? (cond
                                     ((not time) #t)
                                     ((> time period) #t)
                                     (else #f))))
                   (and add-build? (db-add-build build)))
                 (db-add-build build))))))

  (with-db-worker-thread db
    (log-message "Registering builds for evaluation ~a." eval-id)
    (exec-query db "BEGIN TRANSACTION;")
    (let ((derivations (filter-map register jobs)))
      (exec-query db "COMMIT;")
      derivations)))

(define (db-get-last-status drv)
  "Return the status of the last completed build with the same 'job_name' and
specification' as DRV."
  (with-db-worker-thread db
    (match (expect-one-row
            (exec-query/bind db "
SELECT Builds.status FROM
(SELECT evaluation, job_name, specification FROM Builds
INNER JOIN Evaluations ON Builds.evaluation = Evaluations.id WHERE
derivation = " drv ") AS cur, Builds INNER JOIN
Evaluations ON Builds.evaluation = Evaluations.id
WHERE cur.job_name = Builds.job_name AND
cur.specification = Evaluations.specification AND
Builds.evaluation < cur.evaluation AND
Builds.status >= 0
ORDER BY Builds.evaluation DESC LIMIT 1"))
      ((status)
       (string->number status))
      (else #f))))

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
    (if (or (= status (build-status started))
            (= status (build-status submitted)))
        (if log-file
            (exec-query/bind db "UPDATE Builds SET starttime=" now
                             ",status=" status ",log=" log-file
                             "WHERE derivation=" drv ";")
            (exec-query/bind db "UPDATE Builds SET starttime=" now
                             ",status="
                             status "WHERE derivation=" drv ";"))

        ;; Update only if we're switching to a different status; otherwise
        ;; leave things unchanged.  This ensures that 'stoptime' remains valid
        ;; and doesn't change every time we mark DRV as 'succeeded' several
        ;; times in a row, for instance.  The 'last_status' field is updated
        ;; with the status of the last completed build with the same
        ;; 'job_name' and 'specification'.
        (let* ((last-status (db-get-last-status drv))
               (weather (build-status->weather status last-status))
               (rows
                (exec-query/bind db "
UPDATE Builds SET stoptime =" now
", status =" status
", last_status = " last-status
", weather = " weather
"WHERE derivation =" drv
" AND status != " status ";")))
          (when (positive? rows)
            (let* ((build (db-get-build drv))
                   (spec (assq-ref build #:specification))
                   (specification (db-get-specification spec))
                   (notifications
                    (specification-notifications specification)))
              (for-each (lambda (notif)
                          (when (or (eq? weather
                                         (build-weather new-success))
                                    (eq? weather
                                         (build-weather new-failure)))
                            (db-push-notification notif
                                                  (assq-ref build #:id))))
                        notifications)))))))

(define* (db-update-build-worker! drv worker)
  "Update the database so that DRV's worker is WORKER."
  (with-db-worker-thread db
    (exec-query/bind db "UPDATE Builds SET worker=" worker
                     "WHERE derivation=" drv ";")))

(define (db-restart-build! build-id)
  "Restart the build with BUILD-ID id."
  (with-db-worker-thread db
    (exec-query/bind db "UPDATE Builds SET status="
                     (build-status scheduled)
                     ", starttime = 0, stoptime = 0
                     WHERE id=" build-id ";")))

(define (db-restart-evaluation! eval-id)
  "Restart the evaluation with EVAL-ID id."
  (with-db-worker-thread db
    (exec-query/bind db "UPDATE Builds SET status="
                     (build-status scheduled)
                     ", starttime = 0, stoptime = 0
                     WHERE evaluation=" eval-id ";")))

(define (db-retry-evaluation! eval-id)
  "Retry the evaluation with EVAL-ID id."
  (with-db-worker-thread db
    (exec-query/bind db "\
DELETE FROM Checkouts WHERE evaluation=" eval-id ";")))

(define (db-cancel-pending-builds! eval-id)
  "Cancel the pending builds of the evaluation with EVAL-ID id."
  (with-db-worker-thread db
    (exec-query/bind db "UPDATE Builds SET status="
                     (build-status canceled)
                     "WHERE evaluation=" eval-id
                     "AND status = " (build-status started) ";")))

(define (query->bind-arguments query-string)
  "Return a list of keys to query strings by parsing QUERY-STRING."
  (define status-values
    `(("success" . ,(build-status succeeded))
      ("failed"  . ,(build-status failed))
      ("failed-dependency" . ,(build-status failed-dependency))
      ("failed-other" . ,(build-status failed-other))
      ("canceled" . ,(build-status canceled))))
  (let ((args (map
               (lambda (token)
                 (match (string-split token #\:)
                   (("system" system)
                    `(#:system . ,system))
                   (("spec" spec)
                    `(#:spec . ,spec))
                   (("status" status)
                    `(#:status . ,(assoc-ref status-values status)))
                   ((_ invalid) '())    ; ignore
                   ((query)
                    ;; Remove any '%' that could make the search too slow and
                    ;; add one at the end of the query.
                    `(#:query . ,(string-append
                                  (string-join
                                   (string-split query #\%)
                                   "")
                                  "%")))))
               (string-tokenize query-string))))
    ;; Normalize arguments
    (fold (lambda (key acc)
            (if (assq key acc)
                acc
                (cons (cons key #f) acc)))
          args '(#:spec #:system))))

(define (db-get-build-products build-id)
  "Return the build products associated to the given BUILD-ID."
  (with-db-worker-thread db
    (let loop ((rows (exec-query/bind db "
SELECT id, type, file_size, checksum, path from BuildProducts
WHERE build = " build-id))
               (products '()))
      (match rows
        (() (reverse products))
        (((id type file-size checksum path)
          . rest)
         (loop rest
               (cons `((#:id . ,(string->number id))
                       (#:type . ,type)
                       (#:file-size . ,(string->number file-size))
                       (#:checksum . ,checksum)
                       (#:path . ,path))
                     products)))))))

(define (db-get-builds-by-search filters)
  "Retrieve all builds in the database which are matched by given FILTERS.
FILTERS is an assoc list whose possible keys are the symbols query,
border-low-id, border-high-id, and nr."
  (with-db-worker-thread db
    (let* ((query (format #f "
SELECT * FROM
(SELECT Builds.id, Builds.timestamp,
Builds.starttime,Builds.stoptime, Builds.log, Builds.status,
Builds.job_name, Builds.system, Builds.nix_name, Specifications.name
FROM Builds
INNER JOIN Evaluations ON Builds.evaluation = Evaluations.id
INNER JOIN Specifications ON Evaluations.specification = Specifications.name
WHERE (Builds.nix_name LIKE :query)
AND ((Builds.status = :status) OR :status IS NULL)
AND ((Specifications.name = :spec) OR :spec IS NULL)
AND ((Builds.system = :system) OR :system IS NULL)
AND ((:borderlowid < Builds.id) OR :borderlowid IS NULL)
AND ((:borderhighid > Builds.id) OR :borderhighid IS NULL)
ORDER BY
(CASE WHEN :borderlowid IS NULL THEN Builds.id
ELSE -Builds.id END) DESC
LIMIT :nr) Builds
ORDER BY Builds.id DESC;"))
           (builds
            (exec-query/bind-params
             db
             query
             `((#:borderlowid . ,(assq-ref filters 'border-low-id))
               (#:borderhighid . ,(assq-ref filters 'border-high-id))
               (#:nr . ,(match (assq-ref filters 'nr)
                          (#f -1)
                          (x x)))
               ,@(query->bind-arguments (assq-ref filters 'query))))))
      (let loop ((builds builds)
                 (result '()))
        (match builds
          (() (reverse result))
          (((id timestamp starttime stoptime log status job-name
                system nix-name specification)
            . rest)
           (loop rest
                 (cons `((#:id . ,(string->number id))
                         (#:timestamp . ,(string->number timestamp))
                         (#:starttime . ,(string->number starttime))
                         (#:stoptime . ,(string->number stoptime))
                         (#:log . ,log)
                         (#:status . ,(string->number status))
                         (#:job-name . ,job-name)
                         (#:system . ,system)
                         (#:nix-name . ,nix-name)
                         (#:specification . ,specification)
                         (#:buildproducts . ,(db-get-build-products id)))
                       result))))))))

(define (db-get-builds filters)
  "Retrieve all builds in the database which are matched by given FILTERS.
FILTERS is an assoc list whose possible keys are 'derivation | 'id | 'jobset |
'job | 'system | 'nr | 'order | 'status | 'evaluation."

  ;; XXX: Make sure that all filters are covered by an index.
  (define (filters->order filters)
    (lambda (inner)
      (match (assq 'order filters)
        (('order . 'build-id) "Builds.id ASC")
        (('order . 'evaluation) "Builds.evaluation DESC")
        (('order . 'finish-time) "stoptime DESC")
        (('order . 'finish-time+build-id)
         (if inner
             "CASE WHEN CAST(:borderlowid AS integer) IS NULL THEN
 stoptime ELSE -stoptime END DESC,
CASE WHEN CAST(:borderlowid AS integer) IS NULL THEN
 Builds.id ELSE -Builds.id END DESC"
             "stoptime DESC, Builds.id DESC"))
        ;; With this order, builds in 'running' state (-1) appear
        ;; before those in 'scheduled' state (-2).
        (('order . 'status+submission-time)
         "Builds.status DESC, Builds.timestamp DESC, Builds.id ASC")
        (('order . 'priority+timestamp)
         "Builds.priority ASC, Builds.timestamp DESC")
        (_ "Builds.id DESC"))))

  ;; XXX: Make sure that all filters are covered by an index.
  (define (where-conditions filters)
    (define filter-name->sql
      `((id              . "Builds.id = :id")
        (jobset          . "Specifications.name = :jobset")
        (derivation      . "Builds.derivation = :derivation")
        (job             . "Builds.job_name = :job")
        (system          . "Builds.system = :system")
        (worker          . "Builds.worker = :worker")
        (oldevaluation   . "Builds.evaluation < :oldevaluation")
        (evaluation      . "Builds.evaluation = :evaluation")
        (status          . ,(match (assq-ref filters 'status)
                              (#f         #f)
                              ('done      "Builds.status >= 0")
                              ('scheduled "Builds.status = -2")
                              ('started   "Builds.status = -1")
                              ('pending   "Builds.status < 0")
                              ('succeeded "Builds.status = 0")
                              ('failed    "Builds.status > 0")))
        (weather
         . ,(match (assq-ref filters 'weather)
              (#f         #f)
              ('all       "Builds.weather >= 0")
              ('new       "(Builds.weather = 0 OR Builds.weather = 1)")))
        (border-low-time
         . "(((:borderlowtime, :borderlowid) < (Builds.stoptime, Builds.id))
OR :borderlowtime IS NULL OR :borderlowid IS NULL)")
        (border-high-time
         . "(((:borderhightime, :borderhighid) > (Builds.stoptime, Builds.id))
OR :borderhightime IS NULL OR :borderhighid IS NULL)")))

    (filter
     string?
     (fold
      (lambda (filter-name where-condition-parts)
        (if (assq-ref filters filter-name)
            (cons (assq-ref filter-name->sql filter-name)
                  where-condition-parts)
            where-condition-parts))
      '()
      (map car filters))))

  (define (format-outputs names paths)
    (map (lambda (name path)
           `(,name . ((#:path . ,path))))
         (string-split names #\,)
         (string-split paths #\,)))

  (define (format-build-products ids types file-sizes checksums paths)
    (define (split list)
      (if list
          (string-split list #\,)
          '()))

    (map (lambda (id type file-size checksum path)
           `((#:id . ,(string->number id))
             (#:type . ,type)
             (#:file-size . ,(string->number file-size))
             (#:checksum . ,checksum)
             (#:path . ,path)))
         (split ids)
         (split types)
         (split file-sizes)
         (split checksums)
         (split paths)))

  (with-db-worker-thread db
    (let* ((order (filters->order filters))
           (where (match (where-conditions filters)
                    (() "")
                    ((condition)
                     (string-append "WHERE " condition "\n"))
                    ((first-condition rest ...)
                     (string-append "WHERE " first-condition "\n  AND "
                                    (string-join rest " AND ")))))
           (query
            (format #f " SELECT Builds.derivation, Builds.id, Builds.timestamp,
Builds.starttime, Builds.stoptime, Builds.log, Builds.status,
Builds.last_status, Builds.weather, Builds.priority, Builds.max_silent,
Builds.timeout, Builds.job_name, Builds.system,
Builds.worker, Builds.nix_name, Builds.evaluation, agg.name, agg.outputs_name,
agg.outputs_path,agg.bp_build, agg.bp_type, agg.bp_file_size,
agg.bp_checksum, agg.bp_path
FROM
(SELECT B.id, B.derivation, B.name,
string_agg(Outputs.name, ',') AS outputs_name,
string_agg(Outputs.path, ',') AS outputs_path,
string_agg(cast(BP.id AS text), ',') AS bp_build,
string_agg(BP.type, ',') AS bp_type,
string_agg(cast(BP.file_size AS text), ',') AS bp_file_size,
string_agg(BP.checksum, ',') AS bp_checksum,
string_agg(BP.path, ',') AS bp_path FROM
(SELECT Builds.id, Builds.derivation, Specifications.name FROM Builds
INNER JOIN Evaluations ON Builds.evaluation = Evaluations.id
INNER JOIN Specifications ON Evaluations.specification = Specifications.name
~a
ORDER BY ~a
LIMIT :nr) B
INNER JOIN Outputs ON Outputs.derivation = B.derivation
LEFT JOIN BuildProducts as BP ON BP.build = B.id
GROUP BY B.derivation, B.id, B.name) agg
JOIN Builds on agg.id = Builds.id
ORDER BY ~a;"
                    where (order #t) (order #f)))
           (params
            (map (match-lambda
                   ((name . value)
                    (cons (symbol->keyword
                           (or (assq-ref
                                '((border-low-time  . borderlowtime)
                                  (border-high-time . borderhightime)
                                  (border-low-id    . borderlowid)
                                  (border-high-id   . borderhighid))
                                name)
                               name))
                          (match name
                            ('nr value)
                            ('order #f) ; Doesn't need binding.
                            ('status #f) ; Doesn't need binding.
                            (else value)))))
                 filters))
           (builds (exec-query/bind-params db query params)))
      (let loop ((builds builds)
                 (result '()))
        (match builds
          (() (reverse result))
          (((derivation id timestamp starttime stoptime log status
                        last-status weather priority max-silent timeout
                        job-name system worker nix-name eval-id
                        specification outputs-name outputs-path
                        products-id products-type products-file-size
                        products-checksum products-path)
            . rest)
           (loop rest
                 (cons `((#:derivation . ,derivation)
                         (#:id . ,(string->number id))
                         (#:timestamp . ,(string->number timestamp))
                         (#:starttime . ,(string->number starttime))
                         (#:stoptime . ,(string->number stoptime))
                         (#:log . ,log)
                         (#:status . ,(string->number status))
                         (#:last-status . ,(and last-status
                                                (string->number last-status)))
                         (#:weather . ,(if weather
                                           (string->number weather)
                                           (build-weather unknown)))
                         (#:priority . ,(string->number priority))
                         (#:max-silent . ,(string->number max-silent))
                         (#:timeout . ,(string->number timeout))
                         (#:job-name . ,job-name)
                         (#:system . ,system)
                         (#:worker . ,worker)
                         (#:nix-name . ,nix-name)
                         (#:eval-id . ,(string->number eval-id))
                         (#:specification . ,specification)
                         (#:outputs . ,(format-outputs outputs-name
                                                       outputs-path))
                         (#:buildproducts .
                          ,(format-build-products products-id
                                                  products-type
                                                  products-file-size
                                                  products-checksum
                                                  products-path)))
                       result))))))))

(define (db-get-build derivation-or-id)
  "Retrieve a build in the database which corresponds to DERIVATION-OR-ID."
  (let ((key (if (number? derivation-or-id) 'id 'derivation)))
    (expect-one-row (db-get-builds `((,key . ,derivation-or-id))))))

(define (db-get-pending-derivations)
  "Return the list of derivation file names corresponding to pending builds in
the database.  The returned list is guaranteed to not have any duplicates."
  (with-db-worker-thread db
    (map (match-lambda ((drv) drv))
         (exec-query db "
SELECT derivation FROM Builds WHERE Builds.status < 0;"))))

(define (db-get-checkouts eval-id)
  (with-db-worker-thread db
    (let loop ((rows (exec-query/bind
                      db "SELECT revision, channel, directory FROM Checkouts
WHERE evaluation =" eval-id " ORDER BY channel ASC;"))
               (checkouts '()))
      (match rows
        (() (reverse checkouts))
        (((revision channel directory)
          . rest)
         (loop rest
               (cons `((#:commit . ,revision)
                       (#:channel . ,(string->symbol channel))
                       (#:directory . ,directory))
                     checkouts)))))))

(define (parse-evaluation evaluation)
  (match evaluation
    ((id specification status timestamp checkouttime evaltime)
     `((#:id . ,(string->number id))
       (#:specification . ,specification)
       (#:status . ,(string->number status))
       (#:timestamp . ,(string->number timestamp))
       (#:checkouttime . ,(string->number checkouttime))
       (#:evaltime . ,(string->number evaltime))
       (#:checkouts . ,(db-get-checkouts id))))))

(define (db-get-evaluation id)
  (with-db-worker-thread db
    (match (exec-query/bind db "SELECT id, specification, status,
timestamp, checkouttime, evaltime
FROM Evaluations WHERE id = " id)
      (() #f)
      ((evaluation)
       (parse-evaluation evaluation)))))

(define (db-get-evaluations limit)
  (with-db-worker-thread db
    (let loop ((rows  (exec-query/bind db "SELECT id, specification, status,
timestamp, checkouttime, evaltime
FROM Evaluations ORDER BY id DESC LIMIT " limit ";"))
               (evaluations '()))
      (match rows
        (() (reverse evaluations))
        ((evaluation . rest)
         (loop rest
               (cons (parse-evaluation evaluation) evaluations)))))))

(define (db-get-evaluations-build-summary spec limit border-low border-high)
  (with-db-worker-thread db
    (let ((query "
SELECT E.id, E.status,
SUM(CASE WHEN B.status = 0 THEN 1 ELSE 0 END) as succeeded,
SUM(CASE WHEN B.status > 0 THEN 1 ELSE 0 END) as failed,
SUM(CASE WHEN B.status < 0 THEN 1 ELSE 0 END) as scheduled FROM
(SELECT id, status FROM Evaluations
WHERE specification=:spec
AND (id > :borderlow OR :borderlow IS NULL)
AND (id < :borderhigh OR :borderhigh IS NULL)
ORDER BY CASE WHEN :borderlow IS NULL THEN id ELSE -id END DESC
LIMIT :limit) E
LEFT JOIN Builds as B
ON B.evaluation=E.id
GROUP BY E.id, E.status
ORDER BY E.id DESC;")
          (params `((#:spec . ,spec)
                    (#:limit . ,limit)
                    (#:borderlow . ,border-low)
                    (#:borderhigh . ,border-high))))
      (let loop ((rows (exec-query/bind-params db query params))
                 (evaluations '()))
        (match rows
          (() (reverse evaluations))
          (((id status succeeded failed scheduled) . rest)
           (loop rest
                 (cons `((#:id . ,(string->number id))
                         (#:status . ,(string->number status))
                         (#:checkouts . ,(db-get-checkouts id))
                         (#:succeeded . ,(or (string->number succeeded) 0))
                         (#:failed . ,(or (string->number failed) 0))
                         (#:scheduled . ,(or (string->number scheduled) 0)))
                       evaluations))))))))

(define (db-get-evaluations-id-min spec)
  "Return the min id of evaluations for the given specification SPEC."
  (with-db-worker-thread db
    (match (expect-one-row
            (exec-query/bind db "
SELECT MIN(id) FROM Evaluations
WHERE specification=" spec))
      ((min) (and min (string->number min))))))

(define (db-get-evaluations-id-max spec)
  "Return the max id of evaluations for the given specification SPEC."
  (with-db-worker-thread db
    (match (expect-one-row
            (exec-query/bind db "
SELECT MAX(id) FROM Evaluations
WHERE specification=" spec))
      ((max) (and max (string->number max))))))

(define (db-get-evaluation-summary id)
  (with-db-worker-thread db
    (match (expect-one-row
            (exec-query/bind db "
SELECT Evaluations.id, Evaluations.status, Evaluations.timestamp,
Evaluations.checkouttime, Evaluations.evaltime,
SUM(CASE WHEN B.status > -100 THEN 1 ELSE 0 END) as total,
SUM(CASE WHEN B.status = 0 THEN 1 ELSE 0 END) as succeeded,
SUM(CASE WHEN B.status > 0 THEN 1 ELSE 0 END) as failed,
SUM(CASE WHEN B.status < 0 THEN 1 ELSE 0 END) as scheduled
FROM Evaluations
LEFT JOIN Builds as B
ON B.evaluation = Evaluations.id
WHERE Evaluations.id = " id
"GROUP BY Evaluations.id
ORDER BY Evaluations.id ASC;"))
      ((id status timestamp checkouttime evaltime
           total succeeded failed scheduled)
       `((#:id . ,(string->number id))
         (#:status . ,(string->number status))
         (#:total . ,(or (string->number total) 0))
         (#:timestamp . ,(string->number timestamp))
         (#:checkouttime . ,(string->number checkouttime))
         (#:evaltime . ,(string->number evaltime))
         (#:succeeded . ,(or (string->number succeeded) 0))
         (#:failed . ,(or (string->number failed) 0))
         (#:scheduled . ,(or (string->number scheduled) 0))))
      (else #f))))

(define (db-get-builds-query-min filters)
  "Return the smallest build row identifier matching QUERY."
  (with-db-worker-thread db
    (let* ((query "SELECT MIN(Builds.id) FROM Builds
INNER JOIN Evaluations ON Builds.evaluation = Evaluations.id
INNER JOIN Specifications ON Evaluations.specification = Specifications.name
WHERE (Builds.nix_name LIKE :query)
AND (Builds.status = :status OR :status IS NULL)
AND (Specifications.name = :spec OR :spec IS NULL)
AND (Builds.system = :system OR :system IS NULL);")
           (params (query->bind-arguments filters)))
      (match (expect-one-row
              (exec-query/bind-params db query params))
        ((min) (and min
                    (list (string->number min))))))))

(define (db-get-builds-query-max filters)
  "Return the largest build row identifier matching QUERY."
  (with-db-worker-thread db
    (let* ((query "SELECT MAX(Builds.id) FROM Builds
INNER JOIN Evaluations ON Builds.evaluation = Evaluations.id
INNER JOIN Specifications ON Evaluations.specification = Specifications.name
WHERE (Builds.nix_name LIKE :query)
AND (Builds.status = :status OR :status IS NULL)
AND (Specifications.name = :spec OR :spec IS NULL)
AND (Builds.system = :system OR :system IS NULL);")
           (params (query->bind-arguments filters)))
      (match (expect-one-row
              (exec-query/bind-params db query params))
        ((max) (and max
                    (list (string->number max))))))))

(define (db-get-builds-min eval status)
  "Return the min build (stoptime, rowid) pair for the given evaluation EVAL
and STATUS."
  (with-db-worker-thread db
    (let ((query "SELECT stoptime, id FROM Builds
WHERE evaluation = :eval AND
((:status = 'pending' AND Builds.status < 0) OR
(:status = 'succeeded' AND Builds.status = 0) OR
(:status = 'failed' AND Builds.status > 0) OR
:status IS NULL)
ORDER BY stoptime ASC, id ASC
LIMIT 1")
          (params `((#:eval . ,eval)
                    (#:status . ,status))))
      (match (expect-one-row
              (exec-query/bind-params db query params))
        ((stoptime id) (list (string->number stoptime)
                             (string->number id)))
        (else #f)))))

(define (db-get-builds-max eval status)
  "Return the max build (stoptime, rowid) pair for the given evaluation EVAL
and STATUS."
  (with-db-worker-thread db
    (let ((query "SELECT stoptime, id FROM Builds
WHERE evaluation = :eval AND
((:status = 'pending' AND Builds.status < 0) OR
(:status = 'succeeded' AND Builds.status = 0) OR
(:status = 'failed' AND Builds.status > 0) OR
:status IS NULL)
ORDER BY stoptime DESC, id DESC
LIMIT 1")
          (params `((#:eval . ,eval)
                    (#:status . ,status))))
      (match (expect-one-row
              (exec-query/bind-params db query params))
        ((stoptime id) (list (string->number stoptime)
                             (string->number id)))
        (else #f)))))

(define (db-get-evaluation-specification eval)
  "Return specification of evaluation with id EVAL."
  (with-db-worker-thread db
    (match (expect-one-row
            (exec-query/bind db "
SELECT specification FROM Evaluations
WHERE id = " eval))
      ((spec) spec)
      (else #f))))

(define (db-get-build-product-path id)
  "Return the build product with the given ID."
  (with-db-worker-thread db
    (match (expect-one-row
            (exec-query/bind db "
SELECT path FROM BuildProducts
WHERE id = " id))
      ((path) path)
      (else #f))))

(define (db-push-notification notification build)
  "Insert NOTIFICATION into Notifications table."
  (with-db-worker-thread db
    (exec-query/bind db "\
INSERT INTO Notifications (type, build)
VALUES (" (notification->sexp notification) ", " build ");")))

(define (db-pop-notification)
  "Return two values, the latest notification from the Notifications table and
the matching build."
  (with-db-worker-thread db
    (match (expect-one-row
            (exec-query/bind db "
SELECT id, type, build from Notifications ORDER BY id ASC LIMIT 1;"))
      ((id type build)
       (exec-query/bind db "\
DELETE FROM Notifications WHERE id =" id ";")
       (cons (sexp->notification
              (with-input-from-string type read))
             (db-get-build (string->number build))))
      (else #f))))

(define (db-add-or-update-worker worker)
  "Insert WORKER into Worker table."
  (with-db-worker-thread db
    (exec-query/bind db "\
INSERT INTO Workers (name, address, machine, systems, last_seen)
VALUES ("
                     (worker-name worker) ", "
                     (worker-address worker) ", "
                     (worker-machine worker) ", "
                     (string-join (worker-systems worker) ",") ", "
                     (worker-last-seen worker) ")
ON CONFLICT(name) DO UPDATE
SET last_seen = " (worker-last-seen worker) ";")))

(define (db-get-worker name)
  "Return the worker with the given NAME."
  (with-db-worker-thread db
    (match (expect-one-row
            (exec-query/bind db "
SELECT name, address, machine, systems, last_seen from Workers
WHERE name = " name ";"))
      ((name address machine systems last-seen)
       (worker
        (name name)
        (address address)
        (machine machine)
        (systems (string-split systems #\,))
        (last-seen (string->number last-seen))))
      (else #f))))

(define (db-get-workers)
  "Return the workers in Workers table."
  (with-db-worker-thread db
    (let loop ((rows (exec-query db "
SELECT name, address, machine, systems, last_seen from Workers"))
               (workers '()))
      (match rows
        (() (reverse workers))
        (((name address machine systems last-seen)
          . rest)
         (loop rest
               (cons (worker
                      (name name)
                      (address address)
                      (machine machine)
                      (systems (string-split systems #\,))
                      (last-seen (string->number last-seen)))
                     workers)))))))

(define (db-remove-unresponsive-workers timeout)
  "Remove the workers that are unresponsive since at least TIMEOUT seconds.
Also restart the builds that are started on those workers."
  (with-db-worker-thread db
    ;; Restart the builds that are marked as started on those workers.
    (exec-query/bind db "
UPDATE Builds SET status = -2, worker = null FROM
(SELECT id FROM Workers LEFT JOIN Builds
ON builds.worker = workers.name
WHERE status = -1 AND
(extract(epoch from now())::int - last_seen) > " timeout
") AS expired WHERE builds.id = expired.id")
    (exec-query/bind db "DELETE FROM Workers WHERE
(extract(epoch from now())::int - last_seen) > " timeout ";")))

(define (db-clear-workers)
  "Remove all workers from Workers table."
  (with-db-worker-thread db
    (exec-query db "DELETE FROM Workers;")))

(define (db-clear-build-queue)
  "Reset the status of builds in the database that are marked as \"started\"."
  (with-db-worker-thread db
    (exec-query db "UPDATE Builds SET status = -2 WHERE status < 0;")))

;;; Local Variables:
;;; eval: (put 'with-db-worker-thread 'scheme-indent-function 1)
;;; End:
