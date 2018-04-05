;;; base.scm -- Cuirass base module
;;; Copyright © 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (cuirass base)
  #:use-module (fibers)
  #:use-module (cuirass logging)
  #:use-module (cuirass database)
  #:use-module (cuirass utils)
  #:use-module (gnu packages)
  #:use-module (guix build utils)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix git)
  #:use-module (git)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs bytevectors)
  #:export (;; Procedures.
            call-with-time-display
            fetch-repository
            compile
            evaluate
            clear-build-queue
            cancel-old-builds
            restart-builds
            build-packages
            prepare-git
            process-specs
            set-guix-package-path!
            ;; Parameters.
            %guix-package-path
            %package-cachedir
            %use-substitutes?
            %fallback?))

(define-syntax-rule (with-store store exp ...)
  ;; XXX: This is a 'with-store' variant that plays well with delimited
  ;; continuations and fibers.  The 'with-store' macro in (guix store)
  ;; currently closes in a 'dynamic-wind' handler, which means it would close
  ;; the store at each context switch.  Remove this when the real 'with-store'
  ;; has been fixed.
  (let ((store (open-connection)))
    (unwind-protect
     ;; Always set #:keep-going? so we don't stop on the first build failure.
     ;; Set #:print-build-trace explicitly to make sure 'process-build-log'
     ;; sees build events.
     (set-build-options store
                        #:use-substitutes? (%use-substitutes?)
                        #:fallback? (%fallback?)
                        #:keep-going? #t
                        #:print-build-trace #t)
     exp ...
     (close-connection store))))

(cond-expand
  (guile-2.2
   ;; Guile 2.2.2 has a bug whereby 'time-monotonic' objects have seconds and
   ;; nanoseconds swapped (fixed in Guile commit 886ac3e).  Work around it.
   (define time-monotonic time-tai))
  (else #t))

(define %use-substitutes?
  ;; Define whether to use substitutes
  (make-parameter #f))

(define %fallback?
  ;; Define whether to fall back to building when the substituter fails.
  (make-parameter #f))

(define %package-cachedir
  ;; Define to location of cache directory of this package.
  (make-parameter (or (getenv "CUIRASS_CACHEDIR")
                      (string-append (or (getenv "HOME") ".")
                                     "/.cache/cuirass"))
    (lambda (val)
      (if (string? val)
          val
          (scm-error 'wrong-type-arg
                     "%package-cachedir" "Not a string: ~S" (list #f) #f)))))

(define (call-with-time thunk kont)
  "Call THUNK and pass KONT the elapsed time followed by THUNK's return
values."
  (let* ((start  (current-time time-monotonic))
         (result (call-with-values thunk list))
         (end    (current-time time-monotonic)))
    (apply kont (time-difference end start) result)))

(define (call-with-time-display thunk)
  "Call THUNK and write to the current output port its duration."
  (call-with-time thunk
    (lambda (time result)
      (let ((duration (+ (time-second time)
                         (/ (time-nanosecond time) 1e9))))
        (format (current-error-port) "evaluate '~A': ~,3f seconds~%"
                (assq-ref result #:job-name)
                duration)
        (acons #:duration duration result)))))

(define (report-git-error error)
  "Report the given Guile-Git error."
  (format (current-error-port)
          "Git error: ~a~%" (git-error-message error)))

(define-syntax-rule (with-git-error-handling body ...)
  (catch 'git-error
    (lambda ()
      body ...)
    (lambda (key err)
      (report-git-error err))))

(define* (fetch-repository store spec #:key writable-copy?)
  "Get the latest version of repository specified in SPEC.  Return two
values: the content of the git repository at URL copied into a store
directory and the sha1 of the top level commit in this directory.

When WRITABLE-COPY? is true, return a writable copy; otherwise, return a
read-only directory."

  (define (add-origin branch)
    "Prefix branch name with origin if no remote is specified."
    (if (string-index branch #\/)
        branch
        (string-append "origin/" branch)))

  (let ((name   (assq-ref spec #:name))
        (url    (assq-ref spec #:url))
        (branch (and=> (assq-ref spec #:branch)
                       (lambda (b)
                         `(branch . ,(add-origin b)))))
        (commit (and=> (assq-ref spec #:commit)
                       (lambda (c)
                         `(commit . ,c))))
        (tag    (and=> (assq-ref spec #:tag)
                       (lambda (t)
                         `(tag . ,t)))))
    (let-values (((directory commit)
                  (latest-repository-commit store url
                                            #:cache-directory (%package-cachedir)
                                            #:ref (or branch commit tag))))
      ;; TODO: When WRITABLE-COPY? is true, we could directly copy the
      ;; checkout directly in a writable location instead of copying it to the
      ;; store first.
      (values (if writable-copy?
                  (make-writable-copy directory
                                      (string-append (%package-cachedir)
                                                     "/" (assq-ref spec #:name)))
                  directory)
              commit))))

(define (make-writable-copy source target)
  "Create TARGET and make it a writable copy of directory SOURCE; delete
TARGET beforehand if it exists.  Return TARGET."
  (define (chmod+w file stat _)
    (chmod file (logior #o200 (stat:perms stat))))

  (mkdir-p (dirname target))
  ;; Remove any directory with the same name.
  (false-if-exception (delete-file-recursively target))
  (copy-recursively source target)

  ;; Make all the files in TARGET writable.
  (file-system-fold (const #t)                    ;enter?
                    chmod+w                       ;leaf
                    chmod+w                       ;down
                    (const #t)                    ;up
                    (const #t)                    ;skip
                    (const #f)                    ;error
                    *unspecified*                 ;init
                    target)

  target)

(define (compile dir)
  ;; Required for fetching Guix bootstrap tarballs.
  "Compile files in repository in directory DIR."
  (with-directory-excursion dir
    (or (file-exists? "configure") (system* "./bootstrap"))
    (or (file-exists? "Makefile")
        (system* "./configure" "--localstatedir=/var"))
    (zero? (system* "make" "-j" (number->string (current-processor-count))))))

(define-condition-type &evaluation-error &error
  evaluation-error?
  (name evaluation-error-spec-name))

(define (non-blocking-port port)
  "Make PORT non-blocking and return it."
  (let ((flags (fcntl port F_GETFL)))
    (fcntl port F_SETFL (logior O_NONBLOCK flags))
    port))

(define (read/non-blocking port)
  "Like 'read', but uses primitives that don't block and thus play well with
fibers."
  ;; XXX: Since 'read' is not suspendable as of Guile 2.2.3, we use
  ;; 'read-string' (which is suspendable) and then 'read'.
  (setvbuf port 'block 4096)                   ;'read-string' uses 'read-char'
  (match (read-string port)
    ((? eof-object? eof)
     eof)
    ((? string? data)
     (call-with-input-string data read))))

(match (resolve-module '(fibers internal) #t #f #:ensure #f)
  (#f #t)                                         ;Fibers > 1.0.0
  ((? module? internal)                           ;Fibers <= 1.0.0
   ;; Work around <https://github.com/wingo/fibers/issues/19>.
   ;; This monkey-patching aims to replace EPOLLERR occurrences in
   ;; 'schedule-fibers-for-fd' with EPOLLERR | EPOLLHUP.
   (module-define! internal 'EPOLLERR
                   (logior (@ (fibers epoll) EPOLLERR)
                           (@ (fibers epoll) EPOLLHUP)))))

(define (evaluate store db spec source)
  "Evaluate and build package derivations defined in SPEC, using the checkout
in SOURCE directory.  Return a list of jobs."
  (define (augment-job job eval-id)
    (let ((drv (read-derivation-from-file
                (assq-ref job #:derivation))))
      `((#:eval-id . ,eval-id)
        (#:nix-name . ,(derivation-name drv))
        (#:system . ,(derivation-system drv))
        ,@job)))

  (let* ((port (non-blocking-port
                (open-pipe* OPEN_READ
                            "evaluate"
                            (string-append (%package-cachedir) "/"
                                           (assq-ref spec #:name) "/"
                                           (assq-ref spec #:load-path))
                            (%guix-package-path)
                            source (object->string spec))))
         (result (match (read/non-blocking port)
                   ;; If an error occured during evaluation report it,
                   ;; otherwise, suppose that data read from port are
                   ;; correct and keep things going.
                   ((? eof-object?)
                    (raise (condition
                            (&evaluation-error
                             (name (assq-ref spec #:name))))))
                   (data data))))
    (close-pipe port)
    (match result
      (('evaluation eval jobs)
       (let ((eval-id (db-add-evaluation db eval)))
         (log-message "created evaluation ~a for ~a, commit ~a" eval-id
                      (assq-ref eval #:specification)
                      (assq-ref eval #:revision))
         (let ((jobs (map (lambda (job)
                            (augment-job job eval-id))
                          jobs)))
           (for-each (cut db-add-derivation db <>) jobs)
           jobs))))))


;;;
;;; Build status.
;;;

;; TODO: Remove this code once it has been integrated in Guix proper as (guix
;; status).

(define (read-line/non-blocking port)
  "Like 'read-line', but unlike 'read-line', use I/O primitives that can be
suspended when PORT is O_NONBLOCK in a fiber context."
  (let loop ((chars '()))
    (match (read-char port)                       ;can suspend
      ((? eof-object? eof)
       (if (null? chars)
           eof
           (list->string (reverse chars))))
      (#\newline
       (list->string (reverse chars)))
      (chr
       (loop (cons chr chars))))))

(define (process-build-log port proc seed)
  "Read from PORT the build log, calling PROC for each build event like 'fold'
does.  Return the result of the last call to PROC."
  (define (process-line line state)
    (when (string-prefix? "@ " line)
      (match (string-tokenize (string-drop line 2))
        (((= string->symbol event-name) args ...)
         (proc (cons event-name args) state)))))

  (let loop ((state seed))
    (match (read-line/non-blocking port)
      ((? eof-object?)
       state)
      ((? string? line)
       (loop (process-line line state))))))

(define (build-derivations& store lst)
  "Like 'build-derivations' but return two values: a file port from which to
read the build log, and a thunk to call after EOF has been read.  The thunk
returns the value of the underlying 'build-derivations' call, or raises the
exception that 'build-derivations' raised.

Essentially this procedure inverts the inversion-of-control that
'build-derivations' imposes, whereby 'build-derivations' writes to
'current-build-output-port'."
  ;; XXX: Make this part of (guix store)?
  (define result
    (make-atomic-box #f))

  (match (pipe)
    ((input . output)
     (call-with-new-thread
      (lambda ()
        (catch #t
          (lambda ()
            (guard (c ((nix-error? c)
                       (close-port output)
                       (atomic-box-set! result c)))
              (parameterize ((current-build-output-port output))
                (let ((x (build-derivations store lst)))
                  (atomic-box-set! result x))))
            (close-port output))
          (lambda _
            (close-port output)))))

     (values (non-blocking-port input)
             (lambda ()
               (match (atomic-box-ref result)
                 ((? condition? c)
                  (raise c))
                 (x x)))))))


;;;
;;; Building packages.
;;;

(define (shuffle-derivations drv)
  "Shuffle DRV, a list of derivation file names."
  ;; Our shuffling algorithm is simple: we sort by .drv file name.  :-)
  (sort drv string<?))

(define (update-build-statuses! store db lst)
  "Update the build status of the derivations listed in LST, which have just
been passed to 'build-derivations' (meaning that we can assume that, if their
outputs are invalid, that they failed to build.)"
  (define (update! drv)
    (match (derivation-path->output-paths drv)
      (((_ . outputs) ...)
       (if (any (cut valid-path? store <>) outputs)
           (db-update-build-status! db drv (build-status succeeded))
           (db-update-build-status! db drv (build-status failed))))))

  (for-each update! lst))

(define (exception-reporter . results)
  "Return an exception handler that reports the exception on the error port
and returns the values RESULTS."
  (lambda (key . args)
    (false-if-exception
     (let* ((stack (make-stack #t))
            (depth (stack-length stack))
            (frame (or (and (> depth 1) (stack-ref stack 1))
                       (and (> depth 0)) (stack-ref stack 0))))
       (print-exception (current-error-port) frame key args)
       (apply values results)))))

(define* (spawn-builds store db drv
                       #:key (max-batch-size 200))
  "Build the derivations listed in DRV, updating DB as builds complete.
Derivations are submitted in batches of at most MAX-BATCH-SIZE items."
  ;; XXX: We want to pass 'build-derivations' as many derivations at once so
  ;; we benefit from as much parallelism as possible (we must be using
  ;; #:keep-going? #t).
  ;;
  ;; However, 'guix-daemon' currently doesn't scale well when doing a
  ;; 'build-derivations' RPC with a lot of derivations: first it parses each
  ;; .drv from disk (in LocalStore::buildPaths), then it locks each derivation
  ;; and tries to run it (in Worker::run), and *only then* does it start
  ;; listening the stdout/stderr of those builds.  As a consequence, we can
  ;; end up starting, say, 30 builds, and only start listening to their
  ;; stdout/stderr *minutes* later.  In the meantime, the build processes are
  ;; mostly likely stuck in write(1, …) or similar and we can reach build
  ;; timeouts of all sorts.
  ;;
  ;; This code works around it by submitting derivations in batches of at most
  ;; MAX-BATCH-SIZE.

  (define total (length drv))

  (log-message "building ~a derivations in batches of ~a"
               total max-batch-size)

  ;; Shuffle DRV so that we don't build sequentially i686/x86_64/aarch64,
  ;; master/core-updates, etc., which would be suboptimal.
  (let loop ((drv   (shuffle-derivations drv))
             (count total))
    (if (zero? count)
        (log-message "done with ~a derivations" total)
        (let*-values (((batch rest)
                       (if (> count max-batch-size)
                           (split-at drv max-batch-size)
                           (values drv '()))))
          (guard (c ((nix-protocol-error? c)
                     (log-message "batch of builds (partially) failed:\
~a (status: ~a)"
                                  (nix-protocol-error-message c)
                                  (nix-protocol-error-status c))))
            (log-message "building batch of ~a derivations (~a/~a)"
                         max-batch-size (- total count) total)
            (let-values (((port finish)
                          (build-derivations& store drv)))
              (process-build-log port
                                 (lambda (event state)
                                   ;; Catch any errors so we can keep reading
                                   ;; from PORT and eventually close it.
                                   (catch #t
                                     (lambda ()
                                       (handle-build-event db event))
                                     (exception-reporter state)))
                                 #t)
              (close-port port)
              (finish)))

          ;; Most of the time 'handle-build-event' will update the build
          ;; status of derivations.  However, it could be that some
          ;; derivations were built "behind our back", in which case
          ;; 'build-derivations' doesn't actually do anything and
          ;; 'handle-build-event' doesn't see any event.  Because of that,
          ;; adjust DB here.
          (update-build-statuses! store db drv)

          (loop rest (max (- count max-batch-size) 0))))))

(define* (handle-build-event db event)
  "Handle EVENT, a build event sexp as produced by 'build-event-output-port',
updating DB accordingly."
  (define (valid? file)
    ;; FIXME: Sometimes we might get bogus events due to the interleaving of
    ;; build messages.  This procedure prevents us from propagating the bogus
    ;; file name to the database.
    (and (store-path? file)
         (string-suffix? ".drv" file)))

  (match event
    (('build-started drv _ ...)
     (if (valid? drv)
         (begin
           (log-message "build started: '~a'" drv)
           (db-update-build-status! db drv (build-status started)))
         (log-message "bogus build-started event for '~a'" drv)))
    (('build-remote drv host _ ...)
     (log-message "'~a' offloaded to '~a'" drv host))
    (('build-succeeded drv _ ...)
     (if (valid? drv)
         (begin
           (log-message "build succeeded: '~a'" drv)
           (db-update-build-status! db drv (build-status succeeded)))
         (log-message "bogus build-succeeded event for '~a'" drv)))
    (('build-failed drv _ ...)
     (if (valid? drv)
         (begin
           (log-message "build failed: '~a'" drv)
           (db-update-build-status! db drv (build-status failed)))
         (log-message "bogus build-failed event for '~a'" drv)))
    (('substituter-started item _ ...)
     (log-message "substituter started: '~a'" item))
    (('substituter-succeeded item _ ...)
     (log-message "substituter succeeded: '~a'" item))
    (_
     (log-message "build event: ~s" event))))

(define (build-derivation=? build1 build2)
  "Return true if BUILD1 and BUILD2 correspond to the same derivation."
  (string=? (assq-ref build1 #:derivation)
            (assq-ref build2 #:derivation)))

(define (clear-build-queue db)
  "Reset the status of builds in DB that are marked as \"started\".  This
procedure is meant to be called at startup."
  (log-message "marking stale builds as \"scheduled\"...")
  (sqlite-exec db "UPDATE Builds SET status = -2 WHERE status = -1;"))

(define (cancel-old-builds db age)
  "Cancel builds older than AGE seconds."
  (log-message "canceling builds older than ~a seconds..." age)
  (sqlite-exec db
               "UPDATE Builds SET status = 4 WHERE status = -2 AND timestamp < "
               (- (time-second (current-time time-utc)) age)
               ";"))

(define (restart-builds db)
  "Restart builds whose status in DB is \"pending\" (scheduled or started)."
  (with-store store
    ;; Note: On a big database, 'db-get-pending-derivations' can take a couple
    ;; of minutes, hence 'non-blocking'.
    (log-message "retrieving list of pending builds...")
    (let*-values (((valid stale)
                   (partition (cut valid-path? store <>)
                              (non-blocking (db-get-pending-derivations db)))))
      ;; We cannot restart builds listed in STALE, so mark them as canceled.
      (log-message "canceling ~a stale builds" (length stale))
      (for-each (lambda (drv)
                  (db-update-build-status! db drv (build-status canceled)))
                stale)

      ;; Those in VALID can be restarted.  If some of them were built in the
      ;; meantime behind our back, that's fine: 'spawn-builds' will DTRT.
      (log-message "restarting ~a pending builds" (length valid))
      (spawn-builds store db valid)
      (log-message "done with restarted builds"))))

(define (build-packages store db jobs)
  "Build JOBS and return a list of Build results."
  (define (register job)
    (let* ((name     (assq-ref job #:job-name))
           (drv      (assq-ref job #:derivation))
           (eval-id  (assq-ref job #:eval-id))
           ;; XXX: How to keep logs from several attempts?
           (log      (log-file store drv))
           (outputs  (filter-map (lambda (res)
                                   (match res
                                     ((name . path)
                                      `(,name . ,path))))
                                 (derivation-path->output-paths drv)))
           (cur-time (time-second (current-time time-utc))))
      (let ((build `((#:derivation . ,drv)
                     (#:eval-id . ,eval-id)

                     ;; XXX: We'd leave LOG to #f (i.e., NULL) but that
                     ;; currently violates the non-NULL constraint.
                     (#:log . ,(or log ""))

                     (#:status . ,(build-status scheduled))
                     (#:outputs . ,outputs)
                     (#:timestamp . ,cur-time)
                     (#:starttime . 0)
                     (#:stoptime . 0))))
        (db-add-build db build))))

  (define build-ids
    (map register jobs))

  (spawn-builds store db
                (map (cut assq-ref <> #:derivation) jobs))

  (let* ((results (filter-map (cut db-get-build db <>) build-ids))
         (status (map (cut assq-ref <> #:status) results))
         (success (count (lambda (status)
                           (= status (build-status succeeded)))
                         status))
         (outputs (map (cut assq-ref <> #:outputs) results))
         (outs (append-map (match-lambda
                             (((_ (#:path . (? string? outputs))) ...)
                              outputs))
                           outputs))
         (fail (- (length jobs) success)))
    (log-message "outputs:\n~a" (string-join outs "\n"))
    (log-message "success: ~a, fail: ~a" success fail)
    results))

(define (prepare-git)
  "Prepare Guile-Git's TLS support and all."
  ;; Catch and report git errors.
  (with-git-error-handling
   ;; Try the 'GIT_SSL_CAINFO' or 'SSL_CERT_FILE' file first, then search the
   ;; 'SSL_CERT_DIR' directory.
   (let ((directory (getenv "SSL_CERT_DIR"))
         (file      (or (getenv "GIT_SSL_CAINFO")
                        (getenv "SSL_CERT_FILE"))))
     (when (or directory file)
       (set-tls-certificate-locations! directory file)))))

(define (process-specs db jobspecs)
  "Evaluate and build JOBSPECS and store results in DB."
  (define (process spec)
    (define compile?
      (not (assq-ref spec #:no-compile?)))

    (with-store store
      (let ((stamp (db-get-stamp db spec))
            (name  (assoc-ref spec #:name)))
         (log-message "considering spec '~a', URL '~a'"
                      name (assoc-ref spec #:url))
         (receive (checkout commit)
             (non-blocking (fetch-repository store spec
                                             #:writable-copy? compile?))
           (log-message "spec '~a': fetched commit ~s (stamp was ~s)"
                        name commit stamp)
           (when commit
             (unless (string=? commit stamp)
               ;; Immediately mark COMMIT as being processed so we don't spawn
               ;; a concurrent evaluation of that same commit.
               (db-add-stamp db spec commit)

               (when compile?
                 (non-blocking (compile checkout)))

               (spawn-fiber
                (lambda ()
                  (guard (c ((evaluation-error? c)
                             (log-message "failed to evaluate spec '~s'"
                                          (evaluation-error-spec-name c))
                             #f))
                    (log-message "evaluating '~a' with commit ~s"
                                 name commit)
                    (with-store store
                      (with-database db
                        (let* ((spec* (acons #:current-commit commit spec))
                               (jobs  (evaluate store db spec* checkout)))
                          (log-message "building ~a jobs for '~a'"
                                       (length jobs) name)
                          (build-packages store db jobs)))))))

               ;; 'spawn-fiber' returns zero values but we need one.
               *unspecified*))))))

  (for-each process jobspecs))


;;;
;;; Guix package path.
;;;

(define %guix-package-path
  ;; Extension of package modules search path.
  (make-parameter ""))

(define (set-guix-package-path! path)
  "Use PATH to find custom packages not defined in (gnu packages ...)
namespace or not already present in current Guile load paths.  PATH is
expected to be a colon-separated string of directories."
  (define (set-paths! dir)
    (%package-module-path (cons dir (%package-module-path)))
    (%patch-path (cons dir (%patch-path)))
    (set! %load-path (cons dir %load-path))
    (set! %load-compiled-path (cons dir %load-compiled-path)))

  (let ((dirs (parse-path path)))
    (for-each set-paths! dirs)))
