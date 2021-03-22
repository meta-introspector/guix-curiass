;;; base.scm -- Cuirass base module
;;; Copyright © 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017, 2020, 2021 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
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
  #:use-module (fibers channels)
  #:use-module (cuirass logging)
  #:use-module (cuirass database)
  #:use-module (cuirass remote)
  #:use-module (cuirass specification)
  #:use-module (cuirass utils)
  #:use-module ((cuirass config) #:select (%localstatedir))
  #:use-module (gnu packages)
  #:use-module (guix build utils)
  #:use-module (guix channels)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (guix git)
  #:use-module (guix cache)
  #:use-module (zlib)
  #:use-module ((guix config) #:select (%state-directory))
  #:use-module (git)
  #:use-module (ice-9 binary-ports)
  #:use-module ((ice-9 suspendable-ports)
                #:select (current-read-waiter
                          current-write-waiter))
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 ports internal)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs bytevectors)
  #:export (;; Procedures.
            call-with-time-display
            read-parameters
            evaluate
            build-derivations&
            set-build-successful!
            clear-build-queue
            cancel-old-builds
            restart-builds
            build-packages
            prepare-git
            process-specs
            evaluation-log-file

            ;; Parameters.
            %package-cachedir
            %gc-root-directory
            %gc-root-ttl
            %build-remote?
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
     ;; sees build events; set #:build-verbosity 1 so that we don't receive
     ;; output from the builders (that is, we only get build traces, nothing
     ;; more), which in turn makes sure we can correctly process build traces.
     (set-build-options store
                        #:use-substitutes? (%use-substitutes?)
                        #:fallback? (%fallback?)
                        #:keep-going? #t
                        #:print-build-trace #t
                        #:build-verbosity 1)
     exp ...
     (close-connection store))))

(cond-expand
  (guile-2.2
   ;; Guile 2.2.2 has a bug whereby 'time-monotonic' objects have seconds and
   ;; nanoseconds swapped (fixed in Guile commit 886ac3e).  Work around it.
   (define time-monotonic time-tai))
  (else #t))

(define %build-remote?
  ;; Define whether to use the remote build mechanism.
  (make-parameter #f))

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

(define %gc-root-directory
  ;; Directory where garbage collector roots are stored.  We register build
  ;; outputs there.
  (make-parameter (string-append %state-directory
                                 "/gcroots/profiles/per-user/"
                                 (passwd:name (getpwuid (getuid)))
                                 "/cuirass")))

(define %gc-root-ttl
  ;; The "time to live" (TTL) of GC roots.
  (make-parameter (* 30 24 3600)))

(define (gc-root-expiration-time file)
  "Return \"expiration time\" of FILE (a symlink in %GC-ROOT-DIRECTORY)
computed as its modification time + TTL seconds."
  (match (false-if-exception (lstat file))
    (#f 0)                         ;FILE may have been deleted in the meantime
    (st (+ (stat:mtime st) (%gc-root-ttl)))))

(define (register-gc-root item)
  "Create a GC root pointing to ITEM, a store item."
  (catch 'system-error
    (lambda ()
      (symlink item
               (string-append (%gc-root-directory)
                              "/" (basename item))))
    (lambda args
      ;; If the symlink already exist, assume it points to ITEM.
      (unless (= EEXIST (system-error-errno args))
        (apply throw args)))))

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

(define-condition-type &evaluation-error &error
  evaluation-error?
  (name evaluation-error-spec-name)
  (id   evaluation-error-id))

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

(define %cuirass-state-directory
  ;; Directory where state files are stored, usually "/var".
  (make-parameter (or (getenv "CUIRASS_STATE_DIRECTORY")
                      %localstatedir)))

(define (evaluation-log-file eval-id)
  "Return the name of the file containing the output of evaluation EVAL-ID."
  (string-append (%cuirass-state-directory)
                 "/log/cuirass/evaluations/"
                 (number->string eval-id) ".gz"))

(define (evaluate store spec eval-id)
  "Evaluate and build package derivations defined in SPEC, using CHECKOUTS.
Return a list of jobs that are associated to EVAL-ID."
  (define log-file
    (evaluation-log-file eval-id))

  (define log-pipe
    (pipe))

  (mkdir-p (dirname log-file))

  ;; Spawn a fiber that reads standard error from 'evaluate' and writes it to
  ;; LOG-FILE.
  (spawn-fiber
   (lambda ()
     (define input
       (non-blocking-port (car log-pipe)))

     (define output
       ;; Note: Don't use 'call-with-gzip-output-port' as it doesn't play well
       ;; with fibers (namely, its dynamic-wind handler would close the output
       ;; port as soon as a context switch occurs.)
       (make-gzip-output-port (open-output-file log-file)
                              #:level 8 #:buffer-size 16384))

     (dump-port input output)
     (close-port input)
     (close-port output)))

  (let* ((port (non-blocking-port
                (with-error-to-port (cdr log-pipe)
                  (lambda ()
                    (open-pipe* OPEN_READ "cuirass"
                                "evaluate"
                                (%package-database)
                                (object->string eval-id))))))
         (result (match (read/non-blocking port)
                   ;; If an error occured during evaluation report it,
                   ;; otherwise, suppose that data read from port are
                   ;; correct and keep things going.
                   ((? eof-object?)
                    (db-set-evaluation-status eval-id
                                              (evaluation-status failed))
                    (close-port (cdr log-pipe))
                    (raise (condition
                            (&evaluation-error
                             (name (specification-name spec))
                             (id eval-id)))))
                   (_ #t))))
    (close-port (cdr log-pipe))
    (close-pipe port)
    (let ((spec-name (specification-name spec)))
      (log-message "evaluation ~a for '~a' completed" eval-id spec-name))))


;;;
;;; Read parameters.
;;;

(define (read-parameters file)
  (let ((modules (make-user-module '((cuirass parameters)))))
    (load* file modules)))


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
            (guard (c ((store-error? c)
                       (atomic-box-set! result c)))
              (parameterize ((current-build-output-port output))
                (let ((x (build-derivations store lst)))
                  (atomic-box-set! result x))))
            (close-port output))
          (lambda _
            (close-port output)))))

     (values input
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

(define* (set-build-successful! drv #:optional log)
  "Update the build status of DRV as successful and register any eventual
build products."
  (let* ((build (db-get-build drv))
         (spec  (and build
                     (db-get-specification
                      (assq-ref build #:specification)))))
    (when (and spec build)
      (create-build-outputs build
                            (specification-build-outputs spec))))
  (db-update-build-status! drv (build-status succeeded)
                           #:log-file log))

(define (update-build-statuses! store lst)
  "Update the build status of the derivations listed in LST, which have just
been passed to 'build-derivations' (meaning that we can assume that, if their
outputs are invalid, that they failed to build.)"
  (define (update! drv)
    (match (false-if-exception
            (derivation-path->output-paths drv))
      (((_ . outputs) ...)
       (if (any (cut valid-path? store <>) outputs)
           (set-build-successful! drv)
           (db-update-build-status! drv
                                    (if (log-file store drv)
                                        (build-status failed)
                                        (build-status failed-dependency)))))
      (else
       (db-update-build-status! drv (build-status failed)))))

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

(define* (spawn-builds store drv
                       #:key
                       (max-batch-size 200))
  "Build the derivations listed in DRV, updating the database as builds
complete.  Derivations are submitted in batches of at most MAX-BATCH-SIZE
items."
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
          (guard (c ((store-protocol-error? c)
                     (log-message "batch of builds (partially) failed: \
~a (status: ~a)"
                                  (store-protocol-error-message c)
                                  (store-protocol-error-status c))))
            (log-message "building batch of ~a derivations (~a/~a)"
                         max-batch-size (- total count) total)
            (let-values (((port finish)
                          (build-derivations& store batch)))
              (process-build-log port
                                 (lambda (event state)
                                   ;; Catch any errors so we can keep reading
                                   ;; from PORT and eventually close it.
                                   (catch #t
                                     (lambda ()
                                       (handle-build-event event))
                                     (exception-reporter state)))
                                 #t)
              (close-port port)
              (finish)))

          ;; Most of the time 'handle-build-event' will update the build
          ;; status of derivations.  However, it could be that some
          ;; derivations were built "behind our back", in which case
          ;; 'build-derivations' doesn't actually do anything and
          ;; 'handle-build-event' doesn't see any event.  Because of that,
          ;; adjust the database here.
          (update-build-statuses! store batch)

          (loop rest (max (- count max-batch-size) 0))))))

(define* (handle-build-event event)
  "Handle EVENT, a build event sexp as produced by 'build-event-output-port',
updating the database accordingly."
  (define (valid? file)
    ;; When builder output is turned off (build-verbosity = 1), we normally
    ;; only see valid derivation file names in EVENT.  To be on the safe side,
    ;; double-check that this is the case.
    (and (store-path? file)
         (string-suffix? ".drv" file)))

  (define (gc-roots directory)
    ;; Return the list of GC roots (symlinks) in DIRECTORY.
    (map (cut string-append directory "/" <>)
         (scandir directory
                  (lambda (file)
                    (not (member file '("." "..")))))))

  (match event
    (('build-started drv _ ...)
     (if (valid? drv)
         (begin
           (log-message "build started: '~a'" drv)
           (db-update-build-status! drv (build-status started)))
         (log-message "bogus build-started event for '~a'" drv)))
    (('build-remote drv host _ ...)
     (log-message "'~a' offloaded to '~a'" drv host)
     (db-update-build-worker! drv host))
    (('build-succeeded drv _ ...)
     (if (valid? drv)
         (begin
           (log-message "build succeeded: '~a'" drv)
           (set-build-successful! drv)

           (for-each (match-lambda
                       ((name . output)
                        (register-gc-root output)))
                     (derivation-path->output-paths drv))
           (maybe-remove-expired-cache-entries (%gc-root-directory)
                                               gc-roots
                                               #:entry-expiration
                                               gc-root-expiration-time))
         (log-message "bogus build-succeeded event for '~a'" drv)))
    (('build-failed drv _ ...)
     (if (valid? drv)
         (begin
           (log-message "build failed: '~a'" drv)
           (db-update-build-status! drv (build-status failed)))
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

(define (clear-build-queue)
  "Reset the status of builds in the database that are marked as \"started\".
This procedure is meant to be called at startup."
  (log-message "marking stale builds as \"scheduled\"...")
  (db-clear-build-queue))

(define (restart-builds)
  "Restart builds whose status in the database is \"pending\" (scheduled or
started)."
  (with-store store
    (log-message "retrieving list of pending builds...")
    (let*-values (((valid stale)
                   (partition (cut valid-path? store <>)
                              (db-get-pending-derivations))))
      ;; We cannot restart builds listed in STALE, so mark them as canceled.
      (log-message "canceling ~a stale builds" (length stale))
      (for-each (lambda (drv)
                  (db-update-build-status! drv (build-status canceled)))
                stale)

      ;; Those in VALID can be restarted.  If some of them were built in the
      ;; meantime behind our back, that's fine: 'spawn-builds' will DTRT.
      (log-message "restarting ~a pending builds" (length valid))
      (unless (%build-remote?)
        (spawn-builds store valid))
      (log-message "done with restarted builds"))))

(define (create-build-outputs build build-outputs)
  "Given BUILDS a list of built derivations, save the build products described
by BUILD-OUTPUTS."
  (define (build-has-products? job-regex)
    (let ((job-name (assq-ref build #:job-name)))
      (string-match job-regex job-name)))

  (define* (find-product build build-output)
    (let* ((outputs (assq-ref build #:outputs))
           (output (build-output-output build-output))
           (path (build-output-path build-output))
           (root (and=> (assoc-ref outputs output)
                        (cut assq-ref <> #:path))))
      (and root
           (if (string=? path "")
               root
               (string-append root "/" path)))))

  (define (file-size file)
    (stat:size (stat file)))

  (for-each (lambda (build-output)
              (let ((product (and (build-has-products?
                                   (build-output-job build-output))
                                  (find-product build build-output))))
                (when (and product (file-exists? product))
                  (log-message "Adding build product ~a" product)
                  (db-add-build-product
                   `((#:build . ,(assq-ref build #:id))
                     (#:type . ,(build-output-type build-output))
                     (#:file-size . ,(file-size product))
                     ;; TODO: Implement it.
                     (#:checksum . "")
                     (#:path . ,product))))))
            build-outputs))

(define (build-packages store eval-id)
  "Build JOBS and return a list of Build results."
  (define builds
    (db-get-builds `((evaluation . ,eval-id))))

  (define derivations
    (map (cut assq-ref <> #:derivation) builds))

  (log-message "evaluation ~a registered ~a new derivations"
               eval-id (length derivations))
  (db-set-evaluation-status eval-id
                            (evaluation-status succeeded))

  (unless (%build-remote?)
    (spawn-builds store derivations)

    (let* ((results (filter-map (cut db-get-build <>) derivations))
           (status (map (cut assq-ref <> #:status) results))
           (success (count (lambda (status)
                             (= status (build-status succeeded)))
                           status))
           (outputs (map (cut assq-ref <> #:outputs) results))
           (outs (append-map (match-lambda
                               (((_ (#:path . (? string? outputs))) ...)
                                outputs))
                             outputs))
           (fail (- (length derivations) success)))

      (log-message "outputs:\n~a" (string-join outs "\n"))
      results)))

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

(define (latest-channel-instances* . args)
  (parameterize ((current-output-port (%make-void-port "w"))
                 (current-error-port (%make-void-port "w"))
                 (guix-warning-port (%make-void-port "w")))
    (apply latest-channel-instances args)))

(define (process-specs jobspecs)
  "Evaluate and build JOBSPECS and store results in the database."
  (define (process spec)
    (with-store store
      (let* ((name (specification-name spec))
             (timestamp (time-second (current-time time-utc)))
             (channels (specification-channels spec))
             (instances (non-blocking
                         (log-message "Fetching channels for spec '~a'." name)
                         (latest-channel-instances* store channels
                                                    #:authenticate? #f)))
             (checkouttime (time-second (current-time time-utc)))
             (eval-id (db-add-evaluation name instances
                                         #:timestamp timestamp
                                         #:checkouttime checkouttime)))
        (when eval-id
          (spawn-fiber
           (lambda ()
             (guard (c ((evaluation-error? c)
                        (log-message "failed to evaluate spec '~a'; see ~a"
                                     (evaluation-error-spec-name c)
                                     (evaluation-log-file
                                      (evaluation-error-id c)))
                        #f))
               (log-message "evaluating spec '~a'" name)
               (with-store store
                 (evaluate store spec eval-id)
                 (db-set-evaluation-time eval-id)
                 (build-packages store eval-id)))))

          ;; 'spawn-fiber' returns zero values but we need one.
          *unspecified*))))

  (for-each (lambda (spec)
              ;; Catch Git errors, which might be transient, and keep going.
              (catch 'git-error
                (lambda ()
                  (process spec))
                (lambda (key error)
                  (log-message "Git error while fetching inputs of '~a': ~s~%"
                               (specification-name spec)
                               (git-error-message error)))))
            jobspecs))
