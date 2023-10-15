;;; base.scm -- Cuirass base module
;;; Copyright © 2016-2019, 2022-2023 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (cuirass store)
  #:use-module (cuirass utils)
  #:use-module ((cuirass config) #:select (%localstatedir))
  #:use-module (gnu packages)
  #:use-module (guix build utils)
  #:use-module (guix channels)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (guix git)
  #:autoload   (guix build syscalls) (set-thread-name)
  #:use-module (zlib)
  #:use-module (git)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 ports internal)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:autoload   (ice-9 threads) (current-processor-count)
  #:use-module (ice-9 vlist)
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
            set-build-successful!
            clear-build-queue
            cancel-old-builds
            restart-builds
            prepare-git

            spawn-local-builder
            spawn-remote-builder
            spawn-channel-update-service
            spawn-jobset-evaluator
            spawn-jobset-registry

            lookup-jobset
            register-jobset
            update-jobset

            evaluation-log-file
            latest-checkouts

            ;; Parameters.
            %bridge-socket-file-name
            %package-cachedir
            %fallback?))

;;; Commentary:
;;;
;;; This file splits continuous integration activity among several "actors",
;;; currently all running inside the 'cuirass register' process.  Actors are
;;; implemented as fibers that serve requests received as messages on their
;;; channel.  The main actors are the following:
;;;
;;;  - The "channel updater" is responsible for updating Git checkouts for
;;;    channels.  There's a single instance of this actor; it limits
;;;    concurrent Git updates.
;;;
;;;  - The "evaluator" spawns evaluations of jobsets for the given channel
;;;    instances, again limiting the number of concurrent evaluations.
;;;
;;;  - The "builder" spawns derivation builds.  There are currently two
;;;    implementations: the local builder sends build requests to the local
;;;    'guix-daemon' process, while the remote build delegates builds to
;;;    'cuirass remote-server'.
;;;
;;;  - Each jobset as an associated "monitor"; it requests channel updates,
;;;    evaluations, and builds to the actors above.  It also receives requests
;;;    such as evaluation triggers that can come, for example, from the
;;;    /jobset/NAME/hook/evaluate HTTP endpoint.
;;;
;;;  - The "jobset" registry is a directory that maps jobset names to their
;;;    monitor.
;;;
;;; Code:

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

(define %cuirass-state-directory
  ;; Directory where state files are stored, usually "/var".
  (make-parameter (or (getenv "CUIRASS_STATE_DIRECTORY")
                      %localstatedir)))

(define %cuirass-run-state-directory
  ;; Directory where state files with the same lifetime as the process are
  ;; stored, usually "/var/run".
  (make-parameter (or (getenv "CUIRASS_RUN_STATE_DIRECTORY")
                      (string-append (%cuirass-state-directory) "/run"))))

(define %bridge-socket-file-name
  (make-parameter (string-append (%cuirass-run-state-directory)
                                 "/cuirass/bridge")))


;;;
;;; Read parameters.
;;;

(define (read-parameters file)
  (let ((modules (make-user-module '((cuirass parameters)))))
    (load* file modules)))


;;;
;;; Building packages.
;;;

(define (shuffle-derivations drv)
  "Shuffle DRV, a list of derivation file names."
  ;; Our shuffling algorithm is simple: we sort by .drv file name.  :-)
  (sort drv string<?))

(define* (set-build-successful! drv)
  "Update the build status of DRV as successful and register any eventual
build products."
  (let* ((build (db-get-build drv))
         (spec  (and build
                     (db-get-specification
                      (build-specification-name build)))))
    (when (and spec build)
      (create-build-outputs build
                            (specification-build-outputs spec))))
  (db-update-build-status! drv (build-status succeeded)))

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

  (set-build-options store
                     #:keep-going? #t
                     #:print-build-trace #t)
  (log-info "building ~a derivations in batches of ~a"
            total max-batch-size)

  ;; Shuffle DRV so that we don't build sequentially i686/x86_64/aarch64,
  ;; master/core-updates, etc., which would be suboptimal.
  (let loop ((drv   (shuffle-derivations drv))
             (count total))
    (if (zero? count)
        (log-info "done with ~a derivations" total)
        (let*-values (((batch rest)
                       (if (> count max-batch-size)
                           (split-at drv max-batch-size)
                           (values drv '()))))
          (guard (c ((store-protocol-error? c)
                     (log-error "batch of builds (partially) failed: \
~a (status: ~a)"
                                (store-protocol-error-message c)
                                (store-protocol-error-status c))))
            (log-info "building batch of ~a derivations (~a/~a)"
                      max-batch-size (- total count) total)
            (let-values (((port finish)
                          (build-derivations& store batch)))
              (process-build-log port
                                 (lambda (event state)
                                   ;; Catch any errors so we can keep reading
                                   ;; from PORT and eventually close it.
                                   (catch #t
                                     (lambda ()
                                       (handle-build-event store event))
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

(define* (handle-build-event store event)
  "Handle EVENT, a build event sexp as produced by 'build-event-output-port',
updating the database accordingly."
  (define (valid? file)
    ;; When builder output is turned off (build-verbosity = 1), we normally
    ;; only see valid derivation file names in EVENT.  To be on the safe side,
    ;; double-check that this is the case.
    (and (store-path? file)
         (string-suffix? ".drv" file)))

  (match event
    (('build-started drv _ ...)
     (if (valid? drv)
         (begin
           (log-info "build started: '~a'" drv)
           (db-update-build-status! drv (build-status started)
                                    #:log-file (log-file store drv)))
         (log-error "bogus build-started event for '~a'" drv)))
    (('build-remote drv host _ ...)
     (log-info "'~a' offloaded to '~a'" drv host)
     (db-update-build-worker! drv host))
    (('build-succeeded drv _ ...)
     (if (valid? drv)
         (begin
           (log-info "build succeeded: '~a'" drv)
           (set-build-successful! drv)
           (register-gc-roots drv))
         (log-warning "bogus build-succeeded event for '~a'" drv)))
    (('build-failed drv _ ...)
     (if (valid? drv)
         (begin
           (log-info "build failed: '~a'" drv)
           (db-update-build-status! drv (build-status failed)))
         (log-warning "bogus build-failed event for '~a'" drv)))
    (('substituter-started item _ ...)
     (log-debug "substituter started: '~a'" item))
    (('substituter-succeeded item _ ...)
     (log-debug "substituter succeeded: '~a'" item))
    (_
     (log-debug "build event: ~s" event))))

(define (clear-build-queue)
  "Reset the status of builds in the database that are marked as \"started\".
This procedure is meant to be called at startup."
  (log-info "marking stale builds as \"scheduled\"...")
  (db-clear-build-queue))

(define (restart-builds builder)
  "Restart builds whose status in the database is \"pending\" (scheduled or
started) by sending them to BUILDER."
  (with-store store
    (log-info "retrieving list of pending builds...")
    (let*-values (((valid stale)
                   (partition (cut valid-path? store <>)
                              (db-get-pending-derivations))))
      ;; We cannot restart builds listed in STALE, so mark them as canceled.
      (log-info "canceling ~a stale builds" (length stale))
      (for-each (lambda (drv)
                  (db-update-build-status! drv (build-status canceled)))
                stale)

      ;; Those in VALID can be restarted.  If some of them were built in the
      ;; meantime behind our back, that's fine: 'spawn-builds' will DTRT.
      (log-info "restarting ~a pending builds" (length valid))
      (put-message builder `(build ,valid)))))

(define (create-build-outputs build outputs)
  "Given BUILDS, a list of <build> records, save the build products described by
OUTPUTS, a list of <build-output> records."
  (define (build-has-products? job-regex)
    (let ((job-name (build-job-name build)))
      (string-match job-regex job-name)))

  (define* (find-product build build-output)
    (let* ((outputs (build-outputs build))
           (output (build-output-output build-output))
           (path (build-output-path build-output))
           (root (and=> (find (lambda (o)
                                (string=? (output-name o) output))
                              outputs)
                        output-item)))
      (and root
           (if (string=? path "")
               root
               (string-append root "/" path)))))

  (define (file-size file)
    (stat:size (stat file)))

  (for-each (lambda (build-output)
              (let ((file (and (build-has-products?
                                (build-output-job build-output))
                               (find-product build build-output))))
                (when (and file (file-exists? file))
                  (log-info "Adding build product ~a" file)
                  (catch 'system-error
                    (lambda ()
                      (register-gc-root file))
                    (lambda args
                      ;; This might be ENOENT, for instance because
                      ;; /var/guix/gcroots/profiles is missing, as is the case
                      ;; in build environments.
                      (log-warning
                       "failed to create GC root for '~a' (build '~a'): ~a"
                       file (build-nix-name build)
                       (strerror (system-error-errno args)))))
                  (db-add-build-product
                   (build-product
                    (build-id (build-id build))
                    (type (build-output-type build-output))
                    (file file)
                    (file-size (file-size file))
                    (checksum ""))))))            ;TODO: Implement it.
            outputs))

(define (local-builder channel)
  (lambda ()
    (log-info "builds will be made via the local build daemon")
    (let loop ()
      (match (get-message channel)
        (`(build ,derivations)
         (spawn-fiber
          (lambda ()
            (with-store/non-blocking store
              (spawn-builds store derivations)

              (let* ((results (filter-map (cut db-get-build <>) derivations))
                     (status (map build-current-status results))
                     (success (count (lambda (status)
                                       (= status (build-status succeeded)))
                                     status))
                     (outputs (map build-outputs results))
                     (outs (append-map build-output-path outputs))
                     (fail (- (length derivations) success)))

                (log-info "outputs:\n~a" (string-join outs "\n"))
                results))))))
      (loop))))

(define (spawn-local-builder)
  "Spawn a build actor that executes the derivation build requests it receives
by handing them to the local build daemon."
  (let ((channel (make-channel)))
    (spawn-fiber (local-builder channel))
    channel))

(define (remote-builder channel)
  (lambda ()
    (log-info "builds will be delegated to 'cuirass remote-server'")
    (let loop ()
      (match (get-message channel)
        (`(build ,derivations)
         ;; Currently there's nothing to do here: 'cuirass remote-server'
         ;; periodically calls 'db-get-pending-build'.
         ;; TODO: Push notifications to 'remote-server' instead.
         (log-info "~a pending derivation builds" (length derivations))))
      (loop))))

(define (spawn-remote-builder)
  "Spawn a build actor that performs builds using \"remote workers\"."
  (let ((channel (make-channel)))
    (spawn-fiber (remote-builder channel))
    channel))


;;;
;;; Updating Git checkouts.
;;;

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

(define (latest-checkouts spec eval-id)
  "Return the complete list of checkouts used for the EVAL-ID evaluation of
SPEC (as opposed to 'db-get-checkouts', which only returns checkouts that
different from the previous evaluation of SPEC)."
  (let ((name (specification-name spec))
        (channels (specification-channels spec)))
    (map (lambda (channel)
           (let ((channel (channel-name channel)))
             (db-get-latest-checkout name channel eval-id)))
         channels)))

(define exception-with-kind-and-args?
  (exception-predicate &exception-with-kind-and-args))

(define (channel-update-service channel)
  "Return a thunk (an actor) that reads messages on CHANNEL and is responsible
to update Git checkouts, effectively serializing all Git operations."
  ;; Note: All Git operations are serialized when in fact it would be enough
  ;; to serialize operations with the same URL (because they are cached in the
  ;; same directory).
  (define (fetch store channels)
    (let/ec return
      (with-exception-handler
          (lambda (exception)
            (if (exception-with-kind-and-args? exception)
                (match (exception-kind exception)
                  ('git-error
                   (log-error "Git error while fetching channels from~{ ~a~}: ~a"
                              (map channel-url channels)
                              (git-error-message
                               (first (exception-args exception)))))
                  ('system-error
                   (log-error "while fetching channels from~{ ~a~}': ~s"
                              (map channel-url channels)
                              (strerror
                               (system-error-errno
                                (cons 'system-error
                                      (exception-args exception))))))
                  (kind
                   (log-error "uncaught '~a' exception: ~s"
                              kind (exception-args exception))))
                (log-error "uncaught exception: ~s" exception))
            (return #f))
        (lambda ()
          (non-blocking
           (set-thread-name "git-checkout")
           (latest-channel-instances* store channels))))))

  (lambda ()
    (with-store store
      (let loop ()
        (match (get-message channel)
          (`(fetch ,channels ,reply)
           (log-info "fetching channels:~{ '~a'~}"
                     (map channel-name channels))
           (let ((result (fetch store channels)))
             (if result
                 (log-info "pulled commits~{ ~a~}"
                           (zip (map (compose channel-name
                                              channel-instance-channel)
                                     result)
                                (map channel-instance-commit result)))
                 (log-info "failed to fetch channels~{ '~a'~}"
                           (map channel-name channels)))
             (put-message reply result))
           (loop)))))))

(define (spawn-channel-update-service)
  "Spawn an actor responsible for fetching the latest revisions of a set of Guix
channels, and return its communication channel."
  (let ((channel (make-channel)))
    (spawn-fiber (channel-update-service channel))
    channel))


;;;
;;; Evaluating jobsets.
;;;

(define (evaluation-log-file eval-id)
  "Return the name of the file containing the output of evaluation EVAL-ID."
  (string-append (%cuirass-state-directory)
                 "/log/cuirass/evaluations/"
                 (number->string eval-id) ".gz"))

(define (evaluate spec eval-id)
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
         (result (match (read port)
                   ;; If an error occured during evaluation report it,
                   ;; otherwise, suppose that data read from port are
                   ;; correct and keep things going.
                   ((? eof-object?)
                    (db-set-evaluation-status eval-id
                                              (evaluation-status failed))
                    #f)
                   (_ #t))))
    (close-port (cdr log-pipe))
    (let ((spec-name (specification-name spec))
          (status (close-pipe port)))
      (if (and (zero? status) result)
          (log-info "evaluation ~a for '~a' completed" eval-id spec-name)
          (begin
            (log-info "evaluation ~a for '~a' failed" eval-id spec-name)
            (raise (condition
                    (&evaluation-error
                     (name (specification-name spec))
                     (id eval-id)))))))))

(define (start-evaluation spec instances timestamp)
  "Start an evaluation of SPEC using the given channel INSTANCES.  Return #f if
nothing has changed (and thus no new evaluation was created), otherwise return
the ID of the new evaluation."
  (let* ((channels (map channel-instance-channel instances))
         (new-spec (specification
                    (inherit spec)
                    ;; Include possible channel dependencies
                    (channels channels)))
         (checkouttime (time-second (current-time time-utc)))
         (eval-id (db-add-evaluation (specification-name spec) instances
                                     #:timestamp timestamp
                                     #:checkouttime checkouttime)))

    (and eval-id
         (guard (c ((evaluation-error? c)
                    (log-error "failed to evaluate spec '~a'; see ~a"
                               (evaluation-error-spec-name c)
                               (evaluation-log-file
                                (evaluation-error-id c)))
                    #f))
           (log-info "evaluating spec '~a'" (specification-name spec))

           ;; The LATEST-CHANNEL-INSTANCES procedure may return channel
           ;; dependencies that are not declared in the initial specification
           ;; channels.  Update the given SPEC to take them into account.
           (db-add-or-update-specification new-spec)
           (evaluate spec eval-id)
           (db-set-evaluation-time eval-id)

           eval-id))))

(define* (jobset-evaluator channel
                           #:key
                           builder
                           (max-parallel-evaluations
                            (current-processor-count)))
  (define pool
    (make-resource-pool (iota max-parallel-evaluations)))

  (lambda ()
    (log-info "will perform up to ~a evaluations concurrently"
              max-parallel-evaluations)
    (let loop ()
      (match (get-message channel)
        (`(evaluate ,spec ,instances ,timestamp)
         ;; Take a token a perform the given evaluation.
         (spawn-fiber
          (lambda ()
            (define eval-id
              (with-resource-from-pool pool token
                (log-info "evaluating '~a' with token #~a"
                          (specification-name spec) token)
                (start-evaluation spec instances timestamp)))

            (when eval-id
              (let* ((builds (db-get-builds `((evaluation . ,eval-id))))
                     (derivations (map build-derivation builds)))
                (log-info "evaluation ~a of jobset '~a' registered ~a builds"
                          eval-id (specification-name spec) (length builds))
                (db-set-evaluation-status eval-id
                                          (evaluation-status succeeded))

                ;; Register a GC root for each derivation so that they are not
                ;; garbage collected before getting built.
                (for-each (cut register-gc-roots <> #:mode 'derivation)
                          derivations)

                ;; Let BUILDER build those derivations.
                (put-message builder `(build ,derivations))))))
         (loop))))))


(define* (spawn-jobset-evaluator #:key
                                 builder
                                 (max-parallel-evaluations
                                  (current-processor-count)))
  "Spawn the actor responsible for evaluating jobsets for a given spec and set
of channel instances.  The actor performs at most MAX-PARALLEL-EVALUATIONS
concurrently; it sends derivation build requests to BUILDER."
  (let ((channel (make-channel)))
    (spawn-fiber (jobset-evaluator channel
                                   #:builder builder
                                   #:max-parallel-evaluations
                                   max-parallel-evaluations))
    channel))

(define %jobset-trigger-rate-window
  ;; Window (seconds) over which the jobset trigger rate is computed.
  (* 5 60))                                       ;5 minutes

(define %jobset-trigger-maximum-rate
  ;; Maximum rate (triggers per seconds) at which jobsets may be triggered.
  (/ 3 (* 2 60.)))                                ;3 times in 2 minutes

(define* (jobset-monitor channel spec
                         #:key (polling-period 60)
                         update-service evaluator)
  (define name (specification-name spec))

  (lambda ()
    (log-info "starting monitor for spec '~a'" name)
    (let loop ((spec spec)
               (last-updates '()))
      (define period
        (if (> (specification-period spec) 0)
            (specification-period spec)
            polling-period))

      (define channels
        (specification-channels spec))

      (define (perform-update)
        (let* ((timestamp (time-second (current-time time-utc)))
               (recent? (lambda (time)
                          (>= time (- timestamp %jobset-trigger-rate-window)))))
          (define (rate lst)
            ;; Return the (approximate) trigger rate (triggers per second).
            (/ (count recent? lst) %jobset-trigger-rate-window 1.))

          ;; Mitigate the risk of a DoS attack by rejecting frequent requests.
          (if (> (rate last-updates) %jobset-trigger-maximum-rate)
              (begin
                (log-warning "trigger rate for jobset '~a' exceeded; skipping"
                             name)
                (loop spec last-updates))
              (begin
                (match (let ((reply (make-channel)))
                         (log-info "fetching channels for spec '~a'" name)
                         (put-message update-service
                                      `(fetch ,channels ,reply))
                         (get-message reply))
                  (#f
                   (log-warning "failed to fetch channels for '~a'" name))
                  (instances
                   (log-info "fetched channels for '~a':~{ ~a~}"
                             name (map channel-name channels))
                   (put-message evaluator
                                `(evaluate ,spec ,instances ,timestamp))))

                (loop spec
                      (cons timestamp (take-while recent? last-updates)))))))

      (if (specification-is-active? spec)
          (if (null? last-updates)                ;first time?
              (perform-update)
              (match (get-message* channel polling-period 'timeout)
                ('timeout
                 (log-info "polling jobset '~a' after ~as timeout expiry"
                           name polling-period)
                 (perform-update))
                ('trigger
                 (log-info "triggered update of jobset '~a'" name)
                 (perform-update))
                (`(update-spec ,spec)
                 (log-info "updating spec of jobset '~a'" name)
                 (loop spec last-updates))
                (message
                 (log-warning "jobset '~a' got bogus message: ~s"
                              name message)
                 (loop spec last-updates))))
          (match (get-message channel)            ;currently inactive
            (`(update-spec ,spec)
             (log-info "updating spec of inactive jobset '~a'" name)
             (loop spec last-updates))
            (message
             (log-warning "inactive jobset '~a' got unexpected message: ~s"
                          name message)
             (loop spec last-updates)))))))

(define* (spawn-jobset-monitor spec
                               #:key (polling-period 60)
                               update-service evaluator)
  "Spawn an actor responsible for monitoring the jobset corresponding to SPEC,
a <specification> record, and return it.  The actor will send messages to
UPDATE-SERVICE anytime it needs Guix channels to be updated, at most every
POLLING-PERIOD seconds."
  (let ((channel (make-channel)))
    (spawn-fiber (jobset-monitor channel spec
                                 #:update-service update-service
                                 #:evaluator evaluator
                                 #:polling-period polling-period))
    channel))


;;;
;;; Jobset registry.
;;;

(define* (jobset-registry channel
                          #:key (polling-period 60)
                          update-service evaluator)
  (lambda ()
    (spawn-fiber
     (lambda ()
       (let ((specs (db-get-specifications #:filter-inactive? #f)))
         (log-info "registering ~a jobsets" (length specs))
         (for-each (lambda (spec)
                     (register-jobset channel spec))
                   specs))))

    (let loop ((registry vlist-null))
      (match (get-message channel)
        (`(lookup ,jobset ,reply)
         (put-message reply
                      (match (vhash-assq jobset registry)
                        (#f #f)
                        ((_ . actor) actor)))
         (loop registry))
        (`(update ,spec)
         (let ((name (string->symbol (specification-name spec))))
           (match (vhash-assq name registry)
             (#f
              (log-error "cannot update non-existent spec '~s'" name))
             ((_ . monitor)
              (put-message monitor `(update-spec ,spec)))))
         (loop registry))
        (`(register ,spec)
         (match (vhash-assq (specification-name spec) registry)
           (#f
            (let* ((period (match (specification-period spec)
                             (0 polling-period)
                             (period period)))
                   (monitor (spawn-jobset-monitor spec
                                                  #:update-service
                                                  update-service
                                                  #:evaluator evaluator
                                                  #:polling-period period))
                   (name (specification-name spec)))
              (log-info "registering new jobset '~a'" name)
              (loop (vhash-consq (string->symbol name) monitor
                                 registry))))
           ((_ . monitor)
            (log-info "jobset '~a' was already registered"
                      (specification-name spec))
            (loop registry))))))))

(define* (spawn-jobset-registry #:key (polling-period 60)
                                 update-service evaluator)
  "Spawn a jobset registry.  In turn, the registry creates a new jobset
monitoring actor for each 'register' message it receives."
  (let ((channel (make-channel)))
    (spawn-fiber (jobset-registry channel
                                  #:update-service update-service
                                  #:evaluator evaluator
                                  #:polling-period polling-period))
    channel))

(define* (lookup-jobset registry jobset)
  "Return the monitor of JOBSET, a specification name (symbol)."
  (let ((reply (make-channel)))
    (put-message registry `(lookup ,jobset ,reply))
    (get-message reply)))

(define (register-jobset registry spec)
  "Register a new jobset of SPEC.  REGISTRY is the channel returned by
'spawn-jobset-registry'."
  (put-message registry `(register ,spec)))

(define* (update-jobset registry spec)
  "Update SPEC, so far known under FORMER-NAME, in REGISTRY."
  (put-message registry `(update ,spec)))
