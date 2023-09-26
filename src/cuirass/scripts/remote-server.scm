;;; remote-server.scm -- Remote build server.
;;; Copyright © 2020, 2021 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2023 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Cuirass.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (cuirass scripts remote-server)
  #:autoload   (cuirass base) (read-parameters set-build-successful!)
  #:use-module (cuirass config)
  #:use-module (cuirass database)
  #:use-module (cuirass logging)
  #:use-module (cuirass ui)
  #:use-module (cuirass notification)
  #:use-module (cuirass parameters)
  #:use-module (cuirass remote)
  #:use-module (cuirass store)
  #:use-module (cuirass utils)
  #:use-module (guix avahi)
  #:use-module (guix derivations)
  #:use-module ((guix pki) #:select (%public-key-file %private-key-file))
  #:use-module (guix scripts)
  #:autoload   (guix serialization) (nar-error?)
  #:use-module ((guix store)
                #:select (current-build-output-port
                          ensure-path
                          store-protocol-error?))
  #:use-module (guix ui)
  #:use-module ((guix utils)
                #:select (cache-directory call-with-temporary-output-file))
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:autoload   (guix build download) (url-fetch)
  #:autoload   (gcrypt pk-crypto) (read-file-sexp)
  #:use-module (simple-zmq)
  #:use-module (srfi srfi-1)
  #:use-module ((srfi srfi-19) #:select (time-second time-nanosecond))
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 match)
  #:use-module ((ice-9 threads)
                #:select (current-processor-count join-thread))
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:export (cuirass-remote-server))

;; Indicate if the process has to be stopped.
(define %stop-process?
  (make-atomic-box #f))

(define %cache-directory
  (make-parameter
   (string-append (cache-directory #:ensure? #t) "/cuirass")))

(define %trigger-substitute-url
  (make-parameter #f))

(define %private-key
  (make-parameter #f))

(define %public-key
  (make-parameter #f))

(define %log-port
  (make-parameter #f))

(define %publish-port
  (make-parameter #f))

(define service-name
  "Cuirass remote server")

;; The number of queued fetch requests.
(define %fetch-queue-size
  (make-atomic-box 0))

(define (show-help)
  (format #t (G_ "Usage: ~a remote-server [OPTION]...
Start a remote build server.\n") (%program-name))
  (display (G_ "
  -b, --backend-port=PORT   listen worker connections on PORT"))
  (display (G_ "
  -l, --log-port=PORT       listen build logs on PORT"))
  (display (G_ "
  -p, --publish-port=PORT   publish substitutes on PORT"))
  (display (G_ "
  -P, --parameters=FILE     Read parameters from FILE"))
  (display (G_ "
  -t, --ttl=DURATION        keep build results live for at least DURATION"))
  (display (G_ "
  -D, --database=DB         Use DB to read and store build results"))
  (display (G_ "
  -c, --cache=DIRECTORY     cache built items to DIRECTORY"))
  (display (G_ "
  -T, --trigger-substitute-url=URL
                            trigger substitute baking at URL"))
  (display (G_ "
  -u, --user=USER           change privileges to USER as soon as possible"))
  (display (G_ "
      --no-publish          do not start a publish server"))
  (display (G_ "
      --public-key=FILE     use FILE as the public key for signatures"))
  (display (G_ "
      --private-key=FILE    use FILE as the private key for signatures"))
  (newline)
  (display (G_ "
  -h, --help                display this help and exit"))
  (display (G_ "
  -V, --version             display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda _
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda _
                  (show-version-and-exit "cuirass remote-server")))
        (option '(#\b "backend-port") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'backend-port (string->number* arg) result)))
        (option '(#\l "log-port") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'log-port (string->number* arg) result)))
        (option '(#\p "publish-port") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'publish-port (string->number* arg) result)))
        (option '(#\P "parameters") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'parameters arg result)))
        (option '(#\t "ttl") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'ttl arg result)))
        (option '(#\D "database") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'database arg result)))
        (option '(#\c "cache") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'cache arg result)))
        (option '(#\s "no-publish") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'no-publish #t result)))
        (option '(#\T "trigger-substitute-url") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'trigger-substitute-url arg result)))
        (option '(#\u "user") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'user arg result)))
        (option '("public-key") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'public-key-file arg result)))
        (option '("private-key") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'private-key-file arg result)))))

(define %default-options
  `((backend-port     . 5555)
    (log-port         . 5556)
    (publish-port     . 5557)
    (no-publish       . #f)
    (ttl              . "3d")
    (public-key-file  . ,%public-key-file)
    (private-key-file . ,%private-key-file)))


;;;
;;; Atomic procedures.
;;;

(define (atomic-box-fetch-and-update! box proc)
  "Atomically fetch the value stored inside BOX, pass it to the PROC procedure
and store the result inside the BOX."
  (let lp ((cur (atomic-box-ref box)))
    (let* ((next (proc cur))
           (cur* (atomic-box-compare-and-swap! box cur next)))
      (if (eqv? cur cur*)
          cur
          (lp cur*)))))

(define (atomic-box-fetch-and-inc! box)
  "Atomically increment the value of the integer stored inside the given BOX."
  (atomic-box-fetch-and-update! box 1+))

(define (atomic-box-fetch-and-dec! box)
  "Atomically decrement the value of the integer stored inside the given BOX."
  (atomic-box-fetch-and-update! box 1-))


;;;
;;; Build workers.
;;;

(define (random-seed)
  (logxor (getpid) (car (gettimeofday))))

(define shuffle                            ;copied from (guix scripts offload)
  (let ((state (seed->random-state (random-seed))))
    (lambda (lst)
      "Return LST shuffled (using the Fisher-Yates algorithm.)"
      (define vec (list->vector lst))
      (let loop ((result '())
                 (i (vector-length vec)))
        (if (zero? i)
            result
            (let* ((j (random i state))
                   (val (vector-ref vec j)))
              (vector-set! vec j (vector-ref vec (- i 1)))
              (loop (cons val result) (- i 1))))))))

(define (pop-build name)
  "Return a pending build that worker NAME can perform."
  (let ((worker (db-get-worker name)))
    (and worker
         (any db-get-pending-build
              (shuffle (worker-systems worker))))))

(define* (read-worker-exp sexp #:key peer-address reply-worker)
  "Read the given SEXP sent by a worker.  REPLY-WORKER is a procedure that can
be used to reply to the worker."
  (define (update-worker! base-worker)
    (let* ((worker* (worker
                     (inherit (sexp->worker base-worker))
                     (last-seen (current-time)))))
      (log-debug (G_ "worker ~a is up and running")
                 (worker-name worker*))
      (db-add-or-update-worker worker*)))

  (match sexp
    (('worker-ready worker)
     (update-worker! worker))
    (('worker-request-info)
     (reply-worker
      (server-info-message peer-address (%log-port) (%publish-port))))
    (('worker-request-work name)
     (let ((worker (db-get-worker name)))
       (when worker
         (log-debug "~a (~a): request work."
                    (worker-address worker)
                    (worker-name worker)))
       (let ((build (pop-build name)))
         (if build
             (let ((derivation (build-derivation build))
                   (priority (build-priority build))
                   (timeout (build-timeout build))
                   (max-silent (build-max-silent-time build)))
               (when worker
                 (log-debug "~a (~a): build ~a submitted."
                            (worker-address worker)
                            (worker-name worker)
                            derivation))
               (db-update-build-worker! derivation name)
               (db-update-build-status! derivation (build-status submitted))
               (reply-worker
                (build-request-message derivation
                                       #:priority priority
                                       #:timeout timeout
                                       #:max-silent max-silent
                                       #:system (build-system build))))
             (begin
               (when worker
                 (log-debug "~a (~a): no available build."
                            (worker-address worker)
                            (worker-name worker)))
               (reply-worker
                (no-build-message)))))))
    (('worker-ping worker)
     (update-worker! worker))
    (('build-started ('drv drv) ('worker name))
     (let ((log-file (log-path (%cache-directory) drv))
           (worker (db-get-worker name)))
       (when worker
         (log-info "~a (~a): build started: '~a'."
                      (worker-address worker)
                      (worker-name worker)
                      drv))
       (db-update-build-worker! drv name)
       (db-update-build-status! drv (build-status started)
                                #:log-file log-file)))))


;;;
;;; Fetch workers.
;;;

(define (url-fetch* url file)
  (parameterize ((current-output-port (%make-void-port "w"))
                 (current-error-port (%make-void-port "w")))
    (url-fetch url file)))

(define (publish-narinfo-url publish-url store-hash)
  "Return the URL of STORE-HASH narinfo file on PUBLISH-URL."
  (let ((hash (and=> (string-index store-hash #\-)
                     (cut string-take store-hash <>))))
    (format #f "~a/~a.narinfo" publish-url hash)))

(define (ensure-path* store output)
  (guard (c ((store-protocol-error? c)
             (log-error "Failed to add ~a to store: store protocol error." output)
             (log-error "The remote-worker signing key might not be authorized.")
             #f)
            ((nar-error? c)
             (log-error "Failed to add ~a to store: nar error." output)
             (log-error "The guix-daemon process may have returned unexpectedly.")
             #f))
    (ensure-path store output)))

(define (trigger-substitutes-baking output url)
  (let* ((store-hash (strip-store-prefix output))
         (narinfo-url (publish-narinfo-url url store-hash)))
    (log-debug "Bake: ~a" narinfo-url)
    (call-with-temporary-output-file
     (lambda (tmp-file port)
       (url-fetch* narinfo-url tmp-file)))))

(define (add-to-store drv outputs url)
  "Add the OUTPUTS that are available from the substitute server at URL to the
store.  Register GC roots for the matching DRV and trigger a substitute baking
at URL."
  (parameterize ((current-build-output-port (%make-void-port "w")))
    (with-store/non-blocking store
      (set-build-options* store (list url))
      (for-each
       (lambda (output)
         (and (ensure-path* store output)
              (register-gc-roots drv)

              ;; Force the baking of the NAR substitutes so that the
              ;; first client doesn't receive a 404 error.
              (when (%trigger-substitute-url)
                (trigger-substitutes-baking output
                                            (%trigger-substitute-url)))))
       (map derivation-output-path outputs)))))

(define (need-fetching? message)
  "Return #t if the received MESSAGE implies that some output fetching is
required and #f otherwise."
  (match message
    (('build-succeeded ('drv drv) _ ...)
     (log-debug "fetching required for ~a (success)" drv)
     #t)
    (('build-failed ('drv drv) _ ...)
     (log-debug "fetching required for ~a (fail)" drv)
     #t)
    (_ #f)))

(define* (run-fetch message)
  "Read MESSAGE and download the corresponding build outputs.  If
%CACHE-DIRECTORY is set, download the matching NAR and NARINFO files in this
directory."
  (define (build-outputs drv)
    (catch 'system-error
      (lambda ()
        (map (match-lambda
               ((output-name . output)
                output))
             (derivation-outputs
              (read-derivation-from-file drv))))
      (const '())))

  (match message
    (('build-succeeded ('drv drv) ('url url) _ ...)
     (let ((outputs (build-outputs drv)))
       (log-info "fetching '~a' from ~a" drv url)
       (call-with-time
        (lambda ()
          (add-to-store drv outputs url))
        (lambda (time result)
          (let ((duration (+ (time-second time)
                             (/ (time-nanosecond time) 1e9))))
            (when (> duration 60)
              (log-warning "fetching '~a' took ~a seconds."
                           drv duration)))))
       (log-info "build succeeded: '~a'" drv)
       (set-build-successful! drv)))
    (('build-failed ('drv drv) ('url url) _ ...)
     (log-info "build failed: '~a'" drv)
     (db-update-build-status! drv (build-status failed)))))

(define (fetch-worker channel max-parallel-downloads)
  (lambda ()
    (let ((pool (make-resource-pool (iota max-parallel-downloads))))
      (log-info "starting fetch worker with up to ~a concurrent downloads"
                max-parallel-downloads)
      (let loop ()
        (let ((message (get-message channel)))
          (atomic-box-fetch-and-inc! %fetch-queue-size)
          (spawn-fiber
           (lambda ()
             (with-resource-from-pool pool token
               (log-debug "fetching with token #~a" token)
               (run-fetch message)
               (atomic-box-fetch-and-dec! %fetch-queue-size)))))
        (loop)))))

(define* (spawn-fetch-worker #:key (max-parallel-downloads 8))
  "Spawn a fetch worker fiber, which takes care of downloading build outputs as
requested received on its channel."
  (let ((channel (make-channel)))
    (spawn-fiber (fetch-worker channel max-parallel-downloads))
    channel))


;;;
;;; Periodic updates.
;;;

(define (spawn-periodic-updates-fiber)
  "Start a fiber running periodic update queries."
  (spawn-fiber
   (lambda ()
     (let loop ()
       (let ((resumable (db-update-resumable-builds!))
             (failed (db-update-failed-builds!)))
         (log-info "period update: ~a resumable, ~a failed builds."
                   resumable failed)
         (log-info "period update: ~a items in the fetch queue."
                   (atomic-box-ref %fetch-queue-size)))
       (sleep 30)
       (loop)))))


;;;
;;; ZMQ connection.
;;;

(define %zmq-context
  (zmq-create-context))

(define (zmq-backend-endpoint backend-port)
  "Return a ZMQ endpoint string allowing TCP connections on BACKEND-PORT from
all network interfaces."
  (string-append "tcp://*:" (number->string backend-port)))

(define (zmq-start-proxy backend-port fetch-worker)
  "Open a zmq socket on BACKEND-PORT and listen for messages coming from
'cuirass remote-worker' messages.  When a message denoting a successful build
is received, pass it on to FETCH-WORKER to download the build's output(s)."
  (let ((build-socket (zmq-create-socket %zmq-context ZMQ_ROUTER)))

    ;; Send bootstrap messages on worker connection to wake up the workers
    ;; that were hanging waiting for request-work responses.
    (zmq-set-socket-option build-socket ZMQ_PROBE_ROUTER 1)

    (zmq-bind-socket build-socket (zmq-backend-endpoint backend-port))

    (spawn-fiber
     (lambda ()
       (let loop ()
         (sleep (quotient (%worker-timeout) 2))
         (log-debug (G_ "updating list of live workers"))
         (db-remove-unresponsive-workers (%worker-timeout))
         (loop))))

    ;; Do not use the built-in zmq-proxy as we want to edit the envelope of
    ;; frontend messages before forwarding them to the backend.
    (let loop ()
      (let* ((command sender sender-address
                      (receive-message build-socket #:router? #t))
             (reply-worker (lambda (message)
                             (send-message build-socket message
                                           #:recipient sender))))
        (if (need-fetching? command)
            (put-message fetch-worker command)
            (read-worker-exp command
                             #:peer-address sender-address
                             #:reply-worker reply-worker))
        (loop)))))


;;;
;;; Entry point.
;;;

;; The PID of the publish process.
(define %publish-pid
  (make-atomic-box #f))

;; The thread running the Avahi publish service.
(define %avahi-thread
  (make-atomic-box #f))

(define (terminate-helper-processes)
  (let ((publish-pid (atomic-box-ref %publish-pid))
        (avahi-thread (atomic-box-ref %avahi-thread)))
    (atomic-box-set! %stop-process? #t)

    (when publish-pid
      (kill publish-pid SIGHUP)
      (waitpid publish-pid))

    (when avahi-thread
      (join-thread avahi-thread))))

(define (signal-handler)
  "Catch SIGINT to stop the Avahi event loop and the publish process before
exiting."
  (sigaction SIGINT
    (lambda (signum)
      (terminate-helper-processes)
      (primitive-exit 1))))

(define (gather-user-privileges user)
  "switch to the identity of user, a user name."
  (catch 'misc-error
    (lambda ()
      (let ((user (getpw user)))
        (setgroups #())
        (setgid (passwd:gid user))
        (setuid (passwd:uid user))))
    (lambda (key proc message args . rest)
      (leave (G_ "user '~a' not found: ~a~%")
             user (apply format #f message args)))))

(define (cuirass-remote-server args)
  (signal-handler)
  (with-error-handling
    (let* ((opts (args-fold* (cdr args) %options
                             (lambda (opt name arg result)
                               (leave (G_ "~A: unrecognized option~%") name))
                             (lambda (arg result)
                               (leave (G_ "~A: extraneous argument~%") arg))
                             %default-options))
           (backend-port (assoc-ref opts 'backend-port))
           (log-port (assoc-ref opts 'log-port))
           (no-publish (assoc-ref opts 'no-publish))
           (publish-port (and (not no-publish)
                              (assoc-ref opts 'publish-port)))
           (cache (assoc-ref opts 'cache))
           (parameters (assoc-ref opts 'parameters))
           (ttl (assoc-ref opts 'ttl))
           (database (assoc-ref opts 'database))
           (trigger-substitute-url (assoc-ref opts 'trigger-substitute-url))
           (user (assoc-ref opts 'user))
           (public-key
            (read-file-sexp
             (assoc-ref opts 'public-key-file)))
           (private-key
            (read-file-sexp
             (assoc-ref opts 'private-key-file))))

      (parameterize ((%log-port log-port)
                     (%publish-port publish-port)
                     (%trigger-substitute-url trigger-substitute-url)
                     (%package-database database)
                     (%gc-root-ttl
                      (time-second (string->duration ttl)))
                     (%public-key public-key)
                     (%private-key private-key))

        ;; Enable core dump generation.
        (setrlimit 'core #f #f)

        ;; Increase max open files limit to 2048.
        (let ((limit 2048))
          (call-with-values (lambda () (getrlimit 'nofile))
            (lambda (soft hard)
              (when (and soft (< soft limit))
                (if hard
                    (setrlimit 'nofile (min hard limit) hard)
                    (setrlimit 'nofile limit #f))
                (log-info
                 "increased maximum number of open files from ~d to ~d"
                 soft (if hard (min hard limit) limit))))))

        ;; Show wider backtraces.
        (setenv "COLUMNS" "500")

        (and cache
             (%cache-directory cache))

        (when user
          (gather-user-privileges user))

        (and parameters
             (read-parameters parameters))

        (mkdir-p (%cache-directory))

        ;; Reset the GC root directory now that we have gathered user
        ;; privileges.
        (%gc-root-directory
         (default-gc-root-directory))

        (unless no-publish
          (atomic-box-set!
           %publish-pid
           (publish-server publish-port
                           #:public-key public-key
                           #:private-key private-key)))

        (atomic-box-set!
         %avahi-thread
         (avahi-publish-service-thread
          service-name
          #:type remote-server-service-type
          #:port backend-port
          #:stop-loop? (lambda ()
                         (atomic-box-ref %stop-process?))
          #:txt `(,(string-append "log-port="
                                  (number->string log-port))
                  ,@(if publish-port
                        (list (string-append "publish-port="
                                             (number->string publish-port)))
                        '()))))

        (run-fibers
         (lambda ()
           (with-database
             (receive-logs log-port (%cache-directory))
             (spawn-notification-fiber)
             (spawn-periodic-updates-fiber)

             (let ((fetch-worker (spawn-fetch-worker)))
               (catch 'zmq-error
                 (lambda ()
                   (zmq-start-proxy backend-port fetch-worker))
                 (lambda (key errno message . _)
                   (log-error (G_ "failed to start worker/database proxy: ~a")
                              message)
                   (terminate-helper-processes)
                   (primitive-exit 1))))))
         #:hz 0
         #:parallelism (min 8 (current-processor-count)))))))
