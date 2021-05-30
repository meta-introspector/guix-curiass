;;; remote-server.scm -- Remote build server.
;;; Copyright © 2020 Mathieu Othacehe <othacehe@gnu.org>
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
  #:use-module (cuirass base)
  #:use-module (cuirass config)
  #:use-module (cuirass database)
  #:use-module (cuirass logging)
  #:use-module (cuirass ui)
  #:use-module (cuirass notification)
  #:use-module (cuirass remote)
  #:use-module (cuirass utils)
  #:use-module (gcrypt pk-crypto)
  #:use-module (guix avahi)
  #:use-module (guix base32)
  #:use-module (guix base64)
  #:use-module (guix config)
  #:use-module (guix derivations)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix pki)
  #:use-module (guix scripts)
  #:use-module ((guix store)
                #:select (current-build-output-port
                          ensure-path
                          store-protocol-error?
                          with-store))
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix workers)
  #:use-module (guix build download)
  #:use-module (guix build syscalls)
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:use-module (gcrypt hash)
  #:use-module (gcrypt pk-crypto)
  #:use-module (simple-zmq)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module ((srfi srfi-19) #:select (time-second))
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 threads)
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

;; The number of fetch worker threads.
(define %fetch-workers
  (make-parameter 8))

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
                  (show-version-and-exit "guix publish")))
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
    (ttl              . "3d")
    (public-key-file  . ,%public-key-file)
    (private-key-file . ,%private-key-file)))


;;;
;;; Build workers.
;;;

(define (pop-build name)
  (define (random-system systems)
    (list-ref systems (random (length systems))))

  (let ((worker (db-get-worker name)))
    (and worker
         (let ((system (random-system
                        (worker-systems worker))))
           (match (db-get-builds `((status . scheduled)
                                   (system . ,system)
                                   (order . priority+timestamp)
                                   (no-dependencies . #t)
                                   (nr . 1)))
             ((build) build)
             (() #f))))))

(define* (read-worker-exp msg #:key reply-worker)
  "Read the given MSG sent by a worker.  REPLY-WORKER is a procedure that can
be used to reply to the worker."
  (define (update-worker! base-worker)
    (let* ((worker* (worker
                     (inherit (sexp->worker base-worker))
                     (last-seen (current-time)))))
      (db-add-or-update-worker worker*)))

  (match (zmq-read-message
          (zmq-message-string msg))
    (('worker-ready worker)
     (update-worker! worker))
    (('worker-request-info)
     (reply-worker
      (zmq-server-info (zmq-remote-address msg) (%log-port) (%publish-port))))
    (('worker-request-work name)
     (let ((build (pop-build name)))
       (if build
           (let ((derivation (assq-ref build #:derivation))
                 (priority (assq-ref build #:priority))
                 (timeout (assq-ref build #:timeout))
                 (max-silent (assq-ref build #:max-silent)))
             (db-update-build-worker! derivation name)
             (db-update-build-status! derivation (build-status submitted))
             (reply-worker
              (zmq-build-request-message derivation
                                         #:priority priority
                                         #:timeout timeout
                                         #:max-silent max-silent)))
           (reply-worker
            (zmq-no-build-message)))))
    (('worker-ping worker)
     (update-worker! worker))
    (('build-started ('drv drv) ('worker worker))
     (let ((log-file (log-path (%cache-directory) drv)))
       (log-message "build started: '~a' on ~a." drv worker)
       (db-update-build-worker! drv worker)
       (db-update-build-status! drv (build-status started)
                                #:log-file log-file)))))


;;;
;;; Fetch workers.
;;;

(define (zmq-fetch-workers-endpoint)
  "inproc://fetch-workers")

(define (zmq-fetch-worker-socket)
  "Return a socket used to communicate with the fetch workers."
  (let ((socket (zmq-create-socket %zmq-context ZMQ_PULL))
        (endpoint (zmq-fetch-workers-endpoint)))
    (zmq-connect socket endpoint)
    socket))

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
             (log-message "Failed to add ~a to store." output)
             #f))
    (ensure-path store output)))

(define (add-to-store outputs url)
  "Add the OUTPUTS that are available from the substitute server at URL to the
store."
  (parameterize ((current-build-output-port (%make-void-port "w")))
    (with-store store
      (set-build-options* store url)
      (for-each (lambda (output)
                  (ensure-path* store output))
                (map derivation-output-path outputs)))))

(define (trigger-substitutes-baking outputs url)
  (for-each (lambda (output)
              (let* ((path (derivation-output-path output))
                     (store-hash (strip-store-prefix path))
                     (narinfo-url (publish-narinfo-url url store-hash)))
                (call-with-temporary-output-file
                 (lambda (tmp-file port)
                   (url-fetch* narinfo-url tmp-file)))))
            outputs))

(define (need-fetching? message)
  "Return #t if the received MESSAGE implies that some output fetching is
required and #f otherwise."
  (match (zmq-read-message message)
    (('build-succeeded _ ...)
     #t)
    (('build-failed _ ...)
     #t)
    (else #f)))

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

  (match (zmq-read-message message)
    (('build-succeeded ('drv drv) ('url url) _ ...)
     (let ((outputs (build-outputs drv)))
       (log-message "fetching '~a' from ~a" drv url)
       (add-to-store outputs url)
       (register-gc-roots drv)

       ;; Force the baking of the NAR substitutes so that the first client
       ;; doesn't receive a 404 error.
       (when (%trigger-substitute-url)
         (trigger-substitutes-baking outputs (%trigger-substitute-url)))

       (log-message "build succeeded: '~a'" drv)
       (set-build-successful! drv)))
    (('build-failed ('drv drv) ('url url) _ ...)
     (log-message "build failed: '~a'" drv)
     (db-update-build-status! drv (build-status failed)))))

(define (start-fetch-worker name)
  "Start a fetch worker thread with the given NAME.  This worker takes care of
downloading build outputs.  It communicates with the remote server using a ZMQ
socket."
  (call-with-new-thread
   (lambda ()
     (use-modules (cuirass parameters)) ;XXX: Needed for mu-debug variable.
     (set-thread-name name)
     (let ((socket (zmq-fetch-worker-socket)))
       (let loop ()
         (match (zmq-message-receive* socket)
           ((message)
            (run-fetch (bv->string
                        (zmq-message-content message)))))
         (loop))))))


;;;
;;; Periodic updates.
;;;

(define (start-periodic-updates-thread)
  "Start a thread running periodic update queries."
  (call-with-new-thread
   (lambda ()
     (set-thread-name "periodic-updates")
     (let loop ()
       (let ((resumable (db-update-resumable-builds!))
             (failed (db-update-failed-builds!)))
         (log-message "period update: ~a resumable, ~a failed builds."
                      resumable failed))
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

(define (zmq-start-proxy backend-port)
  "This procedure starts a proxy between client connections from the IPC
frontend to the workers connected through the TCP backend."
  (define (socket-ready? items socket)
    (find (lambda (item)
            (eq? (poll-item-socket item) socket))
          items))

  ;; The poll loop below must not be blocked.  Print a warning message if a
  ;; loop iteration takes more than %LOOP-TIMEOUT seconds to complete.
  (define %loop-timeout 5)

  (let* ((build-socket
          (zmq-create-socket %zmq-context ZMQ_ROUTER))
         (fetch-socket
          (zmq-create-socket %zmq-context ZMQ_PUSH))
         (poll-items (list
                      (poll-item build-socket ZMQ_POLLIN))))

    (zmq-bind-socket build-socket (zmq-backend-endpoint backend-port))
    (zmq-bind-socket fetch-socket (zmq-fetch-workers-endpoint))

    ;; Do not use the built-in zmq-proxy as we want to edit the envelope of
    ;; frontend messages before forwarding them to the backend.
    (let loop ()
      (let* ((items (zmq-poll* poll-items 1000))
             (start-time (current-time)))
        (when (zmq-socket-ready? items build-socket)
          (match (zmq-message-receive* build-socket)
            ((worker empty rest)
             (let* ((fetch-msg (zmq-msg-init
                                (zmq-message-content rest)))
                    (command (bv->string
                              (zmq-message-content rest)))
                    (reply-worker
                    (lambda (message)
                      (zmq-message-send-parts
                       build-socket
                       (map zmq-msg-init
                            (list (zmq-message-content worker)
                                  (zmq-empty-delimiter)
                                  (string->bv message)))))))
               (if (need-fetching? command)
                   (zmq-message-send fetch-socket fetch-msg)
                   (read-worker-exp rest
                                    #:reply-worker reply-worker))))))
        (db-remove-unresponsive-workers (%worker-timeout))
        (let ((delta (- (current-time) start-time)))
          (when (> delta %loop-timeout)
            (log-message "Poll loop busy during ~a seconds." delta)))
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

(define (signal-handler)
  "Catch SIGINT to stop the Avahi event loop and the publish process before
exiting."
  (sigaction SIGINT
    (lambda (signum)
      (let ((publish-pid (atomic-box-ref %publish-pid))
            (avahi-thread (atomic-box-ref %avahi-thread)))
        (atomic-box-set! %stop-process? #t)

        (and publish-pid
             (begin
               (kill publish-pid SIGHUP)
               (waitpid publish-pid)))

        (and avahi-thread
             (join-thread avahi-thread))

        (exit 1)))))

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
           (publish-port (assoc-ref opts 'publish-port))
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

        (and cache
             (%cache-directory cache))

        (mkdir-p (%cache-directory))

        (when user
          (gather-user-privileges user))

        (and parameters
             (read-parameters parameters))

        ;; Reset the GC root directory now that we have gathered user
        ;; privileges.
        (%gc-root-directory
         (default-gc-root-directory))

        (atomic-box-set!
         %publish-pid
         (publish-server publish-port
                         #:public-key public-key
                         #:private-key private-key))

        (atomic-box-set!
         %avahi-thread
         (avahi-publish-service-thread
          service-name
          #:type remote-server-service-type
          #:port backend-port
          #:stop-loop? (lambda ()
                         (atomic-box-ref %stop-process?))
          #:txt (list (string-append "log-port="
                                     (number->string log-port))
                      (string-append "publish-port="
                                     (number->string publish-port)))))

        (receive-logs log-port (%cache-directory))

        (with-database
            (start-notification-thread)
            (start-periodic-updates-thread)
            (for-each (lambda (number)
                        (start-fetch-worker
                         (string-append "fetch-worker-"
                                        (number->string number))))
                      (iota (%fetch-workers)))

            (zmq-start-proxy backend-port))))))
