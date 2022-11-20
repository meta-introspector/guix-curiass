;;; remote-worker.scm -- Remote build worker.
;;; Copyright © 2020, 2021 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (cuirass scripts remote-worker)
  #:use-module (cuirass base)
  #:use-module (cuirass logging)
  #:use-module (cuirass remote)
  #:use-module (cuirass ui)
  #:use-module (gcrypt pk-crypto)
  #:use-module (guix avahi)
  #:use-module (guix config)
  #:use-module (guix derivations)
  #:use-module (guix diagnostics)
  #:use-module (guix pki)
  #:use-module (guix records)
  #:use-module (guix scripts)
  #:use-module (guix serialization)
  #:use-module ((guix store)
                #:select (%store-prefix
                          %default-substitute-urls
                          current-build-output-port
                          store-error?
                          store-protocol-error?
                          store-protocol-error-message
                          with-store))
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (guix scripts publish)
  #:use-module (simple-zmq)
  #:use-module (rnrs bytevectors)
  #:use-module (web uri)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:export (cuirass-remote-worker))

;; Indicate if the process has to be stopped.
(define %stop-process?
  (make-atomic-box #f))

;; The build request period in seconds.
(define %request-period
  (make-parameter
   (or (and=> (getenv "REQUEST_PERIOD")
              string->number)
       10)))

(define %substitute-urls
  (make-parameter #f))

(define (show-help)
  (format #t "Usage: ~a remote-worker [OPTION]...
Start a remote build worker.\n" (%program-name))
  (display (G_ "
  -w, --workers=COUNT       start COUNT parallel workers"))
  (display (G_ "
  -p, --publish-port=PORT   publish substitutes on PORT"))
  (display (G_ "
  -t, --ttl=DURATION        keep build results live for at least DURATION"))
  (display (G_ "
  -s, --server=SERVER       connect to SERVER"))
  (display (G_ "
  -S, --systems=SYSTEMS     list of supported SYSTEMS"))
  (display (G_ "
      --minimum-disk-space=THRESHOLD
                            refuse builds if free space is below THRESHOLD GiB"))
  (display (G_ "
      --substitute-urls=URLS
                            check for available substitutes at URLS"))
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
        (option '(#\w "workers") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'workers (string->number* arg) result)))
        (option '(#\p "publish-port") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'publish-port (string->number* arg) result)))
        (option '(#\t "ttl") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'ttl arg result)))
        (option '("minimum-disk-space") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'minimum-disk-space
                              (* (string->number* arg) (expt 2 30))
                              result)))
        (option '(#\s "server") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'server arg result)))
        (option '(#\S "systems") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'systems
                              (string-split arg #\,) result)))
        (option '("substitute-urls") #t #f
                (lambda (opt name arg result . rest)
                  (let ((urls (string-tokenize arg)))
                    (for-each (lambda (url)
                                (unless (string->uri url)
                                  (leave (G_ "~a: invalid URL~%") url)))
                              urls)
                    (apply values
                           (alist-cons 'substitute-urls urls
                                       (alist-delete 'substitute-urls result))
                           rest))))
        (option '("public-key") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'public-key-file arg result)))
        (option '("private-key") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'private-key-file arg result)))))

(define %minimum-disk-space
  ;; Minimum disk space required on the build machine before accepting more
  ;; builds.
  (make-parameter (* 5 (expt 2 30)))) ;5GiB

(define %default-options
  `((workers . 1)
    (minimum-disk-space . ,(%minimum-disk-space))
    (publish-port . 5558)
    (ttl . "1d")
    (systems . ,(list (%current-system)))
    (substitute-urls . ,%default-substitute-urls)
    (public-key-file . ,%public-key-file)
    (private-key-file . ,%private-key-file)))


;;;
;;; ZMQ connection.
;;;

(define %zmq-context
  (zmq-create-context))

(define (zmq-backend-endpoint address port)
  "Return a ZMQ endpoint identifying the build server available by TCP at
ADDRESS and PORT."
  (string-append "tcp://" address ":" (number->string port)))

(define (zmq-dealer-socket)
  "The ZMQ socket to communicate with the worker threads."
  (zmq-create-socket %zmq-context ZMQ_DEALER))


;;;
;;; Worker.
;;;

;; The port of the local publish server.
(define %local-publish-port
  (make-atomic-box #f))

(define (local-publish-url address)
  "Return the URL of the local publish server."
  (let ((port (atomic-box-ref %local-publish-port)))
    (publish-url address port)))

(define* (run-build drv server
                    #:key
                    reply
                    timeout
                    max-silent
                    worker)
  "Build DRV and send messages upon build start, failure or completion to the
build server identified by SERVICE-NAME using the REPLY procedure.

The publish server of the build server is added to the list of the store
substitutes-urls.  This way derivations that are not present on the worker can
still be substituted."
  (with-store store
    (let ((address (server-address server))
          (log-port (server-log-port server))
          (publish-url (server-publish-url server))
          (local-publish-url (worker-publish-url worker))
          (name (worker-name worker)))
      (set-build-options* store (if publish-url
                                    (cons publish-url (%substitute-urls))
                                    (%substitute-urls))
                          #:timeout timeout
                          #:max-silent max-silent)
      (reply (zmq-build-started-message drv name))
      (guard (c ((store-protocol-error? c)
                 (log-info (G_ "~a: derivation `~a' build failed: ~a")
                           name drv (store-protocol-error-message c))
                 (reply (zmq-build-failed-message drv local-publish-url))))
        (let ((result
               (let-values (((port finish)
                             (build-derivations& store (list drv))))
                 (catch 'system-error
                   (lambda ()
                     (send-log address log-port drv port))
                   (lambda args
                     (log-error (G_ "could not send logs to ~a:~a")
                                address log-port)
                     (dump-port port (%make-void-port "w"))))
                 (close-port port)
                 (finish))))
          (if result
              (begin
                (log-info (G_ "~a: derivation ~a build succeeded.")
                          name drv)
                (register-gc-roots drv)
                (reply (zmq-build-succeeded-message drv local-publish-url)))
              (begin
                (log-info (G_ "~a: derivation ~a build failed.")
                          name drv)
                (reply
                 (zmq-build-failed-message drv local-publish-url)))))))))

(define* (run-command command server
                      #:key
                      reply worker)
  "Run COMMAND.  SERVICE-NAME is the name of the build server that sent the
command.  REPLY is a procedure that can be used to reply to this server."
  (match (zmq-read-message command)
    (('build ('drv drv)
             ('priority priority)
             ('timeout timeout)
             ('max-silent max-silent)
             ('timestamp timestamp)
             ('system system))
     (log-info (G_ "~a: building `~a' derivation.")
               (worker-name worker) drv)
     (run-build drv server
                #:reply reply
                #:worker worker
                #:timeout timeout
                #:max-silent max-silent))
    (('no-build)
     (log-info (G_ "~a: no available build.")
               (worker-name worker))
     #t)))

(define (worker-ping worker server)
  (define (ping socket)
    (zmq-send-msg-parts-bytevector
     socket
     (list (make-bytevector 0)
           (string->bv
            (zmq-worker-ping (worker->sexp worker))))))

  (call-with-new-thread
   (lambda ()
     (let* ((socket (zmq-dealer-socket))
            (address (server-address server))
            (port (server-port server))
            (endpoint (zmq-backend-endpoint address port)))
       (zmq-connect socket endpoint)
       (let loop ()
         (log-info (G_ "~a: ping ~a.") (worker-name worker) endpoint)
         (ping socket)
         (sleep 60)
         (loop))))))

(define (low-disk-space?)
  "Return true if disk space is low."
  (or (< (free-disk-space (%store-prefix)) (%minimum-disk-space))
      (< (free-disk-space (or (getenv "TMPDIR") "/tmp"))
         (%minimum-disk-space))))

(define (start-worker wrk serv)
  "Start a worker thread named NAME, reading commands from the DEALER socket
and executing them.  The worker can reply on the same socket."
  (define (reply socket)
    (lambda (message)
      (zmq-send-msg-parts-bytevector
       socket
       (list (zmq-empty-delimiter) (string->bv message)))))

  (define (ready socket worker)
    (zmq-send-msg-parts-bytevector
     socket
     (list (make-bytevector 0)
           (string->bv
            (zmq-worker-ready-message (worker->sexp worker))))))

  (define (request-work socket worker)
    (let ((name (worker-name worker)))
      (zmq-send-msg-parts-bytevector
       socket
       (list (make-bytevector 0)
             (string->bv (zmq-worker-request-work-message name))))))

  (define (request-info socket)
    (zmq-send-msg-parts-bytevector
     socket
     (list (make-bytevector 0)
           (string->bv (zmq-worker-request-info-message)))))

  (define (read-server-info socket)
    (request-info socket)
    (match (zmq-get-msg-parts-bytevector socket '())
      ((empty info)
       (match (zmq-read-message (bv->string info))
         (('server-info
           ('worker-address worker-address)
           ('log-port log-port)
           ('publish-port publish-port))
          (list worker-address log-port publish-port))))))

  (define (server-info->server info serv)
    (match info
      ((_ log-port publish-port)
       (let ((url (and publish-port
                       (publish-url (server-address serv)
                                    publish-port))))
         (server
          (inherit serv)
          (log-port log-port)
          (publish-url url))))))

  (define (server-info->worker info w)
    (match info
      ((worker-address _ _)
       (let ((url (local-publish-url worker-address)))
         (worker
          (inherit w)
          (address worker-address)
          (publish-url url))))))

  (match (primitive-fork)
    (0
     (set-thread-name (worker-name wrk))
     (let* ((socket (zmq-dealer-socket))
            (address (server-address serv))
            (port (server-port serv))
            (endpoint (zmq-backend-endpoint address port)))
       (zmq-connect socket endpoint)
       (let* ((srv-info (read-server-info socket))
              (server (server-info->server srv-info serv))
              (worker (server-info->worker srv-info wrk)))
         (ready socket worker)
         (worker-ping worker server)
         (let loop ()
           (if (low-disk-space?)
               (log-info (G_ "warning: low disk space, doing nothing"))
               (begin
                 (log-info (G_ "~a: request work.") (worker-name wrk))
                 (request-work socket worker)
                 (match (zmq-get-msg-parts-bytevector socket '())
                   ((empty command)
                    (run-command (bv->string command) server
                                 #:reply (reply socket)
                                 #:worker worker)))))

           (sleep (%request-period))
           (loop)))))
    (pid pid)))


;;;
;;; Entry point.
;;;

;; The PID of the publish process.
(define %publish-pid
  (make-atomic-box #f))

(define %worker-pids
  (make-atomic-box '()))

(define (add-to-worker-pids! pid)
  (let ((pids (atomic-box-ref %worker-pids)))
    (atomic-box-set! %worker-pids (cons pid pids))))

(define (signal-handler)
  "Catch SIGINT to stop the Avahi event loop and the publish process before
exiting."
  (sigaction SIGINT
    (lambda (signum)
      (let ((publish-pid (atomic-box-ref %publish-pid))
            (worker-pids (atomic-box-ref %worker-pids)))
        (atomic-box-set! %stop-process? #t)

        (for-each (lambda (pid)
                    (when pid
                      (kill pid SIGKILL)
                      (waitpid pid)))
                  (cons publish-pid worker-pids))

        (exit 1)))))

(define (cuirass-remote-worker args)
  (signal-handler)
  (with-error-handling
    (let* ((opts (args-fold* (cdr args) %options
                             (lambda (opt name arg result)
                               (leave (G_ "~A: unrecognized option~%") name))
                             (lambda (arg result)
                               (leave (G_ "~A: extraneous argument~%") arg))
                             %default-options))
           (workers (assoc-ref opts 'workers))
           (publish-port (assoc-ref opts 'publish-port))
           (ttl (assoc-ref opts 'ttl))
           (server-address (assoc-ref opts 'server))
           (systems (assoc-ref opts 'systems))
           (urls    (assoc-ref opts 'substitute-urls))
           (public-key
            (read-file-sexp
             (assoc-ref opts 'public-key-file)))
           (private-key
            (read-file-sexp
             (assoc-ref opts 'private-key-file))))

      (false-if-exception (mkdir-p (%gc-root-directory)))

      (parameterize ((%gc-root-ttl (time-second (string->duration ttl)))
                     (%substitute-urls urls)
                     (%minimum-disk-space
                      (assoc-ref opts 'minimum-disk-space)))
        (atomic-box-set! %local-publish-port publish-port)

        (atomic-box-set!
         %publish-pid
         (publish-server publish-port
                         #:public-key public-key
                         #:private-key private-key))

        (if server-address
            (for-each
             (lambda (n)
               (let* ((worker (worker
                               (name (generate-worker-name))
                               (machine (gethostname))
                               (systems systems)))
                      (addr (string-split server-address #\:))
                      (server (match addr
                                ((address port)
                                 (server
                                  (address address)
                                  (port (string->number port)))))))
                 (add-to-worker-pids!
                  (start-worker worker server))))
             (iota workers))
            (avahi-browse-service-thread
             (lambda (action service)
               (case action
                 ((new-service)
                  (for-each
                   (lambda (n)
                     (let* ((address (avahi-service-local-address service))
                            (publish-url (local-publish-url address)))
                       (add-to-worker-pids!
                        (start-worker (worker
                                       (name (generate-worker-name))
                                       (address address)
                                       (machine (gethostname))
                                       (publish-url publish-url)
                                       (systems systems))
                                      (avahi-service->server service)))))
                   (iota workers))
                  (atomic-box-set! %stop-process? #t))))
             #:ignore-local? #f
             #:types (list remote-server-service-type)
             #:stop-loop? (lambda ()
                            (atomic-box-ref %stop-process?))))

        (while #t
          (sleep 1))))))
