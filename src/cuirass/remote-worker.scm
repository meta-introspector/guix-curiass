;;; remote-worker.scm -- Remote build worker.
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

(define-module (cuirass remote-worker)
  #:use-module (cuirass base)
  #:use-module (cuirass remote)
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
                #:select (current-build-output-port
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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)

  #:export (remote-worker))

;; Indicate if the process has to be stopped.
(define %stop-process?
  (make-atomic-box #f))

(define (show-help)
  (format #t (G_ "Usage: remote-worker [OPTION]...
Start a remote build worker.\n"))
  (display (G_ "
  -w, --workers=COUNT       start COUNT parallel workers"))
  (display (G_ "
  -p, --publish-port=PORT   publish substitutes on PORT"))
  (display (G_ "
  -S, --server=SERVER       connect to SERVER"))
  (display (G_ "
  -s, --systems=SYSTEMS     list of supported SYSTEMS"))
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
        (option '(#\a "address") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'address arg result)))
        (option '(#\w "workers") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'workers (string->number* arg) result)))
        (option '(#\p "publish-port") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'publish-port (string->number* arg) result)))
        (option '(#\s "server") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'server arg result)))
        (option '(#\S "systems") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'systems
                              (string-split arg #\,) result)))
        (option '("public-key") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'public-key-file arg result)))
        (option '("private-key") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'private-key-file arg result)))))

(define %default-options
  `((workers . 1)
    (publish-port . 5558)
    (systems . ,(list (%current-system)))
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
      (set-build-options* store publish-url
                          #:timeout timeout
                          #:max-silent max-silent)
      (reply (zmq-build-started-message drv name))
      (guard (c ((store-protocol-error? c)
                 (info (G_ "Derivation `~a' build failed: ~a~%")
                       drv (store-protocol-error-message c))
                 (reply (zmq-build-failed-message drv local-publish-url))))
        (let ((result
               (let-values (((port finish)
                             (build-derivations& store (list drv))))
                 (send-log address log-port drv port)
                 (close-port port)
                 (finish))))
          (if result
              (begin
                (info (G_ "Derivation ~a build succeeded.~%") drv)
                (reply (zmq-build-succeeded-message drv local-publish-url)))
              (begin
                (info (G_ "Derivation ~a build failed.~%") drv)
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
     (info (G_ "Building `~a' derivation.~%") drv)
     (run-build drv server
                #:reply reply
                #:worker worker
                #:timeout timeout
                #:max-silent max-silent))
    (('no-build)
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
         (ping socket)
         (sleep 60)
         (loop))))))

(define (start-worker worker server)
  "Start a worker thread named NAME, reading commands from the DEALER socket
and executing them.  The worker can reply on the same socket."
  (define (reply socket)
    (lambda (message)
      (zmq-send-msg-parts-bytevector
       socket
       (list (zmq-empty-delimiter) (string->bv message)))))

  (define (ready socket)
    (zmq-send-msg-parts-bytevector
     socket
     (list (make-bytevector 0)
           (string->bv
            (zmq-worker-ready-message (worker->sexp worker))))))

  (define (request-work socket)
    (let ((name (worker-name worker)))
      (zmq-send-msg-parts-bytevector
       socket
       (list (make-bytevector 0)
             (string->bv (zmq-worker-request-work-message name))))))

  (match (primitive-fork)
    (0
     (set-thread-name (worker-name worker))
     (let* ((socket (zmq-dealer-socket))
            (address (server-address server))
            (port (server-port server))
            (endpoint (zmq-backend-endpoint address port)))
       (zmq-connect socket endpoint)
       (ready socket)
       (worker-ping worker server)
       (let loop ()
         (request-work socket)
         (match (zmq-get-msg-parts-bytevector socket '())
           ((empty command)
            (run-command (bv->string command) server
                         #:reply (reply socket)
                         #:worker worker)))
         (sleep 10)
         (loop))))
    (pid pid)))


;;;
;;; Entry point.
;;;

;; The PID of the publish process.
(define %publish-pid
  (make-atomic-box #f))

(define %worker-pids
  (make-atomic-box '()))

(define (load-server file)
  (let ((user-module (make-user-module '((cuirass remote)))))
    (load* file user-module)))

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

(define (remote-worker args)
  (signal-handler)

  (with-error-handling
    (let* ((opts (args-fold* args %options
                             (lambda (opt name arg result)
                               (leave (G_ "~A: unrecognized option~%") name))
                             (lambda (arg result)
                               (leave (G_ "~A: extraneous argument~%") arg))
                             %default-options))
           (address (assoc-ref opts 'address))
           (workers (assoc-ref opts 'workers))
           (publish-port (assoc-ref opts 'publish-port))
           (server (assoc-ref opts 'server))
           (systems (assoc-ref opts 'systems))
           (public-key
            (read-file-sexp
             (assoc-ref opts 'public-key-file)))
           (private-key
            (read-file-sexp
             (assoc-ref opts 'private-key-file))))

      (atomic-box-set! %local-publish-port publish-port)

      (atomic-box-set!
       %publish-pid
       (publish-server publish-port
                       #:public-key public-key
                       #:private-key private-key))

      (when (and server (not address))
        (leave (G_ "Address must be set when server is provided.~%")))

      (if server
          (let ((server (load-server server)))
            (for-each
             (lambda (n)
               (let ((publish-url (local-publish-url address)))
                 (add-to-worker-pids!
                  (start-worker (worker
                                 (address address)
                                 (publish-url publish-url)
                                 (name (generate-worker-name))
                                 (systems systems))
                                server))))
             (iota workers))
            (while #t
              (sleep 1)))
          (avahi-browse-service-thread
           (lambda (action service)
             (case action
               ((new-service)
                (for-each
                 (lambda (n)
                   (let* ((address (or address
                                       (avahi-service-local-address service)))
                          (publish-url (local-publish-url address)))
                     (add-to-worker-pids!
                      (start-worker (worker
                                     (address address)
                                     (publish-url publish-url)
                                     (name (generate-worker-name))
                                     (systems systems))
                                    (avahi-service->server service)))))
                 (iota workers)))))
           #:ignore-local? #f
           #:types (list remote-server-service-type)
           #:stop-loop? (lambda ()
                          (atomic-box-ref %stop-process?)))))))
