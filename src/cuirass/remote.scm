;;; remote.scm -- Build on remote machines.
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

(define-module (cuirass remote)
  #:use-module (cuirass logging)
  #:use-module (cuirass utils)
  #:use-module (guix avahi)
  #:use-module (guix config)
  #:use-module (guix derivations)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix build download)
  #:use-module (guix build syscalls)
  #:use-module ((guix build utils) #:select (dump-port mkdir-p))
  #:use-module (guix scripts publish)
  #:use-module (simple-zmq)
  #:use-module (zlib)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:export (worker
            worker?
            worker-name
            worker-address
            worker-machine
            worker-publish-url
            worker-systems
            worker-last-seen
            worker->sexp
            sexp->worker
            generate-worker-name
            %worker-timeout

            server
            server?
            server-address
            server-port
            server-log-port
            server-publish-url
            publish-url
            avahi-service->server

            publish-server
            set-build-options*

            strip-store-prefix
            log-path
            receive-logs
            send-log

            zmq-poll*

            build-request-message
            no-build-message
            build-started-message
            build-failed-message
            build-succeeded-message
            worker-ping
            worker-ready-message
            worker-request-work-message
            worker-request-info-message
            server-info-message

            send-message
            receive-message

            remote-server-service-type))


;;;
;;; Workers.
;;;

(define-record-type* <worker>
  worker make-worker
  worker?
  (name           worker-name)
  (address        worker-address
                  (default #f))
  (machine        worker-machine)
  (publish-url    worker-publish-url
                  (default #f))
  (systems        worker-systems)
  (last-seen      worker-last-seen
                  (default 0)))

(define (worker->sexp worker)
  "Return an sexp describing WORKER."
  (let ((name (worker-name worker))
        (address (worker-address worker))
        (machine (worker-machine worker))
        (systems (worker-systems worker))
        (last-seen (worker-last-seen worker)))
    `(worker
      (name ,name)
      (address ,address)
      (machine ,machine)
      (systems ,systems)
      (last-seen ,last-seen))))

(define (sexp->worker sexp)
  "Turn SEXP, an sexp as returned by 'worker->sexp', into a <worker> record."
  (match sexp
    (('worker ('name name)
              ('address address)
              ('machine machine)
              ('systems systems)
              ('last-seen last-seen))
     (worker
      (name name)
      (address address)
      (machine machine)
      (systems systems)
      (last-seen last-seen)))))

(define (generate-worker-name)
  "Return the service name of the server."
  (random-string 8))

(define %worker-timeout
  (make-parameter 120))


;;;
;;; Server.
;;;

(define-record-type* <server>
  server make-server
  server?
  (address        server-address)
  (port           server-port)
  (log-port       server-log-port
                  (default #f))
  (publish-url    server-publish-url
                  (default #f)))

(define (publish-url address port)
  "Return the publish url at ADDRESS and PORT."
  (string-append "http://" address ":" (number->string port)))

(define (avahi-service->params service)
  "Return the URL of the publish server corresponding to the service with the
given NAME."
  (define (service-txt->params txt)
    "Parse the service TXT record."
    (fold (lambda (param params)
            (match (string-split param #\=)
              ((key value)
               (cons (cons (string->symbol key) value)
                     params))))
          '()
          txt))

  (define (number-param params param)
    (string->number (assq-ref params param)))

  (let* ((address (avahi-service-address service))
         (txt (avahi-service-txt service))
         (params (service-txt->params txt))
         (log-port (number-param params 'log-port))
         (publish-port (and=> (assq-ref params 'publish-port)
                              string->number))
         (publish-url (and publish-port
                           (publish-url address publish-port))))
    `((#:log-port . ,log-port)
      (#:publish-url . ,publish-url))))

(define (avahi-service->server service)
  (let* ((address (avahi-service-address service))
         (port (avahi-service-port service))
         (params (avahi-service->params service))
         (log-port (assq-ref params #:log-port))
         (publish-url (assq-ref params #:publish-url)))
    (server
     (address address)
     (port port)
     (log-port log-port)
     (publish-url publish-url))))


;;;
;;; Store publishing.
;;;

(define* (set-build-options* store urls
                             #:key
                             timeout
                             max-silent)
  "Use URLS as substitution servers, set TIMEOUT and MAX-SILENT store
properties."
  (set-build-options store
                     #:use-substitutes? #t
                     #:fallback? #t
                     #:keep-going? #t
                     #:timeout timeout
                     #:max-silent-time max-silent
                     #:verbosity 1
                     #:substitute-urls urls))

(define* (publish-server port
                         #:key
                         public-key
                         private-key)
  "This procedure starts a publishing server listening on PORT in a new
process and returns the pid of the forked process.  Use PUBLIC-KEY and
PRIVATE-KEY to sign narinfos."
  (match (primitive-fork)
    (0
     (parameterize ((%public-key public-key)
                    (%private-key private-key))
       (with-store store
         (let ((log-file (open-file "/dev/null" "w")))
           (close-fdes 1)
           (close-fdes 2)
           (dup2 (fileno log-file) 1)
           (dup2 (fileno log-file) 2)
           (close-port log-file)

           ;; Use a default locale.
           (setlocale LC_ALL "en_US.utf8")

           (let* ((address (make-socket-address AF_INET INADDR_ANY 0))
                  (socket-address
                   (make-socket-address (sockaddr:fam address)
                                        (sockaddr:addr address)
                                        port))
                  (socket (open-server-socket socket-address)))
             ;; Do not cache missing store items on workers.
             (run-publish-server socket store
                                 #:narinfo-negative-ttl 0))))))
    (pid pid)))


;;;
;;; Logs.
;;;

(define (strip-store-prefix file)
  ; Given a file name like "/gnu/store/…-foo-1.2/bin/foo", return
  ;; "/bin/foo".
  (let* ((len  (string-length %store-directory))
         (base (string-drop file (+ 1 len))))
    (match (string-index base #\/)
      (#f    base)
      (index (string-drop base index)))))

(define (log-path cache derivation)
  (let* ((store-hash (strip-store-prefix derivation))
         (hash (and=> (string-index store-hash #\-)
                      (cut string-take store-hash <>))))
    (string-append cache "/" hash ".log.gz")))

(define (receive-logs port cache)
  (define (read-log port)
    (match (false-if-exception (read port))
      (('log ('version 0)
             ('derivation derivation))
       (let ((file (log-path cache derivation)))
         (call-with-output-file file
           (lambda (output)
             (dump-port port output)))))
      (_
       (log-error "invalid log received.")
       #f)))

  (define (wait-for-client port proc)
    (let ((sock (socket AF_INET SOCK_STREAM 0)))
      (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
      (bind sock AF_INET INADDR_ANY port)
      (listen sock 1024)
      (while #t
        (match (select (list sock) '() '() 60)
          (((_) () ())
           (match (accept sock)
             ((client . address)
              (catch 'system-error
                (lambda ()
                  (write '(log-server (version 0)) client)
                  (force-output client)
                  (proc client))
                (lambda args
                  (let ((errno (system-error-errno args)))
                    (when (memv errno (list EPIPE ECONNRESET ECONNABORTED))
                      (log-error "~a when replying to ~a."
                                 (strerror errno) (fileno client)))))))))
          ((() () ())
           #f)))))

  (define (client-handler client)
    (call-with-new-thread
     (lambda ()
       (set-thread-name
        (string-append "log-server-"
                       (number->string (port->fdes client))))
       (and=> client read-log)
       (when client
         (close-port client)))))

  (call-with-new-thread
   (lambda ()
     (set-thread-name "log-server")
     (wait-for-client port client-handler))))

(define-syntax-rule (swallow-zlib-error exp ...)
  "Swallow 'zlib-error' exceptions raised by EXP..."
  (catch 'zlib-error
    (lambda ()
      exp ...)
    (const #f)))

(define* (send-log address port derivation log)
  (let* ((sock (socket AF_INET SOCK_STREAM 0))
         (in-addr (inet-pton AF_INET address))
         (addr (make-socket-address AF_INET in-addr port)))
    (connect sock addr)
    (match (select (list sock) '() '() 10)
      (((_) () ())
       (match (read sock)
         (('log-server ('version version ...))
          (let ((header `(log
                          (version 0)
                          (derivation ,derivation))))
            (write header sock)
            (swallow-zlib-error
             (call-with-gzip-output-port sock
               (lambda (sock-compressed)
                 (dump-port log sock-compressed))))
            (close-port sock)))
         (x
          (log-error "invalid handshake ~s." x)
          (close-port sock)
          #f)))
      ((() () ())                                 ;timeout
       (log-error "timeout while sending log")
       (close-port sock)
       #f))))


;;;
;;; ZMQ.
;;;

(define (EINTR-safe proc)
  "Return a variant of PROC that catches EINTR 'zmq-error' exceptions and
retries a call to PROC."
  (define (safe . args)
    (catch 'zmq-error
      (lambda ()
        (apply proc args))
      (lambda (key errno . rest)
        (if (= errno EINTR)
            (apply safe args)
            (apply throw key errno rest)))))

  safe)

(define zmq-poll*
  ;; Return a variant of ZMQ-POLL that catches EINTR errors.
  (EINTR-safe zmq-poll))

(define zmq-message-receive*
  (EINTR-safe zmq-message-receive))

(define (zmq-empty-delimiter)
  "Return an empty ZMQ delimiter used to format message envelopes."
  (make-bytevector 0))

(define* (send-message socket sexp
                       #:key recipient)
  "Send SEXP over SOCKET, a ZMQ socket.  When RECIPIENT is true, assume SOCKET
is a ROUTER socket and use RECIPIENT, a bytevector, as the routing prefix of
the message."
  (let ((payload (list (zmq-empty-delimiter)
                       (string->bv (object->string sexp)))))
    (zmq-send-msg-parts-bytevector socket
                                   (if recipient
                                       (cons recipient payload)
                                       payload))))

(define* (receive-message socket #:key router?)
  "Read an sexp from SOCKET, a ZMQ socket, and return it.  Return the
unspecified value when reading a message without payload.

When ROUTER? is true, assume messages received start with a routing
prefix (the identity of the peer, as a bytevector), and return three values:
the payload, the peer's identity (a bytevector), and the peer address."
  (if router?
      (match (zmq-message-receive* socket)
        ((sender (= zmq-message-size 0) data)
         (values (call-with-input-string (bv->string
                                          (zmq-message-content data))
                   read)
                 (zmq-message-content sender)
                 (zmq-message-gets data "Peer-Address")))
        ((sender #vu8())
         (values *unspecified* sender)))
      (match (zmq-get-msg-parts-bytevector socket '())
        ((#vu8() data)
         (call-with-input-string (bv->string data)
           read))
        ((#vu8())
         *unspecified*))))

;; ZMQ Messages.
(define* (build-request-message drv
                                #:key
                                priority
                                timeout
                                max-silent
                                timestamp
                                system)
  "Return a message requesting the build of DRV for SYSTEM."
  `(build (drv ,drv)
          (priority ,priority)
          (timeout ,timeout)
          (max-silent ,max-silent)
          (timestamp ,timestamp)
          (system ,system)))

(define (no-build-message)
  "Return a message that indicates that no builds are available."
  `(no-build))

(define (build-started-message drv worker)
  "Return a message that indicates that the build of DRV has started."
  `(build-started (drv ,drv) (worker ,worker)))

(define* (build-failed-message drv url #:optional log)
  "Return a message that indicates that the build of DRV has failed."
  `(build-failed (drv ,drv) (url ,url) (log ,log)))

(define* (build-succeeded-message drv url #:optional log)
  "Return a message that indicates that the build of DRV is done."
  `(build-succeeded (drv ,drv) (url ,url) (log ,log)))

(define (worker-ping worker)
  "Return a message that indicates that WORKER is alive."
  `(worker-ping ,worker))

(define (worker-ready-message worker)
  "Return a message that indicates that WORKER is ready."
  `(worker-ready ,worker))

(define (worker-request-work-message name)
  "Return a message that indicates that WORKER is requesting work."
  `(worker-request-work ,name))

(define (worker-request-info-message)
  "Return a message requesting server information."
  '(worker-request-info))

(define (server-info-message worker-address log-port publish-port)
  "Return a message containing server information."
  `(server-info (worker-address ,worker-address)
                (log-port ,log-port)
                (publish-port ,publish-port)))

(define remote-server-service-type
  "_remote-server._tcp")
