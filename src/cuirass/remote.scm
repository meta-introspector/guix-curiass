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
            zmq-socket-ready?
            zmq-empty-delimiter

            zmq-build-request-message
            zmq-no-build-message
            zmq-build-started-message
            zmq-build-failed-message
            zmq-build-succeeded-message
            zmq-worker-ping
            zmq-worker-ready-message
            zmq-worker-request-work-message
            zmq-worker-request-info-message
            zmq-server-info
            zmq-remote-address
            zmq-message-string
            zmq-read-message

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

(define %seed
  (seed->random-state
   (logxor (getpid) (car (gettimeofday)))))

(define (integer->alphanumeric-char n)
  "Map N, an integer in the [0..62] range, to an alphanumeric character."
  (cond ((< n 10)
         (integer->char (+ (char->integer #\0) n)))
        ((< n 36)
         (integer->char (+ (char->integer #\A) (- n 10))))
        ((< n 62)
         (integer->char (+ (char->integer #\a) (- n 36))))
        (else
         (error "integer out of bounds" n))))

(define (random-string len)
  "Compute a random string of size LEN where each character is alphanumeric."
  (let loop ((chars '())
             (len len))
    (if (zero? len)
        (list->string chars)
        (let ((n (random 62 %seed)))
          (loop (cons (integer->alphanumeric-char n) chars)
                (- len 1))))))

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
         (publish-port (number-param params 'publish-port))
         (publish-url (publish-url address publish-port)))
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

(define* (set-build-options* store url
                             #:key
                             timeout
                             max-silent)
  "Add URL to the list of STORE substitutes-urls."
  (set-build-options store
                     #:use-substitutes? #t
                     #:fallback? #t
                     #:keep-going? #t
                     #:timeout timeout
                     #:max-silent-time max-silent
                     #:verbosity 1
                     #:substitute-urls
                     (cons url %default-substitute-urls)))

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
             (run-publish-server socket store
                                 #:compressions
                                 (list %default-gzip-compression)))))))
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
       (log-message "invalid log received.~%")
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
              (write '(log-server (version 0)) client)
              (force-output client)
              (proc client))))
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
          (log-message "invalid handshake ~s.~%" x)
          (close-port sock)
          #f)))
      ((() () ())                                 ;timeout
       (log "timeout while sending files to ~a.~%" port)
       (close-port sock)
       #f))))


;;;
;;; ZMQ.
;;;

(define %zmq-context
  (zmq-create-context))

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

(define (zmq-socket-ready? items socket)
  "Return #t if the given SOCKET is part of ITEMS, a list returned by a
'zmq-poll' call, return #f otherwise."
  (find (lambda (item)
          (eq? (poll-item-socket item) socket))
        items))

(define (zmq-remote-address message)
  (zmq-message-gets message "Peer-Address"))

(define (zmq-message-string message)
  (bv->string
   (zmq-message-content message)))

(define (zmq-read-message msg)
  (call-with-input-string msg read))

(define (zmq-empty-delimiter)
  "Return an empty ZMQ delimiter used to format message envelopes."
  (make-bytevector 0))

;; ZMQ Messages.
(define* (zmq-build-request-message drv
                                    #:key
                                    priority
                                    timeout
                                    max-silent
                                    timestamp
                                    system)
  "Return a message requesting the build of DRV for SYSTEM."
  (format #f "~s" `(build (drv ,drv)
                          (priority ,priority)
                          (timeout ,timeout)
                          (max-silent ,max-silent)
                          (timestamp ,timestamp)
                          (system ,system))))

(define (zmq-no-build-message)
  "Return a message that indicates that no builds are available."
  (format #f "~s" `(no-build)))

(define (zmq-build-started-message drv worker)
  "Return a message that indicates that the build of DRV has started."
  (format #f "~s" `(build-started (drv ,drv) (worker ,worker))))

(define* (zmq-build-failed-message drv url #:optional log)
  "Return a message that indicates that the build of DRV has failed."
  (format #f "~s" `(build-failed (drv ,drv) (url ,url) (log ,log))))

(define* (zmq-build-succeeded-message drv url #:optional log)
  "Return a message that indicates that the build of DRV is done."
  (format #f "~s" `(build-succeeded (drv ,drv) (url ,url) (log ,log))))

(define (zmq-worker-ping worker)
  "Return a message that indicates that WORKER is alive."
  (format #f "~s" `(worker-ping ,worker)))

(define (zmq-worker-ready-message worker)
  "Return a message that indicates that WORKER is ready."
  (format #f "~s" `(worker-ready ,worker)))

(define (zmq-worker-request-work-message name)
  "Return a message that indicates that WORKER is requesting work."
  (format #f "~s" `(worker-request-work ,name)))

(define (zmq-worker-request-info-message)
  "Return a message requesting server information."
  (format #f "~s" '(worker-request-info)))

(define (zmq-server-info worker-address log-port publish-port)
  "Return a message containing server information."
  (format #f "~s" `(server-info (worker-address ,worker-address)
                                (log-port ,log-port)
                                (publish-port ,publish-port))))

(define remote-server-service-type
  "_remote-server._tcp")
