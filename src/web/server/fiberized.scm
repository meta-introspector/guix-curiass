;;; Web I/O: Non-blocking HTTP

;; Copyright (C) 2012, 2018 Free Software Foundation, Inc.

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:
;;;
;;; This is the non-blocking HTTP implementation of the (web server)
;;; interface.
;;;
;;; It is a modified version of (web server fibers) from Fibers 1.0.0 that
;;; does not create new threads and does not call 'run-fibers'.  Instead it
;;; expects to be running directly in a fiberized program.
;;;
;;; (Modifications by Ludovic Courtès, 2018-01.)
;;;
;;; Code:

(define-module (web server fiberized)
  #:use-module (guix build utils)
  #:use-module ((srfi srfi-1) #:select (fold
                                        alist-delete
                                        alist-cons))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (web http)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (cuirass logging)
  #:use-module (cuirass utils))

(define (make-default-socket family addr port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (fcntl sock F_SETFD FD_CLOEXEC)
    (bind sock family addr port)
    (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL)))
    sock))

(define-record-type <server>
  (make-server request-channel)
  server?
  (request-channel server-request-channel))

;; -> server
(define* (open-server #:key
                      (host #f)
                      (family AF_INET)
                      (addr (if host
                                (inet-pton family host)
                                INADDR_LOOPBACK))
                      (port 8080)
                      (socket (make-default-socket family addr port)))
  ;; We use a large backlog by default.  If the server is suddenly hit
  ;; with a number of connections on a small backlog, clients won't
  ;; receive confirmation for their SYN, leading them to retry --
  ;; probably successfully, but with a large latency.
  (listen socket 1024)
  (fcntl socket F_SETFL (logior O_NONBLOCK (fcntl socket F_GETFL)))
  (sigaction SIGPIPE SIG_IGN)
  (let ((request-channel (make-channel)))
    (spawn-fiber
     (lambda ()
       (socket-loop socket request-channel)))
    (make-server request-channel)))

(define (bad-request msg . args)
  (throw 'bad-request msg args))

(define (keep-alive? response)
  (let ((v (response-version response)))
    (and (or (< (response-code response) 400)
             (= (response-code response) 404))
         (case (car v)
           ((1)
            (case (cdr v)
              ((1) (not (memq 'close (response-connection response))))
              ((0) (memq 'keep-alive (response-connection response)))))
           (else #f)))))

;; This procedure and the next one are copied from (guix scripts publish).
(define (strip-headers response)
  "Return RESPONSE's headers minus 'Content-Length' and our internal headers."
  (fold alist-delete
        (response-headers response)
        '(content-length x-raw-file x-nar-compression)))

(define (with-content-length response length)
  "Return RESPONSE with a 'content-length' header set to LENGTH."
  (set-field response (response-headers)
             (alist-cons 'content-length length
                         (strip-headers response))))

(define-syntax-rule (with-ignored-disconnects exp ...)
  "Run EXP and ignore silently any exceptions caused by a premature client
disconnection. Re-raise any other kind of exceptions."
  (catch 'system-error
    (lambda ()
      exp ...)
    (lambda args
      (unless (memv (system-error-errno args)
                    (list EPIPE ECONNRESET))
        (apply throw args)))))

(define (client-loop client have-request)
  ;; Always disable Nagle's algorithm, as we handle buffering
  ;; ourselves.
  (setsockopt client IPPROTO_TCP TCP_NODELAY 1)
  (setvbuf client 'block 1024)
  (with-ignored-disconnects
   (with-throw-handler #t
     (lambda ()
       (let ((response-channel (make-channel)))
         (let loop ()
           (cond
            ((eof-object? (lookahead-u8 client))
             (close-port client))
            (else
             (call-with-values
                 (lambda ()
                   (catch #t
                     (lambda ()
                       (let* ((request (read-request client))
                              (body (read-request-body request)))
                         (have-request response-channel request body)))
                     (lambda (key . args)
                       (display "While reading request:\n"
                                (current-error-port))
                       (print-exception (current-error-port) #f key args)
                       (values (build-response #:version '(1 . 0) #:code 400
                                               #:headers
                                               '((content-length . 0)))
                               #vu8()))))
               (lambda (response body)
                 (match (assoc-ref (response-headers response) 'x-raw-file)
                   ((? string? file)
                    (non-blocking
                     (call-with-input-file file
                       (lambda (input)
                         (let* ((size     (stat:size (stat input)))
                                (response (write-response
                                           (with-content-length response size)
                                           client))
                                (output   (response-port response)))
                           (setsockopt client SOL_SOCKET SO_SNDBUF
                                       (* 128 1024))
                           (if (file-port? output)
                               (sendfile output input size)
                               (dump-port input output))
                           (close-port output)
                           (values))))))
                   (#f (begin
                         (write-response response client)
                         (when body
                           (put-bytevector client body))
                         (force-output client))
                       (if (and (keep-alive? response)
                                (not (eof-object? (peek-char client))))
                           (loop)
                           (close-port client)))))))))))
     (lambda (k . args)
       (catch #t
         (lambda () (close-port client))
         (lambda (k . args)
           (display "While closing port:\n" (current-error-port))
           (print-exception (current-error-port) #f k args)))))))

(define (socket-loop socket request-channel)
  (define (have-request response-channel request body)
    (put-message request-channel (vector response-channel request body))
    (match (get-message response-channel)
      (#(response body)
       (values response body))))
  (let loop ()
    (match (accept socket (logior SOCK_NONBLOCK SOCK_CLOEXEC))
      ((client . sockaddr)
       (spawn-fiber (lambda () (client-loop client have-request))
                    #:parallel? #t)
       (loop)))))

;; -> (client request body | #f #f #f)
(define (server-read server)
  (match (get-message (server-request-channel server))
    (#(response-channel request body)
     (let ((client response-channel))
       (values client request body)))))

;; -> 0 values
(define (server-write server client response body)
  (let ((response-channel client))
    (put-message response-channel (vector response body)))
  (values))

;; -> unspecified values
(define (close-server server)
  ;; FIXME: We should terminate the 'socket-loop' fiber somehow.
  *unspecified*)

(define-server-impl fiberized
  open-server
  server-read
  server-write
  close-server)
