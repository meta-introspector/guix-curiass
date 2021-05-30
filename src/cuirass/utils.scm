;;; utils.scm -- helper procedures
;;; Copyright © 2012, 2013, 2016, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
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

(define-module (cuirass utils)
  #:use-module (cuirass logging)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module ((ice-9 suspendable-ports)
                #:select (current-read-waiter
                          current-write-waiter))
  #:use-module (ice-9 ports internal)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (json)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers timers)
  #:export (alist?
            object->json-scm
            object->json-string
            define-enumeration
            unwind-protect

            with-timeout
            get-message-with-timeout
            put-message-with-timeout

            make-worker-thread-channel
            call-with-worker-thread
            with-worker-thread

            %non-blocking
            non-blocking
            essential-task
            bytevector-range

            date->rfc822-str
            random-string
            call-with-time))

(define (alist? obj)
  "Return #t if OBJ is an alist."
  (and (list? obj)
       (every pair? obj)))

(define (object->json-scm obj)
  "Prepare OBJ for JSON usage."
  (cond ((string? obj)  obj)
        ((number? obj)  obj)
        ((boolean? obj) obj)
        ((null? obj)    obj)
        ((symbol? obj)  (symbol->string obj))
        ((keyword? obj) (object->json-scm (keyword->symbol obj)))
        ((vector? obj)  (list->vector
                         (map object->json-scm (vector->list obj))))
        ((alist? obj)   (map object->json-scm obj))
        ((pair? obj)    (cons (object->json-scm (car obj))
                              (object->json-scm (cdr obj))))
        (else           (object->string obj))))

(define* (object->json-string object #:key pretty)
  "Return OBJECT as a JSON object."
  (scm->json-string (object->json-scm object) #:pretty pretty))

(define-syntax-rule (define-enumeration name (symbol value) ...)
  "Define an 'enum' type with the given SYMBOL/VALUE pairs.  NAME is defined a
macro that accepts one of these symbols and expands to the corresponding
value."
  (define-syntax name
    (syntax-rules (symbol ...)
      ((_ symbol) value)
      ...)))

(define-syntax-rule (unwind-protect body ... conclude)
  "Evaluate BODY... and return its result(s), but always evaluate CONCLUDE
before leaving, even if an exception is raised.

This is *not* implemented with 'dynamic-wind' in order to play well with
delimited continuations and fibers."
  (let ((conclusion (lambda () conclude)))
    (catch #t
      (lambda ()
        (call-with-values
            (lambda ()
              body ...)
          (lambda results
            (conclusion)
            (apply values results))))
      (lambda args
        (conclusion)
        (apply throw args)))))

(define %worker-thread-args
  (make-parameter #f))

(define* (make-worker-thread-channel initializer
                                     #:key (parallelism 1))
  "Return a channel used to offload work to a dedicated thread.  ARGS are the
arguments of the worker thread procedure."
  (parameterize (((@@ (fibers internal) current-fiber) #f))
    (let ((channel (make-channel)))
      (for-each
       (lambda _
         (let ((args (initializer)))
           (call-with-new-thread
            (parameterize ((current-read-waiter (lambda (port)
                                                  (port-poll port "r")))
                           (current-write-waiter (lambda (port)
                                                   (port-poll port "w"))))
              (lambda ()
                (parameterize ((%worker-thread-args args))
                  (let loop ()
                    (match (get-message channel)
                      (((? channel? reply) . (? procedure? proc))
                       (put-message
                        reply
                        (catch #t
                          (lambda ()
                            (apply proc args))
                          (lambda (key . args)
                            (cons* 'worker-thread-error key args))))))
                    (loop))))))))
       (iota parallelism))
      channel)))

(define* (with-timeout op #:key (seconds 0.05) (wrap values))
  "Return an operation that succeeds if the given OP succeeds or if SECONDS
have elapsed.  In the first case, the result of OP is returned and in the
second case, the wrapping procedure WRAP is called and its result returned."
  (choice-operation op
                    (wrap-operation (sleep-operation seconds) wrap)))

(define* (get-message-with-timeout channel
                                   #:key
                                   seconds
                                   (retry? #t)
                                   timeout-proc)
  "Perform a get-operation on CHANNEL with a timeout set to SECONDS.  If the
timout expires and RETRY? is set to false, return 'timeout.  If RETRY is true,
call the TIMEOUT-PROC procedure on timeout and retry the get-operation until
it succeeds."
  (define (get-message*)
    (perform-operation
     (with-timeout
      (get-operation channel)
      #:seconds seconds
      #:wrap (const 'timeout))))

  (let ((res (get-message*)))
    (if retry?
        (begin
          (let loop ((res res))
            (if (eq? res 'timeout)
                (begin
                  (and timeout-proc (timeout-proc))
                  (loop (get-message*)))
                res)))
        res)))


(define* (put-message-with-timeout channel message
                                   #:key
                                   seconds
                                   (retry? #t)
                                   timeout-proc)
  "Perform a put-operation sending MESSAGE on CHANNEL with a timeout set to
SECONDS.  If the timout expires and RETRY? is set to false, return 'timeout.
If RETRY is true, call the TIMEOUT-PROC procedure on timeout and retry the
put-operation until it succeeds."
  (define (put-message*)
    (perform-operation
     (with-timeout
      (wrap-operation (put-operation channel message) (const #t))
      #:seconds seconds
      #:wrap (const 'timeout))))

  (let ((res (put-message*)))
    (if retry?
        (begin
          (let loop ((res res))
            (if (eq? res 'timeout)
                (begin
                  (and timeout-proc (timeout-proc))
                  (loop (put-message*)))
                res)))
        res)))

(define* (call-with-worker-thread channel proc
                                  #:key
                                  send-timeout
                                  send-timeout-proc
                                  receive-timeout
                                  receive-timeout-proc)
  "Send PROC to the worker thread through CHANNEL.  Return the result of PROC.
If already in the worker thread, call PROC immediately.

If SEND-TIMEOUT is set to a duration in seconds, SEND-TIMEOUT-PROC is called
every time a delay of SEND-TIMEOUT seconds expires, when trying to send PROC
to a worker thread.

The same goes for RECEIVE-TIMEOUT and RECEIVE-TIMEOUT-PROC, except that the
timer expires if there is no response from the database worker PROC was sent
to."
  (let ((args (%worker-thread-args)))
    (if args
        (apply proc args)
        (let* ((reply (make-channel))
               (message (cons reply proc)))
          (if (and send-timeout (current-fiber))
              (put-message-with-timeout channel message
                                        #:seconds send-timeout
                                        #:timeout-proc send-timeout-proc)
              (put-message channel message))
          (match (if (and receive-timeout (current-fiber))
                     (get-message-with-timeout reply
                                               #:seconds
                                               receive-timeout
                                               #:timeout-proc
                                               receive-timeout-proc)
                     (get-message reply))
            (('worker-thread-error key args ...)
             (apply throw key args))
            (result result))))))

(define-syntax-rule (with-worker-thread channel (vars ...) exp ...)
  "Evaluate EXP... in the worker thread corresponding to CHANNEL.
VARS... are bound to the arguments of the worker thread."
  (call-with-worker-thread channel
                           (lambda (vars ...) exp ...)))

(define (%non-blocking thunk)
  (let ((channel (make-channel)))
    (call-with-new-thread
     (lambda ()
       (parameterize (((@@ (fibers internal) current-fiber) #f))
         (catch #t
           (lambda ()
             (call-with-values thunk
               (lambda values
                 (put-message channel `(values ,@values)))))
           (lambda args
             (put-message channel `(exception ,@args)))))))

    (match (get-message channel)
      (('values . results)
       (apply values results))
      (('exception . args)
       (apply throw args)))))

(define-syntax-rule (non-blocking exp ...)
  "Evalaute EXP... in a separate thread so that it doesn't block the execution
of fibers.

This is useful when passing control to non-cooperative and non-resumable code
such as a 'clone' call in Guile-Git."
  (%non-blocking (lambda () exp ...)))

(define (essential-task name exit-channel thunk)
  "Return a thunk that wraps THUNK, catching exceptions and writing an exit
code to EXIT-CHANNEL when an exception occurs.  The idea is that the other end
of the EXIT-CHANNEL will exit altogether when that occurs.

This is often necessary because an uncaught exception in a fiber causes it to
die silently while the rest of the program keeps going."
  (lambda ()
    (catch #t
      thunk
      (lambda _
        (put-message exit-channel 1))             ;to be sure...
      (lambda (key . args)
        ;; If something goes wrong in this fiber, we have a problem, so stop
        ;; everything.
        (log-message "fatal: uncaught exception '~a' in '~a' fiber!"
                     key name)
        (log-message "exception arguments: ~s" args)

        (false-if-exception
         (let ((stack (make-stack #t)))
           (display-backtrace stack (current-error-port))
           (print-exception (current-error-port)
                            (stack-ref stack 0)
                            key args)))

        ;; Tell the other end to exit with a non-zero code.
        (put-message exit-channel 1)))))

(define %weak-references
  (make-weak-key-hash-table))

(define (bytevector-range bv offset count)
  "Return a bytevector that aliases the COUNT bytes of BV starting at OFFSET."
  (cond ((and (zero? offset) (= count (bytevector-length bv)))
         bv)
        ((or (> (+ offset count) (bytevector-length bv))
             (< offset 0))
         (throw 'out-of-range "bytevector-range"
                "Bytevector range is invalid: ~S ~S"
                (list offset count) (list offset count)))
        (else
         (let* ((pointer (bytevector->pointer bv offset))
                (range   (pointer->bytevector pointer count)))
           (hashq-set! %weak-references range bv)
           range))))

(define (date->rfc822-str date)
  (date->string date "~a, ~d ~b ~Y ~T ~z"))

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

(define (call-with-time thunk kont)
  "Call THUNK and pass KONT the elapsed time followed by THUNK's return
values."
  (let* ((start  (current-time time-monotonic))
         (result (call-with-values thunk list))
         (end    (current-time time-monotonic)))
    (apply kont (time-difference end start) result)))
