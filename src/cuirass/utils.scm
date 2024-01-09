;;; utils.scm -- helper procedures
;;; Copyright © 2012-2013, 2016, 2018-2019, 2023-2024 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-71)
  #:autoload   (guix i18n) (G_)
  #:autoload   (guix ui) (leave)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers operations)
  #:use-module (fibers timers)
  #:export (define-enumeration

            make-resource-pool
            with-resource-from-pool

            get-message*
            %non-blocking
            non-blocking
            essential-task

            date->rfc822-str
            random-string
            call-with-time
            with-timing-check
            gather-user-privileges))

(define-syntax-rule (define-enumeration name (symbol value) ...)
  "Define an 'enum' type with the given SYMBOL/VALUE pairs.  NAME is defined a
macro that accepts one of these symbols and expands to the corresponding
value."
  (define-syntax name
    (syntax-rules (symbol ...)
      ((_ symbol) value)
      ...)))

(define (make-resource-pool resources)
  "Return a channel implementing a pool over RESOURCES, a list of objects such
as database connections.  The channel can then be passed to
'with-resource-from-pool'."
  (define channel
    (make-channel))

  (spawn-fiber
   (lambda ()
     (let loop ((pool resources)
                (waiters '()))
       (match (get-message channel)
         (('get reply)
          (match pool
            (()
             (log-debug "queuing request on resource pool ~x"
                        (object-address channel))
             (loop pool (cons reply waiters)))
            ((head . tail)
             (put-message reply head)
             (loop tail waiters))))
         (('put resource)
          (match waiters
            (()
             (loop (cons resource pool) waiters))
            ((rest ... reply)                     ;XXX: linear
             (put-message reply resource)
             (loop pool rest))))))))

  channel)

(define (call-with-resource-from-pool pool proc)
  "Call PROC with a resource from POOL, blocking until a resource becomes
available.  Return the resource once PROC has returned."
  (let ((reply (make-channel)))
    (put-message pool `(get ,reply))
    (let* ((resource (get-message reply))
           (type value (with-exception-handler
                           (lambda (exception)
                             ;; Note: Do not call 'put-message' from the
                             ;; handler because 'raise-exception' is a
                             ;; continuation barrier as of Guile 3.0.9.
                             (values 'exception exception))
                         (lambda ()
                           (let ((result (proc resource)))
                             (values 'value result)))
                         #:unwind? #t)))
      (put-message pool `(put ,resource))
      (match type
        ('exception (raise-exception value))
        ('value value)))))

(define-syntax-rule (with-resource-from-pool pool resource exp ...)
  "Evaluate EXP... with RESOURCE bound to a resource taken from POOL.  When
POOL is empty, wait until a resource is returned to it.  Return RESOURCE when
evaluating EXP... is done."
  (call-with-resource-from-pool pool (lambda (resource) exp ...)))

(define* (get-message* channel timeout #:optional default)
  "Receive a message from @var{channel} and return it, or, if the message hasn't
arrived before @var{timeout} seconds, return @var{default}."
  (call-with-values
      (lambda ()
        (perform-operation
         (choice-operation (get-operation channel)
                           (sleep-operation timeout))))
    (match-lambda*
      (()                               ;'sleep' operation returns zero values
       default)
      ((message)                            ;'get' operation returns one value
       message))))

(define (%non-blocking thunk)
  (let ((channel (make-channel)))
    (call-with-new-thread
     (lambda ()
       (catch #t
         (lambda ()
           (call-with-values thunk
             (lambda values
               (put-message channel `(values ,@values)))))
         (lambda args
           (put-message channel `(exception ,@args))))))

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
        (log-error "fatal: uncaught exception '~a' in '~a' fiber!"
                   key name)
        (log-error "exception arguments: ~s" args)

        (false-if-exception
         (let ((stack (make-stack #t)))
           (display-backtrace stack (current-error-port))
           (print-exception (current-error-port)
                            (stack-ref stack 0)
                            key args)))

        ;; Tell the other end to exit with a non-zero code.
        (put-message exit-channel 1)))))

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

(define* (call-with-timing-check label thunk #:key (threshold 60))
  (call-with-time thunk
    (lambda (time . results)
      (let ((duration (+ (time-second time)
                         (/ (time-nanosecond time) 1e9))))
        (when (> duration 60)
          (log-warning "~a took ~a seconds" label duration)))
      (apply values results))))

(define-syntax-rule (with-timing-check label exp args ...)
  "Evaluate EXP, printing a warning if its execution time exceeds #:threshold
seconds (60 seconds by default)."
  (call-with-timing-check label (lambda () exp) args ...))

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
