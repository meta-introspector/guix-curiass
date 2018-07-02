;;; utils.scm -- helper procedures
;;; Copyright © 2012, 2013, 2016, 2018 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (json)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:export (alist?
            object->json-scm
            object->json-string
            define-enumeration
            unwind-protect

            make-critical-section
            call-with-critical-section
            with-critical-section

            non-blocking
            essential-task
            bytevector-range))

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

(define (make-critical-section . args)
  "Return a channel used to implement a critical section.  That channel can
then be passed to 'join-critical-section', which will ensure sequential
ordering.  ARGS are the arguments of the critical section.

Critical sections are implemented by passing the procedure to execute to a
dedicated fiber."
  (let ((channel (make-channel)))
    (spawn-fiber
     (lambda ()
       (let loop ()
         (match (get-message channel)
           ((? procedure? proc)
            (put-message channel (apply proc args))))
         (loop))))
    channel))

(define (call-with-critical-section channel proc)
  "Call PROC in the critical section corresponding to CHANNEL.  Return the
result of PROC."
  (put-message channel proc)
  (get-message channel))

(define-syntax-rule (with-critical-section channel (vars ...) exp ...)
  "Evaluate EXP... in the critical section corresponding to CHANNEL.
VARS... are bound to the arguments of the critical section."
  (call-with-critical-section channel
                              (lambda (vars ...) exp ...)))

(define (%non-blocking thunk)
  (parameterize (((@@ (fibers internal) current-fiber) #f))
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
         (apply throw args))))))

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
