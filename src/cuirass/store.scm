;;; store.scm -- Fiberized access to the store.
;;; Copyright © 2016-2019, 2022-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2020, 2021 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (cuirass store)
  #:use-module (guix store)
  #:autoload   (guix derivations) (build-derivations
                                   derivation-path->output-paths)
  #:use-module ((guix config) #:select (%state-directory))
  #:use-module (srfi srfi-34)
  #:use-module ((srfi srfi-35) #:select (condition?))
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 match)
  #:autoload   (ice-9 rdelim) (read-line)
  #:use-module (ice-9 threads)
  #:export (non-blocking-port
            with-store/non-blocking
            process-build-log
            build-derivations&

            register-gc-root
            register-gc-roots
            default-gc-root-directory
            %gc-root-directory))


;;;
;;; Garbage collector roots.
;;;

(define (default-gc-root-directory)
  (string-append %state-directory
                 "/gcroots/profiles/per-user/"
                 (passwd:name (getpwuid (getuid)))
                 "/cuirass"))

(define %gc-root-directory
  ;; Directory where garbage collector roots are stored.  We register build
  ;; outputs there.
  (make-parameter (default-gc-root-directory)))

(define (register-gc-root item)
  "Create a GC root pointing to ITEM, a store item."
  (let ((root (string-append (%gc-root-directory) "/" (basename item))))
    (catch 'system-error
      (lambda ()
        (symlink item root))
      (lambda args
        ;; If the symlink already exist, assume it points to ITEM, but update
        ;; its mtime so it doesn't get GC'd too early.
        (if (= EEXIST (system-error-errno args))
            (let ((now (current-time)))
              (utime root now now 0 0 AT_SYMLINK_NOFOLLOW))
            (apply throw args))))))

(define* (register-gc-roots drv
                            #:key (mode 'outputs))
  "Register GC roots for the outputs of the given DRV when MODE is 'outputs or
for DRV itself when MODE is 'derivation.  Also remove the expired GC roots if
any."
  (catch 'system-error
    (lambda ()
      (case mode
        ((outputs)
         (for-each (match-lambda
                     ((name . output)
                      (register-gc-root output)))
                   (derivation-path->output-paths drv)))
        ((derivation)
         (register-gc-root drv))))
    (lambda args
      (unless (= ENOENT (system-error-errno args)) ;collected in the meantime
        (apply throw args)))))


;;;
;;; Fiberized access to the store.
;;;

(define (non-blocking-port port)
  "Make PORT non-blocking and return it."
  (let ((flags (fcntl port F_GETFL)))
    (when (zero? (logand O_NONBLOCK flags))
      (fcntl port F_SETFL (logior O_NONBLOCK flags)))
    port))

(define (ensure-non-blocking-store-connection store)
  "Mark the file descriptor that backs STORE, a <store-connection>, as
O_NONBLOCK."
  (match (store-connection-socket store)
    ((? file-port? port)
     (non-blocking-port port))
    (_ #f)))

(define-syntax-rule (with-store/non-blocking store exp ...)
  "Like 'with-store', bind STORE to a connection to the store, but ensure that
said connection is non-blocking (O_NONBLOCK).  Evaluate EXP... in that
context."
  (with-store store
    (ensure-non-blocking-store-connection store)
    (let ()
      exp ...)))

(define (process-build-log port proc seed)
  "Read from PORT the build log, calling PROC for each build event like 'fold'
does.  Return the result of the last call to PROC."
  (define (process-line line state)
    (when (string-prefix? "@ " line)
      (match (string-tokenize (string-drop line 2))
        (((= string->symbol event-name) args ...)
         (proc (cons event-name args) state)))))

  (let loop ((state seed))
    (match (read-line port)
      ((? eof-object?)
       state)
      ((? string? line)
       (loop (process-line line state))))))

(define (build-derivations& store lst)
  "Like 'build-derivations' but return two values: a file port from which to
read the build log, and a thunk to call after EOF has been read.  The thunk
returns the value of the underlying 'build-derivations' call, or raises the
exception that 'build-derivations' raised.

Essentially this procedure inverts the inversion-of-control that
'build-derivations' imposes, whereby 'build-derivations' writes to
'current-build-output-port'."
  ;; XXX: Make this part of (guix store)?
  (define result
    (make-atomic-box #f))

  (match (pipe)
    ((input . output)
     (call-with-new-thread
      (lambda ()
        (catch #t
          (lambda ()
            ;; String I/O primitives are going to be used on PORT so make it
            ;; Unicode-capable and resilient to encoding issues.
            (set-port-encoding! output "UTF-8")
            (set-port-conversion-strategy! output 'substitute)

            (guard (c ((store-error? c)
                       (atomic-box-set! result c)))
              (parameterize ((current-build-output-port output))
                (let ((x (build-derivations store lst)))
                  (atomic-box-set! result x))))
            (close-port output))
          (lambda _
            (close-port output)))))

     (values (non-blocking-port input)
             (lambda ()
               (match (atomic-box-ref result)
                 ((? condition? c)
                  (raise c))
                 (x x)))))))

