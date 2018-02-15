;;; logging.scm -- Event logging.
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (cuirass logging)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 ftw)
  #:export (current-logging-port
            current-logging-procedure
            log-message
            with-time-logging
            log-monitoring-stats))

(define current-logging-port
  (make-parameter (current-error-port)))

(define (log-to-port port str)
  (define now
    (current-time time-utc))

  (define date
    (date->string (time-utc->date now) "~5"))

  (display (string-append date " " str "\n")
           port))

(define current-logging-procedure
  ;; The logging procedure.  This could be 'syslog', for instance.
  (make-parameter (lambda (str)
                    (log-to-port (current-logging-port) str))))

(define-syntax-rule (log-message fmt args ...)
  "Log the given message as one line."
  ;; Note: Use '@' to make sure -Wformat detects this use of 'format'.
  ((current-logging-procedure)
   ((@ (ice-9 format) format) #f fmt args ...)))

(define (call-with-time-logging name thunk)
  (let* ((start   (current-time time-utc))
         (result  (thunk))
         (end     (current-time time-utc))
         (elapsed (time-difference end start)))
    (log-message "~a took ~a seconds" name
                 (+ (time-second elapsed)
                    (/ (time-nanosecond elapsed) 1e9)))
    result))

(define-syntax-rule (with-time-logging name exp ...)
  "Log under NAME the time taken to evaluate EXP."
  (call-with-time-logging name (lambda () exp ...)))

(define (log-monitoring-stats)
  "Log info about useful metrics: heap size, number of threads, etc."
  (log-message "heap: ~,2f MiB; threads: ~a; file descriptors: ~a"
               (/ (assoc-ref (gc-stats) 'heap-size) (expt 2. 20))
               (length (all-threads))
               (length
                ;; In theory 'scandir' cannot return #f, but in practice,
                ;; we've seen it before.
                (or (scandir "/proc/self/fd"
                             (lambda (file)
                               (not (member file '("." "..")))))
                    '()))))
