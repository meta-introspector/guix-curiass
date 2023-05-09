;;; logging.scm -- Event logging.
;;; Copyright © 2018, 2023 Ludovic Courtès <ludo@gnu.org>
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
            log-info
            log-debug
            log-warning
            log-error
            with-time-logging
            log-monitoring-stats
            query-logging-port
            log-query))

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
  (make-parameter (if (isatty? (current-logging-port))
                      (lambda (str)
                        (log-to-port (current-logging-port) str))
                      (lambda (str)
                        ;; Most likely we're writing to some log file handled
                        ;; by shepherd or similar, so no need to add a
                        ;; timestamp.
                        (format (current-logging-port) "~a~%" str)))))

(define current-logging-level
  ;; Messages at this level and "above" this level are all logged; messages
  ;; below this level are discarded.
  (make-parameter (or (and=> (getenv "CUIRASS_LOGGING_LEVEL")
                             string->symbol)
                      'info)
                  (lambda (value)
                    (unless (memq value '(debug info warning error))
                      (log-error "~s: invalid logging level~%" value)
                      (exit 1))
                    value)))

(define (log-message fmt level . args)
  "Log the given message as one line."
  ;; Note: Use '@' to make sure -Wformat detects this use of 'format'.
  (when (or (and (eq? level 'debug)
                 (eq? (current-logging-level) 'debug))
            (and (eq? level 'info)
                 (memq (current-logging-level) '(debug info)))
            (and (eq? level 'warning)
                 (memq (current-logging-level) '(debug info warning))))
    (let ((fmt (cond
                ((eq? level 'info)
                 fmt)
                ((eq? level 'debug)
                 (string-append "debug: " fmt))
                ((eq? level 'warning)
                 (string-append "warning: " fmt))
                ((eq? level 'error)
                 (string-append "error: " fmt)))))
      ((current-logging-procedure)
       (apply (@ (ice-9 format) format) #f fmt args)))))

(define-syntax-rule (log-info fmt args ...)
  (log-message fmt 'info args ...))

(define-syntax-rule (log-debug fmt args ...)
  (log-message fmt 'debug args ...))

(define-syntax-rule (log-warning fmt args ...)
  (log-message fmt 'warning args ...))

(define-syntax-rule (log-error fmt args ...)
  (log-message fmt 'error args ...))

(define (call-with-time-logging name thunk)
  (let* ((start   (current-time time-utc))
         (result  (thunk))
         (end     (current-time time-utc))
         (elapsed (time-difference end start)))
    (log-info "~a took ~a seconds" name
              (+ (time-second elapsed)
                 (/ (time-nanosecond elapsed) 1e9)))
    result))

(define-syntax-rule (with-time-logging name exp ...)
  "Log under NAME the time taken to evaluate EXP."
  (call-with-time-logging name (lambda () exp ...)))

(define (log-monitoring-stats)
  "Log info about useful metrics: heap size, number of threads, etc."
  (log-info "heap: ~,2f MiB; threads: ~a; file descriptors: ~a"
            (/ (assoc-ref (gc-stats) 'heap-size) (expt 2. 20))
            (length (all-threads))
            (length
             ;; In theory 'scandir' cannot return #f, but in practice,
             ;; we've seen it before.
             (or (scandir "/proc/self/fd"
                          (lambda (file)
                            (not (member file '("." "..")))))
                 '()))))

(define query-logging-port
  (make-parameter #f))

(define (log-query query time)
  (define now
    (current-time time-utc))

  (define date
    (date->string (time-utc->date now) "~5"))

  (format (query-logging-port) "~a ~a ~,2f~%"
          date
          (string-join
           (string-tokenize query
                            (char-set-complement
                             (char-set #\space #\newline #\;)))
           " ")
          (+ (time-second time)
             (/ (time-nanosecond time) 1e9))))
