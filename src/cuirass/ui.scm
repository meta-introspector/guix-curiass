;;; ui.scm -- user interface facilities for command-line tools
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (cuirass ui)
  #:use-module (cuirass config)
  #:use-module ((cuirass logging) #:select (current-logging-port))
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (;; Procedures.
            show-version
            show-package-information
            cuirass-main
            ;; Parameters.
            %program-name))

(define %program-name
  (make-parameter "cuirass"))

(define (show-version)
  "Display version information for COMMAND."
  (simple-format #t "~a (~a) ~a~%"
                 (%program-name) %package-name %package-version)
  (display "Copyright (C) 2024 the Cuirass authors
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.")
  (newline)
  (exit 0))

(define (show-package-information)
  (newline)
  (format #t "Report bugs to: ~a." %package-bugreport)
  (newline)
  (format #t "~A home page: <~A>" %package-name %package-url)
  (newline))

(define (install-locale)
  "Install the current locale settings."
  (catch 'system-error
    (lambda _
      (setlocale LC_ALL ""))
    (lambda args
      ;; We're now running in the "C" locale.  Try to install a UTF-8 locale
      ;; instead.
      (false-if-exception (setlocale LC_ALL "en_US.utf8")))))

(define (initialize-cuirass)
  "Perform the usual initialization for stand-alone Cuirass commands."
  (install-locale)

  (setvbuf (current-output-port) 'line)
  (setvbuf (current-error-port) 'line))

(define (show-cuirass-usage)
  (format (current-error-port)
          "Try `cuirass --help' for more information.~%")
  (exit 1))

(define (show-cuirass-help)
  (format #t "Usage: cuirass COMMAND ARGS...
Run COMMAND with ARGS.\n")
  (newline)
  (format #t "COMMAND must be one of the sub-commands listed below:
- register
- remote-server
- remote-worker
- web~%"))

(define (run-cuirass-command command . args)
  "Run COMMAND with the given ARGS.  Report an error when COMMAND is not
found."
  (define module
    ;; Check if there is a matching extension.
    (catch 'misc-error
      (lambda ()
        (resolve-interface `(cuirass scripts ,command)))
      (lambda _
        (format (current-error-port)
                "cuirass: ~a: command not found~%" command)
        (show-cuirass-usage))))

  (let ((command-main (module-ref module
                                  (symbol-append 'cuirass- command))))
    ;; Disable canonicalization so we don't don't stat unreasonably.
    (with-fluids ((%file-port-name-canonicalization #f))
      (dynamic-wind
        (const #f)
        (lambda ()
          (command-main (cons command args)))
        (lambda ()
          #t)))))

(define-syntax-rule (leave-on-EPIPE exp ...)
  "Run EXP... in a context where EPIPE errors are caught and lead to 'exit'
with successful exit code.  This is useful when writing to the standard output
may lead to EPIPE, because the standard output is piped through 'head' or
similar."
  (catch 'system-error
    (lambda ()
      exp ...)
    (lambda args
      ;; We really have to exit this brutally, otherwise Guile eventually
      ;; attempts to flush all the ports, leading to an uncaught EPIPE down
      ;; the path.
      (if (= EPIPE (system-error-errno args))
          (primitive-_exit 0)
          (apply throw args)))))

(define (run-cuirass args)
  "Run the 'cuirass' command defined by command line ARGS."
  (define option? (cut string-prefix? "-" <>))

  ;; The default %LOAD-EXTENSIONS includes the empty string, which doubles the
  ;; number of 'stat' calls per entry in %LOAD-PATH.  Shamelessly remove it.
  (set! %load-extensions '(".scm"))

  ;; Since messages can be written from several threads, arrange to print
  ;; lines at once.
  (setvbuf (current-logging-port) 'line)

  (match args
    (()
     (format (current-error-port)
             "cuirass: missing command name~%")
     (show-cuirass-usage))
    ((or ("-h") ("--help"))
     (leave-on-EPIPE (show-cuirass-help)))
    ((or ("-V") ("--version"))
     (show-version))
    (((? option? o) args ...)
     (format (current-error-port)
             "cuirass: unrecognized option '~a'~%" o)
     (show-cuirass-usage))
    (("help" command)
     (apply run-cuirass-command (string->symbol command)
            '("--help")))
    (("help" args ...)
     (leave-on-EPIPE (show-cuirass-help)))
    ((command args ...)
     (apply run-cuirass-command (string->symbol command) args))))

(define (cuirass-main arg0 . args)
  (initialize-cuirass)
  (run-cuirass args))
