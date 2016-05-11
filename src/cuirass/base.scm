;;;; base.scm - Cuirass base module
;;;
;;; Copyright © 2012, 2016 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Cuirass.
;;;
;;; Cuirass is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Cuirass is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Cuirass.  If not, see <http://www.gnu.org/licenses/>.

(define-module (cuirass base)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-19)
  #:export (guix-variable
            with-directory-excursion
            call-with-time-display))

(define (guix-variable module name)
  "Dynamically link variable NAME under Guix module MODULE and return it.
Note: this is used instead of `@', because when using `@' in an uncompiled
file, Guile tries to load the module directly as it reads the source, which
fails in our case, leading to the creation of empty (guix ...) modules."
  (let ((m (resolve-interface `(guix ,module))))
    (module-ref m name)))

(define-syntax-rule (with-directory-excursion dir body ...)
  "Run BODY with DIR as the process's current directory."
  (let ((init (getcwd)))
    (dynamic-wind
      (lambda ()
        (chdir dir))
      (lambda ()
        body ...)
      (lambda ()
        (chdir init)))))

(define (call-with-time thunk kont)
  "Call THUNK and pass KONT the elapsed time followed by THUNK's return
values."
  (let* ((start  (current-time time-monotonic))
         (result (call-with-values thunk list))
         (end    (current-time time-monotonic)))
    (apply kont (time-difference end start) result)))

(define (call-with-time-display thunk)
  "Call THUNK and write to the current output port its duration."
  (call-with-time thunk
    (lambda (time . results)
      (format #t "~,3f seconds~%"
              (+ (time-second time)
                 (/ (time-nanosecond time) 1e9)))
      (apply values results))))
