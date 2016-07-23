;;; ui.scm -- user interface facilities for command-line tools
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
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
  #:export (;; Procedures.
            show-version
            show-package-information
            ;; Parameters.
            %program-name))

(define %program-name
  ;; Similar in spirit to Gnulib 'progname' module.
  (make-parameter ""
    (λ (val)
      (cond ((not (string? val))
             (scm-error 'wrong-type-arg
                        "%program-name" "Not a string: ~S" (list #f) #f))
            ((string-rindex val #\/) => (λ (idx) (substring val (1+ idx))))
            (else val)))))

(define (show-version)
  "Display version information for COMMAND."
  (simple-format #t "~a (~a) ~a~%"
                 (%program-name) %package-name %package-version)
  (display "Copyright (C) 2016 the Cuirass authors
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.")
  (newline))

(define (show-package-information)
  (newline)
  (format #t "Report bugs to: ~a." %package-bugreport)
  (newline)
  (display "General help using GNU software: <http://www.gnu.org/gethelp/>")
  (newline))
