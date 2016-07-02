;;;; utils.scm -- tests for (cuirass utils) module
;;;
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
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

(use-modules (cuirass utils)
             (srfi srfi-64))

(define dir-1 (make-parameter ""))
(define dir-2 (make-parameter ""))

(test-assert "with-directory-excursion"
  (let ((old (getcwd))
        (tmp (tmpnam)))
    (dynamic-wind
      (λ ()
        (mkdir tmp))
      (λ ()
        (with-directory-excursion tmp
          (dir-1 (getcwd)))
        (dir-2 (getcwd))
        (and (string=? (dir-1) tmp)
             (string=? (dir-2) old)))
      (λ ()
        (rmdir tmp)))))
