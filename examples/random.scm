;;; random.scm -- Job specification that creates random build jobs
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

(let ((top-srcdir (canonicalize-path
                   (string-append (dirname (current-filename)) "/.."))))
  (list
   `((#:name . "random")
     (#:url . ,(string-append "file://" top-srcdir))
     (#:branch . "master")
     (#:no-compile? . #t)
     (#:load-path . ".")
     (#:proc . make-random-jobs)
     (#:file . "examples/random-jobs.scm")
     (#:arguments . ()))))
