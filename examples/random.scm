;;; random.scm -- Job specification that creates random build jobs
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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
     (#:load-path-inputs . ())          ;use the Guix shipped with Cuirass
     (#:package-path-inputs . ())
     (#:proc-input . "cuirass")
     (#:proc-file . "examples/random-jobs.scm")
     (#:proc . make-random-jobs)
     (#:proc-args . ())
     (#:inputs . (((#:name . "cuirass")
                   (#:url . ,(string-append "file://" top-srcdir))
                   (#:load-path . ".")
                   (#:branch . "master")
                   (#:no-compile? . #t))))
     (#:build-outputs . ()))))
