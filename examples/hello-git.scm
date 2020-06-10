;;; hello-git.scm -- job specification test for hello git repository
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
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

;; building GNU hello from git is too much work
(define cuirass-git "https://git.savannah.gnu.org/git/guix/guix-cuirass.git")
;; ... so let's track cuirass' git

;; This builds the Guix Cuirass package with its source replaced by the last
;; commit of Cuirass' git repository.
(let ((top-srcdir (canonicalize-path
                   (string-append (dirname (current-filename)) "/.."))))
  (list
   `((#:name . "cuirass")
     (#:load-path-inputs . ("guix"))
     (#:package-path-inputs . ())
     (#:proc-input . "cuirass")
     (#:proc-file . "examples/guix-track-git.scm")
     (#:proc . guix-jobs)
     (#:proc-args (name . "cuirass") (url . ,cuirass-git))
     (#:inputs . (((#:name . "guix")
                   (#:url . "git://git.savannah.gnu.org/guix.git")
                   (#:load-path . ".")
                   (#:branch . "master")
                   (#:no-compile? . #t))
                  ((#:name . "cuirass")
                   (#:url . ,(string-append "file://" top-srcdir))
                   (#:load-path . ".")
                   (#:branch . "master")
                   (#:no-compile? . #t))))
     (#:build-outputs . ()))))
