;;; hello-singleton.scm -- job specification test for hello in master
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
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

(define hello-master
  '((#:name . "guix-master")
    (#:load-path-inputs . ("guix"))
    (#:package-path-inputs . ())
    (#:proc-input . "cuirass")
    (#:proc-file . "examples/gnu-system.scm")
    (#:proc . hydra-jobs)
    (#:proc-args (subset . "hello"))
    (#:inputs . (((#:name . "guix")
                  (#:url . "git://git.savannah.gnu.org/guix.git")
                  (#:load-path . ".")
                  (#:branch . "master")
                  (#:no-compile? . #t))
                 ((#:name . "cuirass")
                  (#:url . "https://git.savannah.gnu.org/git/guix/guix-cuirass.git")
                  (#:load-path . ".")
                  (#:branch . "master")
                  (#:no-compile? . #t))))
    (#:build-outputs . ())))

(list hello-master)
