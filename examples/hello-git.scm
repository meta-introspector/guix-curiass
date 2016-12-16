;;; hello-git.scm -- job specification test for hello git repository
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
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

(use-modules (srfi srfi-1))

(define (local-file file)
  ;; In the common case jobs will be defined relative to the repository.
  ;; However for testing purpose use local gnu-system.scm instead.
  (string-append (dirname (current-filename)) "/" file))

(define (url->file-name url)
  (string-trim
   (string-map (lambda (c) (if (memq c (string->list ":/")) #\- c)) url)
   #\-))

(define vc
  ;; where your version-control checkouts live
  (string-append (getenv "HOME") "/src"))
(define guix-checkout (string-append vc "/guix"))

;; building GNU hello from git is too much work
;; (define hello-checkout (string-append vc "/hello"))
;; (define hello-git "http://git.savannah.gnu.org/r/hello.git")
;; ... so let's track cuirass' git
(define cuirass-checkout (string-append vc "/cuirass"))
(define cuirass-git "https://notabug.org/mthl/cuirass")
;;(define cuirass-git "https://gitlab.com/janneke/cuirass.git")

(list
 `((#:name . ,(url->file-name cuirass-checkout))
   (#:url . ,cuirass-git)
   (#:branch . "master")
   (#:no-compile? . #t)
   (#:load-path . ,guix-checkout)
   (#:proc . guix-jobs)
   (#:file . ,(local-file "guix-track-git.scm"))
   (#:arguments (name . "cuirass") (url . ,cuirass-git))))
