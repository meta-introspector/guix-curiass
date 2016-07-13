;;; guix-jobs.scm -- job specification test for Guix
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

(define (local-file file)
  ;; In the common case jobs will be defined relative to the repository.
  ;; However for testing purpose use local gnu-system.scm instead.
  (string-append (dirname (current-filename)) "/" file))

(define job-base
  `((#:name . "guix")
    (#:url . "git://git.savannah.gnu.org/guix.git")
    (#:load-path . ".")
    (#:file . ,(local-file "gnu-system.scm"))
    (#:proc . hydra-jobs)))

(define guix-master
  (acons #:branch "master" job-base))

(define guix-0.10
  (acons #:tag "v0.10.0" job-base))

(list guix-master guix-0.10)
