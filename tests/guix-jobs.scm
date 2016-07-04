;;;; guix-jobs.scm - job specification test for Guix.
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

(use-modules (cuirass job))

;; In the common case jobs will be defined relative to the repository.
;; However for testing purpose use local gnu-system.scm instead.
(define (local-file file)
  (string-append (dirname (current-filename)) "/" file))

(list (make-job-spec
       #:name "guix"
       #:url "git://git.savannah.gnu.org/guix.git"
       #:load-path "."
       #:branch "master"
       #:file (local-file "gnu-system.scm")
       #:proc 'hydra-jobs)
      (make-job-spec
       #:name "guix"
       #:url "git://git.savannah.gnu.org/guix.git"
       #:load-path "."
       #:tag "v0.10.0"
       #:file (local-file "gnu-system.scm")
       #:proc 'hydra-jobs))
