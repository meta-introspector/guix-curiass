;;;; repo.scm -- tests for (cuirass repo) module
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

(use-modules (cuirass repo)
             (cuirass utils)
             (guix store)
             (srfi srfi-64))

(test-begin "repo")

(test-equal "<repo> datatype"
  ;; Check that all the procedures for manipulating <repo> objects are
  ;; exported and that the keywords of the constructor matches their slot.
  '(1 2 3 4 5 6)
  (let ((obj (repo #:id 1 #:url 2 #:location 3 #:ref 4
                   #:snapshoter 5 #:updater 6)))
    (and (repo? obj)
         (list (repo-id obj)
               (repo-url obj)
               (repo-location obj)
               (repo-reference obj)
               (repo-snapshoter obj)
               (repo-updater obj)))))

(define file-name
  (pk (simple-format #f "tmp-~S" (getpid))))

(define store
  (open-connection))

(define (create-file name)
  "Create a dummy file in current directory."
  (with-output-to-file name
    (λ () (display "test!\n"))))

(define (in-store? file-name)
  "Check if FILE-NAME is in the store.  FILE-NAME must be an absolute file
name."
  (string-prefix? "/gnu/store" file-name))

;;;
;;; File repository.
;;;

(test-group-with-cleanup "file-repo"
  (define rpt (pk (file-repo file-name)))

  ;; Since file doesn't exist yet, 'repo-update' should throw an error.
  (test-error "file-repo-update: file not found"
    'system-error
    (repo-update rpt))

  (create-file file-name)

  (test-assert "file-repo-update"
    (repo-update rpt))

  (test-assert "file-repo-snapshot"
    (in-store? (repo-snapshot rpt store)))

  ;; Cleanup.
  (delete-file file-name))

(close-connection store)

(test-end)
