;;;; repo.scm -- manage code repositories
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

(define-module (cuirass repo)
  #:use-module (cuirass utils)
  #:use-module (guix store)
  #:use-module (srfi srfi-9 gnu)
  #:export (repo
            repo?
            repo-id
            repo-url
            repo-location
            repo-reference
            repo-snapshoter
            repo-snapshot
            repo-updater
            repo-update
            file-repo
            git-repo))

(define-immutable-record-type <repo>
  ;; An Abstract repository.  Use "repo" as a shortname for "repository".
  (make-repo id url location ref snapshoter updater)
  repo?
  (id         repo-id)                  ;string
  (url        repo-url)                 ;string
  (location   repo-location)            ;string
  (ref        repo-reference)           ;string
  (snapshoter repo-snapshoter)          ;method
  (updater    repo-updater))            ;method

(define* (repo #:key id url location ref snapshoter updater)
  ;; Convenient <repo> constructor using keyword arguments.
  (make-repo id url location ref snapshoter updater))

(define (repo-snapshot repo store)
  "Send a snapshot of REPO to the STORE."
  ((repo-snapshoter repo) repo store))

(define* (repo-update repo #:optional ref)
  "Pull changes from REPO according to reference REF."
  ((repo-updater repo) repo ref))

;;;
;;; Concrete repositories.
;;;

(define file-repo
  (let ((hash-algo "sha256"))
    (define (file-repo-snapshot this store)
      ;; Send file to the STORE.
      (let* ((basename   (repo-id this))
             (file       (repo-location this))
             (directory? (eq? 'directory (stat:type (stat file)))))
        (add-to-store store basename directory? hash-algo file)))

    (define (file-repo-update this ref)
      ;; Ensure that file still exists.
      (stat (repo-location this)))

    (λ* (file-name #:key id)
      "Basic repository that handles a local file or directory."
      (repo #:id (or id file-name)
            #:location file-name
            #:snapshoter file-repo-snapshot
            #:updater file-repo-update))))

(define git-repo
  (let ((git       "git")
        (hash-algo "sha256"))
    (define (git-repo-snapshot this store)
      "Add a snapshot of URL to STORE. "
      (let ((dir (repo-location this))
            (id  (repo-id this)))
        (call-with-temporary-directory
         (λ (tmpdir)
           (let ((tmp-repo (string-append tmpdir "/" dir)))
             (and (zero? (system* "cp" "-R" dir tmpdir))
                  (with-directory-excursion tmp-repo
                    (zero? (system* "rm" "-rf" ".git")))
                  (add-to-store store id #t hash-algo tmp-repo)))))))

    (define (git-repo-update this ref)
      (let ((url (repo-url this))
            (dir (repo-location this)))
        (and
         (or (file-exists? dir)
             (zero? (system* git "clone" url dir))
             (error "file not found"))
         (with-directory-excursion dir
           (and (zero? (system* git "pull"))
                (zero? (system* git "reset" "--hard" ref)))))))

    (λ* (#:key url dir)
      "Create a Git repository.  URL is the location of the remote repository.
REF is the identifier that is tracked."
      (repo #:id dir
            #:url url
            #:location dir
            #:snapshoter git-repo-snapshot
            #:updater git-repo-update))))
