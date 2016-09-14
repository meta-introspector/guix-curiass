;;; guix-track-git.scm -- job specification tracking a guix packages's git
;;; Copyright © 2012, 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Cuirass.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

;;;
;;; This file defines build jobs for the Hydra continuation integration
;;; tool.
;;;

(define local-guix (string-append (getenv "HOME") "/src/guix"))
(define local-cuirass (string-append (getenv "HOME") "/src/cuirass/src"))

;; Attempt to use our very own Guix modules.
(eval-when (compile load eval)

  (set! %load-path (cons* local-guix local-cuirass %load-path))
  (set! %load-path (cons (string-append local-cuirass "/gnu/packages/patches") %load-path))
  (set! %load-compiled-path (cons local-guix %load-compiled-path))
  (set! %load-compiled-path (cons local-cuirass %load-compiled-path))

  ;; Ignore any available .go, and force recompilation.  This is because our
  ;; checkout in the store has mtime set to the epoch, and thus .go files look
  ;; newer, even though they may not correspond.
  (set! %fresh-auto-compile #t))

(use-modules (guix config)
             (guix store)
             (guix grafts)
             (guix packages)
             (guix derivations)
             (guix monads)
             ((guix licenses)
              #:select (gpl3+ license-name license-uri license-comment))
             ((guix utils) #:select (%current-system))
             ((guix scripts system) #:select (read-operating-system))
             (gnu packages)
             (gnu packages gcc)
             (gnu packages base)
             (gnu packages gawk)
             (gnu packages guile)
             (gnu packages gettext)
             (gnu packages compression)
             (gnu packages multiprecision)
             (gnu packages make-bootstrap)
             (gnu packages commencement)
             (gnu packages package-management)
             (gnu system)
             (gnu system vm)
             (gnu system install)
             (gnu tests)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 optargs)
             (ice-9 match))

;; XXX: Debugging hack: since `hydra-eval-guile-jobs' redirects the output
;; port to the bit bucket, let us write to the error port instead.
(setvbuf (current-error-port) _IOLBF)
(set-current-output-port (current-error-port))

(define (license->alist lcs)
  "Return LCS <license> object as an alist."
  ;; Sometimes 'license' field is a list of licenses.
  (if (list? lcs)
      (map license->alist lcs)
      `((name . ,(license-name lcs))
        (uri . ,(license-uri lcs))
        (comment . ,(license-comment lcs)))))

(define (package-metadata package)
  "Convert PACKAGE to an alist suitable for Hydra."
  `((#:description . ,(package-synopsis package))
    (#:long-description . ,(package-description package))
    (#:license . ,(license->alist (package-license package)))
    (#:home-page . ,(package-home-page package))
    (#:maintainers . ("bug-guix@gnu.org"))
    (#:max-silent-time . ,(or (assoc-ref (package-properties package)
                                         'max-silent-time)
                              3600))      ;1 hour by default
    (#:timeout . ,(or (assoc-ref (package-properties package) 'timeout)
                      72000))))           ;20 hours by default

(define (package-job store job-name package system)
  "Return a job called JOB-NAME that builds PACKAGE on SYSTEM."
  (λ ()
    `((#:job-name . ,(string-append (symbol->string job-name) "." system))
      (#:derivation . ,(derivation-file-name
                        (parameterize ((%graft? #f))
                          (package-derivation store package system
                                              #:graft? #f))))
      ,@(package-metadata package))))

(define job-name
  ;; Return the name of a package's job.
  (compose string->symbol package-full-name))

(define package->job
  (let ((base-packages
         (delete-duplicates
          (append-map (match-lambda
                       ((_ package _ ...)
                        (match (package-transitive-inputs package)
                          (((_ inputs _ ...) ...)
                           inputs))))
                      %final-inputs))))
    (lambda (store package system)
      "Return a job for PACKAGE on SYSTEM, or #f if this combination is not
valid."
      (cond ((member package base-packages)
             #f)
            ((supported-package? package system)
             (package-job store (job-name package) package system))
            (else
             #f)))))

;;; END hydra/gnu-system.scm


;;;
;;; Cuirass CI tracking packages' git
;;;

(use-modules (srfi srfi-11)
             (srfi srfi-9 gnu)
             (rnrs io ports)
             (gnu packages)
             (guix base32)
             (guix git-download)
             (guix hash)
             (guix packages)
             (guix serialization)
             (guix utils)
             (guix ui)
             (cuirass base))

(define (url->file-name url)
  (string-trim
   (string-map (lambda (c) (if (memq c (string->list ":/")) #\- c)) url)
    #\-))

(define* (package->spec pkg #:key (branch "master") commit url)
  (let ((url (or url ((compose git-reference-url origin-uri package-source) pkg))))
    `((#:name . ,(url->file-name url))
      (#:url . ,url)
      (#:branch . ,branch)
      (#:commit . ,commit))))

(define (vcs-file? file stat)
  (case (stat:type stat)
    ((directory)
     (member (basename file) '(".bzr" ".git" ".hg" ".svn" "CVS")))
    (else
     #f)))

(define select? (negate vcs-file?))

(define (file-hash file)
  ;; Compute the hash of FILE.
  ;; Catch and gracefully report possible '&nar-error' conditions.
  (with-error-handling
    (let-values (((port get-hash) (open-sha256-port)))
      (write-file file port #:select? select?)
      (flush-output-port port)
      (get-hash))))

(define (commit? string)
  (string-every (string->char-set "0123456789abcdef") string))

(define (call-with-output-fdes fdes new-file thunk)
  (let ((outport (fdes->outport fdes))
        (port (open-file new-file "w")))
    (move->fdes port fdes)
    (let ((result (thunk)))
      (move->fdes port fdes)
      result)))

(define* (package->git-tracked pkg #:key (branch "master") commit url)
  (let* ((source (package-source pkg))
         (uri (origin-uri source)))
    (if (not branch) pkg
        (let* ((spec (package->spec pkg #:branch branch #:commit commit #:url url))
               (commit (call-with-output-fdes 1 "/dev/null"
                                              (lambda () (fetch-repository spec))))
               (url (or url (git-reference-url uri)))
               (git-dir (string-append (%package-cachedir) "/" (url->file-name url)))
               (hash (bytevector->nix-base32-string (file-hash git-dir)))
               (source (origin (uri (git-reference (url url) (commit commit)))
                              (method git-fetch)
                              (sha256 (base32 hash)))))
          (set-fields pkg ((package-source) source))))))


;;;
;;; Guix entry point.
;;;

(define (guix-jobs store arguments)
  (let* ((name (or (assoc-ref arguments 'name) "hello"))
         (pkg (specification->package name))
         (branch (or (assoc-ref arguments 'branch) "master"))
         (url (assoc-ref arguments 'url))
         (pkg.git (package->git-tracked pkg #:branch branch #:url url))
         (system (or (assoc-ref arguments 'system) "x86_64-linux")))
    (parameterize ((%graft? #f))
      (list (package-job store (job-name pkg) pkg.git system)))))
