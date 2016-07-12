;;;; gnu-system.scm - build jobs for Guix
;;;
;;; Copyright © 2012, 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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

;; Attempt to use Guix modules from git repository.
(eval-when (compile load eval)
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
             ((guix licenses) #:select (gpl3+))
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
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match))

(define (package-metadata package)
  "Convert PACKAGE to an alist suitable for Hydra."
  `((#:description . ,(package-synopsis package))
    (#:long-description . ,(package-description package))
    ;; (#:license . ,(package-license package))
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

(define (package-cross-job store job-name package target system)
  "Return a job called TARGET.JOB-NAME that cross-builds PACKAGE
for TARGET on SYSTEM."
  (λ ()
    `((#:job-name . ,(string-join (list target (symbol->string job-name) system)
                                  "."))
      (#:derivation . ,(derivation-file-name
                        (parameterize ((%graft? #f))
                          (package-cross-derivation store package target system
                                                    #:graft? #f))))
      ,@(package-metadata package))))

(define %core-packages
  ;; Note: Don't put the '-final' package variants because (1) that's
  ;; implicit, and (2) they cannot be cross-built (due to the explicit input
  ;; chain.)
  (list gcc-4.8 gcc-4.9 gcc-5 glibc binutils
        gmp mpfr mpc coreutils findutils diffutils patch sed grep
        gawk gnu-gettext hello guile-2.0 zlib gzip xz
        %bootstrap-binaries-tarball
        %binutils-bootstrap-tarball
        %glibc-bootstrap-tarball
        %gcc-bootstrap-tarball
        %guile-bootstrap-tarball
        %bootstrap-tarballs))

(define %packages-to-cross-build
  %core-packages)

(define %cross-targets
  '("mips64el-linux-gnu"
    "mips64el-linux-gnuabi64"))

(define (tarball-job store system)
  "Return Hydra jobs to build the self-contained Guix binary tarball."
  (λ ()
    `((#:job-name . (string-append "binary-tarball." system))
      (#:derivation . ,(derivation-file-name
                        (parameterize ((%graft? #f))
                          (run-with-store store
                            (mbegin %store-monad
                              (set-guile-for-build (default-guile))
                              (self-contained-tarball))
                            #:system system))))
      (#:description . "Stand-alone binary Guix tarball")
      (#:long-description . "This is a tarball containing binaries of Guix
and all its dependencies, and ready to be installed on non-GuixSD
distributions.")
      ;; (#:license . ,gpl3+)
      (#:home-page . ,%guix-home-page-url)
      (#:maintainers . ("bug-guix@gnu.org")))))

(define %job-name
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
             (package-job store (%job-name package) package system))
            (else
             #f)))))


;;;
;;; Hydra entry point.
;;;

(define (hydra-jobs store arguments)
  "Return Hydra jobs."
  (define subset
    (match (assoc-ref arguments 'subset)
      ("core" 'core)                    ; only build core packages
      ("hello" 'hello)                  ; only build hello
      (_ 'all)))                        ; build everything

  (define (cross-jobs system)
    (define (from-32-to-64? target)
      ;; Return true if SYSTEM is 32-bit and TARGET is 64-bit.  This hack
      ;; prevents known-to-fail cross-builds from i686-linux or armhf-linux to
      ;; mips64el-linux-gnuabi64.
      (and (or (string-prefix? "i686-" system)
               (string-prefix? "armhf-" system))
           (string-suffix? "64" target)))

    (define (same? target)
      ;; Return true if SYSTEM and TARGET are the same thing.  This is so we
      ;; don't try to cross-compile to 'mips64el-linux-gnu' from
      ;; 'mips64el-linux'.
      (string-contains target system))

    (define (either proc1 proc2)
      (lambda (x)
        (or (proc1 x) (proc2 x))))

    (append-map (lambda (target)
                  (map (lambda (package)
                         (package-cross-job store (%job-name package)
                                            package target system))
                       %packages-to-cross-build))
                (remove (either from-32-to-64? same?) %cross-targets)))

  ;; Turn off grafts.  Grafting is meant to happen on the user's machines.
  (parameterize ((%graft? #f))
    ;; Return one job for each package, except bootstrap packages.
    (append-map (lambda (system)
                  (case subset
                    ((all)
                     ;; Build everything, including replacements.
                     (let ((pkgs (fold-packages
                                  (lambda (package result)
                                    (if (package-replacement package)
                                        (cons* package
                                               (package-replacement package)
                                               result)
                                        (cons package result)))
                                  '())))
                       (append (filter-map (lambda (pkg)
                                             (package->job store pkg system))
                                           pkgs)
                               (list (tarball-job store system))
                               (cross-jobs system))))
                    ((core)
                     ;; Build core packages only.
                     (append (map (lambda (package)
                                    (package-job store (%job-name package)
                                                 package system))
                                  %core-packages)
                             (cross-jobs system)))
                    ((hello)
                     ;; Build hello package only.
                     (if (string=? system (%current-system))
                         (list (package-job store (%job-name hello) hello system))
                         '()))
                    (else
                     (error "unknown subset" subset))))
                %hydra-supported-systems)))
