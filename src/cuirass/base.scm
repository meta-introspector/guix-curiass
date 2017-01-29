;;; base.scm -- Cuirass base module
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (cuirass base)
  #:use-module (cuirass database)
  #:use-module (cuirass utils)
  #:use-module (gnu packages)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-19)
  #:export (;; Procedures.
            call-with-time-display
            fetch-repository
            compile
            evaluate
            build-packages
            process-specs
            set-guix-package-path!
            ;; Parameters.
            %guix-package-path
            %package-cachedir
            %use-substitutes?))

(define %use-substitutes?
  ;; Define whether to use substitutes
  (make-parameter #f))

(define %package-cachedir
  ;; Define to location of cache directory of this package.
  (make-parameter (or (getenv "CUIRASS_CACHEDIR")
                      (string-append (or (getenv "HOME") ".")
                                     "/.cache/cuirass"))
    (λ (val)
      (if (string? val)
          val
          (scm-error 'wrong-type-arg
                     "%package-cachedir" "Not a string: ~S" (list #f) #f)))))

(define (call-with-time thunk kont)
  "Call THUNK and pass KONT the elapsed time followed by THUNK's return
values."
  (let* ((start  (current-time time-monotonic))
         (result (call-with-values thunk list))
         (end    (current-time time-monotonic)))
    (apply kont (time-difference end start) result)))

(define (call-with-time-display thunk)
  "Call THUNK and write to the current output port its duration."
  (call-with-time thunk
    (λ (time result)
      (let ((duration (+ (time-second time)
                         (/ (time-nanosecond time) 1e9))))
        (format (current-error-port) "evaluate '~A': ~,3f seconds~%"
                (assq-ref result #:job-name)
                duration)
        (acons #:duration duration result)))))

(define (fetch-repository spec)
  "Get the latest version of repository specified in SPEC.  Clone repository
if required.  Return the last commit ID on success, #f otherwise."
  (define (current-commit)
    (let* ((pipe   (open-input-pipe "git log -n1"))
           (log    (read-string pipe))
           (commit (cadr (string-split log char-set:whitespace))))
      (close-pipe pipe)
      commit))

  (let ((cachedir (%package-cachedir)))
    (or (file-exists? cachedir) (mkdir-p cachedir))
    (with-directory-excursion cachedir
      (let ((name   (assq-ref spec #:name))
            (url    (assq-ref spec #:url))
            (branch (assq-ref spec #:branch))
            (commit (assq-ref spec #:commit))
            (tag    (assq-ref spec #:tag)))
        (and (or (file-exists? name)
                 (zero? (system* "git" "clone" url name)))
             (with-directory-excursion name
               (and (zero? (system* "git" "fetch"))
                    (zero? (system* "git" "reset" "--hard"
                                    (or tag
                                        commit
                                        (string-append "origin/" branch))))
                    (current-commit))))))))

(define (compile dir)
  ;; Required for fetching Guix bootstrap tarballs.
  "Compile files in repository in directory DIR."
  (with-directory-excursion dir
    (or (file-exists? "configure") (system* "./bootstrap"))
    (or (file-exists? "Makefile")
        (system* "./configure" "--localstatedir=/var"))
    (zero? (system* "make" "-j" (number->string (current-processor-count))))))

(define (evaluate store db spec)
  "Evaluate and build package derivations.  Return a list of jobs."
  (let* ((port (open-pipe* OPEN_READ
                           "evaluate"
                           (string-append (%package-cachedir) "/"
                                          (assq-ref spec #:name) "/"
                                          (assq-ref spec #:load-path))
                           (%guix-package-path)
                           (%package-cachedir)
                           (object->string spec)
                           (%package-database)))
         (jobs (read port)))
    (close-pipe port)
    jobs))

(define (build-packages store db jobs)
  "Build JOBS and return a list of Build results."
  (define (build job)
    (let* ((name     (assq-ref job #:job-name))
           (drv      (assq-ref job #:derivation))
           (eval-id  (assq-ref job #:eval-id))
           (success? #t)
           (error-log (string-append (%package-cachedir) "/"
                                     name ".log")))
      (simple-format #t "building ~A...\n" drv)
      (let ((log (call-with-output-string
                   (λ (port)
                     (parameterize ((current-build-output-port port))
                       (catch 'srfi-34
                         (λ ()
                           (build-derivations store (list drv)))
                         (λ (key . args)
                           (set! success? #f)
                           (pk "kets key:" key "args:" args))))))))
        (when (not success?)
          (with-output-to-file error-log
            (λ () (display log)))
          (simple-format #t "build failed: ~a\n" error-log))
        (let* ((output (and success? (derivation-path->output-path drv)))
               (log    (if success? (log-file store output) error-log))
               (build  `((#:derivation . ,drv)
                         (#:eval-id . ,eval-id)
                         (#:log . ,log)
                         (#:output . ,output))))
          (db-add-build db build)
          (simple-format #t "~A\n" output)
          build))))

  (map build jobs))

(define (process-specs db jobspecs)
  "Evaluate and build JOBSPECS and store results in DB."
  (define (process spec)
    (let ((commit (fetch-repository spec))
          (stamp  (db-get-stamp db spec)))
      (when commit
        (unless (string=? commit stamp)
          (unless (assq-ref spec #:no-compile?)
            (compile (string-append (%package-cachedir) "/"
                                    (assq-ref spec #:name))))
          (with-store store
            (let* ((spec* (acons #:current-commit commit spec))
                   (jobs  (evaluate store db spec*)))
              (unless (%use-substitutes?)
                (set-build-options store #:use-substitutes? #f))
              (build-packages store db jobs))))
        (db-add-stamp db spec commit))))

  (for-each process jobspecs))


;;;
;;; Guix package path.
;;;

(define %guix-package-path
  ;; Extension of package modules search path.
  (make-parameter ""))

(define (set-guix-package-path! path)
  "Use PATH to find custom packages not defined in (gnu packages ...)
namespace or not already present in current Guile load paths."
  (%package-module-path (cons path (%package-module-path)))
  (%patch-path (cons path (%patch-path)))
  (set! %load-path (cons path %load-path))
  (set! %load-compiled-path (cons path %load-compiled-path)))
