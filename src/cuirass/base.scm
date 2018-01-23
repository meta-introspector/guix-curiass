;;; base.scm -- Cuirass base module
;;; Copyright © 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (fibers)
  #:use-module (cuirass logging)
  #:use-module (cuirass database)
  #:use-module (gnu packages)
  #:use-module (guix build utils)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix git)
  #:use-module (git)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs bytevectors)
  #:export (;; Procedures.
            call-with-time-display
            fetch-repository
            compile
            evaluate
            build-packages
            prepare-git
            process-specs
            set-guix-package-path!
            ;; Parameters.
            %guix-package-path
            %package-cachedir
            %use-substitutes?
            %fallback?))

(define-syntax-rule (with-store store exp ...)
  ;; XXX: This is a 'with-store' variant that plays well with delimited
  ;; continuations and fibers.  The 'with-store' macro in (guix store)
  ;; currently closes in a 'dynamic-wind' handler, which means it would close
  ;; the store at each context switch.  Remove this when the real 'with-store'
  ;; has been fixed.
  (let* ((store (open-connection))
         (result (begin exp ...)))
    (close-connection store)
    result))

(cond-expand
  (guile-2.2
   ;; Guile 2.2.2 has a bug whereby 'time-monotonic' objects have seconds and
   ;; nanoseconds swapped (fixed in Guile commit 886ac3e).  Work around it.
   (define time-monotonic time-tai))
  (else #t))

(define %use-substitutes?
  ;; Define whether to use substitutes
  (make-parameter #f))

(define %fallback?
  ;; Define whether to fall back to building when the substituter fails.
  (make-parameter #f))

(define %package-cachedir
  ;; Define to location of cache directory of this package.
  (make-parameter (or (getenv "CUIRASS_CACHEDIR")
                      (string-append (or (getenv "HOME") ".")
                                     "/.cache/cuirass"))
    (lambda (val)
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
    (lambda (time result)
      (let ((duration (+ (time-second time)
                         (/ (time-nanosecond time) 1e9))))
        (format (current-error-port) "evaluate '~A': ~,3f seconds~%"
                (assq-ref result #:job-name)
                duration)
        (acons #:duration duration result)))))

(define (report-git-error error)
  "Report the given Guile-Git error."
  (format (current-error-port)
          "Git error: ~a~%" (git-error-message error)))

(define-syntax-rule (with-git-error-handling body ...)
  (catch 'git-error
    (lambda ()
      body ...)
    (lambda (key err)
      (report-git-error err))))

(define (fetch-repository store spec)
  "Get the latest version of repository specified in SPEC.  Return two
values: the content of the git repository at URL copied into a store
directory and the sha1 of the top level commit in this directory."

  (define (add-origin branch)
    "Prefix branch name with origin if no remote is specified."
    (if (string-index branch #\/)
        branch
        (string-append "origin/" branch)))

  (let ((name   (assq-ref spec #:name))
        (url    (assq-ref spec #:url))
        (branch (and=> (assq-ref spec #:branch)
                       (lambda (b)
                         `(branch . ,(add-origin b)))))
        (commit (and=> (assq-ref spec #:commit)
                       (lambda (c)
                         `(commit . ,c))))
        (tag    (and=> (assq-ref spec #:tag)
                       (lambda (t)
                         `(tag . ,t)))))
    (latest-repository-commit store url
                              #:cache-directory (%package-cachedir)
                              #:ref (or branch commit tag))))

(define (copy-repository-cache repo spec)
  "Copy REPO directory in cache. The directory is named after NAME
  field in SPEC."
  (let ((cachedir (%package-cachedir)))
    (mkdir-p cachedir)
    (with-directory-excursion cachedir
      (let ((name (assq-ref spec #:name)))
        ;; Flush any directory with the same name.
        (false-if-exception (delete-file-recursively name))
        (copy-recursively repo name)
        (system* "chmod" "-R" "+w" name)))))

(define (compile dir)
  ;; Required for fetching Guix bootstrap tarballs.
  "Compile files in repository in directory DIR."
  (with-directory-excursion dir
    (or (file-exists? "configure") (system* "./bootstrap"))
    (or (file-exists? "Makefile")
        (system* "./configure" "--localstatedir=/var"))
    (zero? (system* "make" "-j" (number->string (current-processor-count))))))

(define-condition-type &evaluation-error &error
  evaluation-error?
  (name evaluation-error-spec-name))

(define (non-blocking-port port)
  "Make PORT non-blocking and return it."
  (let ((flags (fcntl port F_GETFL)))
    (fcntl port F_SETFL (logior O_NONBLOCK flags))
    port))

(define (evaluate store db spec)
  "Evaluate and build package derivations.  Return a list of jobs."
  (let* ((port (non-blocking-port
                (open-pipe* OPEN_READ
                            "evaluate"
                            (string-append (%package-cachedir) "/"
                                           (assq-ref spec #:name) "/"
                                           (assq-ref spec #:load-path))
                            (%guix-package-path)
                            (%package-cachedir)
                            (object->string spec)
                            (%package-database))))
         ;; XXX: Since 'read' is not suspendable as of Guile 2.2.3, we use
         ;; 'read-string' (which is suspendable) and then 'read'.
         (jobs (match (read-string port)
                 ;; If an error occured during evaluation report it,
                 ;; otherwise, suppose that data read from port are
                 ;; correct and keep things going.
                 ((? eof-object?)
                  (raise (condition
                          (&evaluation-error
                           (name (assq-ref spec #:name))))))
                 ((? string? data)
                  (call-with-input-string data read)))))
    (close-pipe port)
    jobs))


;;;
;;; Build status.
;;;

;; TODO: Remove this code once it has been integrated in Guix proper as (guix
;; status).

(define %newline
  (char-set #\return #\newline))

(define (build-event-output-port proc seed)
  "Return an output port for use as 'current-build-output-port' that calls
PROC with its current state value, initialized with SEED, on every build
event.  Build events passed to PROC are tuples corresponding to the \"build
traces\" produced by the daemon:

  (build-started \"/gnu/store/...-foo.drv\" ...)
  (substituter-started \"/gnu/store/...-foo\" ...)

and so on. "
  (define %fragments
    ;; Line fragments received so far.
    '())

  (define %state
    ;; Current state for PROC.
    seed)

  (define (process-line line)
    (when (string-prefix? "@ " line)
      (match (string-tokenize (string-drop line 2))
        (((= string->symbol event-name) args ...)
         (set! %state
           (proc (cons event-name args)
                 %state))))))

  (define (write! bv offset count)
    (let loop ((str (utf8->string bv)))
      (match (string-index str %newline)
        ((? integer? cr)
         (let ((tail (string-take str cr)))
           (process-line (string-concatenate-reverse
                          (cons tail %fragments)))
           (set! %fragments '())
           (loop (string-drop str (+ 1 cr)))))
        (#f
         (set! %fragments (cons str %fragments))
         count))))

  (make-custom-binary-output-port "filtering-input-port"
                                  write!
                                  #f #f #f))


;;;
;;; Building packages.
;;;

(define* (handle-build-event db event
                             #:key (log-port (current-error-port)))
  "Handle EVENT, a build event sexp as produced by 'build-event-output-port'."
  ;; TODO: Update DB according to EVENT.
  (match event
    (('build-started drv _ ...)
     (log-message "build started: '~a'" drv))
    (('build-remote drv host _ ...)
     (log-message "'~a' offloaded to '~a'" drv host))
    (('build-succeeded drv _ ...)
     (log-message "build succeeded: '~a'" drv))
    (('build-failed drv _ ...)
     (log-message "build failed: '~a'" drv))
    (('substituter-started item _ ...)
     (log-message "substituter started: '~a'" item))
    (('substituter-succeeded item _ ...)
     (log-message "substituter succeeded: '~a'" item))
    (_
     (log-message "build event: ~s" event))))

(define (build-packages store db jobs)
  "Build JOBS and return a list of Build results."

  (define hydra-build-status
    ;; Build status as expected by hydra compatible API's.
    '((succeeded         . 0)
      (failed            . 1)
      (failed-dependency . 2)
      (failed-other      . 3)
      (cancelled         . 4)))

  (define (register job)
    (let* ((name     (assq-ref job #:job-name))
           (drv      (assq-ref job #:derivation))
           (eval-id  (assq-ref job #:eval-id))
           ;; XXX: How to keep logs from several attempts?
           (log      (log-file store drv))
           (outputs  (filter-map (lambda (res)
                                   (match res
                                     ((name . path)
                                      (and (valid-path? store path)
                                           `(,name . ,path)))))
                                 (derivation-path->output-paths drv)))
           (cur-time (time-second (current-time time-utc))))
      (let ((build `((#:derivation . ,drv)
                     (#:eval-id . ,eval-id)
                     (#:log . ,log)
                     (#:status .
                      ,(match (length outputs)
                         (0 (assq-ref hydra-build-status 'failed))
                         (_ (assq-ref hydra-build-status 'succeeded))))
                     (#:outputs . ,outputs)
                     ;;; XXX: For now, we do not know start/stop build time.
                     (#:timestamp . ,cur-time)
                     (#:starttime . ,cur-time)
                     (#:stoptime . ,cur-time))))
        (db-add-build db build)
        build)))

  ;; Pass all the jobs at once so we benefit from as much parallelism as
  ;; possible (we must be using #:keep-going? #t).  Swallow build logs (the
  ;; daemon keeps them anyway), and swallow build errors.
  (guard (c ((nix-protocol-error? c) #t))
    (format #t "load-path=~s\n" %load-path)
    (format #t "load-compiled-path=~s\n" %load-compiled-path)
    (format #t "building ~a derivations...~%" (length jobs))
    (parameterize ((current-build-output-port
                    (build-event-output-port (lambda (event status)
                                               (handle-build-event db event))
                                             #t)))
      (build-derivations store
                         (map (lambda (job)
                                (assq-ref job #:derivation))
                              jobs))))

  ;; Register the results in the database.
  ;; XXX: The 'build-derivations' call is blocking so we end updating the
  ;; database potentially long after things have been built.
  (let* ((results (map register jobs))
         (status (map (cut assq-ref <> #:status) results))
         (success (length (filter zero? status)))
         (outputs (map (cut assq-ref <> #:outputs) results))
         (outs (filter-map (cut assoc-ref <> "out") outputs))
         (fail (- (length jobs) success)))
    (format #t "outputs:\n~a\n" (string-join outs "\n"))
    (format #t "success: ~a, fail: ~a\n" success fail)
    results))

(define (prepare-git)
  "Prepare Guile-Git's TLS support and all."
  ;; Catch and report git errors.
  (with-git-error-handling
   ;; Try the 'GIT_SSL_CAINFO' or 'SSL_CERT_FILE' file first, then search the
   ;; 'SSL_CERT_DIR' directory.
   (let ((directory (getenv "SSL_CERT_DIR"))
         (file      (or (getenv "GIT_SSL_CAINFO")
                        (getenv "SSL_CERT_FILE"))))
     (when (or directory file)
       (set-tls-certificate-locations! directory file)))))

(define (process-specs db jobspecs)
  "Evaluate and build JOBSPECS and store results in DB."
  (define (process spec)
    (with-store store
      (let ((stamp (db-get-stamp db spec))
            (name  (assoc-ref spec #:name)))
         (log-message "considering spec '~a', URL '~a'"
                      name (assoc-ref spec #:url))
         (receive (checkout commit)
             (fetch-repository store spec)
           (log-message "spec '~a': fetched commit ~s (stamp was ~s)"
                        name commit stamp)
           (when commit
             (unless (string=? commit stamp)
               ;; Immediately mark COMMIT as being processed so we don't spawn
               ;; a concurrent evaluation of that same commit.
               (db-add-stamp db spec commit)

               (copy-repository-cache checkout spec)

               (unless (assq-ref spec #:no-compile?)
                 (compile (string-append (%package-cachedir) "/"
                                         (assq-ref spec #:name))))
               ;; Always set #:keep-going? so we don't stop on the first build
               ;; failure.
               (set-build-options store
                                  #:use-substitutes? (%use-substitutes?)
                                  #:fallback? (%fallback?)
                                  #:keep-going? #t)

               (spawn-fiber
                (lambda ()
                  (guard (c ((evaluation-error? c)
                             (log-message "failed to evaluate spec '~s'"
                                          (evaluation-error-spec-name c))
                             #f))
                    (log-message "evaluating '~a' with commit ~s"
                                 name commit)
                    (with-store store
                      (with-database db
                        (let* ((spec* (acons #:current-commit commit spec))
                               (jobs  (evaluate store db spec*)))
                          (log-message "building ~a jobs for '~a'"
                                       (length jobs) name)
                          (build-packages store db jobs)))))))

               ;; 'spawn-fiber' returns zero values but we need one.
               *unspecified*))))))

  (for-each process jobspecs))


;;;
;;; Guix package path.
;;;

(define %guix-package-path
  ;; Extension of package modules search path.
  (make-parameter ""))

(define (set-guix-package-path! path)
  "Use PATH to find custom packages not defined in (gnu packages ...)
namespace or not already present in current Guile load paths.  PATH is
expected to be a colon-separated string of directories."
  (define (set-paths! dir)
    (%package-module-path (cons dir (%package-module-path)))
    (%patch-path (cons dir (%patch-path)))
    (set! %load-path (cons dir %load-path))
    (set! %load-compiled-path (cons dir %load-compiled-path)))

  (let ((dirs (parse-path path)))
    (for-each set-paths! dirs)))
