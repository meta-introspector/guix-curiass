;;;; evaluate -- convert a specification to a job list
;;; Copyright © 2016, 2018, 2022, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017, 2018, 2021 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
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

(define-module (cuirass scripts evaluate)
  #:use-module ((cuirass base) #:select (latest-checkouts))
  #:use-module (cuirass database)
  #:use-module (cuirass specification)
  #:use-module (guix channels)
  #:use-module (guix derivations)
  #:use-module (guix inferior)
  #:use-module (guix monads)
  #:use-module ((guix store) #:hide (build))
  #:autoload   (guix ui) (show-what-to-build*)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:export (cuirass-evaluate))

(define (checkouts->channel-instances checkouts)
  "Return the list of CHANNEL-INSTANCE records describing the given
CHECKOUTS."
  (map (lambda (checkout)
         (let ((channel (checkout-channel checkout))
               (directory (checkout-directory checkout))
               (commit (checkout-commit checkout)))
           (checkout->channel-instance directory
                                       #:name channel
                                       #:commit commit)))
       checkouts))

(define (user-alists->builds jobs specification-name evaluation-id)
  "Convert JOBS, the user-supplied list of job alists for SPECIFICATION-NAME and
EVALUATION-ID, into a list of <build> records."
  (map (lambda (alist)
         (build (evaluation-id evaluation-id)
                (specification-name specification-name)
                (job-name (assq-ref alist #:job-name))
                (nix-name (assq-ref alist #:nix-name))
                (system (assq-ref alist #:system))
                (timeout (assq-ref alist #:timeout))
                (max-silent-time (assq-ref alist #:max-silent-time))
                (derivation (assq-ref alist #:derivation))
                (dependencies (assq-ref alist #:inputs))
                (outputs
                 (map (match-lambda
                        ((name . item)
                         (output (name name)
                                 (item item)
                                 (derivation derivation))))
                      (or (assq-ref alist #:outputs) '())))))
       jobs))

(define* (inferior-evaluation store profile
                              #:key
                              eval-id instances
                              spec build systems)
  "Spawn an inferior that uses the given STORE and PROFILE. Withing that
inferior, call EVAL-PROC from the EVAL-MODULE.  Register the returned jobs in
database for the EVAL-ID evaluation of the SPEC specification.

Pass the BUILD, CHANNELS and SYSTEMS arguments to the EVAL-PROC procedure."
  ;; The module where the below procedure is defined.
  (define eval-module '(gnu ci))

  ;; The Guix procedure for job evaluation.
  (define eval-proc 'cuirass-jobs)

  (define channels
    (map channel-instance->sexp instances))

  (let* ((inferior (open-inferior profile))
         (args `((channels . ,channels)
                 (systems . ,systems)
                 (subset . ,build))))
    (inferior-eval `(use-modules ,eval-module) inferior)
    (let ((jobs
           (inferior-eval-with-store
            inferior store
            `(lambda (store)
               (,eval-proc store ',args)))))
      (close-inferior inferior)

      ;; EVAL-PROC returns a list of job alists: this has the advantage of
      ;; being serializable and immune to ABI and API changes.  Here, convert
      ;; it to <build> records for internal consumption.
      (db-register-builds (user-alists->builds jobs
                                               (specification-name spec)
                                               eval-id)
                          spec))))

(define (channel-instances->profile instances)
  "Return a directory containing a guix filetree defined by INSTANCES, a list
of channel instances."
  (with-store store
    (run-with-store store
      (mlet* %store-monad ((profile
                            (channel-instances->derivation instances)))
        (mbegin %store-monad
          (show-what-to-build* (list profile))
          (built-derivations (list profile))
          (return (derivation->output-path profile)))))))

(define (cuirass-evaluate args)
  "This procedure spawns an inferior on the given channels.  An evaluation
procedure is called within that inferior, it returns a list of jobs that are
registered in database."
  (match args
    ((command database eval-str)
     (parameterize ((%package-database database))
       (with-database
           (let* ((eval-id (with-input-from-string eval-str read))
                  (name (db-get-evaluation-specification eval-id))
                  (spec (db-get-specification name))
                  (checkouts (latest-checkouts spec eval-id))
                  (instances (checkouts->channel-instances checkouts))
                  (profile (channel-instances->profile instances))
                  (build (specification-build spec))
                  (systems (specification-systems spec)))
             (define statuses
               ;; Evaluate jobs on a per-system basis for two reasons.  It
               ;; speeds up the evaluation as the evaluations can be performed
               ;; concurrently.  It also decreases the amount of memory needed
               ;; per evaluation process.
               (n-par-map
                (min (length systems) (current-processor-count))
                (lambda (system)
                  (with-store store
                    (inferior-evaluation store profile
                                         #:eval-id eval-id
                                         #:instances instances
                                         #:spec spec
                                         #:build build
                                         #:systems (list system))
                    'success))
                systems))

             ;; Display something if and only if all the evaluations succeeded
             ;; ('n-par-map' swallows exceptions, hence this trick.)  'cuirass
             ;; register' equates "no message on stdout" with evaluation
             ;; failure.
             (when (equal? statuses (map (const 'success) systems))
               (display 'done))))))
    (x
     (format (current-error-port) "Wrong command: ~a~%." x)
     (exit 1))))
