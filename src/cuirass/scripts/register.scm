;;;; cuirass -- continuous integration tool
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2018, 2023 Ludovic Courtès <ludo@gnu.org>
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

(define-module (cuirass scripts register)
  #:use-module (cuirass)
  #:use-module (cuirass base)
  #:use-module (cuirass database)
  #:use-module (cuirass ui)
  #:use-module (cuirass logging)
  #:use-module (cuirass metrics)
  #:use-module (cuirass notification)
  #:use-module (cuirass specification)
  #:use-module (cuirass utils)
  #:use-module (cuirass watchdog)
  #:use-module (cuirass zabbix)
  #:use-module (guix ui)
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 getopt-long)
  #:export (cuirass-register))

(define (show-help)
  (format #t "Usage: ~a register [OPTIONS]~%" (%program-name))
  (display "Register build jobs in database.

      --one-shot            Evaluate and build jobs only once
      --cache-directory=DIR Use DIR for storing repository data
      --fallback            Fall back to building when the substituter fails.
  -S  --specifications=SPECFILE
                            Add specifications from SPECFILE to database.
  -P  --parameters=PARAMFILE
                            Read parameters for PARAMFILE.
  -D  --database=DB         Use DB to store build results.
      --ttl=DURATION        Keep build results live for at least DURATION.
  -I, --interval=N          Wait N seconds between each poll
      --build-remote        Use the remote build mechanism
      --threads=N           Use up to N kernel threads
  -V, --version             Display version
  -h, --help                Display this help message")
  (newline)
  (show-package-information))

(define %options
  '((one-shot                         (value #f))
    (cache-directory                  (value #t))
    (specifications (single-char #\S) (value #t))
    (parameters     (single-char #\P) (value #t))
    (database       (single-char #\D) (value #t))
    (interval       (single-char #\I) (value #t))
    (build-remote                     (value #f))
    (use-substitutes                  (value #f)) ;unused, for back compat
    (threads                          (value #t))
    (fallback                         (value #f))
    (ttl                              (value #t))
    (version        (single-char #\V) (value #f))
    (help           (single-char #\h) (value #f))))


;;;
;;; Entry point.
;;;

(define (cuirass-register args)
  (let ((opts (getopt-long args %options)))
    (parameterize
        ((%create-database? #t)
         (%package-database (option-ref opts 'database (%package-database)))
         (%package-cachedir
          (option-ref opts 'cache-directory (%package-cachedir)))
         (%build-remote? (option-ref opts 'build-remote #f))
         (%fallback? (option-ref opts 'fallback #f))
         (%gc-root-ttl
          (time-second (string->duration (option-ref opts 'ttl "30d")))))
      (cond
       ((option-ref opts 'help #f)
        (show-help)
        (exit 0))
       ((option-ref opts 'version #f)
        (show-version)
        (exit 0))
       (else
        ;; If we cannot create the gcroot directory, it should be done later
        ;; on by guix-daemon itself.
        (false-if-exception (mkdir-p (%gc-root-directory)))
        (let ((one-shot? (option-ref opts 'one-shot #f))
              (interval (string->number (option-ref opts 'interval "300")))
              (specfile (option-ref opts 'specifications #f))
              (paramfile (option-ref opts 'parameters #f))

              ;; Since our work is mostly I/O-bound, default to a maximum of 4
              ;; kernel threads.  Going beyond that can increase overhead (GC
              ;; may not scale well, work-stealing may become detrimental,
              ;; etc.) for little in return.
              (threads   (or (and=> (option-ref opts 'threads #f)
                                    string->number)
                             (min (current-processor-count) 4))))
          (prepare-git)

          (log-info "running Fibers on ~a kernel threads" threads)
          (run-fibers
           (lambda ()
             (with-database
                 (and specfile
                      (for-each db-add-or-update-specification
                                (read-specifications specfile)))
                 (and paramfile (read-parameters paramfile))

               (if one-shot?
                   (process-specs (db-get-specifications))
                   (let ((exit-channel (make-channel)))
                     (start-watchdog)
                     (clear-build-queue)

                     ;; If Cuirass was stopped during an evaluation,
                     ;; abort it. Builds that were not registered
                     ;; during this evaluation will be registered
                     ;; during the next evaluation.
                     (db-abort-pending-evaluations)

                     ;; First off, restart builds that had not
                     ;; completed or were not even started on a
                     ;; previous run.
                     (spawn-fiber
                      (essential-task
                       'restart-builds exit-channel
                       (lambda ()
                         (restart-builds))))

                     (spawn-fiber
                      (essential-task
                       'build exit-channel
                       (lambda ()
                         (while #t
                           (process-specs (db-get-specifications))
                           (log-info
                            "next evaluation in ~a seconds" interval)
                           (sleep interval)))))

                     (spawn-fiber
                      (essential-task
                       'metrics exit-channel
                       (lambda ()
                         (while #t
                           (with-time-logging
                            "Metrics update"
                            (db-update-metrics))
                           (sleep 3600)))))

                     (spawn-fiber
                      (essential-task
                       'monitor exit-channel
                       (lambda ()
                         (while #t
                           (log-monitoring-stats)
                           (sleep 600)))))
                     (primitive-exit (get-message exit-channel))))))

           ;; Most of our code is I/O so preemption doesn't matter much (it
           ;; could help while we're doing SQL requests, for instance, but it
           ;; doesn't actually help since these are non-resumable
           ;; continuations.)  Thus, reduce the tick rate.
           #:hz 10

           #:parallelism threads
           #:drain? #t)))))))
