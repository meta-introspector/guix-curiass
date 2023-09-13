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
  #:use-module (cuirass database)
  #:use-module (cuirass ui)
  #:use-module (cuirass logging)
  #:use-module (cuirass metrics)
  #:use-module (cuirass notification)
  #:use-module (cuirass specification)
  #:use-module ((cuirass store)
                #:select (%gc-root-directory %gc-root-ttl))
  #:use-module (cuirass utils)
  #:use-module (cuirass zabbix)
  #:use-module (guix ui)
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
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
  -I, --interval=N          Wait at most N seconds between each poll
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
;;; Bridge with other Cuirass processes.
;;;

;; Other processes such as 'cuirass web' may need to notify 'cuirass register'
;; of events--e.g., configuration changes.  Ideally, they'd transparently talk
;; to the relevant actor, whether it's process-local or not, but we're not
;; there yet (hi, Goblins!).  The "bridge" below works around this
;; shortcoming: it takes commands over a Unix-domain socket and forwards them
;; to the relevant actor.

(define (open-bridge-socket)
  (let ((sock (socket AF_UNIX
                      (logior SOCK_STREAM SOCK_NONBLOCK SOCK_CLOEXEC)
                      0))
        (file (%bridge-socket-file-name)))
    (log-info "opening bridge socket at '~a'" file)
    (mkdir-p (dirname file))
    (chmod (dirname file) #o700)
    (false-if-exception (delete-file file))
    (bind sock AF_UNIX file)
    (listen sock 2)
    sock))

(define (bridge channel                           ;currently unused
                socket registry)
  (define (serve-client socket)
    (let loop ((count 0))
      (define command
        (false-if-exception (read socket)))

      (if (eof-object? command)
          (begin
            (close-port socket)
            (log-info "terminating bridge server after ~a commands"
                      count))
          (begin
            (log-debug "bridge received command: ~s" command)

            ;; Note: The protocol is bare-bones and unversioned; the 'cuirass'
            ;; processes are meant to be upgraded in lockstep.
            (match command
              (`(register-jobset ,name)
               (match (db-get-specification name)
                 (#f (log-warning "requested spec '~a' not found" name))
                 (spec (register-jobset registry spec))))
              (`(update-jobset ,name)
               (let ((spec (db-get-specification name)))
                 (if spec
                     (update-jobset registry spec)
                     (log-warning "cannot update non-existent spec '~a'" name))))
              (`(trigger-jobset ,name)
               (match (lookup-jobset registry name)
                 (#f (log-warning "requested jobset '~a' not found" name))
                 (jobset
                  ;; Trigger a jobset update.  Since the jobset might take a
                  ;; while to get our message (it might be waiting for a
                  ;; previous pull to complete), send it in a separate fiber.
                  (spawn-fiber
                   (lambda ()
                     (log-info "triggering jobset '~a'" name)
                     (put-message jobset 'trigger))))))
              (_
               #f))
            (loop (+ 1 count))))))

  (lambda ()
    (let loop ()
      (match (accept socket (logior SOCK_NONBLOCK SOCK_CLOEXEC))
        ((connection . peer)
         (spawn-fiber (lambda ()
                        (log-info "bridge accepted connection")
                        (serve-client connection)))
         (loop))))))

(define (spawn-bridge socket registry)
  (let ((channel (make-channel)))
    (spawn-fiber (bridge channel socket registry))
    channel))


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
              (interval (string->number (option-ref opts 'interval "600")))
              (specfile (option-ref opts 'specifications #f))
              (paramfile (option-ref opts 'parameters #f))

              ;; Since our work is mostly I/O-bound, default to a maximum of 8
              ;; kernel threads.  Going beyond that can increase overhead (GC
              ;; may not scale well, work-stealing may become detrimental,
              ;; etc.) for little in return.
              (threads   (or (and=> (option-ref opts 'threads #f)
                                    string->number)
                             (min (current-processor-count) 8))))
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
                   (leave (G_ "'--one-shot' is currently unimplemented~%"))
                   (let* ((exit-channel (make-channel))
                          (builder (if (option-ref opts 'build-remote #f)
                                       (spawn-remote-builder)
                                       (spawn-local-builder)))
                          (evaluator (spawn-jobset-evaluator
                                      #:max-parallel-evaluations threads
                                      #:builder builder))
                          (update-service (spawn-channel-update-service)))
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
                         (restart-builds builder))))

                     ;; Spawn one monitoring actor for each jobset.
                     (let ((registry (spawn-jobset-registry
                                      #:update-service update-service
                                      #:evaluator evaluator
                                      #:polling-period interval)))
                       ;; Spawn the bridge through which other 'cuirass'
                       ;; processes, such as 'cuirass web', may talk to the
                       ;; registry.
                       (spawn-bridge (open-bridge-socket) registry))

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

           ;; Most of our code is I/O so preemption doesn't matter much.
           ;; Thus, reduce the tick rate.
           #:hz 2

           #:parallelism threads
           #:drain? #t)))))))
