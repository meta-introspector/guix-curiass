;;;; cuirass -- continuous integration tool
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (cuirass scripts web)
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
  #:export (cuirass-web))

(define (show-help)
  (format #t "Usage: ~a web [OPTIONS]~%" (%program-name))
  (display "Run the Cuirass web server.
  -P  --parameters=PARAMFILE
                            Read parameters for PARAMFILE.
  -D  --database=DB         Use DB to store build results.
  -p  --port=NUM            Port of the HTTP server.
      --listen=HOST         Listen on the network interface for HOST
  -V, --version             Display version
  -h, --help                Display this help message")
  (newline)
  (show-package-information))

(define %options
  '((parameters     (single-char #\P) (value #t))
    (database       (single-char #\D) (value #t))
    (port           (single-char #\p) (value #t))
    (listen                           (value #t))
    (version        (single-char #\V) (value #f))
    (help           (single-char #\h) (value #f))))


;;;
;;; Entry point.
;;;

(define (cuirass-web args)
  (let ((opts (getopt-long args %options)))
    (parameterize
        ((%create-database? #f)
         (%package-database (option-ref opts 'database (%package-database))))
      (cond
       ((option-ref opts 'help #f)
        (show-help)
        (exit 0))
       ((option-ref opts 'version #f)
        (show-version)
        (exit 0))
       (else
        (let ((port (string->number (option-ref opts 'port "8080")))
              (host (option-ref opts 'listen "localhost"))
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
                 (and paramfile (read-parameters paramfile))

                 (let ((exit-channel (make-channel)))
                   (start-watchdog)
                   (spawn-fiber
                    (essential-task
                     'web exit-channel
                     (lambda ()
                       (run-cuirass-server #:host host
                                           #:port port)))
                    #:parallel? #t)

                   (spawn-fiber
                    (essential-task
                     'monitor exit-channel
                     (lambda ()
                       (while #t
                         (log-monitoring-stats)
                         (sleep 600)))))
                   (primitive-exit (get-message exit-channel)))))

           ;; Most of our code is I/O so preemption doesn't matter much (it
           ;; could help while we're doing SQL requests, for instance, but it
           ;; doesn't actually help since these are non-resumable
           ;; continuations.)  Thus, reduce the tick rate.
           #:hz 10

           #:parallelism threads
           #:drain? #t)))))))
