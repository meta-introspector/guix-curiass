;;; watchdog.scm -- Monitor fibers scheduling.
;;; Copyright © 2020 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (cuirass watchdog)
  #:use-module (cuirass logging)
  #:use-module (cuirass utils)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module (fibers internal)
  #:use-module (fibers operations)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:export (start-watchdog))

(define* (watchdog-fiber scheduler channel
                         #:key
                         (period 1))
  "Spawn a fiber running on SCHEDULER that sends over CHANNEL, every PERIOD
seconds, the scheduler name and the current time."
  (spawn-fiber
   (lambda ()
     (while #t
       (put-message channel (list (scheduler-name scheduler)
                                  (current-time)))
       (sleep period)))
   scheduler))

(define* (start-watchdog #:key (timeout 5))
  "Start a watchdog checking that each Fibers scheduler is not blocked for
more than TIMEOUT seconds.

The watchdog mechanism consists in spawning a dedicated fiber per running
Fiber scheduler, using the above watchdog-fiber method.  Those fibers send a
ping signal periodically to a separate thread.  If no signal is received from
one of the schedulers for more than TIMEOUT seconds, a warning message is
printed."
  (define (check-timeouts pings last-check)
    (let* ((check-period timeout)
           (cur-time (current-time))
           (diff-check (- cur-time last-check)))
      (if (> diff-check check-period)
          (begin
            (for-each
             (match-lambda
               ((scheduler . time)
                (let ((diff-ping (- cur-time time)))
                  (when (> diff-ping timeout)
                    (log-message "Scheduler ~a blocked since ~a seconds."
                                 scheduler diff-ping)))))
             pings)
            cur-time)
          last-check)))

  (let ((watchdog-channel (make-channel)))
    (parameterize (((@@ (fibers internal) current-fiber) #f))
      (call-with-new-thread
       (lambda ()
         (let loop ((pings '())
                    (last-check 0))
           (let ((operation-timeout 10))
             (match (perform-operation
                     (with-timeout
                      (get-operation watchdog-channel)
                      #:seconds operation-timeout
                      #:wrap (const 'timeout)))
               ((scheduler ping)
                (loop (assq-set! pings scheduler ping)
                      (check-timeouts pings last-check)))
               ('timeout
                (loop pings
                      (check-timeouts pings last-check)))))))))
    (fold-all-schedulers
     (lambda (name scheduler seed)
       (watchdog-fiber scheduler watchdog-channel))
     '())))
