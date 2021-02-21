;;; notification.scm -- Send build notifications.
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (cuirass notification)
  #:use-module (cuirass database)
  #:use-module (cuirass logging)
  #:use-module (cuirass mail)
  #:use-module (cuirass mastodon)
  #:use-module (cuirass utils)
  #:export (notification-type
            notification-event

            with-notification
            send-notifications))

;; XXX: Some redefinitions to avoid a circular dependency with the (cuirass
;; database) module.
(define weather-success 0)
(define weather-failure 1)

;; The channel to communicate with the notification worker thread.
(define %notification-channel
  (make-parameter #f))

(define-syntax-rule (with-notification body ...)
  "Run BODY with %NOTIFICATION-CHANNEL being dynamically bound to a channel
providing a worker thread that allows to send notifications without
interfering with fibers."
  (parameterize ((%notification-channel
                  (make-worker-thread-channel
                   (const #t))))
    body ...))

(define-syntax-rule (with-notification-worker-thread exp ...)
  "Evaluate EXP... in the critical section corresponding to
%NOTIFICATION-CHANNEL."
  (call-with-worker-thread
   (%notification-channel)
   (lambda args
     exp ...)))

(define-enumeration notification-type
  (email            0)
  (mastodon         1))

(define-enumeration notification-event
  (always            0)
  (broken-builds     1)
  (fixed-builds      2))

(define (build-weather-text build)
  "Return the build weather string."
  (let ((weather (assq-ref build #:weather)))
    (cond
     ((= weather weather-success)
      "fixed")
     ((= weather weather-failure)
      "broken"))))

(define (notification-subject notification)
  "Return the subject for the given NOTIFICATION."
  (let* ((build (assq-ref notification #:build))
         (job-name (assq-ref build #:job-name))
         (specification (assq-ref build #:specification))
         (weather-text (build-weather-text build)))
    (format #f "Build ~a on ~a is ~a."
            job-name specification weather-text)))

(define (notification-text notification)
  "Return the text for the given NOTIFICATION."
  (let* ((build (assq-ref notification #:build))
         (id (assq-ref build #:id))
         (job-name (assq-ref build #:job-name))
         (specification (assq-ref build #:specification))
         (weather-text (build-weather-text build)))
    (format #f "The build ~a for specification ~a is ~a. You can find
the detailed information about this build here: ~a."
            job-name specification weather-text
            (string-append "build/" (number->string id) "/details"))))

(define (notification-email notification)
  "Send an email for the given NOTIFICATION."
  (let* ((from (assq-ref notification #:from))
         (to (assq-ref notification #:to))
         (server (assq-ref notification #:server))
         (subject (notification-subject notification))
         (text (notification-text notification)))
    (send-email server
                #:from from
                #:to to
                #:subject subject
                #:text text)))

(define (notification-mastodon notification)
  "Send a new status for the given NOTIFICATION."
  (let ((name (assq-ref notification #:instance-name))
        (url (assq-ref notification #:instance-url))
        (token (assq-ref notification #:instance-token))
        (text (notification-text notification)))
    (send-status text
                 #:instance-name name
                 #:instance-url url
                 #:instance-token token)))

(define* (send-notifications notifications #:key build)
  "Send the notifications in NOTIFICATIONS list, regarding the given BUILD."
  (with-notification-worker-thread
   (for-each
    (lambda (notification)
      (let* ((event (assq-ref notification #:event))
             (type (assq-ref notification #:type))
             (weather (assq-ref build #:weather))
             (success? (eq? weather weather-success))
             (failure? (eq? weather weather-failure)))
        (when (or
               (and (eq? event (notification-event always))
                    (or success? failure?))
               (and (eq? event (notification-event broken-builds))
                    failure?)
               (and (eq? event (notification-event fixed-builds))
                    success?))
          (let ((notification* (acons #:build build notification)))
            (cond
             ((eq? type (notification-type email))
              (notification-email notification*))
             ((eq? type (notification-type mastodon))
              (notification-mastodon notification*)))))))
    notifications)))
