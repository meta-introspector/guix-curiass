;;; notification.scm -- Send build notifications.
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2023 Ludovic Courtès <ludo@gnu.org>
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
  #:autoload   (cuirass mastodon) (send-status)
  #:use-module (cuirass parameters)
  #:use-module (cuirass utils)
  #:use-module (guix build syscalls)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:autoload   (fibers) (spawn-fiber)
  #:export (email
            email?
            email-from
            email-to
            email-server

            mastodon
            mastodon?

            notification->sexp
            sexp->notification

            spawn-notification-fiber))


;;;
;;; Notification types.
;;;

(define-record-type* <email>
  email make-email
  email?
  (from     email-from) ;string
  (to       email-to) ;string
  (server   email-server)) ;string

(define-record-type* <mastodon>
  mastodon make-mastodon
  mastodon?)

(define (notification->sexp notif)
  "Return an sexp describing NOTIF."
  (cond
   ((email? notif)
    `(email
      (from ,(email-from notif))
      (to ,(email-to notif))
      (server ,(email-server notif))))
   ((mastodon? notif)
    '(mastodon))))

(define (sexp->notification sexp)
  "Return the notification corresponding to SEXP."
  (match sexp
    (('email ('from from)
             ('to to)
             ('server server))
     (email
      (from from)
      (to to)
      (server server)))
    (('mastodon)
     (mastodon))))


;;;
;;; Send notifications.
;;;

;; XXX: Some redefinitions to avoid a circular dependency with the (cuirass
;; database) module.
(define weather-success 0)
(define weather-failure 1)

(define (build-weather-text build)
  "Return the build weather string."
  (let ((weather (build-current-weather build)))
    (cond
     ((= weather weather-success)
      "fixed")
     ((= weather weather-failure)
      "broken"))))

(define (build-details-url build)
  "Return the build details URL for BUILD."
  (let ((id (build-id build))
        (url (or (%cuirass-url) "")))
    (string-append url "/build/" (number->string id) "/details")))

(define (notification-subject build)
  "Return the subject for the given NOTIFICATION."
  (let* ((job-name (build-job-name build))
         (specification (build-specification-name build))
         (weather-text (build-weather-text build)))
    (format #f "Build ~a on ~a is ~a."
            job-name specification weather-text)))

(define (notification-text build)
  "Return the text for the given NOTIFICATION."
  (let* ((url (build-details-url build))
         (job-name (build-job-name build))
         (specification (build-specification-name build))
         (weather-text (build-weather-text build)))
    (format #f "The build ~a for specification ~a is ~a. You can find \
the detailed information about this build here: ~a."
            job-name specification weather-text url)))

(define (send-email* notif build)
  "Send an email for the given NOTIFICATION."
  (let* ((from (email-from notif))
         (to (email-to notif))
         (server (email-server notif))
         (subject (notification-subject build))
         (text (notification-text build)))
    (catch #t
      (lambda ()
        (send-email server
                    #:from from
                    #:to to
                    #:subject subject
                    #:text text))
      (lambda args
        (log-error "Failed to send the email notification: ~a."
                   args)))))

(define (send-mastodon build)
  "Send a new status for the given NOTIFICATION."
  (let ((text (notification-text build)))
    (catch #t
      (lambda ()
        (send-status text))
      (lambda args
        (log-error "Failed to send the mastodon notification: ~a."
                   args)))))

(define (spawn-notification-fiber)
  "Start a fiber sending build notifications."
  ;; TODO: Turn it into an actor that waits for incoming messages instead
  ;; of polling.
  (spawn-fiber
   (lambda ()
     (let loop ()
       (match (db-pop-notification)
         ((notif . build)
          (log-debug "notification ~s for build ~s"
                     notif build)
          (cond
           ((email? notif)
            ;; 'send-email' calls 'mu-message-send', which in turn spawns a
            ;; mailer process and blocks until completion with waitpid(2).  Do
            ;; that in a separate thread to avoid blocking all the fibers.
            (non-blocking (send-email* notif build)))
           ((mastodon? notif)
            (send-mastodon build))))
         (#f #f))
       ((@ (fibers) sleep) 60)
       (loop)))))
