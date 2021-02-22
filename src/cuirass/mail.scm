;;; mail.scm -- Send mails.
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

(define-module (cuirass mail)
  #:use-module (cuirass utils)
  #:use-module (mailutils mailutils)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-19)
  #:export (send-email))

;; Copied from (mumi send-email).
(define* (compute-message-id message #:optional seed)
  "Return a message ID string."
  (string-append "<" (number->string (object-address message) 16)
                 "." (number->string
                      (or seed
                          (string-hash
                           (or (mu-message-get-header message "Subject")
                               "")))
                      16)
                 "@guile.gnu.org>"))

(define* (send-email url
                     #:key
                     from to
                     (date (time-utc->date (current-time time-utc)))
                     subject text)
  "Send an email to URL.  Use the FROM, TO, DATE and SUBJECT arguments to set
the MIME headers.  TEXT is copied as the email body.

This method supports sendmail and SMTP methods.  The URL syntax is described
here: https://mailutils.org/manual/html_node/Mailbox.html#Mailbox."
  (mu-register-format (if (string-prefix? "sendmail" url)
                          "sendmail"
                          "smtp"))
  (let* ((mime    (mu-mime-create))
         (message (mu-message-create))
         (body    (mu-message-get-port message "w")))
    (mu-message-set-header message
                           "Content-Type"
                           "text/plain; charset=utf-8")
    (put-bytevector body (string->utf8 text))
    (newline body)
    (close-port body)
    (mu-mime-add-part mime message)

    (let ((message* (mu-mime-get-message mime)))
      (mu-message-set-header message* "From" from)
      (mu-message-set-header message* "To" to)
      (mu-message-set-header message* "Date" (date->rfc822-str date))
      (mu-message-set-header message* "Message-ID"
                             (compute-message-id message
                                                 (and=> text string-hash)))
      (when subject
        (mu-message-set-header message* "Subject" subject))

      (mu-message-send message* url))))
