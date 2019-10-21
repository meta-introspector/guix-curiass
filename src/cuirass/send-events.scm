;;;; http.scm -- HTTP API
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
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

(define-module (cuirass send-events)
  #:use-module (cuirass config)
  #:use-module (cuirass database)
  #:use-module (cuirass utils)
  #:use-module (cuirass logging)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 textual-ports)
  #:export (send-events))

(define* (send-events target-url
                      #:key (batch-limit 100))
  "Send up to BATCH-LIMIT events to TARGET-URL"
  (with-exponential-backoff-upon-error
   (lambda ()
     (let ((events-to-send
            (db-get-events `((nr . ,batch-limit)))))
       (unless (null? events-to-send)
         (let ((body
                (object->json-string
                 `((items
                    . ,(list->vector
                        (map (lambda (event)
                               (let ((event-json
                                      (json-string->scm
                                       (assq-ref event #:event_json))))
                                 `((id        . ,(assq-ref event #:id))
                                   (type      . ,(assq-ref event #:type))
                                   (timestamp . ,(assq-ref event #:timestamp))
                                   ,@event-json)))
                             events-to-send)))))))
           (let*-values
               (((response body)
                 (http-post target-url
                            #:body body
                            ;; Guile doesn't treat JSON as text, so decode the
                            ;; body manually
                            #:decode-body? #f))
                ((code)
                 (response-code response)))
             (unless (and (>= code 200)
                          (< code 300))
               (throw
                'request-failure
                (simple-format #f "code: ~A response: ~A"
                               code
                               (utf8->string body))))))
         (db-delete-events-with-ids-<=-to
          (assq-ref (last events-to-send) #:id))
         (simple-format #t "Sent ~A events\n" (length events-to-send)))))))

(define* (with-exponential-backoff-upon-error thunk #:key (retry-number 1))
  "Call THUNK and catch exceptions, retrying after a number of seconds that
increases exponentially."
  (catch
    #t
    thunk
    (lambda (key . args)
      (simple-format (current-error-port)
                     "Failure sending events (try ~A)\n"
                     retry-number)
      (print-exception (current-error-port) #f key args)
      (let ((sleep-length (integer-expt 2 retry-number)))
        (simple-format (current-error-port)
                       "\nWaiting for ~A seconds\n"
                       sleep-length)
        (sleep sleep-length)
        (with-exponential-backoff-upon-error thunk #:retry-number
                                             (+ retry-number 1))))))
