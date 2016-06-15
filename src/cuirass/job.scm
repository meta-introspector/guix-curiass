;;;; job.scm - data structures for jobs
;;;
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;;
;;; This file is part of Cuirass.
;;;
;;; Cuirass is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Cuirass is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Cuirass.  If not, see <http://www.gnu.org/licenses/>.

(define-module (cuirass job)
  #:use-module (cuirass base)
  #:use-module (srfi srfi-9)
  #:export (<job>
            make-job
            job?
            job-name
            job-derivation
            job-metadata

            <job-spec>
            make-job-spec
            job-spec?
            job-spec-name
            job-spec-url
            job-spec-branch
            job-spec-file
            job-spec-proc
            job-spec-arguments))

(define-record-type <job>
  (%make-job name derivation metadata)
  job?
  (name       job-name)                 ;string
  (derivation job-derivation)           ;string
  (metadata   job-metadata))            ;alist

(define-syntax make-job
  (syntax-rules ()
    ;; XXX: Different orders for keyword/argument pairs should be allowed.
    ((make-job #:name name #:derivation filename #:metadata metadata)
     (begin
       (format (current-error-port) "evaluating '~a'... " name)
       (force-output (current-error-port))
       (%make-job name
                  (call-with-time-display (λ () filename))
                  metadata)))))

(define-record-type <job-spec>
  (%make-job-spec name url branch file proc arguments)
  job-spec?
  (name      job-spec-name)             ;string
  (url       job-spec-url)              ;string
  (branch    job-spec-branch)           ;string
  (file      job-spec-file)             ;string
  (proc      job-spec-proc)             ;symbol
  (arguments job-spec-arguments))       ;alist

(define* (make-job-spec #:key name url branch file proc arguments)
  (%make-job-spec name url branch file proc arguments))
