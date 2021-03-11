;;; specification.scm -- Specification definition.
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

(define-module (cuirass specification)
  #:use-module (guix channels)
  #:use-module ((guix openpgp)
                #:select (openpgp-public-key-fingerprint
                          openpgp-format-fingerprint))
  #:use-module (guix records)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:export (build-output
            build-output?
            build-output-job
            build-output-type
            build-output-output
            build-output-path

            build-output->sexp
            sexp->build-output

            channel->sexp
            sexp->channel*

            %build-types

            specification
            specification?
            specification-name
            specification-build
            specification-channels
            specification-priority
            specification-build-outputs
            specification-notifications
            specification-systems

            specification->sexp
            sexp->specification
            read-specifications))


;;;
;;; Build output record.
;;;

(define-record-type* <build-output>
  build-output make-build-output
  build-output?
  (job                build-output-job) ;string
  (type               build-output-type) ;string
  (output             build-output-output ;string
                      (default "out"))
  (path               build-output-path ;string
                      (default "")))

(define (build-output->sexp build-output)
  "Return an sexp describing BUILD-OUTPUT."
  `(build-output
    (job ,(build-output-job build-output))
    (type ,(build-output-type build-output))
    (output ,(build-output-output build-output))
    (path ,(build-output-path build-output))))

(define (sexp->build-output sexp)
  "Return the build-output corresponding to SEXP."
  (match sexp
    (('build-output ('job job)
                    ('type type)
                    ('output output)
                    ('path path))
     (build-output
      (job job)
      (type type)
      (output output)
      (path path)))))


;;;
;;; Channels.
;;;

(define (channel->sexp channel)
  "Return an sexp describing CHANNEL."
  (let ((intro (channel-introduction channel)))
    `(repository
      (version 0)
      (url ,(channel-url channel))
      (branch ,(channel-branch channel))
      (commit ,(channel-commit channel))
      (name ,(channel-name channel))
      ,@(if intro
            `((introduction
               (channel-introduction
                (version 0)
                (commit
                 ,(channel-introduction-first-signed-commit
                   intro))
                (signer
                 ,(openpgp-format-fingerprint
                   (channel-introduction-first-commit-signer
                    intro))))))
            '()))))

(define (sexp->channel* sexp)
  "Return the channel corresponding to SEXP."
  (match sexp
    (('repository ('version 0)
                  ('url url)
                  ('branch branch)
                  ('commit commit)
                  ('name name)
                  rest ...)
     (channel (name name)
              (url url)
              (branch branch)
              (commit commit)))
    (_ #f)))


;;;
;;; Specification record.
;;;

;; The list of possible build types.
(define %build-types
  '(all core guix hello packages manifests))

(define-record-type* <specification>
  specification make-specification
  specification?
  (name               specification-name) ;symbol
  (build              specification-build ;symbol for %build-types
                      (default 'all))
  (channels           specification-channels ;list of <channel>
                      (default (list %default-guix-channel)))
  (build-outputs      specification-build-outputs ;list of <build-output>
                      (default '()))
  (notifications      specification-notifications
                      (default '()))
  (priority           specification-priority ;integer
                      (default 9))
  (systems            specification-systems ;list of strings
                      (default (list (%current-system)))))

(define (specification->sexp spec)
  "Return an sexp describing SPEC."
  `(specification (name ,(specification-name spec))
                  (build ,(specification-build spec))
                  (channels ,(specification-channels spec))
                  (build-outputs ,(specification-build-outputs spec))
                  (notifications ,(specification-notifications spec))
                  (priority ,(specification-priority spec))
                  (systems ,(specification-systems spec))))

(define (sexp->specification sexp)
  "Return the specification corresponding to SEXP."
  (match sexp
    (('specification ('name name)
                     ('build build)
                     ('channels channels)
                     ('build-outputs build-outputs)
                     ('notifications notifications)
                     ('priority priority)
                     ('systems systems))
     (specification (name name)
                    (build build)
                    (channels channels)
                    (build-outputs build-outputs)
                    (notifications notifications)
                    (priority priority)
                    (systems systems)))))

(define (read-specifications file)
  (let ((modules (make-user-module '((guix channels)
                                     (cuirass notification)
                                     (cuirass specification)))))
    (load* file modules)))
