;;; random-manifest.scm -- Return a manifest of random entries.
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

(use-modules (guix)
             (guix profiles)
             (srfi srfi-1)
             (srfi srfi-26))

(define* (make-job name lowerable #:optional (output "out"))
  (manifest-entry
    (name name)
    (version "0")
    (item lowerable)
    (output output)))

(define %seed
  (logxor (cdr (gettimeofday))
          (car (gettimeofday))
          (cdr (gettimeofday))))

(define %state
  (seed->random-state %seed))

(define* (random-computed-file #:optional (suffix "")
                               multiple-outputs?
                               dependency)
  (let ((nonce (random 1e6 %state)))
    (computed-file (string-append "random" suffix)
                   #~(let ((delay #$(random 60 %state))
                           (fail? #$(zero? (random 4 %state))))
                       (setvbuf (current-output-port) 'line)
                       (setvbuf (current-error-port) 'line)
                       (set-port-encoding! (current-output-port) "UTF-8")

                       ;; Optionally introduce a dependency.
                       (let ((dependency
                              '#$(and dependency
                                      #~(ungexp (manifest-entry-item dependency)
                                                (manifest-entry-output dependency)))))
                         (when dependency
                           (format #t "dependency on ~a~%" dependency)))

                       (display "Starting build!\n")
                       (display "Here's a UTF-8-encoded lambda: λ.\n")
                       (sleep (pk 'sleeping delay))
                       (when fail?
                         (error "we're faillliiiiing!"))
                       #$nonce
                       #$(if multiple-outputs?
                             #~(begin
                                 (mkdir #$output:first)
                                 (mkdir #$output:second))
                             #~(mkdir #$output))))))


(when (zero? (random 7 %state))
  (error "Evaluation is failing!"))

;; Synthesize a manifest that covers various cases: succeeding/failing jobs,
;; jobs with/without dependencies, etc.
(manifest
 (let loop ((i 0)
            (lst '()))
   (if (>= i 20)
       (reverse lst)
       (let* ((multiple-outputs? (zero? (modulo i 5)))
              (dependency (and (= 0 (modulo i 3))
                               (> i 0)
                               (list-ref lst
                                         (random (length lst) %state))))
              (suffix (string-append
                       (if multiple-outputs?
                           "multiple-outputs"
                           "")
                       (number->string i))))
         (loop (+ i 1)
               (cons (make-job (string-append "entropy-" suffix)
                               (random-computed-file suffix
                                                     multiple-outputs?
                                                     dependency)
                               (if multiple-outputs?
                                   "first"
                                   "out"))
                     lst))))))
