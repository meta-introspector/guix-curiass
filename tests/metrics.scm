;;;; database.scm - tests for (cuirass metrics) module
;;;
;;; Copyright © 2020 Mathieu Othacehe <othacehe@gnu.org>
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

(use-modules (cuirass database)
             (cuirass metrics)
             (cuirass utils)
             ((guix utils) #:select (call-with-temporary-output-file))
             (squee)
             (srfi srfi-64))

(define today
  (let ((time (current-time)))
    (- time (modulo time 86400))))

(define yesterday
  (- today 86400))

(define %db
  (make-parameter #f))

(test-group-with-cleanup "database"
  (test-assert "db-init"
    (begin
      (%db (db-open))
      (%db-channel (make-worker-thread-channel
                    (lambda ()
                      (list (%db)))))
      #t))

  (test-assert "exec-query"
    (begin
      (exec-query (%db) "\
INSERT INTO Specifications (name, load_path_inputs, package_path_inputs,
proc_input, proc_file, proc, proc_args, build_outputs, priority)
VALUES ('guix', '()', '()', 'guix',' build-aux/cuirass/gnu-system.scm',
'cuirass-jobs', '', '', 2);")
      (exec-query (%db) "\
INSERT INTO Evaluations (specification, status,
timestamp, checkouttime, evaltime) VALUES ('guix', -1, 1600174547, 0, 0);")
      (exec-query (%db) (format #f "\
INSERT INTO Evaluations (specification, status,
timestamp, checkouttime, evaltime) VALUES ('guix', 0, ~a, ~a, ~a);\
" yesterday (+ yesterday 100) (+ yesterday 600)))
      (exec-query (%db) "\
INSERT INTO Evaluations (specification, status,
timestamp, checkouttime, evaltime) VALUES ('guix', 1, 1600174547,
1600174548, 0);")
      (exec-query (%db) "\
INSERT INTO Evaluations (specification, status,
timestamp, checkouttime, evaltime) VALUES ('guix', 1, 1600174547,
1600174548, 1600174647);")
      (exec-query (%db) (format #f "\
INSERT INTO Builds (id, derivation, evaluation, job_name, system,
nix_name, log, status, timestamp, starttime, stoptime) VALUES
(1, '/gnu/store/1.drv', 2, '', '', '', '', 0, ~a, ~a, ~a);\
" yesterday (+ yesterday 1600) (+ yesterday 2600)))
      (exec-query (%db) (format #f "\
INSERT INTO Builds (id, derivation, evaluation, job_name, system,
nix_name, log, status, timestamp, starttime, stoptime) VALUES
(2, '/gnu/store/2.drv', 2, '', '', '', '', -2, 0, 0, 0);"))
      (exec-query (%db) (format #f "\
INSERT INTO Builds (id, derivation, evaluation, job_name, system,
nix_name, log, status, timestamp, starttime, stoptime) VALUES
(3, '/gnu/store/3.drv', 4, '', '', '', '', 0, 1600174451, 1600174451,
 1600174651);"))))

  (test-equal "average-eval-duration-per-spec"
    `(("guix" . 350.0))
    (begin
      (db-update-metric 'average-eval-duration-per-spec "guix")
      (db-get-metrics-with-id 'average-eval-duration-per-spec)))

    (test-equal "builds-per-day"
      1.0
      (begin
        (db-update-metric 'builds-per-day)
        (db-get-metric 'builds-per-day yesterday)))

    (test-equal "pending-builds"
      `((,today . 1.0))
      (begin
        (db-update-metric 'pending-builds)
        (db-get-metrics-with-id 'pending-builds)))

    (test-equal "new-derivations-per-day"
      `((,yesterday . 1.0))
      (begin
        (db-update-metric 'new-derivations-per-day)
        (db-get-metrics-with-id 'new-derivations-per-day)))

    (test-equal "percentage-failed-eval-per-spec"
      `(("guix" . 50.0))
      (begin
        (db-update-metric 'percentage-failed-eval-per-spec "guix")
        (db-get-metrics-with-id 'percentage-failed-eval-per-spec)))

    (test-equal "db-update-metrics"
      `((,today . 2.0))
      (begin
        (exec-query (%db) (format #f "\
INSERT INTO Builds (id, derivation, evaluation, job_name, system,
nix_name, log, status, timestamp, starttime, stoptime) VALUES
(4, '/gnu/store/4.drv', 1, '', '', '', '', -2, 0, 0, 0);"))
        (db-update-metrics)
        (db-get-metrics-with-id 'pending-builds)))

    (test-equal "average-eval-build-start-time"
      `((2 . 1000.0))
      (begin
        (db-update-metric 'average-eval-build-start-time 2)
        (db-get-metrics-with-id 'average-eval-build-start-time)))

    (test-equal "average-eval-build-complete-time"
      `((2 . 2000.0))
      (begin
        (db-update-metric 'average-eval-build-complete-time 2)
        (db-get-metrics-with-id 'average-eval-build-complete-time)))

    (test-equal "evaluation-completion-speed"
      900.0
      (begin
        (db-update-metric 'evaluation-completion-speed 4)
        (db-get-metric 'evaluation-completion-speed 4)))

  (test-assert "db-close"
    (begin
      (exec-query (%db) (format #f "DROP OWNED BY CURRENT_USER;"))
      (db-close (%db))
      #t)))
