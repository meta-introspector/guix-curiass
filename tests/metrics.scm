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
             (srfi srfi-64))

(define-syntax-rule (with-temporary-database body ...)
  (call-with-temporary-output-file
   (lambda (file port)
     (parameterize ((%package-database file))
       (db-init file)
       (with-database
         body ...)))))

(define today
  (let ((time (current-time)))
    (- time (modulo time 86400))))

(define yesterday
  (- today 86400))

(define %db
  ;; Global Slot for a database object.
  (make-parameter #t))

(define database-name
  ;; Use an empty and temporary database for the tests.
  (string-append (getcwd) "/" (number->string (getpid)) "-tmp.db"))

(test-group-with-cleanup "database"
  (test-assert "db-init"
    (begin
      (%db (db-init database-name))
      (%db-channel (make-worker-thread-channel
                    (lambda ()
                      (list (%db)))))
      #t))

  (test-assert "sqlite-exec"
    (begin
      (sqlite-exec (%db) "\
INSERT INTO Evaluations (specification, status,
timestamp, checkouttime, evaltime) VALUES ('guix', -1, 1600174547, 0, 0);")
      (sqlite-exec (%db) "\
INSERT INTO Evaluations (specification, status,
timestamp, checkouttime, evaltime) VALUES ('guix', 0, 1600174547, 1600174548,
1600260947);")
      (sqlite-exec (%db) "\
INSERT INTO Evaluations (specification, status,
timestamp, checkouttime, evaltime) VALUES ('guix', 1, 1600174547,
1600174548, 0);")
      (sqlite-exec (%db) "\
INSERT INTO Evaluations (specification, status,
timestamp, checkouttime, evaltime) VALUES ('guix', 2, 1600174547,
1600174548, 0);")
      (sqlite-exec (%db) (format #f "\
INSERT INTO Builds (id, derivation, evaluation, job_name, system,
nix_name, log, status, timestamp, starttime, stoptime) VALUES
(1, '/gnu/store/xxx.drv', 1, '', '', '', '', 0, ~a, ~a, ~a);\
" yesterday (+ yesterday 500) (+ yesterday 1500)))
      (sqlite-exec (%db) (format #f "\
INSERT INTO Builds (id, derivation, evaluation, job_name, system,
nix_name, log, status, timestamp, starttime, stoptime) VALUES
(2, '/gnu/store/yyy.drv', 1, '', '', '', '', -2, 0, 0, 0);"))))

  (test-equal "average-eval-duration-per-spec"
    `(("guix" . 86400.0))
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
      (sqlite-exec (%db) (format #f "\
INSERT INTO Builds (id, derivation, evaluation, job_name, system,
nix_name, log, status, timestamp, starttime, stoptime) VALUES
(3, '/gnu/store/zzz.drv', 1, '', '', '', '', -2, 0, 0, 0);"))
      (db-update-metrics)
      (db-get-metrics-with-id 'pending-builds)))

  (test-assert "db-close"
    (db-close (%db)))

  (begin
    (%db-channel #f)
    (delete-file database-name)))

;;; Local Variables:
;;; eval: (put 'with-temporary-database 'scheme-indent-function 0)
;;; End:
