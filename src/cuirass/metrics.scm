;;; metrics.scm -- Compute and store metrics.
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

(define-module (cuirass metrics)
  #:use-module (cuirass database)
  #:use-module (cuirass logging)
  #:use-module (cuirass specification)
  #:use-module (guix records)
  #:use-module (squee)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 match)
  #:export (metric
            metric?
            metric-id
            metric-field-type
            metric-proc

            %metrics
            metric->type
            compute-metric

            db-get-metric
            db-get-metrics-with-id
            db-update-metric
            db-update-metrics))


;;;
;;; Metric record.
;;;

(define-record-type* <metric> metric make-metric
  metric?
  (id              metric-id)
  (compute-proc    metric-compute-proc)
  (field-type      metric-field-type
                   (default 'int))
  (field-proc      metric-field-proc
                   (default #f)))


;;;
;;; Database procedures.
;;;

(define-syntax-rule (return-exact body ...)
  (match (expect-one-row body ...)
    ((result)
     (and result (string->number result)))))

(define-syntax-rule (return-inexact body ...)
  (match (expect-one-row body ...)
    ((result)
     (and result (locale-string->inexact result)))))

(define* (db-average-eval-duration-per-spec spec #:key limit)
  "Return the average evaluation duration for SPEC.  Limit the average
computation to the most recent LIMIT records if this argument is set."
  (with-db-worker-thread db
    (let ((query "\
SELECT AVG(m.duration) FROM
(SELECT (evaltime - timestamp) as duration
FROM Evaluations WHERE specification = :spec
AND evaltime != 0 ORDER BY id DESC LIMIT ~a) m;")
          (params `((#:spec . ,spec))))
      (return-inexact
       (exec-query/bind-params db
                               (format #f query
                                       (if limit
                                           (number->string limit)
                                           "ALL"))
                               params)))))

(define (db-builds-previous-day _)
  "Return the builds count of the previous day."
  (with-db-worker-thread db
    (return-exact
     (exec-query/bind db "SELECT COUNT(*) from Builds
WHERE to_timestamp(timestamp)::date = 'yesterday'::date AND
to_timestamp(stoptime)::date = 'yesterday'::date;"))))

(define (db-new-derivations-previous-day _)
  "Return the new derivations count of the previous day."
  (with-db-worker-thread db
    (return-exact
     (exec-query/bind db "SELECT COUNT(*) from Builds
WHERE to_timestamp(timestamp)::date = 'yesterday'::date;"))))

(define (db-pending-builds _)
  "Return the current pending builds count."
  (with-db-worker-thread db
    (return-exact
     (exec-query/bind db "SELECT COUNT(*) from Builds
WHERE status < 0;"))))

(define* (db-percentage-failed-eval-per-spec spec #:key limit)
  "Return the failed evaluation percentage for SPEC.  If LIMIT is set, limit
the percentage computation to the most recent LIMIT records."
  (with-db-worker-thread db
    (let ((query "\
SELECT 100 *
CAST(SUM(CASE WHEN m.status > 0 THEN 1 ELSE 0 END) as float) /
COUNT(*) FROM
(SELECT status from Evaluations WHERE specification = :spec
ORDER BY id DESC LIMIT ~a) m")
          (params `((#:spec . ,spec))))
      (return-inexact
       (exec-query/bind-params db
                               (format #f query
                                       (if limit
                                           (number->string limit)
                                           "ALL"))
                               params)))))

(define* (db-average-build-start-time-per-eval eval)
  "Return the average build start time for the given EVAL."
  (with-db-worker-thread db
    (return-inexact
     (exec-query/bind db "\
SELECT AVG(B.starttime - E.evaltime) FROM
(SELECT id, evaltime
FROM Evaluations WHERE id = " eval ") E
LEFT JOIN Builds as B
ON E.id = B.evaluation and B.starttime > 0
GROUP BY E.id;"))))

(define* (db-average-build-complete-time-per-eval eval)
  "Return the average build complete time for the given EVAL."
  (with-db-worker-thread db
    (return-inexact
     (exec-query/bind db "\
SELECT AVG(B.stoptime - E.evaltime) FROM
(SELECT id, evaltime
FROM Evaluations WHERE id = " eval ") E
LEFT JOIN Builds as B
ON E.id = B.evaluation and B.stoptime > 0
GROUP BY E.id;"))))

(define* (db-evaluation-completion-speed eval)
  "Return the evaluation completion speed of the given EVAL. The speed is
expressed in builds per hour."
  ;; completion_speed = 60 * completed_builds / eval_duration.
  ;;
  ;; evaluation_duration (seconds) = current_time - eval_start_time
  ;; If some evaluations builds are not completed.
  ;;
  ;; evaluation_duration (seconds) = max(build_stop_time) - eval_start_time
  ;; If the evaluation builds are all completed.
  (with-db-worker-thread db
    (return-inexact
     (exec-query/bind db "\
SELECT
3600.0 * SUM(CASE WHEN B.status = 0 THEN 1 ELSE 0 END) /
(CASE SUM(CASE WHEN status < 0 THEN 1 ELSE 0 END)
   WHEN 0 THEN MAX(stoptime)
   ELSE extract(epoch from 'today'::date)
END - E.evaltime) FROM
(SELECT id, evaltime
FROM Evaluations WHERE id = " eval ") E
LEFT JOIN Builds as B
ON E.id = B.evaluation and B.stoptime > 0
GROUP BY E.id, E.evaltime;"))))

(define (db-previous-day-timestamp)
  "Return the timestamp of the previous day."
  (with-db-worker-thread db
    (return-exact
     (exec-query
      db "SELECT extract(epoch from 'yesterday'::date);"))))

(define (db-current-day-timestamp)
  "Return the timestamp of the current day."
  (with-db-worker-thread db
    (return-exact
     (exec-query
      db "SELECT extract(epoch from 'today'::date);"))))

(define* (db-latest-evaluations #:key (days 3))
  "Return the successful evaluations added during the previous DAYS."
  (with-db-worker-thread db
    (let ((query (format #f "SELECT id from Evaluations
WHERE to_timestamp(timestamp)::date > 'today'::date - interval '~a day' AND
status = 0 ORDER BY id DESC" days)))
      (let loop ((rows (exec-query db query))
                 (evaluations '()))
        (match rows
          (() (reverse evaluations))
          (((id) . rest)
           (loop rest
                 (cons id evaluations))))))))


;;;
;;; Definitions.
;;;

;; XXX: Make sure to add new metrics at the *end of the list* only, as they
;; are indexed by position in database.
(define %metrics
  (list
   ;; Average evaluation duration per specification.
   (metric
    (id 'average-10-last-eval-duration-per-spec)
    (field-type 'string)
    (compute-proc
     (cut db-average-eval-duration-per-spec <> #:limit 10)))

   (metric
    (id 'average-100-last-eval-duration-per-spec)
    (field-type 'string)
    (compute-proc
     (cut db-average-eval-duration-per-spec <> #:limit 100)))

   (metric
    (id 'average-eval-duration-per-spec)
    (field-type 'string)
    (compute-proc db-average-eval-duration-per-spec))

   ;; Builds count per day.
   (metric
    (id 'builds-per-day)
    (compute-proc db-builds-previous-day)
    (field-proc db-previous-day-timestamp))

   ;; Pending builds count.
   (metric
    (id 'pending-builds)
    (compute-proc db-pending-builds)
    (field-proc db-current-day-timestamp))

   ;; New derivations per day.
   (metric
    (id 'new-derivations-per-day)
    (compute-proc db-new-derivations-previous-day)
    (field-proc db-previous-day-timestamp))

   ;; Percentage of failed evaluations per specification.
   (metric
    (id 'percentage-failure-10-last-eval-per-spec)
    (field-type 'string)
    (compute-proc
     (cut db-percentage-failed-eval-per-spec <> #:limit 10)))

   (metric
    (id 'percentage-failure-100-last-eval-per-spec)
    (field-type 'string)
    (compute-proc
     (cut db-percentage-failed-eval-per-spec <> #:limit 100)))

   (metric
    (id 'percentage-failed-eval-per-spec)
    (field-type 'string)
    (compute-proc db-percentage-failed-eval-per-spec))

   ;; Average time to start a build for an evaluation.
   (metric
    (id 'average-eval-build-start-time)
    (compute-proc db-average-build-start-time-per-eval))

   ;; Average time to complete a build for an evaluation.
   (metric
    (id 'average-eval-build-complete-time)
    (compute-proc db-average-build-complete-time-per-eval))

   ;; Evaluation completion speed in builds/hour.
   (metric
    (id 'evaluation-completion-speed)
    (compute-proc db-evaluation-completion-speed))))

(define (metric->type metric)
  "Return the index of the given METRIC in %metrics list.  This index is used
to identify the metric type in database."
  (list-index
   (lambda (cur-metric)
     (eq? (metric-id cur-metric) (metric-id metric)))
   %metrics))

(define (find-metric id)
  "Find the metric with the given ID."
  (find (lambda (metric)
          (eq? (metric-id metric) id))
        %metrics))

(define* (compute-metric metric field)
  "Compute the given METRIC on FIELD and return the associated value."
  (let ((compute (metric-compute-proc metric)))
    (compute field)))

(define* (db-get-metric id field)
  "Return the metric with the given ID and FIELD."
  (with-db-worker-thread db
    (let* ((metric (find-metric id))
           (type (metric->type metric)))
      (return-inexact
       (exec-query/bind db "SELECT value from Metrics
WHERE type = " type " AND field = " field ";")))))

(define* (db-get-metrics-with-id id
                                 #:key
                                 limit
                                 (order "id DESC"))
  "Return the metrics with the given ID.  If LIMIT is set, the resulting list
if restricted to LIMIT records."
  (with-db-worker-thread db
    (let* ((metric (find-metric id))
           (type (metric->type metric))
           (field-type (metric-field-type metric))
           (limit (or limit "ALL")))
      (let ((query (format #f "SELECT field, value from Metrics
WHERE type = :type ORDER BY ~a LIMIT ~a" order limit))
            (params `((#:type . ,type))))
        (let loop ((rows (exec-query/bind-params db query params))
                   (metrics '()))
          (match rows
            (() (reverse metrics))
            (((field value) . rest)
             (let ((field (match field-type
                            ('int (string->number field))
                            (else field))))
               (loop rest
                     `((,field . ,(locale-string->inexact value))
                       ,@metrics))))))))))

(define* (db-update-metric id #:optional field)
  "Compute and update the value of the metric ID in database.

  FIELD is optional and can be the id of a database object such as an
evaluation or a specification that the METRIC applies to.  If FIELD is not
passed then the METRIC may provide a FIELD-PROC to compute it.  It is useful
for periodical metrics for instance."
  (define now
    (time-second (current-time time-utc)))

  (with-db-worker-thread db
    (let* ((metric (find-metric id))
           (field-proc (metric-field-proc metric))
           (field (or field (field-proc)))
           (value (compute-metric metric field)))
      (if value
          (begin
            (log-message "Updating metric ~a (~a) to ~a."
                         (symbol->string id) field value)
            (exec-query/bind db "\
INSERT INTO Metrics (field, type, value,
timestamp) VALUES ("
                             field ", "
                             (metric->type metric) ", "
                             value ", "
                             now ")
ON CONFLICT ON CONSTRAINT metrics_pkey DO
UPDATE SET value = " value ", timestamp = " now ";"))
          (log-message "Failed to compute metric ~a (~a)."
                       (symbol->string id) field)))))

(define (db-update-metrics)
  "Compute and update all available metrics in database."
  ;; We can not update all evaluations metrics for performance reasons.
  ;; Limit to the evaluations that were added during the past three days.
  (with-db-worker-thread db
    (let ((specifications
           (map specification-name (db-get-specifications)))
          (evaluations (db-latest-evaluations)))
      (exec-query db "BEGIN TRANSACTION;")

      (db-update-metric 'builds-per-day)
      (db-update-metric 'new-derivations-per-day)
      (db-update-metric 'pending-builds)

      ;; Update specification related metrics.
      (for-each (lambda (spec)
                  (db-update-metric
                   'average-10-last-eval-duration-per-spec spec)
                  (db-update-metric
                   'average-100-last-eval-duration-per-spec spec)
                  (db-update-metric
                   'average-eval-duration-per-spec spec)

                  (db-update-metric
                   'percentage-failure-10-last-eval-per-spec spec)
                  (db-update-metric
                   'percentage-failure-100-last-eval-per-spec spec)
                  (db-update-metric
                   'percentage-failed-eval-per-spec spec))
                specifications)

      ;; Update evaluation related metrics.
      (for-each (lambda (evaluation)
                  (db-update-metric
                   'average-eval-build-start-time evaluation)
                  (db-update-metric
                   'average-eval-build-complete-time evaluation)
                  (db-update-metric
                   'evaluation-completion-speed evaluation))
                evaluations)

      (exec-query db "COMMIT;"))))
