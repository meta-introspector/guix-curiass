;;; metrics.scm -- Compute and store metrics.
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
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (metric
            metric?
            metric-id
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
  (field-proc      metric-field-proc
                   (default #f)))


;;;
;;; Database procedures.
;;;

(define* (db-average-eval-duration-per-spec spec #:key limit)
  "Return the evaluation duration of EVAL."
  (with-db-worker-thread db
    (let ((rows (sqlite-exec db "SELECT AVG(duration) FROM
(SELECT (evaltime - timestamp) as duration
FROM Evaluations WHERE specification = " spec
" AND evaltime != 0 LIMIT " (or limit -1) ");")))
      (and=> (expect-one-row rows) (cut vector-ref <> 0)))))

(define (db-builds-previous-day _)
  "Return the builds count of the previous day."
  (with-db-worker-thread db
    (let ((rows (sqlite-exec db "SELECT COUNT(*) from Builds
WHERE date(stoptime, 'unixepoch') = date('now', '-1 day');")))
      (and=> (expect-one-row rows) (cut vector-ref <> 0)))))

(define (db-pending-builds _)
  "Return the current pending builds count."
  (with-db-worker-thread db
    (let ((rows (sqlite-exec db "SELECT COUNT(*) from Builds
WHERE status < 0;")))
      (and=> (expect-one-row rows) (cut vector-ref <> 0)))))

(define (db-previous-day-timestamp)
  "Return the timestamp of the previous day."
  (with-db-worker-thread db
    (let ((rows (sqlite-exec db "SELECT strftime('%s',
date('now', '-1 day'));")))
      (and=> (expect-one-row rows) (cut vector-ref <> 0)))))

(define (db-current-day-timestamp)
  "Return the timestamp of the current day."
  (with-db-worker-thread db
    (let ((rows (sqlite-exec db "SELECT strftime('%s',
date('now'));")))
      (and=> (expect-one-row rows) (cut vector-ref <> 0)))))


;;;
;;; Definitions.
;;;

(define %metrics
  (list
   ;; Average evaluation duration per specification.
   (metric
    (id 'average-10-last-eval-duration-per-spec)
    (compute-proc
     (cut db-average-eval-duration-per-spec <> #:limit 10)))
   (metric
    (id 'average-100-last-eval-duration-per-spec)
    (compute-proc
     (cut db-average-eval-duration-per-spec <> #:limit 100)))
   (metric
    (id 'average-eval-duration-per-spec)
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
    (field-proc db-current-day-timestamp))))

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
  (let* ((metric (find-metric id))
         (type (metric->type metric)))
    (with-db-worker-thread db
    (let ((rows (sqlite-exec db "SELECT value from Metrics
WHERE type = " type " AND field = " field ";")))
      (and=> (expect-one-row rows) (cut vector-ref <> 0))))))

(define* (db-get-metrics-with-id id
                                 #:key
                                 limit
                                 (order "rowid DESC"))
  "Return the metrics with the given ID.  If LIMIT is set, the resulting list
if restricted to LIMIT records."
  (let* ((metric (find-metric id))
         (type (metric->type metric))
         (limit (or limit -1)))
    (with-db-worker-thread db
      (let ((query (format #f "SELECT field, value from Metrics
WHERE type = ? ORDER BY ~a LIMIT ~a" order limit)))
        (let loop ((rows (%sqlite-exec db query type))
                   (metrics '()))
          (match rows
            (() (reverse metrics))
            ((#(field value) . rest)
             (loop rest
                   `((,field . ,value)
                     ,@metrics)))))))))

(define* (db-update-metric id #:optional field)
  "Compute and update the value of the metric ID in database.

  FIELD is optional and can be the id of a database object such as an
evaluation or a specification that the METRIC applies to.  If FIELD is not
passed then the METRIC may provide a FIELD-PROC to compute it.  It is useful
for periodical metrics for instance."
  (define now
    (time-second (current-time time-utc)))

  (let* ((metric (find-metric id))
         (field-proc (metric-field-proc metric))
         (field (or field (field-proc)))
         (value (compute-metric metric field)))
    (if value
        (begin
          (log-message "Updating metric ~a (~a) to ~a."
                       (symbol->string id) field value)
          (with-db-worker-thread db
            (sqlite-exec db "\
INSERT OR REPLACE INTO Metrics (field, type, value,
timestamp) VALUES ("
                         field ", "
                         (metric->type metric) ", "
                         value ", "
                         now ");")
            (last-insert-rowid db)))
        (log-message "Failed to compute metric ~a (~a)."
                     (symbol->string id) field))))

(define (db-update-metrics)
  "Compute and update all available metrics in database."
  (define specifications
    (map (cut assq-ref <> #:name) (db-get-specifications)))

  (db-update-metric 'builds-per-day)
  (db-update-metric 'pending-builds)

  ;; Update specification related metrics.
  (for-each (lambda (spec)
              (db-update-metric
               'average-10-last-eval-duration-per-spec spec)
              (db-update-metric
               'average-100-last-eval-duration-per-spec spec)
              (db-update-metric
               'average-eval-duration-per-spec spec))
            specifications))
