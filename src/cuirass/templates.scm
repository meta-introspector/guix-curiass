;;; templates.scm -- HTTP API
;;; Copyright © 2018 Tatiana Sholokhova <tanja201396@gmail.com>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (cuirass templates)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module ((cuirass database) #:select (build-status))
  #:export (html-page
            specifications-table
            evaluation-info-table
            build-eval-table
            build-search-results-table))

(define (navigation-items navigation)
  (match navigation
    (() '())
    ((item . rest)
     (cons `(li (@ (class "nav-item"))
                (a (@ (class "nav-link" ,(if (null? rest) " active" ""))
                      (href ,(assq-ref item #:link)))
                   ,(assq-ref item #:name)))
           (navigation-items rest)))))

(define search-form
  `(form (@ (id "search")
            (class "form-inline")
            (action "/search"))
         (div
          (@ (class "input-group"))
          (input (@ (type "text")
                    (class "form-control")
                    (id   "query")
                    (name "query")
                    (placeholder "search for builds")))
          (span (@ (class "input-group-append"))
                (button
                 (@ (type "submit")
                    (class "btn btn-primary"))
                 "Search")))))

(define (html-page title body navigation)
  "Return HTML page with given TITLE and BODY."
  `(html (@ (xmlns "http://www.w3.org/1999/xhtml")
            (xml:lang "en")
            (lang "en"))
         (head
          (meta (@ (charset "utf-8")))
          (meta (@ (name "viewport")
                   (content ,(string-join '("width=device-width"
                                            "initial-scale=1"
                                            "shrink-to-fit=no")
                                          ", "))))
          (link (@ (rel "stylesheet")
                   (href "/static/css/bootstrap.css")))
          (link (@ (rel "stylesheet")
                   (href "/static/css/open-iconic-bootstrap.css")))
          (title ,title))
         (body
          (nav (@ (class "navbar navbar-expand navbar-light bg-light"))
               (a (@ (class "navbar-brand pt-0")
                     (href "/"))
                  (img (@ (src "/static/images/logo.png")
                          (alt "logo")
                          (height "25")
                          (style "margin-top: -12px"))))
               (div (@ (class "navbar-collapse"))
                         (ul (@ (class "navbar-nav"))
                             (li (@ (class "nav-item"))
                                 (a (@ (class "nav-link" ,(if (null? navigation)
                                                              " active" ""))
                                       (href "/"))
                                    Home))
                             ,@(navigation-items navigation)))
               ,search-form)
          (main (@ (role "main") (class "container pt-4 px-1"))
                ,body
                (hr)))))

(define (specifications-table specs)
  "Return HTML for the SPECS table."
  `((p (@ (class "lead")) "Specifications")
    (table
     (@ (class "table table-sm table-hover"))
     ,@(if (null? specs)
           `((th (@ (scope "col")) "No elements here."))
           `((thead (tr (th (@ (scope "col")) Name)
                        (th (@ (scope "col")) Inputs)))
             (tbody
              ,@(map
                 (lambda (spec)
                   `(tr (td (a (@ (href "/jobset/" ,(assq-ref spec #:name)))
                               ,(assq-ref spec #:name)))
                        (td ,(string-join
                              (map (lambda (input)
                                     (format #f "~a (on ~a)"
                                             (assq-ref input #:name)
                                             (assq-ref input #:branch)))
                                   (assq-ref spec #:inputs)) ", "))))
                 specs)))))))

(define (pagination first-link prev-link next-link last-link)
  "Return html page navigation buttons with LINKS."
  `(div (@ (class row))
        (nav
         (@ (class "mx-auto") (aria-label "Page navigation"))
         (ul (@ (class "pagination"))
             (li (@ (class "page-item"))
                 (a (@ (class "page-link")
                       (href ,first-link))
                    "<< First"))
             (li (@ (class "page-item"
                      ,(if (string-null? prev-link) " disabled")))
                 (a (@ (class "page-link")
                       (href ,prev-link))
                    "< Previous"))
             (li (@ (class "page-item"
                      ,(if (string-null? next-link) " disabled")))
                 (a (@ (class "page-link")
                       (href ,next-link))
                    "Next >"))
             (li (@ (class "page-item"))
                 (a (@ (class "page-link")
                       (href ,last-link))
                    "Last >>"))))))

(define (input-changes checkouts)
  (let ((changes
         (string-join
          (map (lambda (checkout)
                 (let ((input (assq-ref checkout #:input))
                       (commit (assq-ref checkout #:commit)))
                   (format #f "~a → ~a" input (substring commit 0 7))))
               checkouts)
          ", ")))
    (if (string=? changes "") '(em "None") changes)))

(define (evaluation-badges evaluation)
  (if (zero? (assq-ref evaluation #:in-progress))
      (let ((succeeded (assq-ref evaluation #:succeeded))
            (failed    (assq-ref evaluation #:failed))
            (scheduled (assq-ref evaluation #:scheduled)))
        ;; XXX: Since we don't have information in the database about whether
        ;; an evaluation failed, assume that it failed when it produced zero
        ;; build jobs.
        (if (zero? (+ succeeded failed scheduled))
            `((span (@ (class "oi oi-x text-danger")
                       (title "Failed")
                       (aria-hidden "true"))
                    ""))
            `((a (@ (href "/eval/" ,(assq-ref evaluation #:id) "?status=succeeded")
                    (class "badge badge-success"))
                 ,succeeded)
              (a (@ (href "/eval/" ,(assq-ref evaluation #:id) "?status=failed")
                    (class "badge badge-danger"))
                 ,failed)
              (a (@ (href "/eval/" ,(assq-ref evaluation #:id) "?status=pending")
                    (class "badge badge-secondary"))
                 ,scheduled))))
      '((em "In progress…"))))

(define (evaluation-info-table name evaluations id-min id-max)
  "Return HTML for the EVALUATION table NAME. ID-MIN and ID-MAX are
  global minimal and maximal id."
  `((p (@ (class "lead")) "Evaluations of " ,name)
    (table
     (@ (class "table table-sm table-hover table-striped"))
     ,@(if (null? evaluations)
           `((th (@ (scope "col")) "No elements here."))
           `((thead
              (tr
               (th (@ (scope "col")) "#")
               (th (@ (scope "col")) "Input changes")
               (th (@ (scope "col")) Success)))
             (tbody
              ,@(map
                 (lambda (row)
                   `(tr (th (@ (scope "row"))
                            (a (@ (href "/eval/" ,(assq-ref row #:id)))
                               ,(assq-ref row #:id)))
                        (td ,(input-changes (assq-ref row #:checkouts)))
                        (td ,@(evaluation-badges row))))
                 evaluations)))))
    ,(if (null? evaluations)
         (pagination "" "" "" "")
         (let* ((eval-ids (map (cut assq-ref <> #:id) evaluations))
                (page-id-min (last eval-ids))
                (page-id-max (first eval-ids)))
           (pagination
            (format #f "?border-high=~d" (1+ id-max))
            (if (= page-id-max id-max)
                ""
                (format #f "?border-low=~d" page-id-max))
            (if (= page-id-min id-min)
                ""
                (format #f "?border-high=~d" page-id-min))
            (format #f "?border-low=~d" (1- id-min)))))))

(define (time->string time)
  "Return a string representing TIME in a concise, human-readable way."
  (define now*
    (current-time time-utc))

  (define now
    (time-second now*))

  (define elapsed
    (- now time))

  (cond ((< elapsed 120)
         "seconds ago")
        ((< elapsed 7200)
         (let ((minutes (inexact->exact
                         (round (/ elapsed 60)))))
           (format #f "~a minutes ago" minutes)))
        ((< elapsed (* 48 3600))
         (let ((hours (inexact->exact
                       (round (/ elapsed 3600)))))
           (format #f "~a hours ago" hours)))
        (else
         (let* ((time    (make-time time-utc 0 time))
                (date    (time-utc->date time))
                (year    (date-year date))
                (current (date-year (time-utc->date now*)))
                (format  (if (= year )
                             "~e ~b ~H:~M ~z"
                             "~e ~b ~Y ~H:~M ~z")))
           (date->string date format)))))

(define (build-eval-table eval-id builds build-min build-max status)
  "Return HTML for the BUILDS table evaluation with given STATUS.  BUILD-MIN
and BUILD-MAX are global minimal and maximal (stoptime, rowid) pairs."
  (define (table-header)
    `(thead
      (tr
       (th (@ (scope "col")) '())
       (th (@ (scope "col")) "ID")
       (th (@ (scope "col")) "Specification")
       (th (@ (scope "col")) "Completion time")
       (th (@ (scope "col")) "Job")
       (th (@ (scope "col")) "Name")
       (th (@ (scope "col")) "System")
       (th (@ (scope "col")) "Log"))))

  (define (table-row build)
    (define status
      (assq-ref build #:buildstatus))

    (define completed?
      (or (= (build-status succeeded) status)
          (= (build-status failed) status)))

    `(tr
      (td ,(cond
            ((= (build-status succeeded) status)
             `(span (@ (class "oi oi-check text-success")
                       (title "Succeeded")
                       (aria-hidden "true"))
                    ""))
            ((= (build-status scheduled) status)
             `(span (@ (class "oi oi-clock text-warning")
                       (title "Scheduled")
                       (aria-hidden "true"))
                    ""))
            ((= (build-status canceled) status)
             `(span (@ (class "oi oi-question-mark text-warning")
                       (title "Canceled")
                       (aria-hidden "true"))
                    ""))
            ((= (build-status failed-dependency) status)
             `(span (@ (class "oi oi-warning text-danger")
                       (title "Dependency failed")
                       (aria-hidden "true"))
                    ""))
            (else
             `(span (@ (class "oi oi-x text-danger")
                       (title "Failed")
                       (aria-hidden "true"))
                    ""))))
      (th (@ (scope "row")),(assq-ref build #:id))
      (td ,(assq-ref build #:jobset))
      (td ,(if completed?
               (time->string (assq-ref build #:stoptime))
               "—"))
      (td ,(assq-ref build #:job))
      (td ,(assq-ref build #:nixname))
      (td ,(assq-ref build #:system))
      (td ,(if completed?
               `(a (@ (href "/build/" ,(assq-ref build #:id) "/log/raw"))
                   "raw")
               "—"))))

  (define (build-id build)
    (match build
      ((stoptime id) id)))

  (define (build-stoptime build)
    (match build
      ((stoptime id) stoptime)))

  `((p (@ (class "lead"))
       ,(format #f "~@[~a~] ~:[B~;b~]uilds of evaluation #~a"
                (and=> status string-capitalize)
                status
                eval-id))
    (table
     (@ (class "table table-sm table-hover table-striped"))
     ,@(if (null? builds)
           `((th (@ (scope "col")) "No elements here."))
           `(,(table-header)
             (tbody ,@(map table-row builds)))))
    ,(if (null? builds)
         (pagination "" "" "" "")
         (let* ((build-time-ids (map (lambda (row)
                                       (list (assq-ref row #:stoptime)
                                             (assq-ref row #:id)))
                                     builds))
                (page-build-min (last build-time-ids))
                (page-build-max (first build-time-ids)))
           (pagination
            (format
             #f "?border-high-time=~d&border-high-id=~d~@[&status=~a~]"
             (build-stoptime build-max)
             (1+ (build-id build-max))
             status)
            (if (equal? page-build-max build-max)
                ""
                (format
                 #f "?border-low-time=~d&border-low-id=~d~@[&status=~a~]"
                 (build-stoptime page-build-max)
                 (build-id page-build-max)
                 status))
            (if (equal? page-build-min build-min)
                ""
                (format
                 #f "?border-high-time=~d&border-high-id=~d~@[&status=~a~]"
                 (build-stoptime page-build-min)
                 (build-id page-build-min)
                 status))
            (format
             #f "?border-low-time=~d&border-low-id=~d~@[&status=~a~]"
             (build-stoptime build-min)
             (1- (build-id build-min))
             status))))))

(define (build-search-results-table query builds build-min build-max)
  "Return HTML for the BUILDS table evaluation matching QUERY.  BUILD-MIN
and BUILD-MAX are global minimal and maximal row identifiers."
  (define (table-header)
    `(thead
      (tr
       (th (@ (scope "col")) '())
       (th (@ (scope "col")) "ID")
       (th (@ (scope "col")) "Specification")
       (th (@ (scope "col")) "Completion time")
       (th (@ (scope "col")) "Job")
       (th (@ (scope "col")) "Name")
       (th (@ (scope "col")) "System")
       (th (@ (scope "col")) "Log"))))

  (define (table-row build)
    (define status
      (assq-ref build #:buildstatus))

    (define completed?
      (or (= (build-status succeeded) status)
          (= (build-status failed) status)))

    `(tr
      (td ,(cond
            ((= (build-status succeeded) status)
             `(span (@ (class "oi oi-check text-success")
                       (title "Succeeded")
                       (aria-hidden "true"))
                    ""))
            ((= (build-status scheduled) status)
             `(span (@ (class "oi oi-clock text-warning")
                       (title "Scheduled")
                       (aria-hidden "true"))
                    ""))
            ((= (build-status canceled) status)
             `(span (@ (class "oi oi-question-mark text-warning")
                       (title "Canceled")
                       (aria-hidden "true"))
                    ""))
            ((= (build-status failed-dependency) status)
             `(span (@ (class "oi oi-warning text-danger")
                       (title "Dependency failed")
                       (aria-hidden "true"))
                    ""))
            (else
             `(span (@ (class "oi oi-x text-danger")
                       (title "Failed")
                       (aria-hidden "true"))
                    ""))))
      (th (@ (scope "row")),(assq-ref build #:id))
      (td ,(assq-ref build #:jobset))
      (td ,(if completed?
               (time->string (assq-ref build #:stoptime))
               "—"))
      (td ,(assq-ref build #:job))
      (td ,(assq-ref build #:nixname))
      (td ,(assq-ref build #:system))
      (td ,(if completed?
               `(a (@ (href "/build/" ,(assq-ref build #:id) "/log/raw"))
                   "raw")
               "—"))))

  `((p (@ (class "lead"))
       ,(format #f "Builds matching ~a" query))
    (table
     (@ (class "table table-sm table-hover table-striped"))
     ,@(if (null? builds)
           `((th (@ (scope "col")) "No elements here."))
           `(,(table-header)
             (tbody ,@(map table-row builds)))))

    ,(if (null? builds)
         (pagination "" "" "" "")
         (let* ((build-ids (map (lambda (row) (assq-ref row #:id)) builds))
                (page-build-min (last build-ids))
                (page-build-max (first build-ids)))
           (pagination
            (format
             #f "?query=~a&border-high-id=~d"
             query
             (1+ (first build-max)))
            (if (equal? page-build-max (first build-max))
                ""
                (format
                 #f "?query=~a&border-low-id=~d"
                 query
                 page-build-max))
            (if (equal? page-build-min (first build-min))
                ""
                (format
                 #f "?query=~a&border-high-id=~d"
                 query
                 page-build-min))
            (format
             #f "?query=~a&border-low-id=~d"
             query
             (1- (first build-min))))))))
