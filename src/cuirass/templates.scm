;;; templates.scm -- HTTP API
;;; Copyright © 2018 Tatiana Sholokhova <tanja201396@gmail.com>
;;; Copyright © 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (web uri)
  #:use-module (guix derivations)
  #:use-module (guix progress)
  #:use-module (guix store)
  #:use-module ((guix utils) #:select (string-replace-substring))
  #:use-module ((cuirass database) #:select (build-status))
  #:export (html-page
            specifications-table
            evaluation-info-table
            build-eval-table
            build-search-results-table
            build-details
            evaluation-build-table
            running-builds-table))

(define (navigation-items navigation)
  (match navigation
    (() '())
    ((item . rest)
     (cons `(li (@ (class "nav-item"))
                (a (@ (class "nav-link" ,(if (null? rest) " active" ""))
                      (href ,(assq-ref item #:link)))
                   ,(assq-ref item #:name)))
           (navigation-items rest)))))

(define (search-form query)
  `(form (@ (id "search")
            (class "form-inline")
            (action "/search"))
         (div
          (@ (class "input-group"))
          (input (@ (type "text")
                    (class "form-control")
                    (id   "query")
                    (name "query")
                    ,(if query
                         `(value ,query)
                         '(placeholder "search for builds"))))
          (span (@ (class "input-group-append"))
                (button
                 (@ (type "submit")
                    (class "btn btn-primary"))
                 "Search")))
         (div
          (@ (id "search-hints"))
          (p "You can limit the search results with the following keywords:")
          (ul
           (li (code "spec")
               ", a " (em "specification") " such as " (code "guix-master"))
           (li (code "system")
               ", a build for the given " (em "target system")
               " such as " (code "x86_64-linux"))
           (li (code "status")
               ", to limit the results to builds with the given status.  "
               "This should be one of "
               (code "success") ", "
               (code "failed") ", "
               (code "failed-dependency") ", "
               (code "failed-other") ", or "
               (code "canceled") "."))
          (p "You can also use the anchors " (code "^") " and " (code "$") "
for matching the beginning and the end of a name, respectively.")
          (p "For example, the following query will list successful builds of
the " (code "guix-master") " specification for the " (code "i686-linux") "
system whose names start with " (code "guile-") ":" (br)
(code "spec:guix-master system:i686-linux status:success ^guile-")))))

(define* (html-page title body navigation #:optional query)
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
          (link (@ (rel "stylesheet")
                   (href "/static/css/cuirass.css")))
          (title ,title))
         (body
          (nav (@ (class "navbar navbar-expand-lg navbar-light bg-light"))
               (a (@ (class "navbar-brand pt-0")
                     (href "/"))
                  (img (@ (src "/static/images/logo.png")
                          (alt "logo")
                          (height "25")
                          (style "margin-top: -12px"))))
               (div (@ (class "collapse navbar-collapse"))
                    (ul (@ (class "navbar-nav mr-auto"))
                        (li (@ (class "nav-item dropdown"))
                            (a (@ (class "nav-link dropdown-toggle")
                                  (data-toggle "dropdown")
                                  (href "#")
                                  (role "button")
                                  (aria-haspopup "true")
                                  (aria-expanded "false"))
                               "Status")
                            (div (@ (class "dropdown-menu")
                                    (aria-labelledby "navbarDropdow"))
                                 (a (@ (class "dropdown-item")
                                       (href "/status"))
                                    "Latest builds")))
                        (li (@ (class "nav-item"))
                            (a (@ (class "nav-link" ,(if (null? navigation)
                                                         " active" ""))
                                  (href "/"))
                               Home))
                        ,@(navigation-items navigation)))
               ,(search-form query))
          (main (@ (role "main") (class "container pt-4 px-1"))
                ,body
                (hr)))))

(define (status-class status)
  (cond
    ((= (build-status scheduled)         status) "oi oi-clock         text-warning")
    ((= (build-status started)           status) "oi oi-reload        text-warning")
    ((= (build-status succeeded)         status) "oi oi-check         text-success")
    ((= (build-status failed)            status) "oi oi-x             text-danger")
    ((= (build-status failed-dependency) status) "oi oi-warning       text-danger")
    ((= (build-status failed-other)      status) "oi oi-warning       text-danger")
    ((= (build-status canceled)          status) "oi oi-question-mark text-warning")
    (else                                        "oi oi-warning       text-danger")))

(define (status-title status)
  (cond
    ((= (build-status scheduled)         status) "Scheduled")
    ((= (build-status started)           status) "Started")
    ((= (build-status succeeded)         status) "Succeeded")
    ((= (build-status failed)            status) "Failed")
    ((= (build-status failed-dependency) status) "Failed (dependency)")
    ((= (build-status failed-other)      status) "Failed (other)")
    ((= (build-status canceled)          status) "Canceled")
    (else                                        "Invalid status")))

(define* (specifications-table specs #:optional admin?)
  "Return HTML for the SPECS table."
  `((p (@ (class "lead")) "Specifications")
    (table
     (@ (class "table table-sm table-hover"))
     ,@(if (null? specs)
           `((th (@ (scope "col")) "No elements here."))
           `((thead (tr (th (@ (scope "col")) Name)
                        (th (@ (scope "col")) Inputs)
                        ,@(if admin?
                              '((th (@ (scope "col")) Action))
                              '())))
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
                                   (assq-ref spec #:inputs)) ", "))
                        ,@(if admin?
                              `((form (@ (class "form")
                                         (action ,(string-append "/admin/specifications/delete/"
                                                                 (assq-ref spec #:name)))
                                         (method "POST")
                                         (onsubmit
                                          ,(string-append "return confirm('Please confirm deletion of specification "
                                                          (assq-ref spec #:name)
                                                          ".');")))
                                      `((div
                                         (@ (class "input-group"))
                                         (span (@ (class "input-group-append"))
                                               (button
                                                (@ (type "submit")
                                                   (class "btn"))
                                                "Remove"))))))
                              '())))
                 specs))))
     ,@(if admin?
           `((form (@ (id "add-specification")
                       (class "form")
                       (action "/admin/specifications/add/")
                       (method "POST"))
                    (div
                     (@ (class "input-group"))
                     (input (@ (type "text")
                               (class "form-control")
                               (id   "spec-name")
                               (name "spec-name")
                               (placeholder "specification / branch name")))
                     (span (@ (class "input-group-append"))
                           (button
                            (@ (type "submit")
                               (class "btn btn-primary"))
                            "Add")))))
           '()))))

(define (build-details build products)
  "Return HTML showing details for the BUILD."
  (define status (assq-ref build #:status))
  (define blocking-outputs
    (or (and-let* (((= (build-status failed-dependency) status))
                   (drv (false-if-exception
                         (read-derivation-from-file
                          (assq-ref build #:derivation)))))
          (append-map (lambda (drv)
                        (match (derivation->output-paths drv)
                          (((_ . items) ...)
                           items)))
                      (filter (compose derivation-log-file
                                       derivation-file-name)
                              (with-store store
                                (derivation-build-plan
                                 store (list (derivation-input drv))
                                 #:substitutable-info (const #f))))))
        '()))

  (define completed?
    (or (= (build-status succeeded) status)
        (= (build-status failed) status)))

  (define evaluation
    (assq-ref build #:eval-id))

  `((p (@ (class "lead")) "Build details")
    (table
     (@ (class "table table-sm table-hover"))
     (tbody
      (tr (th "Build ID")
          (td ,(assq-ref build #:id)))
      (tr (th "Evaluation")
          (td (a (@ (href ,(string-append "/eval/"
                                          (number->string evaluation))))
                 ,(number->string evaluation))))
      (tr (th "Status")
          (td (span (@ (class ,(status-class status))
                       (title ,(status-title status)))
                ,(string-append " " (status-title status)))
              ,@(map (lambda (output)
                       `((br)
                         (a (@ (href ,(string-append "/log/" (basename output))))
                            ,output)))
                     blocking-outputs)))
      (tr (th "System")
          (td ,(assq-ref build #:system)))
      (tr (th "Name")
          (td ,(assq-ref build #:nix-name)))
      (tr (th "Duration")
          (td ,(or (and-let* ((start (assq-ref build #:starttime))
                              (stop  (assq-ref build #:stoptime)))
                     (string-append (number->string (- stop start))
                                    " seconds"))
                   "—")))
      (tr (th "Finished")
          (td ,(if completed?
                   (time->string (assq-ref build #:stoptime))
                   "—")))
      (tr (th "Log file")
          (td ,(if completed?
               `(a (@ (href "/build/" ,(assq-ref build #:id) "/log/raw"))
                   "raw")
               "—")))
      (tr (th "Derivation")
          (td (pre ,(assq-ref build #:derivation))))
      (tr (th "Outputs")
          (td ,(map (match-lambda ((out (#:path . path))
                                   `(pre ,path)))
                    (assq-ref build #:outputs))))
      ,@(if (null? products)
            '()
            (let ((product-items
                   (map
                    (lambda (product)
                      (let* ((id (assq-ref product #:id))
                             (size (assq-ref product #:file-size))
                             (type (assq-ref product #:type))
                             (path (assq-ref product #:path))
                             (href (format #f "/download/~a" id)))
                        `(a (@ (href ,href))
                            (li (@ (class "list-group-item"))
                                (div
                                 (@ (class "container"))
                                 (div
                                  (@ (class "row"))
                                  (div
                                   (@ (class "col-md-auto"))
                                   (span
                                    (@ (class "oi oi-data-transfer-download")
                                       (title "Download")
                                       (aria-hidden "true"))))
                                  (div (@ (class "col-md-auto"))
                                       ,path)
                                  (div (@ (class "col-md-auto"))
                                       "(" ,type ")")
                                  (div (@ (class "col-md-auto"))
                                   ,(byte-count->string size))))))))
                    products)))
              `((tr (th "Build outputs")
                    (td
                     (ul (@ (class "list-group d-flex flex-row"))
                         ,product-items))))))))))

(define (pagination first-link prev-link next-link last-link)
  "Return html page navigation buttons with LINKS."
  `(div (@ (class row))
        (nav
         (@ (class "mx-auto") (aria-label "Page navigation"))
         (ul (@ (class "pagination"))
             (li (@ (class "page-item"
                      ,(if (string-null? prev-link) " disabled")))
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
             (li (@ (class "page-item"
                      ,(if (string-null? next-link) " disabled")))
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
            `((a (@ (href "/eval/" ,(assq-ref evaluation #:id) "/log/raw")
                    (class "oi oi-x text-danger")
                    (title "Failed")
                    (aria-hidden "true"))
                 ""))
            `((a (@ (href "/eval/" ,(assq-ref evaluation #:id) "?status=succeeded")
                    (class "badge badge-success")
                    (title "Succeeded"))
                 ,succeeded)
              (a (@ (href "/eval/" ,(assq-ref evaluation #:id) "?status=failed")
                    (class "badge badge-danger")
                    (title "Failed"))
                 ,failed)
              (a (@ (href "/eval/" ,(assq-ref evaluation #:id) "?status=pending")
                    (class "badge badge-secondary")
                    (title "Scheduled"))
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
                (format  (if (= year current)
                             "~e ~b ~H:~M ~z"
                             "~e ~b ~Y ~H:~M")))
           (date->string date format)))))

(define (build-eval-table eval-id builds build-min build-max status)
  "Return HTML for the BUILDS table evaluation with given STATUS.  BUILD-MIN
and BUILD-MAX are global minimal and maximal (stoptime, rowid) pairs."
  (define (table-header)
    `(thead
      (tr
       (th (@ (scope "col") (class "border-0")) '())
       (th (@ (scope "col") (class "border-0")) "ID")
       (th (@ (scope "col") (class "border-0")) "Specification")
       (th (@ (scope "col") (class "border-0")) "Completion time")
       (th (@ (scope "col") (class "border-0")) "Job")
       (th (@ (scope "col") (class "border-0")) "Name")
       (th (@ (scope "col") (class "border-0")) "System")
       (th (@ (scope "col") (class "border-0")) "Log"))))

  (define (table-row build)
    (define status
      (assq-ref build #:buildstatus))

    (define completed?
      (or (= (build-status succeeded) status)
          (= (build-status failed) status)))

    `(tr
      (td (span (@ (class ,(status-class status))
                   (title ,(status-title status))
                   (aria-hidden "true"))
                ""))
      (th (@ (scope "row"))
          (a (@ (href "/build/" ,(assq-ref build #:id) "/details"))
             ,(assq-ref build #:id)))
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

  `((table
     (@ (class "table table-sm table-hover table-striped"))
     ,@(if (null? builds)
           `((th (@ (scope "col") (class "border-0")) "No elements here."))
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

;; FIXME: Copied from (guix scripts describe).
(define %vcs-web-views
  ;; Hard-coded list of host names and corresponding web view URL templates.
  ;; TODO: Allow '.guix-channel' files to specify a URL template.
  (let ((labhub-url (lambda (repository-url commit)
                      (string-append
                       (if (string-suffix? ".git" repository-url)
                           (string-drop-right repository-url 4)
                           repository-url)
                       "/commit/" commit))))
    `(("git.savannah.gnu.org"
       ,(lambda (repository-url commit)
          (string-append (string-replace-substring repository-url
                                                   "/git/" "/cgit/")
                         "/log/?id=" commit)))
      ("notabug.org" ,labhub-url)
      ("framagit.org" ,labhub-url)
      ("gitlab.com" ,labhub-url)
      ("gitlab.inria.fr" ,labhub-url)
      ("github.com" ,labhub-url))))

(define (commit-hyperlink url commit)
  "Return, if possibly, a hyperlink for COMMIT of the repository at URL."
  (let* ((uri  (string->uri url))
         (host (uri-host uri)))
    (match (assoc-ref %vcs-web-views host)
      (#f     commit)
      ((link) `(a (@ (href ,(link url commit))) ,commit)))))

(define* (evaluation-build-table evaluation
                                 #:key
                                 (checkouts '())
                                 (inputs '())
                                 status builds
                                 builds-id-min builds-id-max)
  "Return HTML for an evaluation page, containing a table of builds for that
evaluation."
  (define id        (assq-ref evaluation #:id))
  (define total     (assq-ref evaluation #:total))
  (define succeeded (assq-ref evaluation #:succeeded))
  (define failed    (assq-ref evaluation #:failed))
  (define scheduled (assq-ref evaluation #:scheduled))
  (define spec      (assq-ref evaluation #:spec))

  `((p (@ (class "lead"))
       ,(format #f "Evaluation #~a" id))
    (table (@ (class "table table-sm table-hover"))
           (thead
            (tr (th (@ (class "border-0") (scope "col")) "Input")
                (th (@ (class "border-0") (scope "col")) "Commit")))
           (tbody
            ,@(map (lambda (checkout)
                     (let* ((name  (assq-ref checkout #:input))
                            (input (find (lambda (input)
                                           (string=? (assq-ref input #:name)
                                                     name))
                                         inputs))
                            (url   (assq-ref input #:url))
                            (commit (assq-ref checkout #:commit)))
                       `(tr (td ,url)
                            (td (code ,(commit-hyperlink url commit))))))
                   checkouts)))

    (p (@ (class "lead"))
       ,(format #f "~@[~a~] ~:[B~;b~]uilds of evaluation #~a"
                (and=> status string-capitalize)
                status
                id))
    (ul (@ (class "nav nav-tabs"))
        (li (@ (class "nav-item"))
            (a (@ (class ,(string-append "nav-link "
                                         (match status
                                           (#f "active")
                                           (_ ""))))
                  (href "?all="))
               "All "
               (span (@ (class "badge badge-light badge-pill"))
                     ,total)))
        (li (@ (class "nav-item"))
            (a (@ (class ,(string-append "nav-link "
                                         (match status
                                           ("pending" "active")
                                           (_ ""))))
                  (href "?status=pending"))
               (span (@ (class "oi oi-clock text-warning")
                        (title "Scheduled")
                        (aria-hidden "true"))
                     "")
               " Scheduled "
               (span (@ (class "badge badge-light badge-pill"))
                     ,scheduled)))
        (li (@ (class "nav-item"))
            (a (@ (class ,(string-append "nav-link "
                                         (match status
                                           ("succeeded" "active")
                                           (_ ""))))
                  (href "?status=succeeded"))
               (span (@ (class "oi oi-check text-success")
                        (title "Succeeded")
                        (aria-hidden "true"))
                     "")
               " Succeeded "
               (span (@ (class "badge badge-light badge-pill"))
                     ,succeeded)))
        (li (@ (class "nav-item"))
            (a (@ (class ,(string-append "nav-link "
                                         (match status
                                           ("failed" "active")
                                           (_ ""))))
                  (href "?status=failed"))
               (span (@ (class "oi oi-x text-danger")
                        (title "Failed")
                        (aria-hidden "true"))
                     "")
               " Failed "
               (span (@ (class "badge badge-light badge-pill"))
                     ,failed))))
    (div (@ (class "tab-content pt-3"))
         (div (@ (class "tab-pane show active"))
              ,(build-eval-table
                id
                builds
                builds-id-min
                builds-id-max
                status)))))

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
      (td (span (@ (class ,(status-class status))
                   (title ,(status-title status))
                   (aria-hidden "true"))
                ""))
      (th (@ (scope "row"))
          (a (@ (href "/build/" ,(assq-ref build #:id) "/details"))
             ,(assq-ref build #:id)))
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
       "Builds matching " (em ,query))
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

(define (running-builds-table builds)
  "Return HTML for the running builds table."
  (define (build-row build)
    `(tr
      (th (@ (scope "row"))
          (a (@ (href "/build/" ,(assq-ref build #:id) "/details"))
             ,(assq-ref build #:id)))
      (td ,(assq-ref build #:job-name))
      (td ,(time->string
            (assq-ref build #:starttime)))
      (td ,(assq-ref build #:system))))

  `((p (@ (class "lead")) "Running builds")
    (table
     (@ (class "table table-sm table-hover table-striped"))
     ,@(if (null? builds)
           `((th (@ (scope "col")) "No elements here."))
           `((thead (tr (th (@ (scope "col")) "ID")
                        (th (@ (scope "col")) "Job")
                        (th (@ (scope "col")) "Queued at")
                        (th (@ (scope "col")) "System")))
             (tbody
              ,(map build-row builds)))))))
