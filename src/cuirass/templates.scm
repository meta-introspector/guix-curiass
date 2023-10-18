;;; templates.scm -- HTTP API
;;; Copyright © 2018 Tatiana Sholokhova <tanja201396@gmail.com>
;;; Copyright © 2018, 2019, 2020, 2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (cuirass templates)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (json)
  #:use-module (web uri)
  #:use-module (guix channels)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix progress)
  #:use-module ((guix store) #:hide (build))
  #:use-module ((guix utils) #:select (string-replace-substring
                                       version>?))
  #:use-module (cuirass config)
  #:use-module (cuirass database)
  #:use-module (cuirass remote)
  #:use-module (cuirass specification)
  #:export (html-page
            specifications-table
            specification-edit
            evaluation-info-table
            build-eval-table
            build-search-results-table
            build-details
            evaluation-build-table
            running-builds-table
            global-metrics-content
            workers-status
            machine-status
            evaluation-dashboard
            badge-svg
            javascript-licenses))

(define (completed? status)
    (or (= (build-status succeeded) status)
        (= (build-status failed) status)
        (= (build-status failed-dependency) status)))

(define (completed-with-logs? status)
    (or (= (build-status succeeded) status)
        (= (build-status failed) status)))

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
          (@ (class "input-group")
             (role "search"))
          (label (@ (for "query")
                    (class "sr-only"))
                 "Search for builds")
          (input (@ (type "text")
                    (class "form-control")
                    (id   "query")
                    (name "query")
                    (aria-describedby "search-hints")
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
               ", a " (em "specification") " such as " (code "master"))
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
          (p "For example, the following query will list successful builds of
the " (code "master") " specification for the " (code "i686-linux") "
system whose names start with " (code "guile-") ":" (br)
(code "spec:master system:i686-linux status:success guile-")))))

(define* (html-page title body navigation
                    #:optional query
                    #:key (margin? #t))
  "Return HTML page with given TITLE and BODY."
  `(html (@ (lang "en"))
         (head
          (meta (@ (charset "utf-8")))
          (meta (@ (name "viewport")
                   (content ,(string-join '("width=device-width"
                                            "initial-scale=1"
                                            "shrink-to-fit=no")
                                          ", "))))
          (link (@ (rel "stylesheet")
                   (href "/static/css/bootstrap.min.css")))
          (link (@ (rel "stylesheet")
                   (href "/static/css/datatables.min.css")))
          (link (@ (rel "stylesheet")
                   (href "/static/css/open-iconic-bootstrap.css")))
          (link (@ (rel "stylesheet")
                   (href "/static/css/choices.min.css")))
          (link (@ (rel "stylesheet")
                   (href "/static/css/cuirass.css")))
          (link (@ (rel "icon") (type "image/png")
                   (href "/static/images/icon.png")))
          ;; The empty strings are mandatory to force the SXML parser to
          ;; create end script tags.
          (script (@ (src "/static/js/jquery-3.3.1.min.js")) "")
          (script (@ (src "/static/js/popper.min.js")) "")
          (script (@ (src "/static/js/bootstrap.min.js")) "")
          (script (@ (src "/static/js/datatables.min.js")) "")
          (script (@ (src "/static/js/d3.v6.min.js")) "")
          (script (@ (src "/static/js/choices.min.js")) "")
          (script (@ (src "/static/js/chart.js")) "")
          (script (@ (src "/static/js/cuirass.js")) "")
          (title ,title))
         (body
          (header
           (a (@ (href "/static/about/javascript")
                 (rel "jslicense")
                 (class "d-none"))
              "Javascript license information")
           (nav (@ (class "navbar navbar-expand-lg navbar-light bg-light"))
                (a (@ (class "navbar-brand pt-0")
                      (href "/"))
                   (img (@ (src "/static/images/guix.png")
                           (alt "logo")
                           (height "25")
                           (style "margin-top: -12px"))))
                (button (@ (class "navbar-toggler")
                           (type "button")
                           (data-bs-toggle "collapse")
                           (data-bs-target "navbarDropdown")
                           (aria-controls "navbarDropdown")
                           (aria-expanded "false")
                           (aria-label "Toggle dropdown"))
                        (span (@ (class "navbar-toggler-icon"))))
                (div (@ (id "navbarDropdown")
                        (class "collapse navbar-collapse"))
                     (ul (@ (class "navbar-nav mr-auto"))
                         (li (@ (class "nav-item dropdown"))
                             (a (@ (id "navbarDropdownMenuLink")
                                   (class "nav-link dropdown-toggle")
                                   (data-toggle "dropdown")
                                   (href "#")
                                   (role "button")
                                   (aria-haspopup "true")
                                   (aria-expanded "false"))
                                "Status")
                             (ul
                              (@ (class "dropdown-menu")
                                 (role "menu")
                                 (aria-labelledby "navbarDropdownMenuLink"))
                              (li (@ (role "menuitem"))
                                  (a (@ (class "dropdown-item")
                                        (href "/metrics"))
                                     "Global metrics"))
                              (li (@ (role "menuitem"))
                                  (a (@ (class "dropdown-item")
                                        (href "/workers"))
                                     "Workers status"))
                              (li (@ (role "menuitem"))
                                  (a (@ (class "dropdown-item")
                                        (href "/status"))
                                     "Running builds"))))
                         (li (@ (class "nav-item"))
                             (a (@ (class "nav-link" ,(if (null? navigation)
                                                          " active" ""))
                                   (href "/"))
                                Home))
                         ,@(navigation-items navigation)))
                ,(search-form query)))
          (main (@ (id "content")
                   (class ,(if margin?
                               "container content"
                               "content-fixed-margin")))
                ,body)
          (footer
           (@ (class "footer text-center"))
           (p (@ (class "mt-3"))
              (a (@ (href "http://guix.gnu.org/cuirass/"))
                 ,(string-append "Cuirass " %package-version))
              " — Copyright © 2016–2023 by the GNU Guix community.")))))

(define (status-class status)
  (cond
   ((= (build-status submitted) status)
    "oi oi-clock text-warning")
   ((= (build-status scheduled) status)
    "oi oi-clock text-warning")
   ((= (build-status started) status)
    "oi oi-reload text-warning")
   ((= (build-status succeeded) status)
    "oi oi-check text-success")
   ((= (build-status failed) status)
    "oi oi-x text-danger")
   ((= (build-status failed-dependency) status)
    "oi oi-warning text-danger")
   ((= (build-status failed-other) status)
    "oi oi-warning text-danger")
   ((= (build-status canceled) status)
    "oi oi-question-mark text-warning")
   (else
    "oi oi-warning text-danger")))

(define (status-title status)
  (cond
   ((= (build-status submitted) status)
    "Submitted")
   ((= (build-status scheduled) status)
    "Scheduled")
   ((= (build-status started) status)
    "Started")
   ((= (build-status succeeded) status)
    "Succeeded")
   ((= (build-status failed) status)
    "Failed")
   ((= (build-status failed-dependency) status)
    "Failed (dependency)")
   ((= (build-status failed-other) status)
    "Failed (other)")
   ((= (build-status canceled) status)
    "Canceled")
   (else
    "Invalid status")))

(define (specifications-table specs evaluations summaries latest-evaluations)
  "Return HTML for the SPECS table."
  (define (spec->latest-eval-ok name)
    (find (lambda (e)
            (string=? (evaluation-specification-name e) name))
          evaluations))

  (define (spec->latest-eval name)
    (find (lambda (e)
            (string=? (evaluation-specification-name e) name))
          latest-evaluations))

  (define (eval-summary eval)
    (find (lambda (s)
            (= (evaluation-summary-id s)
               (evaluation-id eval)))
          summaries))

  (define (summary->percentage summary)
    (let ((total (evaluation-summary-total summary))
          (succeeded (evaluation-summary-succeeded summary)))
      (if (zero? total)
          0
          (nearest-exact-integer (* 100 (/ succeeded total))))))

  `((div (@ (class "d-flex flex-row mb-3"))
         (div (@ (class "lead mr-auto"))
              "Specifications")
         ,(let ((name "Add a specification"))
            `(div
              (a (@ (class "btn btn-outline-primary mr-1")
                    (href "/specification/add/")
                    (title ,name)
                    (aria-label ,name)
                    (role "button"))
                 (i (@ (class "oi oi-plus text-primary py-1"))
                    ""))))
         ,(let ((name "RSS events"))
            `(div
              (a (@ (class "btn btn-outline-warning mr-1")
                    (href "/events/rss/")
                    (title ,name)
                    (aria-label ,name)
                    (role "button"))
                 (i (@ (class "oi oi-rss text-warning py-1"))
                    ""))))
         ,(let ((name "Toggle jobs"))
            `(div
              (button (@ (class "btn btn-outline-primary job-toggle")
                         (title ,name)
                         (aria-label ,name)
                         (type "button"))
                      (i (@ (class "oi oi-contrast d-inline-block py-1"))
                         "")))))
    (table
     (@ (id "spec-table")
        (class "table table-sm table-hover"))
     ,@(if (null? specs)
           `((th (@ (scope "col")) "No elements here."))
           `((thead (tr (th (@ (scope "col")) Name)
                        (th (@ (scope "col")) Build)
                        (th (@ (scope "col")) Channels)
                        (th (@ (scope "col")) Priority)
                        (th (@ (scope "col")) Systems)
                        (th (@ (scope "col")) Jobs)
                        (th (@ (scope "col")) Action)))
             (tbody
              ,@(map
                 (lambda (spec)
                   `(tr
                     (td (a (@ (href "/jobset/"
                                     ,(specification-name spec)))
                            ,(specification-name spec)))
                     (td ,(match (specification-build spec)
                            ((? symbol? build)
                             (symbol->string build))
                            ((build _ ...)
                             (symbol->string build))))
                     (td ,(string-join
                           (map (lambda (channel)
                                  (format #f "~a (on ~a)"
                                          (channel-name channel)
                                          (channel-branch channel)))
                                (specification-channels spec)) ", "))
                     (td ,(number->string
                           (specification-priority spec)))
                     (td
                      ,(let* ((systems (specification-systems spec))
                              (systems*
                               (string-join
                                (sort systems string<?)
                                ", "))
                              (tooltip?
                               (> (length systems) 1)))
                         `(span
                           (@ ,@(if tooltip?
                                    `((data-toggle "tooltip")
                                      (title ,systems*))
                                    '()))
                           ,(if tooltip?
                                (string-append (car systems) ", ...")
                                systems))))
                     (td
                      (@
                       (style "vertical-align: middle"))
                      ,@(let* ((summary
                                (and=> (spec->latest-eval-ok
                                        (specification-name spec))
                                       eval-summary))
                               (last-eval
                                (spec->latest-eval
                                 (specification-name spec)))
                               (last-eval-status-ok?
                                (and last-eval
                                     (<= (evaluation-current-status last-eval)
                                         (evaluation-status succeeded))))
                               (percentage
                                (and summary (summary->percentage summary)))
                               (style
                                   (format #f "width: ~a%" percentage)))
                          (cond
                           ((and summary last-eval-status-ok?)
                            `((div
                               (@ (class "progress job-abs")
                                  (title "Percentage succeeded"))
                               (div (@ (class "progress-bar")
                                       (role "progressbar")
                                       (style ,style)
                                       (aria-valuemin "0")
                                       (aria-valuemax "100"))
                                    (strong
                                     (span
                                      (@ (class "text-dark"))
                                      ,percentage
                                      "%"))))
                              " "
                              (div
                               (@ (class "job-rel d-none"))
                               ,(successful-build-badge
                                 (evaluation-summary-succeeded summary))
                               ,(failed-build-badge
                                 (evaluation-summary-failed summary))
                               ,(scheduled-build-badge
                                 (evaluation-summary-scheduled summary)))))
                           ((and last-eval (not last-eval-status-ok?))
                            ;; LAST-EVAL is broken so it's missing from
                            ;; SUMMARIES.
                            `((center
                               ,@(broken-evaluation-badge
                                  (evaluation-id last-eval)
                                  (evaluation-current-status last-eval)))))
                           (else '()))))
                     (td
                      ,@(let* ((name (specification-name spec))
                               (dashboard-name
                                (string-append "Dashboard " name)))
                          `((a (@ (href "/eval/latest/dashboard?spec="
                                        ,(uri-encode name)))
                               (div
                                (@ (class "oi oi-monitor d-inline-block ml-2")
                                   (title ,dashboard-name)
                                   (aria-label ,dashboard-name))
                                ""))))
                      ,(let ((id
                              (string-append
                               "specDropdown-"
                               (specification-name spec)))
                             (name
                              (string-append "Options "
                                             (specification-name spec))))
                         `(div
                           (@ (id ,id)
                              (title ,name)
                              (aria-label ,name)
                              (class "dropdown d-inline-block ml-2"))
                           (a (@ (class "oi oi-menu dropdown-toggle no-dropdown-arrow")
                                 (href "#")
                                 (data-toggle "dropdown")
                                 (role "button")
                                 (aria-haspopup "true")
                                 (aria-expanded "false"))
                              " ")
                           (ul (@ (class "dropdown-menu")
                                  (role "menu")
                                  (aria-labelledby ,id))
                               (li (@ (role "menuitem"))
                                   (a (@ (class "dropdown-item")
                                         (href "/specification/edit/"
                                               ,(specification-name spec)))
                                      " Edit"))
                               (li (@ (role "menuitem"))
                                   (a (@ (class "dropdown-item")
                                         (href "/admin/specifications/deactivate/"
                                               ,(specification-name spec)))
                                      " Deactivate"))))))))
                 specs)))))))

(define* (specification-edit #:optional spec)
  "Return HTML to add a new specification if no argument is passed, or to edit
the existing SPEC otherwise."
  (define (channels->html channels)
    (let ((html
           (fold
            (lambda (channel html)
              (let ((first-row? (null? html))
                    (name (channel-name channel))
                    (url (channel-url channel))
                    (branch (channel-branch channel)))
                (cons
                 `(div (@ (class ,(if first-row?
                                      "form-group row channel"
                                      "form-group row channel-new")))
                       (span (@ (for "channel-name")
                                (class "col-sm-2 col-form-label"))
                             ,(if first-row? "Channels" ""))
                       (div (@ (class "col-sm-2"))
                            (input
                             (@ (type "text")
                                (class "form-control channel-name")
                                (name "channel-name")
                                (placeholder "name")
                                (value ,name)
                                (required)))
                            (label (@ (for "channel-name")
                                      (class "sr-only"))
                                   "Channel name"))
                       (div (@ (class "col-sm-4"))
                            (input
                             (@ (type "text")
                                (class "form-control channel-url")
                                (name "channel-url")
                                (placeholder "url")
                                (value ,url)
                                (required)))
                            (label (@ (for "channel-url")
                                      (class "sr-only"))
                                   "Channel url"))
                       (div (@ (class "col-sm-2"))
                            (input
                             (@ (type "text")
                                (class "form-control channel-branch")
                                (name "channel-branch")
                                (placeholder "branch")
                                (value ,branch)
                                (required)))
                            (label (@ (for "channel-branch")
                                      (class "sr-only"))
                                   "Channel branch"))
                       ,@(if first-row?
                             '((a (@ (class "btn btn-success add-channel")
                                     (href "#")
                                     (role "button"))
                                  "Add"))
                             '((a (@ (class "btn btn-danger remove-channel")
                                     (href "#")
                                     (role "button"))
                                  "Remove"))))
                 html)))
            '()
            channels)))
      (match (reverse html)
        ((first . rest)
         (list first `(div (@ (class "channels"))
                           ,@(if (null? rest)
                                 '("")
                                 rest)))))))

  (let ((name (and spec (specification-name spec)))
        (build (and spec (match (specification-build spec)
                           ((? symbol? build) build)
                           ((build _ ...) build))))
        (channels (and spec (specification-channels spec)))
        (period (and spec (specification-period spec)))
        (priority (and spec (specification-priority spec)))
        (systems (and spec (specification-systems spec))))
    `(span
      (p (@ (class "lead edit-channel"))
         ,(if spec
              (format #f "Edit ~a specification" name)
              "Create a new specification"))
      (form (@ (id "add-specification")
               (class "needs-validation")
               ,@(if spec
                     '((action "/admin/specification/edit"))
                     '((action "/admin/specification/add")))
               (method "POST"))
            (div (@ (class "form-group row"))
                 (label (@ (for "name")
                           (class "col-sm-2 col-form-label"))
                        "Name")
                 (div (@ (class "col-sm-4"))
                      (input (@ (type "text")
                                (class "form-control")
                                (id "name")
                                (name "name")
                                (pattern "[^/]+")
                                (value ,(or name ""))
                                ,@(if spec
                                      '((readonly))
                                      '())
                                (required)))))
            (div (@ (class "form-group row"))
                 (label (@ (for "build")
                           (class "col-sm-2 col-form-label"))
                        "Build")
                 (div (@ (class "col-sm-4"))
                      (select
                       (@ (class "form-control build-select")
                          (id "build")
                          (name "build"))
                       ,@(map (lambda (type)
                                `(option (@ ,@(if (eq? type build)
                                                  '((selected))
                                                  '()))
                                         ,(symbol->string type)))
                              %build-types))))
            ,(if spec
                 (match (specification-build spec)
                   ((build . rest)
                    `((span (@ (class "default-build-param")
                               (hidden "true"))
                            ,(string-join
                              (map (lambda (param)
                                     (cond
                                      ((string? param)
                                       param)
                                      (else
                                       (object->string param))))
                                   rest)
                              ","))))
                   (else ""))
                 '())
            (div (@ (class "form-group row param-select-row"))
                 (label (@(class "col-sm-2 col-form-label"))
                        "Parameter")
                 (div (@ (class "col-sm-4"))
                      (select (@ (type "text")
                                 (class "form-control build-param-select")
                                 (name "param-select")
                                 (multiple))
                              "")))
            (div (@ (class "form-group row param-input-row"))
                 (label (@ (class "col-sm-2 col-form-label"))
                        "Parameter")
                 (div (@ (class "col-sm-4"))
                      (input (@ (type "text")
                                (name "param-input")
                                (class "form-control build-param-input")))))
            ,@(channels->html
               (if spec channels (list %default-guix-channel)))
            (div (@ (class "form-group row"))
                 (label (@ (for "period")
                           (class "col-sm-2 col-form-label"))
                        "Period")
                 (div (@ (class "col-sm-4"))
                      (input
                       (@ (type "number")
                          (class "form-control")
                          (id "period")
                          (name "period")
                          (min 0)
                          (value ,(or period 0))))))
            (div (@ (class "form-group row"))
                 (label (@ (for "priority")
                           (class "col-sm-2 col-form-label"))
                        "Priority")
                 (div (@ (class "col-sm-4"))
                      (input
                       (@ (type "number")
                          (class "form-control")
                          (id "priority")
                          (name "priority")
                          (min 0)
                          (max 9)
                          (value ,(or priority 9))))))
            (div (@ (class "form-group row"))
                 (span (@ (class "col-sm-2 col-form-label"))
                       "Systems")
                 ,@(map (lambda (system)
                          `(div (@ (class "form-check form-check-inline"))
                                (input (@ (class "form-check-input system")
                                          (type "checkbox")
                                          (id ,system)
                                          (name ,system)
                                          ,@(if (and systems
                                                     (member system systems))
                                                '((checked))
                                                '())))
                                (label (@ (class "form-check-label")
                                          (for ,system))
                                       ,system)))
                        %cuirass-supported-systems))
            (div (@ (class "form-group row"))
                 (div (@ (class "col-sm-2"))
                      (button
                       (@ (type "submit")
                          (class "btn btn-primary"))
                       " Submit"))
		 (div (@ (class "col-sm-10 text-warning"))
		      "Declarative configuration updates may overwrite these settings!"))))))

(define (build-details build dependencies products history)
  "Return HTML showing details for the BUILD."
  (define status (build-current-status build))
  (define weather (build-current-weather build))

  (define evaluation
    (build-evaluation-id build))

  (define (find-dependency id)
    (find (lambda (build)
            (= (build-id build) id))
          dependencies))

  (define (history-table-row build)
    (define status
      (build-current-status build))

    `(tr
      (td (span (@ (class ,(status-class status))
                   (title ,(status-title status))
                   (aria-hidden "true"))
                ""))
      (th (@ (scope "row"))
          (a (@ (href "/build/" ,(build-id build) "/details"))
             ,(build-id build)))
      (td ,(build-nix-name build))
      (td ,(if (completed? status)
               (time->string (build-completion-time build))
               "—"))))

  `((div (@ (class "d-flex flex-row mb-3"))
         (div (@ (class "lead mr-auto"))
              "Build details")
         (div (@ (class "dropdown"))
              (a (@ (class "btn btn-warning dropdown-toggle")
                    (href "#")
                    (data-toggle "dropdown")
                    (role "button")
                    (aria-haspopup "true")
                    (aria-expanded "false"))
                 "Action")
              (ul (@ (class "dropdown-menu")
                     (role "menu"))
                  (li (@ (role "menuitem"))
                      (a (@ (class "dropdown-item")
                            (href "/admin/build/"
                                  ,(build-id build) "/restart"))
                         " Restart")))))
    (table
     (@ (class "table table-sm table-hover"))
     (tbody
      (tr (th "Build ID")
          (td ,(build-id build)))
      (tr (th "Evaluation")
          (td (a (@ (href ,(string-append "/eval/"
                                          (number->string evaluation))))
                 ,(number->string evaluation))
              " ("
              (a (@ (href ,(string-append "/jobset/"
                                          (build-specification-name build))))
                 ,(build-specification-name build))
              ")"))
      (tr (th "Status")
          (td (span (@ (class ,(status-class status))
                       (title ,(status-title status)))
                    ,(string-append " " (status-title status)))))
      (tr (th "System")
          (td ,(build-system build)))
      (tr (th "Name")
          (td ,(build-nix-name build)))
      (tr (th "Duration")
          (td ,(let ((timestamp (time-second (current-time time-utc)))
                     (start (build-start-time build))
                     (stop  (build-completion-time build)))
                 (cond
                  ((and (> start 0) (> stop 0))
                   (string-append (number->string (- stop start))
                                  " seconds"))
                  ((> start 0)
                   (string-append (number->string (- timestamp start))
                                  " seconds"))
                  (else "—")))))
      (tr (th "Finished")
          (td ,(if (completed? status)
                   (time->string (build-completion-time build))
                   "—")))
      (tr (th "Weather")
          (td (span (@ (class ,(weather-class weather))
                       (title ,(weather-title weather))
                       (aria-hidden "true"))
                    "")
              " " ,(weather-title weather)))
      (tr (th "Log file")
          (td ,(if (or (= (build-status started) status)
                       (= (build-status succeeded) status)
                       (= (build-status failed) status)
                       (= (build-status canceled) status))
                   `(a (@ (href "/build/" ,(build-id build) "/log/raw"))
                       "raw")
                   "—")))
      (tr (th "Derivation")
          (td (pre ,(build-derivation build))))
      (tr (th "Dependencies")
          (td
           (@ (class "dependencies"))
           ,@(let ((dependencies
                    (build-dependencies/id build))
                   (max-items 10))
               (if (> (length dependencies) 0)
                   `(,(map (lambda (id index)
                             (let* ((build (find-dependency id))
                                    (status (build-current-status build)))
                               `((div
                                  ,@(if (> index (1- max-items))
                                        '((@ (class "collapse collapse-dep")))
                                        '())
                                  (span (@ (class ,(status-class status))
                                           (title ,(status-title status))
                                           (aria-hidden "true"))
                                        "")
                                  " "
                                  (a (@ (href "/build/" ,id "/details"))
                                     ,(build-nix-name build))
                                  (br)))))
                           dependencies
                           (iota (length dependencies)))
                     ,@(if (> (length dependencies) max-items)
                           '((button (@ (id "collapse-dep-btn")
                                        (class "btn btn-primary")
                                        (type "button")
                                        (data-toggle "collapse")
                                        (data-target ".collapse-dep")
                                        (aria-expanded "false")
                                        (aria-controls "collapse-dep")
                                        (aria-label "Toggle dependencies dropdown"))
                                     "Show more"))
                           '()))
                   '("—")))))
      (tr (th "Outputs")
          (td ,(map (lambda (output)
                      `(pre ,(output-item output)))
                    (build-outputs build))))
      ,@(if (null? products)
            '()
            (let ((product-items
                   (map
                    (lambda (product)
                      (let* ((id (build-product-id product))
                             (size (build-product-file-size product))
                             (type (build-product-type product))
                             (path (build-product-file product))
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
                         ,product-items))))))
      ,@(match (build-worker build)
          ((or #f "") '())
          (name
           (let ((worker (db-get-worker name)))
             (if worker
                 `((tr (th "Build machine")
                       (td (a (@ (href
                                  ,(string-append "/machine/"
                                                  (worker-machine worker))))
                              ,(worker-machine worker))
                           ", worker " ,name)))
                 `((tr (th "Worker") (td ,name)))))))))
    ,@(if (null? history)
          '()
          `((div (@ (class "lead mr-auto"))
                 "Build history")
            (table
             (@ (class "table table-sm table-hover table-striped"))
             (thead
              (tr
               (th (@ (scope "col") (class "border-0")) ())
               (th (@ (scope "col") (class "border-0")) "ID")
               (th (@ (scope "col") (class "border-0")) "Name")
               (th (@ (scope "col") (class "border-0")) "Completion time")))
             (tbody ,@(map history-table-row history)))))))

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
                 (let ((input (checkout-channel checkout))
                       (commit (checkout-commit checkout)))
                   (format #f "~a → ~a" input (substring commit 0 7))))
               checkouts)
          ", ")))
    (if (string=? changes "") '(em "None") changes)))

(define (broken-evaluation-badge eval-id status)
  (cond ((= status (evaluation-status failed))
         `((a (@ (href "/eval/" ,eval-id "/log/raw")
                 (class "oi oi-x text-danger")
                 (title "Failed")
                 (aria-hidden "true"))
              "")))
        ((= status (evaluation-status aborted))
         `((a (@ (href "/eval/" ,eval-id "/log/raw")
                 (class "oi oi-x text-warning")
                 (title "Aborted")
                 (aria-hidden "true"))
              "")))))

(define (evaluation-badges evaluation absolute)
  (define (dashboard-link body)
    `(a (@ (href "/eval/" ,(build-summary-evaluation-id evaluation)
                 "/dashboard"))
        ,body))

  (let ((status (build-summary-status evaluation)))
    (if (= status (evaluation-status started))
        '((em "In progress…"))
        (cond
         ((= status (evaluation-status succeeded))
          `((div
             (@ (class "job-abs d-none"))
             ,(dashboard-link
               (successful-build-badge
                (if absolute
                    (evaluation-summary-succeeded absolute)
                    0)))
             ,(dashboard-link
               (failed-build-badge
                (if absolute
                    (evaluation-summary-failed absolute)
                    0)))
             ,(dashboard-link
               (scheduled-build-badge
                (if absolute
                    (evaluation-summary-scheduled absolute)
                    0))))
            (div
             (@ (class "job-rel"))
             ,(successful-build-badge (build-summary-succeeded evaluation)
                                      (string-append
                                       "/eval/"
                                       (number->string
                                        (build-summary-evaluation-id evaluation))
                                       "?status=succeeded"))
             ,(failed-build-badge (build-summary-failed evaluation)
                                  (string-append
                                   "/eval/"
                                   (number->string
                                    (build-summary-evaluation-id evaluation))
                                   "?status=failed"))
             ,(scheduled-build-badge (build-summary-scheduled evaluation)
                                     (string-append
                                      "/eval/"
                                      (number->string
                                       (build-summary-evaluation-id evaluation))
                                      "?status=pending")))))
         (else
          (broken-evaluation-badge (build-summary-evaluation-id evaluation)
                                   status))))))

(define* (evaluation-info-table name evaluations id-min id-max
                                #:key absolute-summary)
  "Return HTML for the EVALUATION table NAME. ID-MIN and ID-MAX are
  global minimal and maximal id."
  (define (eval-absolute-summary eval)
    (find (lambda (e)
            (= (evaluation-summary-id e)
               (build-summary-evaluation-id eval)))
          absolute-summary))

  `((div (@ (class "d-flex flex-row mb-3"))
         (div (@ (class "lead mr-auto"))
              "Evaluations of " ,name)
         ,(let ((rss-name "RSS events"))
            `(div
              (a (@ (class "btn btn-outline-warning mr-1")
                    (href "/events/rss/?specification=" ,name)
                    (title ,rss-name)
                    (aria-label ,rss-name)
                    (role "button"))
                 (i (@ (class "oi oi-rss text-warning py-1")
                       (aria-hidden "true"))
                    ""))))
         ,(let ((name "Toggle jobs"))
            `(div
              (button (@ (class "btn btn-outline-primary job-toggle")
                         (title ,name)
                         (aria-label ,name)
                         (type "button"))
                      (i (@ (class "oi oi-contrast d-inline-block py-1"))
                         "")))))
    (table
     (@ (class "table table-sm table-hover table-striped"))
     ,@(if (null? evaluations)
           `((th (@ (scope "col")) "No elements here."))
           `((thead
              (tr
               (th (@ (scope "col")) "#")
               (th (@ (scope "col")) "Channel changes")
               (th (@ (scope "col"))
                   (div (@ (class "job-rel")) "Build changes")
                   (div (@ (class "job-abs d-none"))
                        "Total number of builds"))
               (th (@ (scope "col")) "Action")))
             (tbody
              ,@(map
                 (lambda (summary)
                   `(tr (th (@ (scope "row"))
                            (a (@ (href
                                   "/eval/"
                                   ,(build-summary-evaluation-id summary)))
                               ,(build-summary-evaluation-id summary)))
                        (td ,(input-changes (build-summary-checkouts summary)))
                        (td
                         ,@(evaluation-badges summary
                                              (eval-absolute-summary summary)))
                        ,(let* ((id (build-summary-evaluation-id summary))
                                (title
                                 (string-append "Dashboard evaluation "
                                                (number->string id))))
                           `(td
                             (a (@ (href "/eval/" ,id "/dashboard"))
                                (div
                                 (@ (class
                                      ,(string-append
                                        "oi oi-monitor d-inline-block "
                                        (if (eq? (build-summary-status summary)
                                                 (evaluation-status succeeded))
                                            "visible"
                                            "invisible")))
                                    (title ,title)
                                    (aria-label ,title))
                                 ""))
                             ,(let ((dropdown-id
                                     (string-append
                                      "evaluationDropdown-"
                                      (number->string id)))
                                    (name
                                     (string-append "Options evaluation "
                                                    (number->string id))))
                                `(div
                                  (@ (id ,dropdown-id)
                                     (title ,name)
                                     (aria-label ,name)
                                     (class "dropdown d-inline-block ml-2"))
                                  (a (@ (class "oi oi-menu dropdown-toggle no-dropdown-arrow")
                                        (href "#")
                                        (data-toggle "dropdown")
                                        (role "button")
                                        (aria-haspopup "true")
                                        (aria-expanded "false"))
                                     " ")
                                  (ul (@ (class "dropdown-menu")
                                         (role "menu")
                                         (aria-labelledby ,id))
                                      (li (@ (role "menuitem"))
                                          (a (@ (class "dropdown-item")
                                                (href "/admin/evaluation/"
                                                      ,(build-summary-evaluation-id summary)
                                                      "/cancel"))
                                             " Cancel pending builds"))
                                      (li (@ (role "menuitem"))
                                          (a (@ (class "dropdown-item")
                                                (href "/admin/evaluation/"
                                                      ,(build-summary-evaluation-id summary)
                                                      "/restart"))
                                             " Restart all builds"))
                                      (li (@ (role "menuitem"))
                                          (a (@ (class "dropdown-item")
                                                (href "/admin/evaluation/"
                                                      ,(build-summary-evaluation-id summary)
                                                      "/retry"))
                                             " Retry the evaluation")))))))))
                 evaluations)))))
    ,(if (null? evaluations)
         (pagination "" "" "" "")
         (let* ((eval-ids (map build-summary-evaluation-id evaluations))
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

(define (weather-class status)
  (cond
   ((= (build-weather unknown) status)
    "oi oi-media-record text-primary mt-1")
   ((= (build-weather new-success) status)
    "oi oi-arrow-thick-top text-success mt-1")
   ((= (build-weather new-failure) status)
    "oi oi-arrow-thick-bottom text-danger mt-1")
   ((= (build-weather still-succeeding) status)
    "oi oi-media-record text-success mt-1")
   ((= (build-weather still-failing) status)
    "oi oi-media-record text-danger mt-1")))

(define (weather-title status)
  (cond
   ((= (build-weather unknown) status) "Unknown")
   ((= (build-weather new-success) status) "New success")
   ((= (build-weather new-failure) status) "New failure")
   ((= (build-weather still-succeeding) status) "Still succeeding")
   ((= (build-weather still-failing) status) "Still failing")))

(define (build-eval-table eval-id builds build-min build-max status)
  "Return HTML for the BUILDS table evaluation with given STATUS.  BUILD-MIN
and BUILD-MAX are global minimal and maximal (stoptime, rowid) pairs."
  (define (table-header)
    `(thead
      (tr
       (th (@ (scope "col") (class "border-0")) '())
       (th (@ (scope "col") (class "border-0")) '())
       (th (@ (scope "col") (class "border-0")) "ID")
       (th (@ (scope "col") (class "border-0")) "Completion time")
       (th (@ (scope "col") (class "border-0")) "Job")
       (th (@ (scope "col") (class "border-0")) "Name")
       (th (@ (scope "col") (class "border-0")) "System"))))

  (define (table-row build)
    (define status
      (build-current-status build))

    (define weather
      (build-current-weather build))

    `(tr
      (td (a (@ (class ,(status-class status))
                (title ,(status-title status))
                (aria-hidden "true")
                ,@(if (completed-with-logs? status)
                      `((href "/build/" ,(build-id build) "/log/raw"))
                      '()))
             ""))
      (td (span (@ (class ,(weather-class weather))
                   (title ,(weather-title weather))
                   (aria-hidden "true"))
                ""))
      (th (@ (scope "row"))
          (a (@ (href "/build/" ,(build-id build) "/details"))
             ,(build-id build)))
      (td ,(if (completed? status)
               (time->string (build-completion-time build))
               "—"))
      (td ,(build-job-name build))
      (td ,(build-nix-name build))
      (td ,(build-system build))))

  (define (page-boundary-build-id build)
    (match build
      ((stoptime id) id)))

  (define (page-boundary-build-stoptime build)
    (match build
      ((stoptime id) stoptime)))

  `((table
     (@ (id "eval-table")
        (class "table table-sm table-hover table-striped"))
     ,@(if (null? builds)
           `((th (@ (scope "col") (class "border-0")) "No elements here."))
           `(,(table-header)
             (tbody ,@(map table-row builds)))))
    ,(if (null? builds)
         (pagination "" "" "" "")
         (let* ((build-time-ids (map (lambda (build)
                                       (list (build-completion-time build)
                                             (build-id build)))
                                     builds))
                (page-build-min (last build-time-ids))
                (page-build-max (first build-time-ids)))
           (pagination
            (format
             #f "?border-high-time=~d&border-high-id=~d~@[&status=~a~]"
             (page-boundary-build-stoptime build-max)
             (1+ (page-boundary-build-id build-max))
             status)
            (if (equal? page-build-max build-max)
                ""
                (format
                 #f "?border-low-time=~d&border-low-id=~d~@[&status=~a~]"
                 (page-boundary-build-stoptime page-build-max)
                 (page-boundary-build-id page-build-max)
                 status))
            (if (equal? page-build-min build-min)
                ""
                (format
                 #f "?border-high-time=~d&border-high-id=~d~@[&status=~a~]"
                 (page-boundary-build-stoptime page-build-min)
                 (page-boundary-build-id page-build-min)
                 status))
            (format
             #f "?border-low-time=~d&border-low-id=~d~@[&status=~a~]"
             (page-boundary-build-stoptime build-min)
             (1- (page-boundary-build-id build-min))
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

(define* (commit-hyperlink url commit #:key shorten?)
  "Return, if possibly, a hyperlink for COMMIT of the repository at URL.  When
SHORTEN? is true, display a shortened version of COMMIT."
  (let ((str (if shorten?
                 (string-take commit 7)
                 commit)))
    (match (string->uri url)
      (#f str)
      (uri
       (let ((host (uri-host uri)))
         (match (assoc-ref %vcs-web-views host)
           (#f     str)
           ((link) `(a (@ (href ,(link url commit))) ,str))))))))

(define (nearest-exact-integer x)
  "Given a real number X, return the nearest exact integer, with ties going to
the nearest exact even integer."
  (inexact->exact (round x)))

(define (seconds->string duration)
  (if (< duration 60)
      (format #f "~a second~:p" duration)
      (format #f "~a minute~:p" (nearest-exact-integer
                                 (/ duration 60)))))

(define (checkout-table checkouts channels)
  "Return SHTML for a table representing CHECKOUTS."
  `(table (@ (class "table table-sm table-hover"))
          (thead
           (tr (th (@ (class "border-0") (scope "col")) "Channel")
               (th (@ (class "border-0") (scope "col")) "Commit")))
          (tbody
           ,@(map (lambda (checkout)
                    ;; Normally CHECKOUT is a <checkout> record that was
                    ;; returned by 'latest-checkouts'.  However, due to old
                    ;; bugs, the database might yield #f for channels that are
                    ;; indirect dependencies; deal with it gracefully.
                    (if checkout
                        (let* ((name  (checkout-channel checkout))
                               (channel (find (lambda (channel)
                                                (eq? (channel-name channel)
                                                     name))
                                              channels)))
                          ;; Some checkout entries may refer to removed
                          ;; inputs.
                          (if channel
                              (let ((url (channel-url channel))
                                    (commit (checkout-commit checkout)))
                                `(tr (td ,url)
                                     (td (code ,(commit-hyperlink url commit)))))
                              '()))
                        `(tr (td "?")
                             (td (i "checkout information is missing")))))
                  checkouts))))

(define* (build-counter-badge value class title
                              #:optional link)
  (if link
      `(a (@ (href ,link)
             (class "badge " ,class " badge-counter"
               ,(if (eqv? 0 value) " hidden" ""))
             (title ,title))
          ,value)
      `(div (@ (class "badge " ,class " badge-counter"
                 ,(if (eqv? 0 value) " hidden" ""))
               (title ,title))
            ,value)))

(define successful-build-badge
  (cut build-counter-badge <> "badge-success" "Succeeded" <...>))
(define failed-build-badge
  (cut build-counter-badge <> "badge-danger" "Failed" <...>))
(define scheduled-build-badge
  (cut build-counter-badge <> "badge-secondary" "Scheduled" <...>))

(define* (evaluation-build-table evaluation
                                 #:key
                                 channels
                                 (checkouts '())
                                 status builds
                                 builds-id-min builds-id-max)
  "Return HTML for an evaluation page, containing a table of builds for that
evaluation."
  (define id        (evaluation-summary-id evaluation))
  (define total     (evaluation-summary-total evaluation))
  (define succeeded (evaluation-summary-succeeded evaluation))
  (define timestamp (evaluation-summary-start-time evaluation))
  (define evaltime  (evaluation-summary-completion-time evaluation))
  (define failed    (evaluation-summary-failed evaluation))
  (define scheduled (evaluation-summary-scheduled evaluation))

  (define duration  (- evaltime timestamp))
  (define spec      (evaluation-specification-name (db-get-evaluation id)))

  `((p (@ (class "lead"))
       "Evaluation #" ,(number->string id)
       ", " (a (@ (href "/jobset/" ,spec)) ,spec) " jobset")
    ,@(if (= timestamp 0)
          '()
          `((p ,(if (= evaltime 0)
                     (format #f "Evaluation started ~a."
                             (time->string timestamp))
                     (format #f "Completed ~a in ~a."
                             (time->string evaltime)
                             (seconds->string duration))))))
    ,(checkout-table checkouts channels)

    (p (@ (class "lead"))
       ,(format #f "~@[~a~] ~:[B~;b~]uilds"
                (and=> status string-capitalize)
                status)
       "  "
       (a (@ (class "oi oi-monitor mr-2")
             (style "font-size:0.8em")
             (href "/eval/" ,id "/dashboard")
             (role "button")))
       (a (@ (id "paginate")
             (class "oi oi-collapse-down")
             (style "font-size:0.7em")
             (href "")
             (role "button"))))
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
       (th (@ (scope "col")) "System"))))

  (define (table-row build)
    (define status
      (build-current-status build))

    `(tr
      (td (a (@ (class ,(status-class status))
                (title ,(status-title status))
                (aria-hidden "true")
                ,@(if (completed-with-logs? status)
                      `((href "/build/" ,(build-id build) "/log/raw"))
                      '()))
             ""))
      (th (@ (scope "row"))
          (a (@ (href "/build/" ,(build-id build) "/details"))
             ,(build-id build)))
      (td ,(build-specification-name build))
      (td ,(if (completed? status)
               (time->string (build-completion-time build))
               "—"))
      (td ,(build-job-name build))
      (td ,(build-nix-name build))
      (td ,(build-system build))))

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
         (let* ((build-ids (map build-id builds))
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
          (a (@ (href "/build/" ,(build-id build) "/details"))
             ,(build-id build)))
      (td ,(build-job-name build))
      (td ,(time->string
            (build-start-time build)))
      (td ,(build-system build))
      (td (a (@ (href "/build/" ,(build-id build) "/log/raw"))
             "raw"))))

  `((p (@ (class "lead")) "Running builds")
    (table
     (@ (class "table table-sm table-hover table-striped"))
     ,@(if (null? builds)
           `((th (@ (scope "col")) "No elements here."))
           `((thead (tr (th (@ (scope "col")) "ID")
                        (th (@ (scope "col")) "Job")
                        (th (@ (scope "col")) "Queued at")
                        (th (@ (scope "col")) "System")
                        (th (@ (scope "col")) "Log")))
             (tbody
              ,(map build-row builds)))))))

(define* (make-line-chart id datasets
                          #:key
                          (interpolation? #t)
                          (legend? #f)
                          (time-x-axes? #f)
                          xaxes-labels
                          x-label
                          y-label
                          (x-unit "day")
                          title
                          labels
                          colors)
  (let* ((normal-xAxes (vector `((type . "category")
                                 (labels . ,xaxes-labels)
                                 (display . #t)
                                 (scaleLabel
                                  . ((display . #t)
                                     (labelString . ,x-label))))))
         (time-xAxes (vector `((type . "time")
                               (time . ((unit . ,x-unit)))
                               (display . #t)
                               (distribution . "series")
                               (scaleLabel
                                . ((display . #t)
                                   (labelString . ,x-label))))))
         (scales  `((xAxes . ,(if time-x-axes?
                                  time-xAxes
                                  normal-xAxes))
                    (yAxes
                     . ,(vector `((display . #t)
                                  (scaleLabel
                                   . ((display . #t)
                                      (labelString . ,y-label))))))))
         (chart `((type . "line")
                  (data . ((datasets
                            . ,(apply vector
                                      (map (lambda (dataset label color)
                                             `((fill . #f)
                                               (label . ,label)
                                               ,@(if interpolation?
                                                     '()
                                                     '((lineTension . 0)))
                                               (borderColor . ,color)
                                               (data . ,dataset)))
                                           datasets labels colors)))))
                  (options . ((responsive . #t)
                              (tooltips . ((enabled . #f)))
                              (legend . ((display . ,legend?)))
                              (title . ((display . #f)
                                        (text . ,title)))
                              (scales . ,scales))))))
    `((script ,(format #f "window.addEventListener('load',
function(event) {\
window.~a = new Chart\
(document.getElementById('~a').getContext('2d'), ~a);\
});" id id (string-replace-substring
            (scm->json-string chart) "\"" "'"))))))

(define* (global-metrics-content #:key
                                 avg-eval-durations
                                 avg-eval-build-start-time
                                 builds-per-day
                                 builds-per-machine
                                 eval-completion-speed
                                 new-derivations-per-day
                                 pending-builds
                                 percentage-failed-eval)
  (define (avg-eval-duration-row . eval-durations)
    (let ((spec (match eval-durations
                  (((spec . _) . rest) spec))))
      `(tr (td ,spec)
           ,@(map (lambda (duration)
                    `(td ,(number->string
                           (nearest-exact-integer duration))))
                  (map cdr eval-durations)))))

  (define (percentage-failed-eval-row . percentages)
    (let ((spec (match percentages
                  (((spec . _) . rest) spec))))
      `(tr (td ,spec)
           ,@(map (lambda (duration)
                    `(td ,(number->string
                           (nearest-exact-integer duration))
                         "%"))
                  (map cdr percentages)))))

  (define (builds-per-machine-rows builds)
    (map (match-lambda
           ((field . value)
            `(tr (td ,field)
                 (td ,(number->string
                       (nearest-exact-integer value))))))
         builds))

  (define (builds->json-scm builds)
    (apply vector
           (map (match-lambda
                  ((field . value)
                   `((x . ,(* field 1000)) (y . ,value))))
                builds)))

  (define (evals->json-scm evals)
    (apply vector
           (map (match-lambda
                  ((field . value)
                   `((x . ,(number->string field)) (y . ,value))))
                evals)))

  (define (evals->labels evals)
    (apply vector
           (map (match-lambda
                  ((field . value) field))
                evals)))

  (let ((builds-chart "builds_per_day")
        (build-start-chart "avg_eval_build_start_time")
        (evaluation-speed-chart "eval_speed_chart")
        (pending-builds-chart "pending_builds"))
    `((p (@ (class "lead")) "Global metrics")
      (h6 "Average evaluation duration per specification (seconds).")
      (table
       (@ (class "table table-sm table-hover table-striped"))
       (thead (tr (th (@ (scope "col")) "Specification")
                  (th (@ (scope "col")) "10 last evaluations")
                  (th (@ (scope "col")) "100 last evaluations")
                  (th (@ (scope "col")) "All evaluations")))
       (tbody
        ,(apply map avg-eval-duration-row avg-eval-durations)))
      (br)
      (h6 "Builds completion.")
      (p "This shows the difference between newly added derivations and built
derivations per day.")
      (canvas (@ (id ,builds-chart)) "")
      (br)
      (h6 "Evaluation average build start time.")
      (p "This is the average time required for an evaluation to start its
builds.")
      (br)
      (canvas (@ (id ,build-start-chart)) "")
      (br)
      (h6 "Evaluation completion speed.")
      (p "The evaluation completion speed is the sum of an evaluation
completed builds divided by the time required to build them.")
      (br)
      (canvas (@ (id ,evaluation-speed-chart)) "")
      (br)
      (h6 "Pending builds.")
      (p "This is the sum of all the currently pending builds.")
      (br)
      (canvas (@ (id ,pending-builds-chart)) "")
      (br)
      (h6 "Builds per machine.")
      (p "This is the builds count per machine during the last day.")
      (table
       (@ (class "table table-sm table-hover table-striped"))
       (thead (tr (th (@ (scope "col")) "Machine")
                  (th (@ (scope "col")) "Builds (last 24 hours)")))
       (tbody
        ,(builds-per-machine-rows builds-per-machine)))
      (br)
      (h6 "Percentage of failed evaluations.")
      (table
       (@ (class "table table-sm table-hover table-striped"))
       (thead (tr (th (@ (scope "col")) "Specification")
                  (th (@ (scope "col")) "10 last evaluations")
                  (th (@ (scope "col")) "100 last evaluations")
                  (th (@ (scope "col")) "All evaluations")))
       (tbody
        ,(apply map percentage-failed-eval-row percentage-failed-eval)))
      ;; Scripts.
      ,@(make-line-chart builds-chart
                         (list (builds->json-scm new-derivations-per-day)
                               (builds->json-scm builds-per-day))
                         #:interpolation? #f
                         #:time-x-axes? #t
                         #:x-label "Day"
                         #:y-label "Builds"
                         #:title "Builds per day"
                         #:legend? #t
                         #:labels '("New derivations"
                                    "Builds completed")
                         #:colors (list "#f6dd27" "#3e95cd"))
      ,@(make-line-chart build-start-chart
                         (list (evals->json-scm avg-eval-build-start-time))
                         #:xaxes-labels (evals->labels
                                         avg-eval-build-start-time)
                         #:x-label "Evaluations"
                         #:y-label "Time (s)"
                         #:title "Evaluation average build start time"
                         #:labels '("Build start time")
                         #:colors (list "#3e95cd"))
      ,@(make-line-chart evaluation-speed-chart
                         (list (evals->json-scm eval-completion-speed))
                         #:xaxes-labels (evals->labels
                                         eval-completion-speed)
                         #:x-label "Evaluations"
                         #:y-label "Speed (builds/hour)"
                         #:title "Evaluation completion speed"
                         #:labels '("Completion speed")
                         #:colors (list "#3e95cd"))
      ,@(make-line-chart pending-builds-chart
                         (list (builds->json-scm pending-builds))
                         #:time-x-axes? #t
                         #:x-label "Day"
                         #:y-label "Builds"
                         #:title "Pending builds"
                         #:labels '("Pending builds")
                         #:colors (list "#3e95cd")))))

(define system->color-class
  (let ((alist (map cons
                    %supported-systems
                    (circular-list "" "bg-success"
                                   "bg-info" "bg-warning"
                                   "bg-danger"))))
    (lambda (system)
      "Return a Bootstrap color class for SYSTEM, a system type such as
\"x86_64-linux\"."
      (or (assoc-ref alist system) ""))))

(define (workers-status workers builds percentages)
  (define (machine-row machine)
    (let* ((workers (sort (filter (lambda (worker)
                                    (string=? (worker-machine worker)
                                              machine))
                                  workers)
                          (lambda (w1 w2)
                            (string<? (worker-name w1)
                                      (worker-name w2))))))
      `(div (@ (class "col-sm-4 mt-3"))
            (a (@(href "/machine/" ,machine))
               (h6 ,machine))
            ,(map (lambda (worker)
                    (define build
                      (find (lambda (build)
                              (and (build-worker build)
                                   (string=? (build-worker build)
                                             (worker-name worker))))
                            builds))

                    (define percentage
                      (and build (assq-ref percentages build)))

                    (let ((style (format #f
                                         "width: ~a%"
                                         (if build
                                             percentage
                                             0))))
                      `(div (@ (class "progress mt-1")
                               (style "height: 20px"))
                            (div (@ (class
                                      "progress-bar "
                                      ,(system->color-class
                                        (if build
                                            (build-system build)
                                            (first (worker-systems worker)))))
                                    (role "progressbar")
                                    (style ,style)
                                    (aria-valuemin "0")
                                    (aria-valuemax "100"))
                                 ,(if build
                                      `(strong
                                        (@ (class "justify-content-center
d-flex position-absolute w-100"))
                                        (a (@ (class "text-dark text-truncate")
                                              (style "max-width: 150px")
                                              (href "/build/"
                                                    ,(build-id build)
                                                    "/details"))
                                           ,(build-job-name build)))
                                      '(em
                                        (@ (class "justify-content-center
text-dark d-flex position-absolute w-100"))
                                        "idle"))))))
                  workers))))

  (let ((machines (reverse
                   (sort (delete-duplicates
                          (map worker-machine workers))
                         version>?))))
    `((p (@ (class "lead")) "Workers status")
      (div (@ (class "container"))
           (div (@ (class "row"))
                ,@(map machine-row machines))))))

(define* (machine-status name workers builds info)
  (define (history->json-scm history)
    (apply vector
           (map (match-lambda
                  ((field . value)
                   `((x . ,(* field 1000)) (y . ,value))))
                history)))

  (define (ram-available->json-scm history)
    (apply vector
           (map (match-lambda
                  ((field . value)
                   `((x . ,(* field 1000))
                     (y . ,(/ value (expt 2 30))))))
                history)))

  `((p (@ (class "lead")) "Machine " ,name)
    ,@(if (null? info)
          '()
          `((table
             (@ (class "table table-sm table-hover table-striped"))
             (tbody
              (tr (th "Hostname")
                  (td ,(assq-ref info #:hostname)))
              (tr (th "Info")
                  (td ,(assq-ref info #:info)))
              (tr (th "Boot time")
                  (td ,(time->string
                        (assq-ref info #:boottime))))
              (tr (th "Total RAM")
                  (td ,(assq-ref info #:ram)))
              (tr (th "Total root disk space")
                  (td ,(assq-ref info #:root-space)))
              (tr (th "Total store disk space")
                  (td ,(assq-ref info #:store-space)))))))
    (h6 "Workers")
    (table
     (@ (class "table table-sm table-hover table-striped"))
     ,@(if (null? workers)
           `((th (@ (scope "col")) "No elements here."))
           `((thead
              (tr
               (th (@ (scope "col")) "Name")
               (th (@ (scope "col")) "Systems")
               (th (@ (scope "col")) "Building")
               (th (@ (scope "col")) "Last seen")))
             (tbody
              ,@(map
                 (lambda (worker build)
                   `(tr (td ,(worker-name worker))
                        (td ,(string-join (worker-systems worker)
                                          ", "))
                        (td ,(match build
                               (() "idle")
                               ((build _ ...)
                                `(a (@ (class "text-truncate")
                                       (style "max-width: 150px")
                                       (href "/build/"
                                             ,(build-id build)
                                             "/details"))
                                    ,(build-job-name build)))))
                        (td ,(time->string
                              (worker-last-seen worker)))))
                 workers builds)))))
    ,@(if (null? info)
          '((div (@ (class "alert alert-danger"))
                 "Could not find machine information using Zabbix."))
          `((h6 "CPU idle time")
            ,@(let ((cpu-idle (assq-ref info #:cpu-idle))
                    (cpu-idle-chart "cpu_idle_chart"))
                `((br)
                  (canvas (@ (id ,cpu-idle-chart)) "")
                  ,@(make-line-chart cpu-idle-chart
                                     (list (history->json-scm cpu-idle))
                                     #:time-x-axes? #t
                                     #:x-label "Time"
                                     #:y-label "Percentage"
                                     #:x-unit "minute"
                                     #:title "CPU idle time"
                                     #:labels '("CPU idle time")
                                     #:colors (list "#3e95cd"))))
            (br)
            (h6 "Available memory")
            ,@(let ((ram-available (assq-ref info #:ram-available))
                    (ram-available-chart "ram_available_chart"))
                `((br)
                  (canvas (@ (id ,ram-available-chart)) "")
                  ,@(make-line-chart ram-available-chart
                                     (list
                                      (ram-available->json-scm ram-available))
                                     #:time-x-axes? #t
                                     #:x-label "Time"
                                     #:y-label "GiB"
                                     #:x-unit "minute"
                                     #:title
                                     "Available memory"
                                     #:labels
                                     '("Available memory")
                                     #:colors (list "#3e95cd"))))
            (br)
            (h6 "Free store disk space percentage")
            ,@(let ((store-free (assq-ref info #:store-free))
                    (store-free-chart "store_free_chart"))
                `((br)
                  (canvas (@ (id ,store-free-chart)) "")
                  ,@(make-line-chart store-free-chart
                                     (list (history->json-scm store-free))
                                     #:time-x-axes? #t
                                     #:x-label "Time"
                                     #:y-label "Percentage"
                                     #:x-unit "minute"
                                     #:title
                                     "Free store disk space percentage"
                                     #:labels
                                     '("Free store disk space percentage")
                                     #:colors (list "#3e95cd"))))))))

(define* (evaluation-dashboard evaluation systems
                               #:key
                               (checkouts (evaluation-checkouts evaluation))
                               channels
                               current-system
                               dashboard-id
                               names
                               prev-eval
                               next-eval)
  (define id
    (evaluation-id evaluation))
  (define time
    (evaluation-completion-time evaluation))

  (let ((jobs
         (if names
             (format #f "/api/jobs?evaluation=~a&names=~a"
                     id names)
             (format #f "/api/jobs?evaluation=~a&system=~a"
                     id current-system))))
    `((nav
       (@ (aria-label "Evaluation navigation")
          (class "eval-nav"))
       (ul (@ (class "pagination pagination-sm"))
           (li
            (p (@ (class "lead mb-0 mr-3"))
               "Dashboard for "
               (a (@ (href ,(string-append "/eval/"
                                           (number->string id))))
                  "evaluation #" ,(number->string id))))
           (li (@ (class
                    ,(string-append "page-item "
                                    (if prev-eval
                                        ""
                                        "disabled"))))
               ,(let ((name "Previous evaluation"))
                  `(a
                    (@ (id "dashboard-prev-link")
                       (class "page-link")
                       (title ,name)
                       (aria-label ,name)
                       (href
                        ,(if prev-eval
                             (format #f "/eval/~a/dashboard~a"
                                     prev-eval
                                     (if dashboard-id
                                         (format #f "/~a" dashboard-id)
                                         ""))
                             "#")))
                    "«")))
           (li (@ (class
                    ,(string-append "page-item "
                                    (if next-eval
                                        ""
                                        "disabled"))))
               ,(let ((name "Next evaluation"))
                  `(a
                    (@ (id "dashboard-next-link")
                       (class "page-link")
                       (title ,name)
                       (aria-label ,name)
                       (href
                        ,(if next-eval
                             (format #f "/eval/~a/dashboard~a"
                                     next-eval
                                     (if dashboard-id
                                         (format #f "/~a" dashboard-id)
                                         ""))
                             "#")))
                    "»")))))
      (details
       (summary ,(format #f "Evaluation completed ~a."
                         (time->string time)))
       ,(checkout-table checkouts channels))

      (form (@ (id "get-dashboard")
               (class
                 ,(string-append "row g-3 mb-3 "
                                 (if names
                                     "d-none"
                                     "")))
               (action "/eval/" ,id "/dashboard")
               (method "GET"))
            (div (@ (class "col-auto"))
                 (select (@ (id "system")
                            (name "system")
                            (class "form-control"))
                         ,@(map (lambda (system)
                                  `(option
                                    (@ (value ,system)
                                       ,@(if (string=? system current-system)
                                             '((selected))
                                             '()))
                                    ,system))
                                systems)))
            (div (@ (class "col-auto"))
                 (button
                  (@ (id "load-btn")
                     (class "btn btn-primary")
                     (type "submit")
                     (disabled))
                  (span
                   (@ (class "spinner-border spinner-border-sm")
                      (role "status")
                      (aria-hidden "true"))
                   "")
                  " Loading")))
      (div
       (@ (class "input-group")
          (role "search")
          (style "width:15em"))
       (label (@ (for "query-jobs")
                 (class "sr-only"))
              "Search for jobs")
       (input (@ (type "text")
                 (class "form-control")
                 (id   "query-jobs")
                 (name "query-jobs")
                 (placeholder "search for jobs"))))

      (br)
      (div (@ (id "dashboard")
              (class "invisible")
              (url ,jobs))))))

(define* (badge-svg spec badge-string summary
                    #:key type)
  "Return the badge SVG for the specification with the given SUMMARY.  The
BADGE-STRING procedure takes a badge name as input an returns the badge
content as a string."
  (define (replace str patterns)
    (match patterns
      (() str)
      (((in out) . rest)
       (replace (string-replace-substring str in out) rest))))

  (if summary
      (let* ((succeeded
              (evaluation-summary-succeeded summary))
             (total
              (evaluation-summary-total summary))
             (percentage
              (nearest-exact-integer
               (* 100 (/ succeeded total))))
             (percentage-str
              (string-append
               (number->string percentage) "%")))
        (case type
          ((0)
           (replace (badge-string "badge-percentage.svg")
             `(("X%" ,percentage-str))))
          (else
           (replace (badge-string "badge-spec-percentage.svg")
             `(("X%" ,percentage-str)
               ("_name" ,spec))))))
      (badge-string "badge-error.svg")))

(define (javascript-licenses)
  "Return the Javascript licenses table, for compatibility with LibreJS. See:
https://www.gnu.org/software/librejs/free-your-javascript.html."
  '((table
     (@ (id "jslicense-labels1"))
     (tr
      (td (a (@ (href "/static/js/popper.min.js")) "popper.min.js"))
      (td (a (@ (href "https://github.com/popperjs/popper-core/blob/master/LICENSE.md"))
             "Expat"))
      (td (a (@ (href "/static/js/popper.min.js")) "popper.js") ))
     (tr
      (td (a (@ (href "/static/js/datatables.min.js")) "datatables.min.js"))
      (td (a (@ (href "https://datatables.net/license/mit")) "Expat"))
      (td (a (@ (href "https://cdn.datatables.net/1.10.24/js/dataTables.bootstrap.js"))
             "dataTables.bootstrap.js")))
     (tr
      (td (a (@ (href "/static/js/d3.v6.min.js")) "d3.v6.min.js"))
      (td (a (@ (href "https://raw.githubusercontent.com/d3/d3/main/LICENSE"))
             "BSD-3-Clause"))
      (td (a (@ (href "https://github.com/d3/d3/releases/tag/v6.6.2"))
             "d3.js")))
     (tr
      (td (a (@ (href "/static/js/choices.min.js")) "choices.min.js"))
      (td (a (@ (href "https://github.com/Choices-js/Choices/blob/master/LICENSE"))
             "Expat"))
      (td (a (@ (href "http://libs.wware.org/choices.js/9.0.1/scripts/choices.js"))
             "choices.js")))
     (tr
      (td (a (@ (href "/static/js/chart.js")) "chart.js"))
      (td (a (@ (href "https://github.com/chartjs/Chart.js/blob/master/LICENSE.md"))
             "Expat"))
      (td (a (@ (href "https://github.com/chartjs/Chart.js/releases/tag/v2.9.3"))
             "Chart.js")))
     (tr
      (td (a (@ (href "/static/js/bootstrap.min.js")) "bootstrap.min.js"))
      (td (a (@ (href "https://github.com/twbs/bootstrap/blob/master/LICENSE"))
             "Expat"))
      (td (a (@ (href "https://github.com/twbs/bootstrap/releases/download/v4.2.1/bootstrap-4.2.1-dist.zip"))
             "bootstrap.js"))))))
