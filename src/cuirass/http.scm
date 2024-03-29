;;;; http.scm -- HTTP API
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017, 2020, 2021 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2018-2020, 2023-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Tatiana Sholokhova <tanja201396@gmail.com>
;;; Copyright © 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (cuirass http)
  #:use-module (cuirass base)
  #:use-module (cuirass config)
  #:use-module (cuirass database)
  #:use-module ((cuirass base) #:select (evaluation-log-file))
  #:use-module (cuirass metrics)
  #:use-module (cuirass utils)
  #:use-module (cuirass logging)
  #:use-module (cuirass notification)
  #:use-module (cuirass remote)
  #:use-module (cuirass rss)
  #:use-module (cuirass specification)
  #:use-module (cuirass zabbix)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:use-module (json)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module ((web server) #:hide (run-server))
  #:use-module (web uri)
  #:use-module (fibers)
  #:use-module (fibers channels)
  #:use-module ((rnrs bytevectors) #:select (utf8->string))
  #:use-module (sxml simple)
  #:use-module (cuirass templates)
  #:use-module (guix channels)
  #:use-module (guix packages)
  #:use-module (guix progress)
  #:use-module (guix utils)
  #:use-module ((guix store) #:select (%store-prefix))
  #:use-module (guix build union)
  #:export (run-cuirass-server))

(define %static-directory
  ;; Define to the static file directory.
  (make-parameter (string-append
                   (or (getenv "CUIRASS_DATADIR")
                       (string-append %datadir "/" %package))
                   "/static")))

(define %page-size 10)

(define %file-mime-types
  '(("css" . (text/css))
    ("otf" . (font/otf))
    ("woff" . (font/woff))
    ("js"  . (text/javascript))
    ("png" . (image/png))
    ("gif" . (image/gif))
    ("html" . (text/html))))

(define %file-white-list
  '("css/bootstrap.min.css"
    "css/choices.min.css"
    "css/datatables.css"
    "css/cuirass.css"
    "css/open-iconic-bootstrap.css"
    "fonts/open-iconic.otf"
    "fonts/open-iconic.woff"
    "images/icon.png"
    "images/guix.png"
    "js/chart.js"
    "js/cuirass.js"
    "js/build-log.js"
    "js/d3.v6.min.js"
    "js/datatables.min.js"
    "js/jquery-3.3.1.min.js"
    "js/bootstrap.min.js"
    "js/choices.min.js"))

(define (build->hydra-build build)
  "Convert BUILD to an assoc list matching hydra API format."
  (define (bool->int bool)
    (if bool 1 0))

  (define finished?
    (>= (build-current-status build) 0))

  `((id . ,(build-id build))
    (evaluation . ,(build-evaluation-id build))
    (jobset . ,(build-specification-name build))
    (job . ,(build-job-name build))

    ;; Hydra's API uses "timestamp" as the time of the last useful event for
    ;; that build: evaluation or completion.
    (timestamp . ,(if finished?
                      (build-completion-time build)
                      (build-creation-time build)))

    (starttime . ,(build-start-time build))
    (stoptime . ,(build-completion-time build))
    (derivation . ,(build-derivation build))
    (buildoutputs . ,(map (lambda (output)
                            (list (output-name output)
                                  (cons "path"
                                        (output-item output))))
                          (build-outputs build)))
    (system . ,(build-system build))
    (nixname . ,(build-nix-name build))
    (buildstatus . ,(build-current-status build))
    (weather . ,(build-current-weather build))
    (busy . ,(bool->int (eqv? (build-status started)
                              (build-current-status build))))
    (priority . ,(build-priority build))
    (finished . ,(bool->int finished?))
    (buildproducts . ,(list->vector
                       (map (lambda (product)
                              `((id . ,(build-product-id product))
                                (type . ,(build-product-type product))
                                (path . ,(build-product-file product))
                                (file-size . ,(build-product-file-size product))))
                            (build-products build))))))

(define (checkout->json-object checkout)
  "Return an alist suitable for 'json->scm' representing CHECKOUT,
a <checkout> record."
  `((commit . ,(checkout-commit checkout))
    (channel . ,(checkout-channel checkout))
    (directory . ,(checkout-directory checkout))))

(define (evaluation->json-object evaluation)
  "Turn EVALUATION into a representation suitable for 'json->scm'."
  `((id . ,(evaluation-id evaluation))
    (specification . ,(evaluation-specification-name evaluation))
    (status . ,(evaluation-current-status evaluation))
    (timestamp . ,(evaluation-completion-time evaluation))
    (checkouttime . ,(evaluation-checkout-time evaluation))
    (evaltime . ,(evaluation-start-time evaluation))
    (checkouts
     . ,(list->vector
         (map checkout->json-object

              ;; Get the complete lists of checkouts, not just those that are
              ;; different from the previous evaluation.
              (let* ((name (evaluation-specification-name evaluation))
                     (id (evaluation-id evaluation))
                     (spec (db-get-specification name)))
                (latest-checkouts spec id)))))))

(define (specification->json-object spec)
  "Turn SPEC into a representation suitable for 'json->scm'."
  (define (channel->json-object channel)
    `((name . ,(channel-name channel))
      (url . ,(channel-url channel))
      (branch . ,(channel-branch channel))
      (commit . ,(channel-commit channel))))

  (define (build-output->json-object build-output)
    `((job . ,(build-output-job build-output))
      (type . ,(build-output-type build-output))
      (output . ,(build-output-output build-output))
      (path . ,(build-output-path build-output))))

  (define (notification->json-object notif)
    (cond
     ((email? notif)
      `((type . email)
        (from . ,(email-from notif))
        (to . ,(email-to notif))
        (server . ,(email-server notif))))
     ((mastodon? notif)
      `((type . mastodon)))))

  `((name . ,(specification-name spec))
    (build . ,(match (specification-build spec)
                ((? symbol? subset)
                 subset)
                (('packages packages ...)
                 `((packages . ,(list->vector packages))))
                (('channels channels ...)
                 `((channels . ,(list->vector channels))))
                (('manifests manifests ...)
                 `((manifests . ,(list->vector manifests))))
                (_
                 'custom)))
    (channels . ,(list->vector
                  (map channel->json-object
                       (specification-channels spec))))
    (build-outputs . ,(list->vector
                       (map build-output->json-object
                            (specification-build-outputs spec))))
    (notifications . ,(list->vector
                       (map notification->json-object
                            (specification-notifications spec))))
    (period . ,(specification-period spec))
    (priority . ,(specification-priority spec))
    (systems . ,(list->vector
                 (specification-systems spec)))))

(define (jobs-history->json-object history)
  "Turn HISTORY into a representation suitable for 'json->scm'."
  (scm->json-string
   (list->vector
    (map (lambda (eval)
           `((evaluation . ,(assq-ref eval 'evaluation))
             (checkouts . ,(list->vector
                            (map checkout->json-object
                                 (assq-ref eval 'checkouts))))
             (jobs . ,(list->vector
                       (assq-ref eval 'jobs)))))
         history))))

(define (handle-build-request build-id)
  "Retrieve build identified by BUILD-ID over the database and convert it to
hydra format. Return #f is not build was found."
  (let ((build (db-get-build build-id)))
    (and=> build build->hydra-build)))

(define (handle-builds-request filters)
  "Retrieve all builds matched by FILTERS in the database and convert them to
Hydra format."
  (let ((builds (with-time-logging "builds request"
                                   (db-get-builds filters))))
    (list->vector (map build->hydra-build builds))))

(define (handle-builds-search-request filters)
  "Retrieve all builds matched by FILTERS in the database and convert them to
Hydra format."
  (let ((builds (with-time-logging "builds search request"
                                   (db-get-builds-by-search filters))))
    (list->vector (map build->hydra-build builds))))

(define (request-parameters request)
  "Parse the REQUEST query parameters and return them under the form
  '((parameter . value) ...)."
  (let* ((uri (request-uri request))
         (query (uri-query uri)))
    (if query
        (fold (lambda (param params)
               (match (string-split param #\=)
                 ((key param)
                  (let ((key-symbol (string->symbol key)))
                    (cons (cons key-symbol
                                (match key-symbol
                                  ('id (string->number param))
                                  ('nr (string->number param))
                                  (_   param)))
                          params)))
                 (_ params)))
              '()
              (string-split query #\&))
        '())))


;;;
;;; HTML rendering.
;;;

(define* (evaluation-html-page evaluation
                               #:key
                               status
                               (paginate? #t)
                               border-high-time border-low-time
                               border-high-id border-low-id)
  "Return the HTML page representing EVALUATION."
  (define id             (evaluation-summary-id evaluation))
  (define builds-id-max  (db-get-builds-max id status))
  (define builds-id-min  (db-get-builds-min id status))
  (define specification  (db-get-evaluation-specification id))
  (define specification* (db-get-specification specification))
  (define channels       (specification-channels specification*))
  (define checkouts      (latest-checkouts specification* id))
  (define checkout-changes (evaluation-checkouts (db-get-evaluation id)))

  (define builds
    (with-time-logging
     "builds request for evaluation page"
     (db-get-builds
      `((evaluation . ,id)
        (status . ,(match (and=> status string->symbol)
                     ('newly-failed 'failed)
                     (status status)))
        ,@(if (and status (string=? status "newly-failed"))
              `((weather . new-failure))
              '())
        ,@(if paginate?
              `((nr . ,%page-size)
                (border-high-time . ,border-high-time)
                (border-low-time . ,border-low-time)
                (border-high-id . ,border-high-id)
                (border-low-id . ,border-low-id))
              '())
        (order . finish-time+build-id)))))

  (html-page
   "Evaluation"
   (evaluation-build-table evaluation
                           #:channels channels
                           #:checkouts checkouts
                           #:checkout-changes checkout-changes
                           #:status status
                           #:builds builds
                           #:builds-id-min builds-id-min
                           #:builds-id-max builds-id-max)
   `(((#:name . ,specification)
      (#:link . ,(string-append "/jobset/" specification)))
     ((#:name . ,(string-append "Evaluation " (number->string id)))
      (#:link . ,(string-append "/eval/" (number->string id)))))))

(define* (metrics-page)
  (html-page
   "Global metrics"
   (global-metrics-content
    #:avg-eval-durations
    (list
     (db-get-metrics-with-id
      'average-10-last-eval-duration-per-spec)
     (db-get-metrics-with-id
      'average-100-last-eval-duration-per-spec)
     (db-get-metrics-with-id
      'average-eval-duration-per-spec))
    #:avg-eval-build-start-time
    (db-get-metrics-with-id 'average-eval-build-start-time
                            #:limit 100
                            #:order "cast(field as int) ASC")
    #:builds-per-day
    (db-get-metrics-with-id 'builds-per-day
                            #:limit 100)
    #:builds-per-machine
    (db-get-metrics-with-id 'builds-per-machine-per-day
                            #:order "field ASC")
    #:eval-completion-speed
    (db-get-metrics-with-id 'evaluation-completion-speed
                            #:limit 100
                            #:order "cast(field as int) ASC")
    #:new-derivations-per-day
    (db-get-metrics-with-id 'new-derivations-per-day
                            #:limit 100)
    #:pending-builds
    (db-get-metrics-with-id 'pending-builds
                            #:limit 100)
    #:percentage-failed-eval
    (list
     (db-get-metrics-with-id
      'percentage-failure-10-last-eval-per-spec)
     (db-get-metrics-with-id
      'percentage-failure-100-last-eval-per-spec)
     (db-get-metrics-with-id
      'percentage-failed-eval-per-spec)))
   '()))

(define (machine-page name)
  (define zabbix-info
    (if (zabbix-available?)
        (with-zabbix-connection
         (let* ((host-id        (zabbix-host-id name))
                (enabled?       (zabbix-host-enabled? name))
                (value          (cut zabbix-item-value <> host-id))
                (history        (lambda (key type)
                                  (zabbix-history
                                   (zabbix-item-id key host-id)
                                   #:limit 100
                                   #:type type))))
           (if enabled?
               `((#:hostname . ,(value "system.hostname"))
                 (#:info . ,(value "system.uname"))
                 (#:boottime . ,(string->number
                                 (value "system.boottime")))
                 (#:ram . ,(byte-count->string
                            (string->number
                             (value "vm.memory.size[total]"))))
                 (#:root-space . ,(byte-count->string
                                   (string->number
                                    (value "vfs.fs.size[/,total]"))))
                 (#:store-space
                  . ,(byte-count->string
                      (string->number
                       (value "vfs.fs.size[/gnu/store,total]"))))
                 (#:cpu-idle . ,(history "system.cpu.util[,idle]" 'float))
                 (#:ram-available . ,(history "vm.memory.size[available]"
                                              'unsigned))
                 (#:store-free . ,(history "vfs.fs.size[/gnu/store,pfree]"
                                           'float)))
               '())))
        '()))

  (let ((builds (db-get-builds `((status . started)
                                 (order . status+submission-time))))
        (workers (filter (lambda (worker)
                           (string=? name (worker-machine worker)))
                         (db-get-workers))))
    (html-page
     name
     (machine-status name workers
                     (map (lambda (worker)
                            (filter (lambda (build)
                                      (string=? (build-worker build)
                                                (worker-name worker)))
                                    builds))
                          workers)
                     zabbix-info)
     '())))

(define (body->specification body)
  "Turn BODY containing the input parameters of an HTML specification form
into a specification record and return it."
  (let* ((query (utf8->string body))
         (params (fold-right
                  (lambda (param params)
                    (match (string-split param #\=)
                      ((key param)
                       (cons (cons (string->symbol key) (uri-decode param))
                             params))))
                  '()
                  (string-split query #\&)))
         (filter-field (lambda (field)
                         (filter-map (match-lambda
                                       ((key . param)
                                        (and (eq? key field) param)))
                                     params)))
         (name (assq-ref params 'name))
         (build (string->symbol
                 (assq-ref params 'build)))
         (build-params (or (and (assq-ref params 'param-select)
                                (map string->symbol
                                     (filter-field 'param-select)))
                           (let ((param (assq-ref params 'param-input)))
                             (and param
                                  (not (string=? param ""))
                                  (let ((param (string-split param #\,)))
                                    (cond
                                     ((eq? build 'custom)
                                      (map
                                       (cut with-input-from-string <> read)
                                       param))
                                     (else
                                      param)))))))
         (channels (map (lambda (name url branch)
                          (channel
                           (name (string->symbol name))
                           (url url)
                           (branch branch)))
                        (filter-field 'channel-name)
                        (filter-field 'channel-url)
                        (filter-field 'channel-branch)))
         (period (string->number
                    (assq-ref params 'period)))
         (priority (string->number
                    (assq-ref params 'priority)))
         (systems (fold
                   (lambda (system systems)
                     (if (assoc (string->symbol system) params)
                         (cons system systems)
                         systems))
                   '()
                   %cuirass-supported-systems)))
    (specification
     (name name)
     (build (if build-params
                (cons build build-params)
                build))
     (channels channels)
     (period period)
     (priority priority)
     (systems systems))))

(define* (dashboard-page evaluation-id
                         #:key dashboard-id system)
  "Return a dashboard page for the evaluation EVALUATION-ID.  If DASHBOARD-ID
is passed, only display jobs registered for this DASHBOARD-ID.  If SYSTEM is
passed, only display JOBS targeting this SYSTEM."
  (let* ((spec-name (db-get-evaluation-specification evaluation-id))
         (spec (db-get-specification spec-name))
         (channels (specification-channels spec))
         (checkouts (latest-checkouts spec evaluation-id))
         (systems (specification-systems spec))
         (default-system
           (if (member "x86_64-linux" systems)
               "x86_64-linux"
               (car systems)))
         (dashboard (db-get-dashboard dashboard-id))
         (names (and dashboard
                     (dashboard-job-ids dashboard)))
         (prev (db-get-previous-eval evaluation-id))
         (next (db-get-next-eval evaluation-id)))
    (html-page
     "Dashboard"
     (evaluation-dashboard (db-get-evaluation evaluation-id)
                           systems
                           #:channels channels
                           #:checkouts checkouts
                           #:current-system
                           (or system default-system)
                           #:dashboard-id dashboard-id
                           #:names names
                           #:prev-eval prev
                           #:next-eval next)
     `(((#:name . ,spec-name)
        (#:link . ,(string-append "/jobset/" spec-name)))
       ((#:name . ,(string-append "Evaluation "
                                  (number->string evaluation-id)))
        (#:link . ,(string-append "/eval/" (number->string evaluation-id)))))
     #:margin? #f)))

(define (badge-string name)
  "Return the content of the badge file with the given NAME as a string."
  (call-with-input-file
      (string-append (%static-directory) "/images/" name)
    get-string-all))


;;;
;;; Web server.
;;;
;;; The api is derived from the hydra one. It is partially described here :
;;;
;;; https://github.com/NixOS/hydra/blob/master/doc/manual/api.xml
;;;

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define %static-file-ttl
  ;; Time-to-live (in seconds) advertised for files under /static.
  (* 12 3600))

(define (url-handler bridge request body)

  (define* (respond response #:key body)
    (values response body #f))

  (define-syntax-rule (respond-json body ...)
    (respond '((content-type . (application/json)))
             #:body body ...))

  (define-syntax-rule (respond-text body ...)
    (respond '((content-type . (text/plain)))
             #:body body ...))

  (define-syntax-rule (respond-svg body ...)
    (respond '((content-type . (image/svg+xml)))
             #:body body ...))

  (define-syntax-rule (respond-json-with-error error-code message)
    (respond
     (build-response #:headers '((content-type . (application/json)))
                     #:code error-code)
     #:body
     (scm->json-string
      `((error . ,message)))))

  (define (redirect ref)
    (let ((uri (string->uri-reference ref)))
      (respond (build-response #:headers `((location . ,uri))
                               #:code 302)
               #:body "Redirected...")))

  (define* (respond-html body #:key code)
    (respond
     (let ((content-type '((content-type . (text/html)))))
       (if code
           (build-response #:headers content-type #:code code)
           content-type))
     #:body
     (lambda (port)
       (format port "<!DOCTYPE html>")
       (sxml->xml body port))))

  (define* (respond-rss body #:key code)
    (respond
     (let ((content-type '((content-type . (application/rss+xml)))))
       (if code
           (build-response #:headers content-type #:code code)
           content-type))
     #:body
     (lambda (port)
       (sxml->xml body port))))

  (define* (respond-file file #:key ttl)
    (let ((content-type (or (assoc-ref %file-mime-types
                                       (file-extension file))
                            '(application/octet-stream))))
      (respond `((content-type . ,content-type)
                 (content-disposition
                  . (form-data (filename . ,(basename file))))
                 ,@(if ttl
                       `((cache-control . ((max-age . ,ttl))))
                       '())
                 (x-raw-file . ,file)))))

  (define (respond-static-file path)
    ;; PATH is a list of path components
    (let ((file-name (string-join path "/"))
          (file-path (string-join (cons* (%static-directory) path) "/")))
      (if (and (member file-name %file-white-list)
               (file-exists? file-path)
               (not (file-is-directory? file-path)))
          (respond-file file-path
                        #:ttl %static-file-ttl)
          (respond-not-found file-name))))

  (define (respond-compressed-file file)
    ;; Return FILE with 'gzip' or 'bzip2' content-encoding.
    (let ((encoding
           (cond ((string-suffix? ".gz" file)
                  '((content-type . (text/plain (charset . "UTF-8")))
                    (content-encoding . (gzip))))
                 ((string-suffix? ".bz2" file)
                  '((content-type . (application/bzip2
                                     (charset . "ISO-8859-1")))))
                 (else '()))))
      (respond `(,@encoding
                 (content-disposition . (inline))
                 (x-raw-file . ,file)))))

  (define (respond-dashboard-not-found dashboard-id)
    (respond-json-with-error
     404
     (format #f "Dashboard with ID ~a doesn't exist." dashboard-id)))

  (define (respond-build-not-found build-id)
    (respond-json-with-error
     404
     (format #f "Build with ID ~a doesn't exist." build-id)))

  (define (respond-output-not-found output-id)
    (respond-json-with-error
     404
     (format #f "Output with ID ~a doesn't exist." output-id)))

  (define (respond-html-eval-not-found eval-id)
    (respond-html
     (html-page "Page not found"
                (format #f "Evaluation with ID ~a doesn't exist." eval-id)
                '())
     #:code 404))

  (define (respond-not-found resource_name)
    (respond (build-response #:code 404)
             #:body (string-append "Resource not found: "
                                   resource_name)))

  (define (job->alist job)
    ;; Convert JOB to an alist representation suitable for JSON conversion.
    `((build . ,(job-build-id job))
      (status . ,(job-status job))
      (name . ,(job-name job))))

  (log-info "~a ~a" (request-method request)
            (uri-path (request-uri request)))

  (match (cons (request-method request)
               (request-path-components request))
    (('POST "admin" "specification" "add")
     (let* ((spec (body->specification body))
            (name (specification-name spec)))
       (if (db-get-specification name)
           (respond-html
            (html-page
             "Creation error"
             `(div (@ (class "alert alert-danger"))
                   ,(format #f "Specification ~a already exists" name))
             '())
            #:code 400)
           (begin
             (db-add-or-update-specification spec)

             (if bridge
                 (begin
                   ;; Notify the jobset registry in the 'cuirass register' process.
                   (write `(register-jobset ,(specification-name spec))
                          bridge)
                   (newline bridge))
                 (log-warning
                  "cannot notify bridge of the addition of jobset '~a'"
                  (specification-name spec)))

             (respond
              (build-response #:code 302
                              #:headers
                              `((location . ,(string->uri-reference "/"))))
              #:body "")))))

    (('POST "admin" "specification" "edit")
     (let* ((spec (body->specification body))
            (name (specification-name spec))
            (old-spec (db-get-specification name))
            (old-outputs (specification-build-outputs old-spec))
            (old-notifications (specification-notifications old-spec)))
       ;; XXX: It is not possible yet to edit build outputs and notifications
       ;; using the web interface.  Use the outputs and notifications from the
       ;; existing specification.

       (db-add-or-update-specification
        (specification
         (inherit spec)
         (build-outputs old-outputs)
         (notifications old-notifications)))

       (if bridge
           (begin
             (write `(update-jobset ,(string->symbol name))
                    bridge)
             (newline bridge))
           (log-error "cannot notify bridge of modification of jobset '~a'"
                      name))
       (respond
        (build-response #:code 302
                        #:headers
                        `((location . ,(string->uri-reference "/"))))
        #:body "")))

    (('GET "admin" "specifications" "deactivate" name)
     (db-deactivate-specification name)
     (respond
      (build-response #:code 302
                      #:headers
                      `((location . ,(string->uri-reference "/"))))
      #:body ""))
    (('GET "admin" "specifications" "delete" name)
     (db-remove-specification name)
     (respond
      (build-response #:code 302
                      #:headers
                      `((location . ,(string->uri-reference "/"))))
      #:body ""))
    (('GET "admin" "build" id "restart")
     (db-restart-build! (string->number id))
     (respond
      (build-response
       #:code 302
       #:headers `((location . ,(string->uri-reference
                                 (string-append "/build/" id "/details")))))
      #:body ""))

    (('GET "admin" "evaluation" id "cancel")
     (let* ((eval (db-get-evaluation id))
            (specification (evaluation-specification-name eval)))
       (db-cancel-pending-builds! (string->number id))
       (respond
        (build-response
         #:code 302
         #:headers `((location
                      . ,(string->uri-reference
                          (string-append "/jobset/" specification)))))
        #:body "")))

    (('GET "admin" "evaluation" id "restart")
     (let* ((eval (db-get-evaluation id))
            (specification (evaluation-specification-name eval)))
       (db-restart-evaluation! (string->number id))
       (respond
        (build-response
         #:code 302
         #:headers `((location
                      . ,(string->uri-reference
                          (string-append "/jobset/" specification)))))
        #:body "")))

    (('GET "admin" "evaluation" id "retry")
     (let* ((eval (db-get-evaluation id))
            (specification (evaluation-specification-name eval)))
       (db-retry-evaluation! (string->number id))
       (respond
        (build-response
         #:code 302
         #:headers `((location
                      . ,(string->uri-reference
                          (string-append "/jobset/" specification)))))
        #:body "")))

    (('GET (or "jobsets" "specifications") . rest)
     (respond-json (scm->json-string
                    (list->vector
                     (map specification->json-object
                          (db-get-specifications))))))

    (('GET "specification" "add")
     (respond-html
      (html-page
       "Add specification"
       (specification-edit)
       '())))

    (('GET "specification" "edit" name)
     (let ((spec (db-get-specification name)))
       (respond-html
        (html-page
         "Edit specification"
         (specification-edit spec)
         '()))))

    (('GET "build" id)
     (let* ((build (if (string-suffix? ".drv" id)
                       (string-append (%store-prefix) "/" id)
                       (string->number id)))
            (hydra-build (and build
                              (handle-build-request build))))
       (if hydra-build
           (respond-json (scm->json-string hydra-build))
           (respond-build-not-found id))))
    (('GET "build" (= string->number id) "details")
     (let* ((build (and id (db-get-build id)))
            (products (and build (build-products build)))
            (spec (and build (db-get-specification
                              (build-specification-name build))))
            (checkouts (and build
                            (latest-checkouts spec
                                              (build-evaluation-id build))))
            (dependencies
             (and build
                  (db-get-builds
                   `((ids . ,(build-dependencies/id build))))))
            (history
             (and build
                  (db-get-builds
                   `((jobset . ,(build-specification-name build))
                     (job . ,(build-job-name build))
                     (oldevaluation . ,(build-evaluation-id build))
                     (status . done)
                     (order . evaluation)
                     (nr . 10)))))
            (previous-checkouts
             (match (and build
                         (db-get-previous-eval
                          (build-evaluation-id build)))
               (#f '())
               (eval (latest-checkouts spec eval))))
            (failure? (and build
                           (= (build-status failed)
                              (build-current-status build))))
            (failure (and failure?
                          (db-get-first-build-failure build))))
       (if build
           (respond-html
            (html-page
             (string-append "Build " (number->string id))
             (build-details build dependencies products history
                            #:channels (specification-channels spec)
                            #:checkouts checkouts
                            #:previous-checkouts previous-checkouts
                            #:first-failure failure)
             `(((#:name . ,(build-specification-name build))
                (#:link
                 . ,(string-append "/jobset/"
                                   (build-specification-name build)))))))
           (respond-build-not-found id))))
    (('GET "build" (= string->number id) "log" "raw")
     (let* ((build (and id (db-get-build id)))
            (log   (and build (build-log build))))
       (if (and log (file-exists? log))
           (respond-compressed-file log)
           (respond-not-found (uri->string (request-uri request))))))
    (('GET "build" (= string->number id) "log")
     (let* ((build (and id (db-get-build id)))
            (log   (and build (build-log build))))
       (if (and log (file-exists? log))
           (respond-html
            (html-page
             (string-append "Build log of build #" (number->string id))
             (pretty-build-log id)
             `(((#:name . ,(string-append "Build #" (number->string id)))
                (#:link
                 . ,(string-append "/build/" (number->string id)
                                   "/details"))))))
           (respond-not-found (uri->string (request-uri request))))))
    (('GET "output" id)
     (let ((output (db-get-output
                    (string-append (%store-prefix) "/" id))))
       (if output
           (let ((build (db-get-build (output-derivation output))))
             (respond-json
              (scm->json-string
               `((name . ,(output-name output))
                 (derivation . ,(output-derivation output))
                 (build . ,(or (and=> build build->hydra-build)
                               #nil))))))
           (respond-output-not-found id))))
    (('GET "api" "jobs")
     (let* ((params (request-parameters request))
            (eval-id (assq-ref params 'evaluation)))
       (if eval-id
           (respond-json
            (scm->json-string
             (list->vector
              (map job->alist
                   (db-get-jobs eval-id
                                `((names
                                   . ,(and=> (assq-ref params 'names)
                                             (cut string-split <> #\,)))
                                  ,@params))))))
           (respond-json-with-error 500 "Parameter not defined!"))))
    (('GET "api" "jobs" "history")
     (let* ((params (request-parameters request))
            (names (and=> (assq-ref params 'names)
                          (cut string-split <> #\,)))
            (spec (assq-ref params 'spec))
            (limit (assq-ref params 'nr)))
       (cond
        ((not (and names spec limit))
         (respond-json-with-error 500 "Parameter not defined"))
        ((> limit 100)
         (respond-json-with-error 500 "Maximum limit exceeded"))
        (else
         (catch 'json-invalid
           (lambda ()
             (respond-json
              (jobs-history->json-object
               (db-get-jobs-history names
                                    #:spec spec
                                    #:limit limit))))
           (lambda _
             (respond-json-with-error 500 "Invalid body")))))))
    (('GET "api" "dashboard" "register")
     (let* ((params (request-parameters request))
            (spec (assq-ref params 'spec))
            (names (assq-ref params 'names)))
       (cond
        ((not (and names spec))
         (respond-json-with-error 500 "Parameter not defined"))
        (else
         (let ((id (db-register-dashboard spec names)))
           (if id
               (respond-json
                (scm->json-string
                 `((id . ,id))))
               (respond-json-with-error
                500
                "Failed to register the dashboard")))))))
    (('GET "api" "evaluation")
     (let* ((params (request-parameters request))
            (id (assq-ref params 'id)))
       (if id
           (respond-json (scm->json-string
                          (evaluation->json-object
                           (db-get-evaluation id))))
           (respond-json-with-error 500 "Parameter not defined!"))))
    (('GET "api" "evaluations")
     (let* ((params (request-parameters request))
            (spec (assq-ref params 'spec))        ;optional
            ;; 'nr parameter is mandatory to limit query size.
            (nr (assq-ref params 'nr)))
       (if nr
           (respond-json (scm->json-string
                          (list->vector
                           (map evaluation->json-object
                                (db-get-evaluations nr spec)))))
           (respond-json-with-error 500 "Parameter not defined!"))))
    (('GET "api" "latestbuilds")
     (let* ((params (request-parameters request))
            ;; 'nr parameter is mandatory to limit query size.
            (limit (assq-ref params 'nr)))
       (cond
        ((not limit)
         (respond-json-with-error 500 "Parameter not defined"))
        ((> limit 1000)
         (respond-json-with-error 500 "Maximum limit exceeded"))
        (else
         ;; Limit results to builds that are "done".  Order the builds by
         ;; descending evaluation numbers.  This ensures that the builds that
         ;; were last registered are first returned even if they take more
         ;; time to complete.  Ordering by timestamp wouldn't work as
         ;; evaluations are not always performed sequentially.
         (respond-json
          (scm->json-string
           (handle-builds-request `((status . done)
                                    ,@params
                                    (order . evaluation)))))))))
    (('GET "api" "queue")
     (let* ((params (request-parameters request))
            ;; 'nr parameter is mandatory to limit query size.
            (limit (assq-ref params 'nr)))
       (cond
        ((not limit)
         (respond-json-with-error 500 "Parameter not defined"))
        ((> limit 1000)
         (respond-json-with-error 500 "Maximum limit exceeded"))
        (else
         (respond-json
          (scm->json-string
           ;; Use the 'status+submission-time' order so that builds in
           ;; 'running' state appear before builds in 'scheduled' state.
           (handle-builds-request `((status . pending)
                                    ,@params
                                    (order . status+submission-time)))))))))
    (('GET)
     (respond-html (html-page
                    "Cuirass"
                    (let ((evals (db-get-latest-evaluations)))
                      (specifications-table
                       (db-get-specifications)
                       evals
                       (db-get-evaluations-absolute-summary
                        (map evaluation-id evals))
                       ;; Get all the latest evaluations, regardless of their
                       ;; status.
                       (db-get-latest-evaluations #:status #f)))
                    '())))
    (('GET "dashboard" id)
     (let ((dashboard (db-get-dashboard id)))
       (if dashboard
           (let* ((spec (dashboard-specification-name dashboard))
                  (evaluations (db-get-latest-evaluations))
                  (evaluation
                   (any (lambda (eval)
                          (string=? (evaluation-specification-name eval)
                                    spec))
                        evaluations))
                  (uri
                   (string->uri-reference
                    (format #f "/eval/~a/dashboard/~a"
                            evaluation id))))
             (respond (build-response #:code 302
                                      #:headers `((location . ,uri)))
                      #:body ""))
           (respond-dashboard-not-found id))))
    (('GET "jobset" name)
     (respond-html
      (let* ((evaluation-id-max (db-get-evaluations-id-max name))
             (evaluation-id-min (db-get-evaluations-id-min name))
             (params (request-parameters request))
             (border-high (assq-ref params 'border-high))
             (border-low (assq-ref params 'border-low))
             (evaluations (db-get-evaluations-build-summary name
                                                            %page-size
                                                            border-low
                                                            border-high))
             (absolute-summary
              (db-get-evaluations-absolute-summary evaluations))
             (last-updates
              (if bridge
                  (begin
                    (write `(jobset-last-update-times ,(string->symbol name))
                           bridge)
                    (newline bridge)
                    (match (read bridge)
                      (`(reply ,times) times)
                      (_ #f)))
                  #f)))
        (html-page name (evaluation-info-table name
                                               evaluations
                                               evaluation-id-min
                                               evaluation-id-max
                                               #:absolute-summary
                                               absolute-summary
                                               #:last-update-times
                                               last-updates)
                   `(((#:name . ,name)
                      (#:link . ,(string-append "/jobset/" name))))))))

    (('GET "eval" "latest")
     (let* ((params (request-parameters request))
            (spec (assq-ref params 'spec))
            (evaluation-id (and spec (db-get-latest-evaluation spec))))
       (if evaluation-id
           (redirect (string-append "/eval/"
                                    (number->string evaluation-id)))
           (respond-not-found "/eval/latest"))))

    (('GET "eval" id)
     (let* ((params (request-parameters request))
            (status (assq-ref params 'status))
            (paginate? (let ((arg (assq-ref params 'paginate)))
                         (if arg (string=? arg "1") #t)))
            (border-high-time (assq-ref params 'border-high-time))
            (border-low-time (assq-ref params 'border-low-time))
            (border-high-id (assq-ref params 'border-high-id))
            (border-low-id (assq-ref params 'border-low-id))
            (specification (db-get-evaluation-specification id))
            (evaluation (db-get-evaluation-summary id)))
       (if specification
           (respond-html (evaluation-html-page evaluation
                                               #:status status
                                               #:paginate? paginate?
                                               #:border-high-time
                                               border-high-time
                                               #:border-low-time
                                               border-low-time
                                               #:border-high-id
                                               border-high-id
                                               #:border-low-id
                                               border-low-id))
           (respond-html-eval-not-found id))))

    (('GET "eval" (= string->number id) "log" "raw")
     (let ((log (and id (evaluation-log-file id))))
       (if (and log (file-exists? log))
           (respond-compressed-file log)
           (respond-not-found (uri->string (request-uri request))))))

    (('GET "eval" "latest" "dashboard")
     (let* ((params (request-parameters request))
            (spec (assq-ref params 'spec))
            (system (assq-ref params 'system))
            (evaluation-id (and spec (db-get-latest-evaluation spec))))
       (if evaluation-id
           (redirect (string-append "/eval/"
                                    (number->string evaluation-id)
                                    "/dashboard"
                                    (if system
                                        (string-append "?system=" system)
                                        "")))
           (respond-not-found "/eval/latest/dashboard"))))

    (('GET "eval" (= string->number id) "dashboard")
     (let* ((params (request-parameters request))
            (spec (db-get-evaluation-specification id))
            (system (assq-ref params 'system)))
       (if spec
           (respond-html
            (dashboard-page id #:system system))
           (respond-html-eval-not-found id))))

    (('GET "eval" (= string->number evaluation-id) "dashboard" dashboard-id)
     (let* ((eval (db-get-evaluation evaluation-id))
            (dashboard (db-get-dashboard dashboard-id)))
       (cond
        ((not eval)
         (respond-html-eval-not-found evaluation-id))
        ((not dashboard)
         (respond-html-eval-not-found dashboard-id))
        (else
         (respond-html
          (dashboard-page evaluation-id
                          #:dashboard-id dashboard-id))))))

    ;; Replicate the Guix publish log interface for compatibility purposes.
    (('GET "log" output)
     (let* ((output (string-append (%store-prefix) "/" output))
            (log (db-get-log-from-output output)))
       (if (and log (file-exists? log))
           (respond-compressed-file log)
           (respond-not-found (uri->string (request-uri request))))))

    (('GET "search")
     (let* ((params (request-parameters request))
            (query (and=> (assq-ref params 'query) uri-decode))
            (builds-id-min (and=> query db-get-builds-query-min))
            (builds-id-max (and=> query db-get-builds-query-max))
            (border-low-id (assq-ref params 'border-low-id))
            (border-high-id (assq-ref params 'border-high-id)))
       (if query
           (respond-html
            (html-page
             "Search results"
             (build-search-results-table
              query
              (with-time-logging
               "job search request"
               (db-get-builds-by-search
                `((query . ,query)
                  (nr . ,%page-size)
                  (order . finish-time+build-id)
                  (border-low-id . ,border-low-id)
                  (border-high-id . ,border-high-id))))
              builds-id-min
              builds-id-max)
             '()
             query))
           (respond-json-with-error 500 "Query parameter not provided!"))))

    (('GET "search" "latest")
     (let* ((params (request-parameters request))
            (query (and=> (assq-ref params 'query) uri-decode)))
       (if query
           (match (vector->list
                   (handle-builds-search-request
                    `((query . ,query)
                      (nr . 1)
                      (order . finish-time+build-id))))
             ((build)
              (let ((uri (string->uri-reference
                          (string-append "/build/"
                                         (number->string
                                          (assoc-ref build 'id))
                                         "/details"))))
                (respond (build-response #:code 302
                                         #:headers `((location . ,uri)))
                         #:body "")))
             (_
              (respond-json-with-error 500 "No build found.")))
           (respond-json-with-error 500 "Query parameter not provided."))))

    (('GET "search" "latest" product-type)
     (let* ((params (request-parameters request))
            (query (and=> (assq-ref params 'query) uri-decode)))
       (if query
           (match (vector->list
                   (handle-builds-search-request
                    `((query . ,query)
                      (nr . 1)
                      (order . finish-time+build-id))))
             ((build)
              (let* ((build-id (assoc-ref build 'id))
                     (products (vector->list
                                (assoc-ref build 'buildproducts)))
                     (product (find (lambda (product)
                                      (string=? (assoc-ref product 'type)
                                                product-type))
                                    products))
                     (product-id (assoc-ref product 'id))
                     (uri (and product-id
                               (string->uri-reference
                                (string-append "/download/"
                                               (number->string product-id))))))
                (if uri
                    (respond (build-response #:code 302
                                             #:headers `((location . ,uri)))
                             #:body "")
                    (respond-json-with-error
                     500
                     "Could not find the request build product."))))
             (_
              (respond-json-with-error 500 "No build found.")))
           (respond-json-with-error 500 "Query parameter not provided."))))

    (('GET "events" "rss")
     (let* ((params (request-parameters request))
            (specification (and params
                                (assq-ref params 'specification))))
       (respond-rss
        (rss-feed
         (db-get-builds `((weather . new)
                          (jobset . ,specification)
                          (nr . 100)
                          (order . evaluation)
                          ,@params))
         #:params params))))

    (('GET "jobset" spec "badge.svg")
     (let* ((params (request-parameters request))
            (type (and=> (assq-ref params 'type)
                         string->number))
            (summary (and=> (db-get-latest-evaluation spec)
                            db-get-evaluation-absolute-summary)))
       (respond-svg
        (badge-svg spec badge-string summary
                   #:type (or type 0)))))

    (('POST "jobset" spec "hook" "evaluate")
     (let ((spec (db-get-specification spec)))
       (if spec
           (if bridge
               (let ((name (specification-name spec)))
                 (write `(trigger-jobset ,(string->symbol name))
                        bridge)
                 (newline bridge)
                 (respond-json
                  (scm->json-string `((jobset . ,name)))))
               (begin
                 (log-warning "evaluation hook disabled")
                 (respond-json-with-error 500 "Evaluation hook disabled.")))
           (respond-json-with-error 404 "Jobset not found."))))

    (('GET "workers")
     (respond-html
      (html-page
       "Workers status"
       (let* ((workers (db-get-workers))
              (builds (db-worker-current-builds))
              (percentages (db-get-build-percentages builds)))
         (workers-status workers builds percentages))
       '())))

    (('GET "metrics")
     (respond-html
      (metrics-page)))

    (('GET "status")
     (respond-html
      (html-page
       "Running builds"
       (running-builds-table
        (db-get-builds `((status . started)
                         (order . status+submission-time))))
       '())))

    (('GET "download" id)
     (let ((file (db-get-build-product-path id))
           (fail (lambda (code)
                   (respond-json-with-error
                    code "Could not find the requested build product."))))
       (if file
           (if (file-exists? file)
               (respond-file file #:ttl %static-file-ttl)
               (fail 500))                     ;something's wrong: it vanished
           (fail 404))))                       ;no such build product

    (('GET "machine" name)
     (respond-html
      (machine-page name)))

    (('GET "static" "about" "javascript")
     (respond-html
      (javascript-licenses)))

    (('GET "static" path ...)
     (respond-static-file path))
    (_
     (respond-not-found (uri->string (request-uri request))))))

(define* (run-cuirass-server #:key (host "localhost") (port 8080)
                             (bridge-socket-file-name
                              (%bridge-socket-file-name)))
  (let* ((host-info  (gethostbyname host))
         (address    (inet-ntop (hostent:addrtype host-info)
                                (car (hostent:addr-list host-info)))))
    (log-info "listening on ~A:~A" address port)

    ;; With 'cuirass web' running as a separate process, we need to open a
    ;; connection over the "bridge" to talk to the 'cuirass register' process.
    (let ((bridge (and bridge-socket-file-name
                       (socket AF_UNIX
                               (logior SOCK_STREAM SOCK_NONBLOCK SOCK_CLOEXEC)
                               0))))
      (when bridge
        (log-info "connecting to bridge at '~a'" bridge-socket-file-name)
        (connect bridge AF_UNIX bridge-socket-file-name))

      ;; Here we use our own web backend, call 'fiberized'.  We cannot use the
      ;; 'fibers' backend that comes with Fibers 1.0.0 because it does its own
      ;; thread creations and calls 'run-fibers' by itself, which isn't
      ;; necessary here (and harmful).
      ;;
      ;; In addition, we roll our own instead of using Guile's 'run-server'
      ;; and 'serve-one-client'.  The key thing here is that we spawn a fiber
      ;; to process each client request and then directly go back waiting for
      ;; the next client (conversely, Guile's 'run-server' loop processes
      ;; clients one after another, sequentially.)  We can do that because we
      ;; don't maintain any state across connections.
      ;;
      ;; XXX: We don't do 'call-with-sigint' like 'run-server' does.
      (let* ((impl (lookup-server-impl 'fiberized))
             (server (open-server impl `(#:host ,address #:port ,port))))
        (let loop ()
          (let ((client request body (read-client impl server)))
            ;; Spawn a fiber to handle REQUEST and reply to CLIENT.
            (spawn-fiber
             (lambda ()
               (let ((response body state
                               (handle-request (cut url-handler bridge <...>)
                                               request body '())))
                 (write-client impl server client response body)))))
          (loop))))))
