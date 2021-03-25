;;;; http.scm -- HTTP API
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017, 2020 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
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
  '("css/bootstrap.css"
    "css/choices.min.css"
    "css/datatables.min.css"
    "css/cuirass.css"
    "css/open-iconic-bootstrap.css"
    "fonts/open-iconic.otf"
    "fonts/open-iconic.woff"
    "images/icon.png"
    "images/guix.png"
    "js/chart.js"
    "js/datatables.min.js"
    "js/jquery-3.6.0.min.js"
    "js/choices.min.js"))

(define (build->hydra-build build)
  "Convert BUILD to an assoc list matching hydra API format."
  (define (bool->int bool)
    (if bool 1 0))

  (define finished?
    (>= (assq-ref build #:status) 0))

  `((#:id . ,(assq-ref build #:id))
    (#:evaluation . ,(assq-ref build #:eval-id))
    (#:jobset . ,(assq-ref build #:specification))
    (#:job . ,(assq-ref build #:job-name))

    ;; Hydra's API uses "timestamp" as the time of the last useful event for
    ;; that build: evaluation or completion.
    (#:timestamp . ,(if finished?
                        (assq-ref build #:stoptime)
                        (assq-ref build #:timestamp)))

    (#:starttime . ,(assq-ref build #:starttime))
    (#:stoptime . ,(assq-ref build #:stoptime))
    (#:derivation . ,(assq-ref build #:derivation))
    (#:buildoutputs . ,(assq-ref build #:outputs))
    (#:system . ,(assq-ref build #:system))
    (#:nixname . ,(assq-ref build #:nix-name))
    (#:buildstatus . ,(assq-ref build #:status))
    (#:weather . ,(assq-ref build #:weather))
    (#:busy . ,(bool->int (eqv? (build-status started)
                                (assq-ref build #:status))))
    (#:priority . ,(assq-ref build #:priority))
    (#:finished . ,(bool->int finished?))
    (#:buildproducts . ,(list->vector
                         (assq-ref build #:buildproducts)))))

(define (evaluation->json-object evaluation)
  "Turn EVALUATION into a representation suitable for 'json->scm'."
  ;; XXX: Since #:checkouts is a list of alists, we must turn it into a vector
  ;; so that 'json->scm' converts it to a JSON array.
  `(,@(alist-delete #:checkouts evaluation eq?)
    (#:checkouts . ,(list->vector
                     (assq-ref evaluation #:checkouts)))))

(define (specification->json-object spec)
  "Turn SPEC into a representation suitable for 'json->scm'."
  (define (channel->json-object channel)
    `((#:name . ,(channel-name channel))
      (#:url . ,(channel-url channel))
      (#:branch . ,(channel-branch channel))
      (#:commit . ,(channel-commit channel))))

  (define (build-output->json-object build-output)
    `((#:job . ,(build-output-job build-output))
      (#:type . ,(build-output-type build-output))
      (#:output . ,(build-output-output build-output))
      (#:path . ,(build-output-path build-output))))

  (define (notification->json-object notif)
    (cond
     ((email? notif)
      `((#:type . email)
        (#:from . ,(email-from notif))
        (#:to . ,(email-to notif))
        (#:server . ,(email-server notif))))
     ((mastodon? notif)
      `((#:type . mastodon)))))

  `((#:name . ,(specification-name spec))
    (#:build . ,(specification-build spec))
    (#:channels . ,(list->vector
                    (map channel->json-object
                         (specification-channels spec))))
    (#:build-outputs . ,(list->vector
                         (map build-output->json-object
                              (specification-build-outputs spec))))
    (#:notifications . ,(list->vector
                         (map notification->json-object
                              (specification-notifications spec))))
    (#:priority . ,(specification-priority spec))
    (#:systems . ,(list->vector
                   (specification-systems spec)))))

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
                               border-high-time border-low-time
                               border-high-id border-low-id)
  "Return the HTML page representing EVALUATION."
  (define id             (assq-ref evaluation #:id))
  (define builds-id-max (db-get-builds-max id status))
  (define builds-id-min (db-get-builds-min id status))
  (define specification (db-get-evaluation-specification id))
  (define channels      (specification-channels
                         (db-get-specification specification)))
  (define checkouts     (db-get-checkouts id))

  (define builds
    (vector->list
     (handle-builds-request
      `((evaluation . ,id)
        (status . ,(and=> status string->symbol))
        (nr . ,%page-size)
        (order . finish-time+build-id)
        (border-high-time . ,border-high-time)
        (border-low-time . ,border-low-time)
        (border-high-id . ,border-high-id)
        (border-low-id . ,border-low-id)))))

  (html-page
   "Evaluation"
   (evaluation-build-table evaluation
                           #:channels channels
                           #:checkouts checkouts
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
                                      (string=? (assq-ref build #:worker)
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
                       (cons (cons (string->symbol key) param)
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
                                  (string-split
                                   (uri-decode param) #\,)))))
         (channels (map (lambda (name url branch)
                          (channel
                           (name (string->symbol name))
                           (url (uri-decode url))
                           (branch branch)))
                        (filter-field 'channel-name)
                        (filter-field 'channel-url)
                        (filter-field 'channel-branch)))
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
     (priority priority)
     (systems systems))))


;;;
;;; Web server.
;;;
;;; The api is derived from the hydra one. It is partially described here :
;;;
;;; https://github.com/NixOS/hydra/blob/master/doc/manual/api.xml
;;;

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (url-handler request body)

  (define* (respond response #:key body)
    (values response body #f))

  (define-syntax-rule (respond-json body ...)
    (respond '((content-type . (application/json)))
             #:body body ...))

  (define-syntax-rule (respond-text body ...)
    (respond '((content-type . (text/plain)))
             #:body body ...))

  (define-syntax-rule (respond-json-with-error error-code message)
    (respond
     (build-response #:headers '((content-type . (application/json)))
                     #:code error-code)
     #:body
     (object->json-string
      `((error . ,message)))))

  (define* (respond-html body #:key code)
    (respond
     (let ((content-type '((content-type . (application/xhtml+xml)))))
       (if code
           (build-response #:headers content-type #:code code)
           content-type))
     #:body
     (lambda (port)
       (format
        port "<!DOCTYPE html PUBLIC ~s ~s>"
        "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd")
       (sxml->xml body port))))

  (define* (respond-xml body #:key code)
    (respond
     (let ((content-type '((content-type . (application/xhtml+xml)))))
       (if code
           (build-response #:headers content-type #:code code)
           content-type))
     #:body
     (lambda (port)
       (format port "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
       (sxml->xml body port))))

  (define* (respond-file file)
    (let ((content-type (or (assoc-ref %file-mime-types
                                       (file-extension file))
                            '(application/octet-stream))))
      (respond `((content-type . ,content-type)
                 (content-disposition
                  . (form-data (filename . ,(basename file))))
                 (x-raw-file . ,file)))))

  (define (respond-static-file path)
    ;; PATH is a list of path components
    (let ((file-name (string-join path "/"))
          (file-path (string-join (cons* (%static-directory) path) "/")))
      (if (and (member file-name %file-white-list)
               (file-exists? file-path)
               (not (file-is-directory? file-path)))
          (respond-file file-path)
          (respond-not-found file-name))))

  (define (respond-gzipped-file file)
    ;; Return FILE with 'gzip' content-encoding.
    (respond `((content-type . (text/plain (charset . "UTF-8")))
               (content-encoding . (gzip))
               (content-disposition . (inline))
               (x-raw-file . ,file))))

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

  (define (respond-build-log-not-found build)
    (let ((drv (assq-ref build #:derivation)))
      (respond-json-with-error
       404
       (format #f "The build log of derivation ~a is not available." drv))))

  (define (respond-not-found resource_name)
    (respond (build-response #:code 404)
             #:body (string-append "Resource not found: "
                                   resource_name)))

  (log-message "~a ~a" (request-method request)
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
       (respond
        (build-response #:code 302
                        #:headers
                        `((location . ,(string->uri-reference "/"))))
        #:body "")))

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
            (specification (assq-ref eval #:specification)))
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
            (specification (assq-ref eval #:specification)))
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
            (specification (assq-ref eval #:specification)))
       (db-retry-evaluation! (string->number id))
       (respond
        (build-response
         #:code 302
         #:headers `((location
                      . ,(string->uri-reference
                          (string-append "/jobset/" specification)))))
        #:body "")))

    (('GET (or "jobsets" "specifications") . rest)
     (respond-json (object->json-string
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
           (respond-json (object->json-string hydra-build))
           (respond-build-not-found id))))
    (('GET "build" (= string->number id) "details")
     (let* ((build (and id (db-get-build id)))
            (products (and build (assoc-ref build #:buildproducts)))
            (history
             (db-get-builds
              `((jobset . ,(assq-ref build #:specification))
                (job . ,(assq-ref build #:job-name))
                (oldevaluation . ,(assq-ref build #:eval-id))
                (status . done)
                (order . evaluation)
                (nr . 5)))))
       (if build
           (respond-html
            (html-page (string-append "Build " (number->string id))
                       (build-details build products history)
                       `(((#:name . ,(assq-ref build #:specification))
                          (#:link . ,(string-append "/jobset/" (assq-ref build #:specification)))))))
           (respond-build-not-found id))))
    (('GET "build" (= string->number id) "log" "raw")
     (let* ((build (and id (db-get-build id)))
            (log   (and build (assq-ref build #:log))))
       (if (and log (file-exists? log))
           (respond-gzipped-file log)
           (respond-not-found (uri->string (request-uri request))))))
    (('GET "output" id)
     (let ((output (db-get-output
                    (string-append (%store-prefix) "/" id))))
       (if output
           (let ((build (db-get-build (assq-ref output #:derivation))))
             (respond-json
              (object->json-string
               (append output
                       `((#:build . ,(or build #nil)))))))
           (respond-output-not-found id))))
    (('GET "api" "evaluation")
     (let* ((params (request-parameters request))
            (id (assq-ref params 'id)))
       (if id
           (respond-json (object->json-string
                          (evaluation->json-object
                           (db-get-evaluation id))))
           (respond-json-with-error 500 "Parameter not defined!"))))
    (('GET "api" "evaluations")
     (let* ((params (request-parameters request))
            ;; 'nr parameter is mandatory to limit query size.
            (nr (assq-ref params 'nr)))
       (if nr
           (respond-json (object->json-string
                          (list->vector
                           (map evaluation->json-object
                                (db-get-evaluations nr)))))
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
          (object->json-string
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
          (object->json-string
           ;; Use the 'status+submission-time' order so that builds in
           ;; 'running' state appear before builds in 'scheduled' state.
           (handle-builds-request `((status . pending)
                                    ,@params
                                    (order . status+submission-time)))))))))
    (('GET)
     (respond-html (html-page
                    "Cuirass"
                    (specifications-table
                     (db-get-specifications))
                    '())))

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
                                                            border-high)))
        (html-page name (evaluation-info-table name
                                               evaluations
                                               evaluation-id-min
                                               evaluation-id-max)
                   `(((#:name . ,name)
                      (#:link . ,(string-append "/jobset/" name))))))))

    (('GET "eval" id)
     (let* ((params (request-parameters request))
            (status (assq-ref params 'status))
            (border-high-time (assq-ref params 'border-high-time))
            (border-low-time (assq-ref params 'border-low-time))
            (border-high-id (assq-ref params 'border-high-id))
            (border-low-id (assq-ref params 'border-low-id))
            (specification (db-get-evaluation-specification id))
            (evaluation (db-get-evaluation-summary id)))
       (if specification
           (respond-html (evaluation-html-page evaluation
                                               #:status status
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
           (respond-gzipped-file log)
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
              (vector->list
               (handle-builds-search-request
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
                                          (assoc-ref build #:id))
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
              (let* ((build-id (assoc-ref build #:id))
                     (products (vector->list
                                (assoc-ref build #:buildproducts)))
                     (product (find (lambda (product)
                                      (string=? (assoc-ref product #:type)
                                                product-type))
                                    products))
                     (product-id (assoc-ref product #:id))
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
       (respond-xml
        (rss-feed
         (db-get-builds `((weather . new)
                          (jobset . ,specification)
                          (nr . 100)
                          (order . evaluation)
                          ,@params))
         #:params params))))

    (('GET "workers")
     (respond-html
      (html-page
       "Workers status"
       (let* ((workers (db-get-workers))
              (builds (db-worker-current-builds))
              (percentages (db-get-build-percentages builds))
              (builds*
               (map (lambda (build percentage)
                      `(,@build
                        (#:percentage . ,percentage)))
                    builds percentages)))
         (workers-status workers builds*))
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
     (let ((path (db-get-build-product-path id)))
       (if path
           (respond-file path)
           (respond-json-with-error
            500
            "Could not find the request build product."))))

    (('GET "machine" name)
     (respond-html
      (machine-page name)))

    (('GET "static" path ...)
     (respond-static-file path))
    (_
     (respond-not-found (uri->string (request-uri request))))))

(define* (run-cuirass-server #:key (host "localhost") (port 8080))
  (let* ((host-info  (gethostbyname host))
         (address    (inet-ntop (hostent:addrtype host-info)
                                (car (hostent:addr-list host-info)))))
    (log-message "listening on ~A:~A" address port)

    ;; Here we use our own web backend, call 'fiberized'.  We cannot use the
    ;; 'fibers' backend that comes with Fibers 1.0.0 because it does its own
    ;; thread creations and calls 'run-fibers' by itself, which isn't
    ;; necessary here (and harmful).
    ;;
    ;; In addition, we roll our own instead of using Guile's 'run-server' and
    ;; 'serve-one-client'.  The key thing here is that we spawn a fiber to
    ;; process each client request and then directly go back waiting for the
    ;; next client (conversely, Guile's 'run-server' loop processes clients
    ;; one after another, sequentially.)  We can do that because we don't
    ;; maintain any state across connections.
    ;;
    ;; XXX: We don't do 'call-with-sigint' like 'run-server' does.
    (let* ((impl (lookup-server-impl 'fiberized))
           (server (open-server impl `(#:host ,address #:port ,port))))
      (let loop ()
        (let-values (((client request body)
                      (read-client impl server)))
          ;; Spawn a fiber to handle REQUEST and reply to CLIENT.
          (spawn-fiber
           (lambda ()
             (let-values (((response body state)
                           (handle-request url-handler request body '())))
               (write-client impl server client response body)))))
        (loop)))))
