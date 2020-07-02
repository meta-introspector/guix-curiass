;;;; http.scm -- HTTP API
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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
  #:use-module (cuirass utils)
  #:use-module (cuirass logging)
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
    "css/cuirass.css"
    "css/open-iconic-bootstrap.css"
    "fonts/open-iconic.otf"
    "fonts/open-iconic.woff"
    "images/logo.png"))

(define (build->hydra-build build)
  "Convert BUILD to an assoc list matching hydra API format."
  (define (bool->int bool)
    (if bool 1 0))

  (define finished?
    (not (memv (assq-ref build #:status)
               (list (build-status scheduled)
                     (build-status started)))))

  (define build-products
    (db-get-build-products (assq-ref build #:id)))

  `((#:id . ,(assq-ref build #:id))
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
    (#:busy . ,(bool->int (eqv? (build-status started)
                                (assq-ref build #:status))))
    (#:priority . 0)
    (#:finished . ,(bool->int finished?))
    (#:buildproducts . ,(list->vector build-products))
    (#:releasename . #nil)
    (#:buildinputs_builds . #nil)))

(define (evaluation->json-object evaluation)
  "Turn EVALUATION into a representation suitable for 'json->scm'."
  ;; XXX: Since #:checkouts is a list of alists, we must turn it into a vector
  ;; so that 'json->scm' converts it to a JSON array.
  `(,@(alist-delete #:checkouts evaluation eq?)
    (#:checkouts . ,(list->vector
                     (assq-ref evaluation #:checkouts)))))

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
        (map (lambda (param)
               (match (string-split param #\=)
                 ((key param)
                  (let ((key-symbol (string->symbol key)))
                    (cons key-symbol
                          (match key-symbol
                            ('id (string->number param))
                            ('nr (string->number param))
                            (_   param)))))))
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
  (define checkouts     (db-get-checkouts id))
  (define inputs        (db-get-inputs specification))

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
                           #:checkouts checkouts
                           #:inputs inputs
                           #:status status
                           #:builds builds
                           #:builds-id-min builds-id-min
                           #:builds-id-max builds-id-max)
   `(((#:name . ,specification)
      (#:link . ,(string-append "/jobset/" specification)))
     ((#:name . ,(string-append "Evaluation " (number->string id)))
      (#:link . ,(string-append "/eval/" (number->string id)))))))


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
    (('POST "admin" "specifications" "add")
     (match (string-split (utf8->string body) #\=)
       (("spec-name" name)
        (db-add-specification
         `((#:name . ,name)
           (#:load-path-inputs . ())
           (#:package-path-inputs . ())
           (#:proc . cuirass-jobs)
           (#:proc-input . ,name)
           (#:proc-file . "build-aux/cuirass/gnu-system.scm")
           (#:proc-args . (systems "x86_64-linux"
                                   "i686-linux"
                                   "armhf-linux"
                                   "aarch64-linux"))
           (#:inputs .
            '((#:name . ,name)
              (#:url . "https://git.savannah.gnu.org/git/guix.git")
              (#:load-path . ".")
              (#:branch . ,name)
              (#:no-compile? . #t)))
           (#:build-outputs . ())))
        (respond (build-response #:code 302
                                 #:headers `((location . ,(string->uri-reference
                                                           "/admin/specifications"))))
                 #:body ""))))
    (('POST "admin" "specifications" "delete" name)
     (db-remove-specification name)
     (respond (build-response #:code 302
                              #:headers `((location . ,(string->uri-reference
                                                        "/admin/specifications"))))
              #:body ""))
    (('GET "admin" "specifications" . rest)
     (respond-html (html-page
                    "Cuirass [Admin]"
                    (specifications-table (db-get-specifications) 'admin)
                    '())))
    (('GET "admin")
     (respond-html (html-page
                    "Cuirass [Admin]"
                    `(ul (li (a (@ (href "/admin/specifications"))
                                "Edit specifications")))
                    '())))
    (('GET (or "jobsets" "specifications") . rest)
     (respond-json (object->json-string
                    (list->vector (db-get-specifications)))))
    (('GET "build" id)
     (let ((hydra-build (handle-build-request
                         (if (string-suffix? ".drv" id)
                             (string-append (%store-prefix) "/" id)
                             (string->number id)))))
       (if hydra-build
           (respond-json (object->json-string hydra-build))
           (respond-build-not-found id))))
    (('GET "build" build-id "details")
     (let ((build (db-get-build (string->number build-id)))
           (products (db-get-build-products build-id)))
       (if build
           (respond-html
            (html-page (string-append "Build " build-id)
                       (build-details build products)
                       `(((#:name . ,(assq-ref build #:specification))
                          (#:link . ,(string-append "/jobset/" (assq-ref build #:specification)))))))
           (respond-build-not-found build-id))))
    (('GET "build" build-id "log" "raw")
     (let ((build (db-get-build (string->number build-id))))
       (if build
           (match (assq-ref build #:outputs)
             (((_ (#:path . (? string? output))) _ ...)
              ;; Redirect to a /log URL, which is assumed to be served
              ;; by 'guix publish'.
              (let ((uri (string->uri-reference
                          (string-append "/log/"
                                         (basename output)))))
                (respond (build-response #:code 302
                                         #:headers `((location . ,uri)))
                         #:body "")))
             (()
              ;; Not entry for BUILD-ID in the 'Outputs' table.
              (respond-json-with-error
               500
               (format #f "Outputs of build ~a are unknown." build-id)))
             (#f
              (respond-build-not-found build-id)))
           (respond-build-not-found build-id))))
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
            (valid-params? (assq-ref params 'nr)))
       (if valid-params?
           ;; Limit results to builds that are "done".
           (respond-json
            (object->json-string
             (handle-builds-request `((status . done)
                                      ,@params
                                      (order . finish-time)))))
           (respond-json-with-error 500 "Parameter not defined!"))))
    (('GET "api" "queue")
     (let* ((params (request-parameters request))
            ;; 'nr parameter is mandatory to limit query size.
            (valid-params? (assq-ref params 'nr)))
       (if valid-params?
           (respond-json
            (object->json-string
             ;; Use the 'status+submission-time' order so that builds in
             ;; 'running' state appear before builds in 'scheduled' state.
             (handle-builds-request `((status . pending)
                                      ,@params
                                      (order . status+submission-time)))))
           (respond-json-with-error 500 "Parameter not defined!"))))
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
                     (products (db-get-build-products build-id))
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
       (respond-file path)))

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
