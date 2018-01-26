;;;; http.scm -- HTTP API
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (cuirass database)
  #:use-module (cuirass utils)
  #:use-module (cuirass logging)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)
  #:export (run-cuirass-server))

(define (build->hydra-build build)
  "Convert BUILD to an assoc list matching hydra API format."
  (define (bool->int bool)
    (if bool 1 0))

  `((#:id . ,(assq-ref build #:id))
    (#:project . ,(assq-ref build #:repo-name))
    (#:jobset . ,(assq-ref build #:branch))
    (#:job . ,(assq-ref build #:job-name))
    (#:timestamp . ,(assq-ref build #:timestamp))
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
    (#:finished . ,(bool->int
                    (not (memv (assq-ref build #:status)
                               (list (build-status scheduled)
                                     (build-status started))))))
    (#:buildproducts . #nil)
    (#:releasename . #nil)
    (#:buildinputs_builds . #nil)))

(define (handle-build-request db build-id)
  "Retrieve build identified by BUILD-ID in DB and convert it to hydra
  format. Return #f is not build was found."
  (let ((build (db-get-build db build-id)))
    (and=> build build->hydra-build)))

(define (handle-builds-request db filters)
  "Retrieve all builds matched by FILTERS in DB and convert them to hydra
  format."
  (let ((builds (with-time-logging "builds request"
                                   (db-get-builds db filters))))
    (map build->hydra-build builds)))

(define (request-parameters request)
  "Parse the REQUEST query parameters and return them under the form
  '((parameter value) ...)."
  (let* ((uri (request-uri request))
         (query (uri-query uri)))
    (and query
         (map (lambda (param)
                (match (string-split param #\=)
                  ((key param)
                   (list (string->symbol key) param))))
              (string-split query #\&)))))


;;;
;;; Web server.
;;;
;;; The api is derived from the hydra one. It is partially described here :
;;;
;;; https://github.com/NixOS/hydra/blob/master/doc/manual/api.xml
;;;

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (url-handler request body db)

  (define* (respond response #:key body (db db))
    (values response body db))

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

  (define (respond-build-not-found build-id)
    (respond-json-with-error
     404
     (format #f "Build with ID ~a doesn't exist." build-id)))

  (define (respond-build-log-not-found build)
    (let ((drv (assq-ref build #:derivation)))
      (respond-json-with-error
       404
       (format #f "The build log of derivation ~a is not available." drv))))

  (log-message "~a ~a" (request-method request)
               (uri-path (request-uri request)))

  ;; Reject OPTIONS, POST, etc.
  (match (if (eq? 'GET (request-method request))
             (request-path-components request)
             'method-not-allowed)
    (((or "jobsets" "specifications") . rest)
     (respond-json (object->json-string (car (db-get-specifications db)))))
    (("build" build-id)
     (let ((hydra-build (handle-build-request db build-id)))
       (if hydra-build
           (respond-json (object->json-string hydra-build))
           (respond-build-not-found build-id))))
    (("build" build-id "log" "raw")
     (let ((build (db-get-build db build-id)))
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
    (("api" "latestbuilds")
     (let* ((params (request-parameters request))
            ;; 'nr parameter is mandatory to limit query size.
            (valid-params? (assq-ref params 'nr)))
       (if valid-params?
           ;; Limit results to builds that are "done".
           (respond-json (object->json-string
                          (handle-builds-request db
                                                 `((status done)
                                                   ,@params
                                                   (order finish-time)))))
           (respond-json-with-error 500 "Parameter not defined!"))))
    (("api" "queue")
     (let* ((params (request-parameters request))
            ;; 'nr parameter is mandatory to limit query size.
            (valid-params? (assq-ref params 'nr)))
       (if valid-params?
           (respond-json (object->json-string
                          (handle-builds-request db
                                                 `((status pending)
                                                   ,@params
                                                   (order submission-time)))))
           (respond-json-with-error 500 "Parameter not defined!"))))
    ('method-not-allowed
     ;; 405 "Method Not Allowed"
     (values (build-response #:code 405) #f db))
    (_
     (respond (build-response #:code 404)
              #:body (string-append "Resource not found: "
                                    (uri->string (request-uri request)))))))

(define* (run-cuirass-server db #:key (host "localhost") (port 8080))
  (let* ((host-info (gethostbyname host))
         (address (inet-ntop (hostent:addrtype host-info)
                             (car (hostent:addr-list host-info)))))
    (log-message "listening on ~A:~A" address port)

    ;; Here we use our own web backend, call 'fiberized'.  We cannot use the
    ;; 'fibers' backend that comes with Fibers 1.0.0 because it does its own
    ;; thread creations and calls 'run-fibers' by itself, which isn't
    ;; necessary here (and harmful).
    (run-server url-handler
                'fiberized
                `(#:host ,address #:port ,port)
                db)))
