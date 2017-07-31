;;;; http.scm -- HTTP API
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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
  #:use-module (guix config)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (json)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)
  #:export (run-cuirass-server))

(define (build->hydra-build build)
  "Convert BUILD to an assoc list matching hydra API format."
  `((#:id . ,(assq-ref build #:id))
    (#:project . ,(assq-ref build #:repo-name))
    (#:jobset . ,(assq-ref build #:branch))
    (#:job . ,(assq-ref build #:job-name))
    (#:timestamp . ,(assq-ref build #:timestamp))
    (#:starttime . ,(assq-ref build #:starttime))
    (#:stoptime . ,(assq-ref build #:stoptime))
    (#:buildoutputs . ,(assq-ref build #:outputs))
    (#:system . ,(assq-ref build #:system))
    (#:nixname . ,(assq-ref build #:nix-name))
    (#:buildstatus . ,(assq-ref build #:status))

    ;; TODO: Fill the fields above with correct values.
    (#:busy . 0)
    (#:priority . 0)
    (#:finished . 1)
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
  (let ((builds (db-get-builds db filters)))
    (map build->hydra-build builds)))

(define (handle-log-request db build)
  "Retrieve the log file of BUILD. Return a lambda which PORT argument is an
  input port from which the content of the log file can be read or #f if the
  log file is not readable."
  (let* ((log (assq-ref build #:log))
         (access (and (string? log)
                      (access? log R_OK))))
    (and access
         (lambda (out-port)
           (let ((in-pipe-port
                  (open-input-pipe
                   (format #f "~a -dc ~a" %bzip2 log))))
             (dump-port in-pipe-port out-port)
             (close-pipe in-pipe-port))))))

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

  (match (request-path-components request)
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
           (let ((log-response (handle-log-request db build)))
             (if log-response
                 (respond-text log-response)
                 (respond-build-log-not-found build)))
           (respond-build-not-found build-id))))
    (("api" "latestbuilds")
     (let* ((params (request-parameters request))
            ;; 'nr parameter is mandatory to limit query size.
            (valid-params? (assq-ref params 'nr)))
       (if valid-params?
           (respond-json (object->json-string
                          (handle-builds-request db params)))
           (respond-json-with-error 500 "Parameter not defined!"))))
    (_
     (respond (build-response #:code 404)
              #:body (string-append "Resource not found: "
                                    (uri->string (request-uri request)))))))

(define* (run-cuirass-server db #:key (port 8080))
  (format (current-error-port) "listening on port ~A~%" port)
  (run-server url-handler
              'http
              `(#:port ,port)
              db))
