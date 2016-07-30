;;; http.scm -- HTTP API
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
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
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)
  #:export (spec->json-string
            run-cuirass-server))

;;;
;;; JSON format.
;;;

(define (object->json-scm obj)
  "Prepare OBJ for JSON usage."
  (cond ((string? obj)  obj)
        ((number? obj)  obj)
        ((boolean? obj) obj)
        ((null? obj)    obj)
        ((symbol? obj)  (symbol->string obj))
        ((keyword? obj) (object->json-scm (keyword->symbol obj)))
        ((alist? obj)   (alist->hash-table (map object->json-scm obj)))
        ((pair? obj)    (cons (object->json-scm (car obj))
                              (object->json-scm (cdr obj))))
        (else           (object->string obj))))

(define* (spec->json-string spec #:key pretty)
  "Return SPEC as a JSON object."
  (scm->json-string (object->json-scm spec) #:pretty pretty))


;;;
;;; Web server.
;;;

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))

(define (url-handler request body)
  (match (request-path-components request)
    (((or "jobsets" "specifications") . rest)
     (values '((content-type . (application/json)))
             (with-database db
               (spec->json-string (car (db-get-specifications db))))))
    (_ (not-found request))))

(define (run-cuirass-server)
  (run-server url-handler))
