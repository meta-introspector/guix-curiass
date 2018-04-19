;;; http.scm -- tests for (cuirass http) module
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017, 2018 Ludovic Courtès <ludo@gnu.org>
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

(use-modules (cuirass http)
             (cuirass database)
             (cuirass utils)
             (json)
             (fibers)
             (web uri)
             (web client)
             (web response)
             (rnrs bytevectors)
             (srfi srfi-1)
             (srfi srfi-64)
             (ice-9 match))

(define (hash-table-keys table)
  (hash-fold (lambda (key value rest)
               (cons key rest))
             '()
             table))

(define (hash-table=? t1 t2)
  (and (lset= equal?
              (hash-table-keys t1)
              (hash-table-keys t2))
       (hash-fold (lambda (key value result)
                    (and result
                         (let ((equal? (if (hash-table? value)
                                           hash-table=?
                                           equal?)))
                           (equal? value
                                   (hash-ref t2 key)))))
                  #t
                  t1)))

(define (http-get-body uri)
  (call-with-values (lambda () (http-get uri))
    (lambda (response body) body)))

(define (wait-until-ready port)
  ;; Wait until the server is accepting connections.
  (let ((conn (socket PF_INET SOCK_STREAM 0)))
    (let loop ()
      (unless (false-if-exception
               (connect conn AF_INET (inet-pton AF_INET "127.0.0.1") port))
        (loop)))))

(define (test-cuirass-uri route)
  (string-append "http://localhost:6688" route))

(define database-name
  ;; Use an empty and temporary database for the tests.
  (string-append (getcwd) "/" (number->string (getpid)) "-tmp.db"))

(define %db
  ;; Global Slot for a database object.
  (make-parameter #t))

(define build-query-result
  '((#:id . 1)
    (#:project . "guix")
    (#:jobset . "master")
    (#:job . "fake-job")
    (#:timestamp . 1501347493)
    (#:starttime . 1501347493)
    (#:stoptime . 1501347493)
    (#:derivation . "/gnu/store/fake.drv")
    (#:buildoutputs . ((out ("path" . "/gnu/store/fake-1.0"))))
    (#:system . "x86_64-linux")
    (#:nixname . "fake-1.0")
    (#:buildstatus . 0)
    (#:busy . 0)
    (#:priority . 0)
    (#:finished . 1)
    (#:buildproducts . #nil)
    (#:releasename . #nil)
    (#:buildinputs_builds . #nil)))

(define evaluations-query-result
  '((#:id . 2)
    (#:specification . "guix")
    (#:revision . "fakesha2")))

(test-group-with-cleanup "http"
  (test-assert "object->json-string"
    ;; Note: We cannot compare the strings directly because field ordering
    ;; depends on the hash algorithm used in Guile's hash tables, and that
    ;; algorithm changed in Guile 2.2.
    (hash-table=?
     (call-with-input-string
         (string-append "{"
                        "\"boolean\" : false,"
                        "\"string\" : \"guix\","
                        "\"alist\" : {\"subset\" : \"hello\"},"
                        "\"list\" : [1, \"2\", \"three\"],"
                        "\"symbol\" : \"hydra-jobs\","
                        "\"number\" : 1"
                        "}")
       json->scm)
     (call-with-input-string
         (object->json-string '((#:number . 1)
                                (string . "guix")
                                ("symbol" . hydra-jobs)
                                (#:alist (subset . "hello"))
                                (list 1 "2" #:three)
                                ("boolean" . #f)))
       json->scm)))

  (test-assert "db-init"
    (%db (db-init database-name)))

  (test-assert "cuirass-run"
    (call-with-new-thread
     (lambda ()
       (run-fibers
        (lambda ()
          (run-cuirass-server (%db) #:port 6688))
        #:drain? #t))))

  (test-assert "wait-server"
    (wait-until-ready 6688))

  (test-assert "fill-db"
    (let* ((build1
            `((#:derivation . "/gnu/store/fake.drv")
              (#:eval-id . 1)
              (#:log . "unused so far")
              (#:status . ,(build-status succeeded))
              (#:outputs . (("out" . "/gnu/store/fake-1.0")))
              (#:timestamp . 1501347493)
              (#:starttime . 1501347493)
              (#:stoptime . 1501347493)))
           (build2
            `((#:derivation . "/gnu/store/fake2.drv")
              (#:eval-id . 1)
              (#:log . "unused so far")
              (#:status . ,(build-status scheduled))
              (#:outputs . (("out" . "/gnu/store/fake-2.0")))
              (#:timestamp . 1501347493)
              (#:starttime . 0)
              (#:stoptime . 0)))
           (derivation1
            '((#:derivation . "/gnu/store/fake.drv")
              (#:job-name . "fake-job")
              (#:system . "x86_64-linux")
              (#:nix-name . "fake-1.0")
              (#:eval-id . 1)))
           (derivation2
            '((#:derivation . "/gnu/store/fake2.drv")
              (#:job-name . "fake-job")
              (#:system . "x86_64-linux")
              (#:nix-name . "fake-2.0")
              (#:eval-id . 1)))
           (specification
            '((#:name . "guix")
              (#:url . "git://git.savannah.gnu.org/guix.git")
              (#:load-path . ".")
              (#:file . "/tmp/gnu-system.scm")
              (#:proc . hydra-jobs)
              (#:arguments (subset . "hello"))
              (#:branch . "master")
              (#:tag . #f)
              (#:commit . #f)
              (#:no-compile? . #f)))
           (evaluation1
            '((#:specification . "guix")
              (#:revision . "fakesha1")))
           (evaluation2
            '((#:specification . "guix")
              (#:revision . "fakesha2"))))
      (db-add-build (%db) build1)
      (db-add-build (%db) build2)
      (db-add-derivation (%db) derivation1)
      (db-add-derivation (%db) derivation2)
      (db-add-specification (%db) specification)
      (db-add-evaluation (%db) evaluation1)
      (db-add-evaluation (%db) evaluation2)))

  (test-assert "/build/1"
    (hash-table=?
     (call-with-input-string
         (utf8->string
          (http-get-body (test-cuirass-uri "/build/1")))
       json->scm)
     (call-with-input-string
         (object->json-string build-query-result)
       json->scm)))

  (test-equal "POST /build/1"
    405                                           ;Method Not Allowed
    (response-code (http-post (test-cuirass-uri "/build/1"))))

  (test-equal "/build/1/log/raw"
    `(302 ,(string->uri-reference "/log/fake-1.0"))
    (let ((response (http-get (test-cuirass-uri "/build/1/log/raw"))))
      (list (response-code response)
            (response-location response))))

  (test-equal "/build/42"
    404
    (response-code (http-get (test-cuirass-uri "/build/42"))))

  (test-equal "/build/42/log/raw"
    404
    (response-code (http-get (test-cuirass-uri "/build/42/log/raw"))))

  (test-equal "/api/latestbuilds"
    500
    (response-code (http-get (test-cuirass-uri "/api/latestbuilds"))))

  (test-assert "/api/latestbuilds?nr=1&project=guix&jobset=master"
    (let ((hash-list
           (call-with-input-string
               (utf8->string
                (http-get-body
                 (test-cuirass-uri
                  "/api/latestbuilds?nr=1&project=guix&jobset=master")))
             json->scm)))
      (and (= (length hash-list) 1)
           (hash-table=?
            (car hash-list)
            (call-with-input-string
                (object->json-string build-query-result)
              json->scm)))))

  (test-assert "/api/latestbuilds?nr=1&project=gnu"
    ;; The result should be an empty JSON array.
    (let ((hash-list
           (call-with-input-string
               (utf8->string
                (http-get-body
                 (test-cuirass-uri
                  "/api/latestbuilds?nr=1&project=gnu")))
             json->scm)))
      (= (length hash-list) 0)))

  (test-equal "/api/queue?nr=100"
    `("fake-2.0" ,(build-status scheduled))
    (match (call-with-input-string
               (utf8->string
                (http-get-body
                 (test-cuirass-uri "/api/queue?nr=100")))
             json->scm)
      ((dictionary)
       (list (hash-ref dictionary "nixname")
             (hash-ref dictionary "buildstatus")))))

  (test-assert "/api/evaluations?nr=1"
    (let ((hash-list
           (call-with-input-string
               (utf8->string
                (http-get-body (test-cuirass-uri "/api/evaluations?nr=1")))
             json->scm)))
      (and (= (length hash-list) 1)
           (hash-table=?
            (car hash-list)
            (call-with-input-string
                (object->json-string evaluations-query-result)
              json->scm)))))

  (test-assert "db-close"
    (db-close (%db)))

  (delete-file database-name))
