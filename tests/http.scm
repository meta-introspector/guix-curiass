;;; http.scm -- tests for (cuirass http) module
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
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
             (ice-9 threads)
             (ice-9 match))

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
    (#:jobset . "guix")
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
  #(((#:id . 2)
     (#:specification . "guix")
     (#:in-progress . 1)
     (#:checkouts . #(((#:commit . "fakesha2")
                       (#:input . "savannah")
                       (#:directory . "dir3")))))))

(test-group-with-cleanup "http"
  (test-assert "object->json-string"
    (lset= equal?
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
                                      (#:alist . ((subset . "hello")))
                                      (list . #(1 "2" #:three))
                                      ("boolean" . #f)))
             json->scm)))

  (test-assert "db-init"
    (begin
      (%db (db-init database-name))
      (%db-channel (make-worker-thread-channel (%db)))
      #t))

  (test-assert "cuirass-run"
    (call-with-new-thread
     (lambda ()
       (run-fibers
        (lambda ()
          (run-cuirass-server #:port 6688))
        #:drain? #t))))

  (test-assert "wait-server"
    (wait-until-ready 6688))

  (test-assert "fill-db"
    (let* ((build1
            `((#:derivation . "/gnu/store/fake.drv")
              (#:eval-id . 1)
              (#:job-name . "fake-job")
              (#:system . "x86_64-linux")
              (#:nix-name . "fake-1.0")
              (#:log . "unused so far")
              (#:status . ,(build-status succeeded))
              (#:outputs . (("out" . "/gnu/store/fake-1.0")))
              (#:timestamp . 1501347493)
              (#:starttime . 1501347493)
              (#:stoptime . 1501347493)))
           (build2
            `((#:derivation . "/gnu/store/fake2.drv")
              (#:eval-id . 1)
              (#:job-name . "fake-job")
              (#:system . "x86_64-linux")
              (#:nix-name . "fake-2.0")
              (#:log . "unused so far")
              (#:status . ,(build-status scheduled))
              (#:outputs . (("out" . "/gnu/store/fake-2.0")))
              (#:timestamp . 1501347493)
              (#:starttime . 0)
              (#:stoptime . 0)))
           (specification
            '((#:name . "guix")
              (#:load-path-inputs . ("savannah"))
              (#:package-path-inputs . ())
              (#:proc-input . "savannah")
              (#:proc-file . "/tmp/gnu-system.scm")
              (#:proc . hydra-jobs)
              (#:proc-args (subset . "hello"))
              (#:inputs . (((#:name . "savannah")
                            (#:url . "git://git.savannah.gnu.org/guix.git")
                            (#:load-path . ".")
                            (#:branch . "master")
                            (#:tag . #f)
                            (#:commit . #f)
                            (#:no-compile? . #f))
                           ((#:name . "packages")
                            (#:url . "git://git.savannah.gnu.org/guix.git")
                            (#:load-path . ".")
                            (#:branch . "master")
                            (#:tag . #f)
                            (#:commit . #f)
                            (#:no-compile? . #f))))))
           (checkouts1
            '(((#:commit . "fakesha1")
               (#:input . "savannah")
               (#:directory . "dir1"))
              ((#:commit . "fakesha3")
               (#:input . "packages")
               (#:directory . "dir2"))))
           (checkouts2
            '(((#:commit . "fakesha2")
               (#:input . "savannah")
               (#:directory . "dir3"))
              ((#:commit . "fakesha3")
               (#:input . "packages")
               (#:directory . "dir4")))))
      (db-add-build build1)
      (db-add-build build2)
      (db-add-specification specification)
      (db-add-evaluation "guix" checkouts1)
      (db-add-evaluation "guix" checkouts2)))

  (test-assert "/build/1"
    (lset= equal?
     (call-with-input-string
         (utf8->string
          (http-get-body (test-cuirass-uri "/build/1")))
       json->scm)
     (call-with-input-string
         (object->json-string build-query-result)
       json->scm)))

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

  (test-assert "/api/latestbuilds?nr=1&jobset=guix"
    (match (json-string->scm
            (utf8->string
             (http-get-body
              (test-cuirass-uri
               "/api/latestbuilds?nr=1&jobset=guix"))))
      (#(build)
       (lset= equal? build
              (json-string->scm
               (object->json-string build-query-result))))))

  (test-equal "/api/latestbuilds?nr=1&jobset=gnu"
    #()                              ;the result should be an empty JSON array
    (json-string->scm
     (utf8->string
      (http-get-body
       (test-cuirass-uri
        "/api/latestbuilds?nr=1&jobset=gnu")))))

  (test-equal "/api/queue?nr=100"
    `("fake-2.0" ,(build-status scheduled))
    (match (json-string->scm
            (utf8->string
             (http-get-body
              (test-cuirass-uri "/api/queue?nr=100"))))
      (#(dictionary)
       (list (assoc-ref dictionary "nixname")
             (assoc-ref dictionary "buildstatus")))))

  (test-equal "/api/evaluations?nr=1"
    (json-string->scm
     (object->json-string evaluations-query-result))
    (json-string->scm
     (utf8->string
      (http-get-body (test-cuirass-uri "/api/evaluations?nr=1")))))

  (test-assert "db-close"
    (db-close (%db)))

  (begin
    (%db-channel #f)
    (delete-file database-name)))
