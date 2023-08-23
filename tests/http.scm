;;; http.scm -- tests for (cuirass http) module
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2020, 2021 Mathieu Othacehe <othacehe@gnu.org>
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
             (cuirass specification)
             (cuirass utils)
             (tests common)
             (guix channels)
             (json)
             (fibers)
             (squee)
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

(define build-query-result
  '((id . 1)
    (evaluation . 1)
    (jobset . "guix")
    (job . "fake-job")
    (timestamp . 1501347493)
    (starttime . 1501347493)
    (stoptime . 1501347493)
    (derivation . "/gnu/store/fake.drv")
    (buildoutputs . ((out ("path" . "/gnu/store/fake-1.0"))))
    (system . "x86_64-linux")
    (nixname . "fake-1.0")
    (buildstatus . 0)
    (weather . -1)
    (busy . 0)
    (priority . 9)
    (finished . 1)
    (buildproducts . #())))

(define evaluations-query-result
  #(((id . 2)
     (specification . "guix")
     (status . -1)
     (timestamp . 1501347493)
     (checkouttime . 0)
     (evaltime . 0)
     (checkouts . #(((commit . "fakesha2")
                     (channel . "guix")
                     (directory . "dir3")))))))

(test-group-with-cleanup "http"
  (test-assert "db-init"
    (begin
      (test-init-db!)
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
            (build (derivation "/gnu/store/fake.drv")
                   (evaluation-id 1)
                   (specification-name "guix")
                   (job-name "fake-job")
                   (system "x86_64-linux")
                   (nix-name "fake-1.0")
                   (log "unused so far")
                   (status (build-status succeeded))
                   (outputs
                    (list (output
                           (item "/gnu/store/fake-1.0")
                           (derivation derivation))))
                   (creation-time 1501347493)
                   (start-time 1501347493)
                   (completion-time 1501347493)))
           (build2
            (build (derivation "/gnu/store/fake2.drv")
                   (evaluation-id 1)
                   (specification-name "guix")
                   (job-name "fake-job")
                   (system "x86_64-linux")
                   (nix-name "fake-2.0")
                   (log "unused so far")
                   (status (build-status scheduled))
                   (outputs
                    (list (output
                           (item "/gnu/store/fake-2.0")
                           (derivation derivation))))
                   (creation-time 1501347493)))
           (spec
            (specification
             (name "guix")
             (build 'hello)
             (channels
              (list (channel
                     (name 'guix)
                     (url "https://gitlab.com/mothacehe/guix.git")
                     (branch "master"))
                    (channel
                     (name 'packages)
                     (url "https://gitlab.com/mothacehe/guix.git")
                     (branch "master"))))))
           (checkouts1
            (list
             (checkout->channel-instance "dir1"
                                         #:name 'guix
                                         #:url "url1"
                                         #:commit "fakesha1")
             (checkout->channel-instance "dir2"
                                         #:name 'packages
                                         #:url "url2"
                                         #:commit "fakesha3")))
           (checkouts2
            (list
             (checkout->channel-instance "dir3"
                                         #:name 'guix
                                         #:url "dir3"
                                         #:commit "fakesha2")
             (checkout->channel-instance "dir4"
                                         #:name 'packages
                                         #:url "dir4"
                                         #:commit "fakesha3"))))
      (db-add-or-update-specification spec)
      (db-add-evaluation "guix" checkouts1
                         #:timestamp 1501347493)
      (db-add-evaluation "guix" checkouts2
                         #:timestamp 1501347493)
      (db-add-build build1)
      (db-add-build build2)))

  (test-assert "/specifications"
    (match (call-with-input-string
               (utf8->string
                (http-get-body (test-cuirass-uri "/specifications")))
             json->scm)
      (#(spec)
       (string=? (assoc-ref spec "name") "guix"))))

  (test-assert "/build/1"
    (lset= equal?
     (call-with-input-string
         (utf8->string
          (http-get-body (test-cuirass-uri "/build/1")))
       json->scm)
     (call-with-input-string
         (scm->json-string build-query-result)
       json->scm)))

  (test-equal "/build/42"
    404
    (response-code (http-get (test-cuirass-uri "/build/42"))))

  (test-equal "/build/42)"
    404
    (response-code (http-get (test-cuirass-uri "/build/42)"))))

  (test-equal "/build/42/log/raw"
    404
    (response-code (http-get (test-cuirass-uri "/build/42/log/raw"))))

  (test-equal "/build/42xx/log/raw"
    404
    (response-code (http-get (test-cuirass-uri "/build/42xx/log/raw"))))

  (test-equal "/build/42/details"
    404
    (response-code (http-get (test-cuirass-uri "/build/42/details"))))

  (test-equal "/build/42xx/details"
    404
    (response-code (http-get (test-cuirass-uri "/build/42xx/details"))))

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
               (scm->json-string build-query-result))))))

  (test-equal "/api/latestbuilds?nr=1&jobset=gnu"
    #()                              ;the result should be an empty JSON array
    (json-string->scm
     (utf8->string
      (http-get-body
       (test-cuirass-uri
        "/api/latestbuilds?nr=1&jobset=gnu")))))

  (test-equal "/api/latestbuilds?nr&jobset=gnu"
    500
    (response-code
     (http-get
      (test-cuirass-uri
       "/api/latestbuilds?nr&jobset=gnu"))))

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
     (scm->json-string evaluations-query-result))
    (json-string->scm
     (utf8->string
      (http-get-body (test-cuirass-uri "/api/evaluations?nr=1")))))

  (test-assert "db-close"
    (begin
      (db-close (%db))
      #t)))
