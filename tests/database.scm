;;;; database.scm - tests for (cuirass database) module
;;;
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;;
;;; This file is part of Cuirass.
;;;
;;; Cuirass is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Cuirass is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Cuirass.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (cuirass database)
             (cuirass remote)
             (cuirass utils)
             ((guix utils) #:select (call-with-temporary-output-file))
             (squee)
             (ice-9 match)
             (srfi srfi-19)
             (srfi srfi-64))

(define example-spec
  '((#:name . "guix")
    (#:load-path-inputs . ("savannah"))
    (#:package-path-inputs . ())
    (#:proc-input . "savannah")
    (#:proc-file . "/tmp/gnu-system.scm")
    (#:proc . hydra-jobs)
    (#:proc-args (subset . "hello"))
    (#:inputs . (((#:name . "maintenance")
                  (#:url . "git://git.savannah.gnu.org/guix/maintenance.git")
                  (#:load-path . ".")
                  (#:branch . "master")
                  (#:tag . #f)
                  (#:commit . #f)
                  (#:no-compile? . #f))
                 ((#:name . "savannah")
                  (#:url . "git://git.savannah.gnu.org/guix.git")
                  (#:load-path . ".")
                  (#:branch . "master")
                  (#:tag . #f)
                  (#:commit . #f)
                  (#:no-compile? . #f))))
    (#:build-outputs . ())
    (#:priority . 9)))

(define (make-dummy-checkouts fakesha1 fakesha2)
  `(((#:commit . ,fakesha1)
     (#:input . "savannah")
     (#:directory . "foo"))
    ((#:commit . ,fakesha2)
     (#:input . "maintenance")
     (#:directory . "bar"))))

(define* (make-dummy-build drv
                           #:optional (eval-id 2)
                           #:key (outputs
                                  `(("foo" . ,(format #f "~a.output" drv)))))
  `((#:derivation . ,drv)
    (#:eval-id . ,eval-id)
    (#:job-name . "job")
    (#:timestamp . ,(time-second (current-time time-utc)))
    (#:system . "x86_64-linux")
    (#:nix-name . "foo")
    (#:log . "log")
    (#:outputs . ,outputs)))

(define %dummy-worker
  (worker
   (name "worker")
   (address "address")
   (systems '("a" "b"))
   (last-seen "1")))

(define %db
  (make-parameter #f))

(define db-name "test_database")
(%record-events? #t)

(test-group-with-cleanup "database"
  (test-assert "db-init"
    (begin
      (%db (db-open))
      (%db-channel (make-worker-thread-channel
                    (lambda ()
                      (list (%db)))))
      #t))

  (test-equal "db-add-specification"
    "guix"
    (db-add-specification example-spec))

  (test-assert "exec-query"
    (begin
      (exec-query (%db) "\
INSERT INTO Evaluations (specification, status,
timestamp, checkouttime, evaltime) VALUES ('guix', 0, 0, 0, 0);")
      (exec-query (%db) "SELECT * FROM Evaluations;")))

  (test-equal "db-get-specification"
    example-spec
    (db-get-specification "guix"))

  (test-equal "db-add-evaluation"
    '(2 3)
    (list
     (db-add-evaluation "guix"
                        (make-dummy-checkouts "fakesha1" "fakesha2"))
     (db-add-evaluation "guix"
                        (make-dummy-checkouts "fakesha3" "fakesha4"))))

  (test-assert "db-set-evaluation-status"
    (db-set-evaluation-status 2 (evaluation-status started)))

  (test-assert "db-set-evaluation-time"
    (db-set-evaluation-time 2))

  (test-assert "db-abort-pending-evaluations"
    (db-abort-pending-evaluations))

  (test-equal "db-add-build"
    "/foo.drv"
    (let ((build (make-dummy-build "/foo.drv")))
      (db-add-build build)))

  (test-equal "db-add-build duplicate"
    "/foo.drv"
    (let ((build (make-dummy-build "/foo.drv")))
      (db-add-build build)))

  (test-assert "db-add-build-product"
    (db-add-build-product `((#:build . 1)
                            (#:type . "1")
                            (#:file-size . 1)
                            (#:checksum . "sum")
                            (#:path . "path"))))

  (test-equal "db-get-output"
    '((#:derivation . "/foo.drv") (#:name . "foo"))
    (db-get-output "/foo.drv.output"))

  (test-equal "db-get-outputs"
    '(("foo" (#:path . "/foo.drv.output")))
    (db-get-outputs "/foo.drv"))

  (test-assert "db-get-time-since-previous-build"
    (db-get-time-since-previous-build "job" "guix"))

  (test-assert "db-register-builds"
    (let ((drv "/test.drv"))
      (db-register-builds `(((#:job-name . "test")
                             (#:derivation . ,drv)
                             (#:system . "x86_64-linux")
                             (#:nix-name . "test")
                             (#:log . "log")
                             (#:outputs .
                              (("foo" . ,(format #f "~a.output" drv))
                               ("foo2" . ,(format #f "~a.output.2" drv))))))
                          2 (db-get-specification "guix"))))

  (test-assert "db-update-build-status!"
    (db-update-build-status! "/test.drv"
                             (build-status failed)))

  (test-assert "db-update-build-worker!"
    (db-update-build-worker! "/test.drv" "worker"))

  (test-equal "db-get-builds-by-search"
    '(3 1 "test")
    (let ((build
           (match (db-get-builds-by-search
                   '((nr . 1)
                     (query . "status:failed test")))
             ((build) build))))
      (list
       (assoc-ref build #:id)
       (assoc-ref build #:status)
       (assoc-ref build #:job-name))))

  (test-assert "db-get-builds"
    (let* ((build (match (db-get-builds `((order . build-id)
                                          (status . failed)))
                    ((build) build)))
           (outputs (assq-ref build #:outputs)))
      (equal? outputs
              '(("foo" (#:path . "/test.drv.output"))
                ("foo2" (#:path . "/test.drv.output.2"))))))

  (test-equal "db-get-builds job-name"
    "/foo.drv"
    (let ((build (match (db-get-builds `((order . build-id)
                                         (job . "job")))
                   ((build) build))))
      (assoc-ref build #:derivation)))

  (test-equal "db-get-build"
    "/foo.drv"
    (let ((build (db-get-build 1)))
      (assoc-ref build #:derivation)))

  (test-equal "db-get-build derivation"
    1
    (let ((build (db-get-build "/foo.drv")))
      (assoc-ref build #:id)))

  (test-equal "db-get-events"
    'evaluation
    (let ((event (match (db-get-events '((nr . 1)
                                         (type . evaluation)))
                   ((event) event))))
      (assoc-ref event #:type)))

  (test-equal "db-delete-events-with-ids-<=-to"
    1
    (db-delete-events-with-ids-<=-to 1))

  (test-equal "db-get-pending-derivations"
    '("/foo.drv")
    (db-get-pending-derivations))

  (test-assert "db-get-checkouts"
    (equal? (db-get-checkouts 2)
            (make-dummy-checkouts "fakesha1" "fakesha2")))

  (test-equal "db-get-evaluation"
    "guix"
    (let ((evaluation (db-get-evaluation 2)))
      (assq-ref evaluation #:specification)))

  (test-equal "db-get-evaluations"
    '("guix" "guix")
    (map (lambda (eval)
           (assq-ref eval #:specification))
         (db-get-evaluations 2)))

  (test-equal "db-get-evaluations-build-summary"
    '((0 0 0) (0 1 1))
    (let ((summaries
           (db-get-evaluations-build-summary "guix" 2 #f #f)))
      (map (lambda (summary)
             (list
              (assq-ref summary #:succeeded)
              (assq-ref summary #:failed)
              (assq-ref summary #:scheduled)))
           summaries)))

  (test-equal "db-get-evaluations-id-min"
    1
    (db-get-evaluations-id-min "guix"))

  (test-equal "db-get-evaluations-id-min"
    #f
    (db-get-evaluations-id-min "foo"))

  (test-equal "db-get-evaluations-id-max"
    3
    (db-get-evaluations-id-max "guix"))

  (test-equal "db-get-evaluations-id-max"
    #f
    (db-get-evaluations-id-max "foo"))

  (test-equal "db-get-evaluation-summary"
    '(2 0 1 1)
    (let* ((summary (db-get-evaluation-summary 2))
           (total (assq-ref summary #:total))
           (succeeded (assq-ref summary #:succeeded))
           (failed (assq-ref summary #:failed))
           (scheduled (assq-ref summary #:scheduled)))
      (list total succeeded failed scheduled)))

  (test-equal "db-get-evaluation-summary empty"
    '(0 0 0 0)
    (let* ((summary (db-get-evaluation-summary 3))
           (total (assq-ref summary #:total))
           (succeeded (assq-ref summary #:succeeded))
           (failed (assq-ref summary #:failed))
           (scheduled (assq-ref summary #:scheduled)))
      (list total succeeded failed scheduled)))

  (test-equal "db-get-builds-query-min"
    '(1)
    (db-get-builds-query-min "spec:guix foo"))

  (test-equal "db-get-builds-query-max"
    '(3)
    (db-get-builds-query-min "spec:guix status:failed test"))

  (test-equal "db-get-builds-min"
    3
    (match (db-get-builds-min 2 "failed")
      ((timestamp id)
       id)))

  (test-equal "db-get-builds-max"
    1
    (match (db-get-builds-max 2 "pending")
      ((timestamp id)
       id)))

  (test-equal "db-get-evaluation-specification"
    "guix"
    (db-get-evaluation-specification 2))

  (test-equal "db-get-build-products"
    `(((#:id . 1)
        (#:type . "1")
        (#:file-size . 1)
        (#:checksum . "sum")
        (#:path . "path")))
    (db-get-build-products 1))

  (test-equal "db-get-build-product-path"
    "path"
    (db-get-build-product-path 1))

  (test-equal "db-add-worker"
    1
    (db-add-worker %dummy-worker))

  (test-equal "db-get-workers"
    (list %dummy-worker)
    (db-get-workers))

  (test-equal "db-clear-workers"
    '()
    (begin
      (db-clear-workers)
      (db-get-workers)))

  (test-equal "db-update-build-status!"
    (list (build-status scheduled)
          (build-status started)
          (build-status succeeded)
          "/foo2.drv.log")
    (let* ((derivation (db-add-build
                        (make-dummy-build "/foo2.drv" 2
                                          #:outputs '(("out" . "/foo")))))
           (get-status (lambda* (#:optional (key #:status))
                         (assq-ref (db-get-build derivation) key))))
      (let ((status0 (get-status)))
        (db-update-build-status! "/foo2.drv" (build-status started))
        (let ((status1 (get-status)))
          (db-update-build-status! "/foo2.drv" (build-status succeeded)
                                   #:log-file "/foo2.drv.log")

          ;; Second call shouldn't make any difference.
          (db-update-build-status! "/foo2.drv" (build-status succeeded)
                                   #:log-file "/foo2.drv.log")

          (let ((status2 (get-status))
                (start   (get-status #:starttime))
                (end     (get-status #:stoptime))
                (log     (get-status #:log)))
            (and (> start 0) (>= end start)
                 (list status0 status1 status2 log)))))))

  (test-equal "db-get-builds"
    '(("/baa.drv" "/bar.drv" "/baz.drv") ;ascending order
      ("/baz.drv" "/bar.drv" "/baa.drv") ;descending order
      ("/baz.drv" "/bar.drv" "/baa.drv") ;ditto
      ("/baz.drv")                               ;nr = 1
      ("/bar.drv" "/baa.drv" "/baz.drv")) ;status+submission-time
    (begin
      (exec-query (%db) "DELETE FROM Builds;")
      (db-add-build (make-dummy-build "/baa.drv" 2
                                      #:outputs `(("out" . "/baa"))))
      (db-add-build (make-dummy-build "/bar.drv" 2
                                      #:outputs `(("out" . "/bar"))))
      (db-add-build (make-dummy-build "/baz.drv" 2
                                      #:outputs `(("out" . "/baz"))))
      (db-update-build-status! "/bar.drv" (build-status started)
                               #:log-file "/bar.drv.log")
      (let ((summarize (lambda (alist)
                         (assq-ref alist #:derivation))))
        (list (map summarize (db-get-builds '((nr . 3) (order . build-id))))
              (map summarize (db-get-builds '()))
              (map summarize (db-get-builds '((jobset . "guix"))))
              (map summarize (db-get-builds '((nr . 1))))
              (map summarize
                   (db-get-builds '((order . status+submission-time))))))))

  (test-equal "db-get-pending-derivations"
    '("/bar.drv" "/foo.drv")
    (begin
      (exec-query (%db) "DELETE FROM Builds;")
      (db-add-build (make-dummy-build "/foo.drv" 1
                                      #:outputs `(("out" . "/foo"))))
      (db-add-build (make-dummy-build "/bar.drv" 2
                                      #:outputs `(("out" . "/bar"))))
      (sort (db-get-pending-derivations) string<?)))

  (test-assert "db-close"
    (begin
      (exec-query (%db) (format #f "DROP OWNED BY CURRENT_USER;"))
      (db-close (%db))
      #t)))
