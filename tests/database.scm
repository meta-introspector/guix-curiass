;;;; database.scm - tests for (cuirass database) module
;;;
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2018, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2020 Mathieu Othacehe <othacehe@gnu.org>
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

(use-modules (cuirass base)
             (cuirass database)
             (cuirass notification)
             (cuirass parameters)
             (cuirass remote)
             (cuirass specification)
             (cuirass utils)
             ((cuirass logging) #:select (current-logging-level))
             (tests common)
             (guix channels)
             ((guix utils) #:select (call-with-temporary-output-file))
             (rnrs io ports)
             (squee)
             (fibers)
             (ice-9 control)
             (ice-9 exceptions)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-19)
             (srfi srfi-64))

(define (mailer)
  (string-append "sendmail://"
                 (search-path %load-path "tests/mail.sh")))

;; The above bash program will be invoked by mailutils.  It copies what's
;; passed on the standard input to the following file.
(define tmp-mail ".tmp-mail")

(define example-spec
  (specification
   (name "guix")
   (build 'hello)
   (channels
    (list (channel
           (name 'guix)
           (url "git://git.savannah.gnu.org/guix.git")
           (branch "master"))
          (channel
           (name 'my-channel)
           (url "git://my-git-channel.git")
           (branch "master"))))
   (build-outputs
    (list (build-output
           (job "job")
           (type "type")
           (output "out")
           (path ""))))
   (notifications
    (list (email
           (from "from")
           (to "to")
           (server (mailer)))))))

(define (make-dummy-instances fakesha1 fakesha2)
  (list
   (checkout->channel-instance "foo"
                               #:name 'guix
                               #:url "git://git.savannah.gnu.org/guix.git"
                               #:commit fakesha1)
   (checkout->channel-instance "bar"
                               #:name 'my-channel
                               #:url "git://my-git-channel.git"
                               #:commit fakesha2)))

(define* (make-dummy-build drv
                           #:optional (eval-id 2)
                           #:key
                           (system "x86_64-linux")
                           (jobset "whatever")
                           (priority 9)
                           (timestamp 0)
                           (job-name "job")
                           (outputs
                            (list
                             (output (name "foo")
                                     (derivation drv)
                                     (item (format #f "~a.output" drv))))))
  (build (derivation drv)
         (evaluation-id eval-id)
         (specification-name jobset)
         (job-name job-name)
         (system system)
         (nix-name "foo")
         (log "log")
         (outputs outputs)
         (priority priority)
         (creation-time timestamp)))

(define %dummy-worker
  (worker
   (name "worker")
   (address "address")
   (machine "machine")
   (systems '("a" "b"))
   (last-seen 1)))

(define-syntax-rule (with-fibers exp ...)
  "Evaluate EXP... in a Fiber context with a database connection pool."
  (let ((db (db-open)))
    (define result
      (run-fibers
       (lambda ()
         (parameterize ((%db-connection-pool
                         (make-resource-pool (list db))))
           (let/ec return
             (with-exception-handler
                 (lambda (exception)
                   ;; XXX: 'display-backtrace' might throw in a way that
                   ;; 'false-if-exception' cannot catch.
                   ;;
                   ;; (false-if-exception
                   ;;  (display-backtrace (make-stack #t) (current-error-port)))
                   (return exception))
               (lambda ()
                 exp ...)))))
       #:drain? #f
       #:parallelism 1
       #:hz 5))

    (db-close db)
    (if (exception? result)
        (raise-exception result)
        result)))

(current-logging-level 'debug)

(test-group-with-cleanup "database"
  (test-assert "db-init"
    (begin
      (test-init-db!)
      #t))

  (test-equal "db-add-or-update-specification"
    "guix"
    (with-fibers
      (db-add-or-update-specification example-spec)))

  (test-equal "db-add-or-update-specification 2"
    'core
    (with-fibers
      (db-add-or-update-specification
       (specification
        (inherit example-spec)
        (build 'core)))
      (specification-build
       (db-get-specification "guix"))))

  (test-assert "db-add-or-update-specification 3"
    (with-fibers
      (db-add-or-update-specification
       (specification
        (inherit example-spec)
        (name "tmp")
        (build 'core)))
      (db-deactivate-specification "tmp")
      (eq? (length (db-get-specifications)) 1)))

  (test-assert "exec-query"
    (begin
      (exec-query (%db) "\
INSERT INTO Evaluations (specification, status,
timestamp, checkouttime, evaltime) VALUES ('guix', 0, 0, 0, 0);")
      (exec-query (%db) "SELECT * FROM Evaluations;")))

  (test-assert "db-get-specification"
    (with-fibers
      (let* ((spec (db-get-specification "guix"))
             (channels (specification-channels spec))
             (build-outputs (specification-build-outputs spec)))
        (and (string=? (specification-name spec) "guix")
             (equal? (map channel-name channels) '(guix my-channel))
             (equal? (map build-output-job build-outputs) '("job"))))))

  (test-equal "db-add-evaluation"
    '(2 3)
    (with-fibers
      (list (db-add-evaluation "guix"
                               (make-dummy-instances "fakesha1" "fakesha2"))
            (db-add-evaluation "guix"
                               (make-dummy-instances "fakesha3" "fakesha4")))))

  (test-equal "db-get-latest-checkout"
    '("fakesha3" "fakesha4")
    (with-fibers
      (map checkout-commit
           (list (db-get-latest-checkout "guix" 'guix 3)
                 (db-get-latest-checkout "guix" 'my-channel 3)))))

  (test-assert "db-set-evaluation-status"
    (with-fibers
      (db-set-evaluation-status 2 (evaluation-status started))))

  (test-assert "db-set-evaluation-time"
    (with-fibers
      (db-set-evaluation-time 2)))

  (test-assert "db-abort-pending-evaluations"
    (with-fibers
      (db-abort-pending-evaluations)))

  (test-equal "db-add-build"
    "/foo.drv"
    (with-fibers
      (let* ((build (make-dummy-build "/foo.drv"))
             (id    (db-add-build build)))
        (and (not (db-add-build build))           ;duplicate
             (build-derivation (db-get-build id))))))

  (test-assert "db-add-build-product"
    (with-fibers
      (db-add-build-product (build-product (build-id 1)
                                           (type "1")
                                           (file-size 1)
                                           (checksum "sum")
                                           (file "path")))))

  (test-equal "db-get-output"
    (output (name "foo") (derivation "/foo.drv")
            (item "/foo.drv.output"))
    (with-fibers
     (db-get-output "/foo.drv.output")))

  (test-equal "db-get-outputs"
    (list (output (name "foo") (derivation "/foo.drv")
                  (item "/foo.drv.output")))
    (with-fibers
      (db-get-outputs "/foo.drv")))

  (test-assert "db-get-time-since-previous-eval"
    (with-fibers
      (db-get-time-since-previous-eval "guix")))

  (test-assert "db-register-builds"
    (with-fibers
      (let ((drv "/test.drv"))
        (db-register-builds
         (list (build (job-name "test")
                      (evaluation-id 2)
                      (specification-name "whatever")
                      (derivation drv)
                      (system "x86_64-linux")
                      (nix-name "test")
                      (log "log")
                      (outputs
                       (list (output
                              (name "foo")
                              (derivation drv)
                              (item (string-append drv ".output")))
                             (output
                              (name "foo2")
                              (derivation drv)
                              (item (string-append drv ".output.2")))))))
         (db-get-specification "guix")))))

  (test-assert "db-get-jobs"
    (with-fibers
      (match (db-get-jobs 2
                          '((#:system . "x86_64-linux")))
        ((job)
         (string=? (job-name job) "test")))))

  (test-assert "db-get-jobs names"
    (with-fibers
      (match (db-get-jobs 2
                          '((names "test")))
        ((job)
         (string=? (job-name job) "test")))))

  (test-assert "db-register-builds same-outputs"
    (with-fibers
      (let ((drv "/test2.drv"))
        (db-add-evaluation "guix"
                           (make-dummy-instances "fakesha5" "fakesha6"))
        (db-register-builds
         (list (build (job-name "test")
                      (evaluation-id 4)
                      (specification-name "whatever")
                      (derivation drv)
                      (system "x86_64-linux")
                      (nix-name "test")
                      (log "log")
                      (outputs
                       (list (output
                              (name "foo")
                              (derivation drv)
                              (item "/test.drv.output"))
                             (output
                              (name "foo2")
                              (derivation drv)
                              (item "/test.drv.output.2"))))))
         (db-get-specification "guix")))))

  (test-equal "db-get-previous-eval"
    1
    (with-fibers
      (db-get-previous-eval 4)))

  (test-assert "db-get-next-eval"
    (with-fibers
      (not (db-get-next-eval 3))))

  (test-equal "db-get-jobs same-outputs"
    "/test.drv"
    (with-fibers
      (match (db-get-jobs 4 '())
        ((job)
         (build-derivation (db-get-build (job-build-id job)))))))

  (test-assert "db-get-jobs-history"
    (with-fibers
      (db-set-evaluation-status 4 (evaluation-status succeeded))
      (match (db-get-jobs-history '("test")
                                  #:spec "guix"
                                  #:limit 2)
        ((eval)
         (and (eq? (assq-ref eval 'evaluation) 4)
              (eq? (length (assq-ref eval 'jobs)) 1))))))

  (test-assert "db-update-build-status!"
    (with-fibers
      (db-update-build-status! "/test.drv"
                               (build-status failed))))

  (test-assert "db-update-build-worker!"
    (with-fibers
      (db-update-build-worker! "/test.drv" "worker")))

  (test-equal "db-get-builds-by-search"
    '(3 1 "test")
    (with-fibers
      (let ((build
             (match (db-get-builds-by-search
                     '((nr . 1)
                       (query . "status:failed test")))
               ((build) build))))
        (list (build-id build)
              (build-current-status build)
              (build-job-name build)))))

  (test-equal "db-get-builds + build-outputs"
    (list (output (name "foo") (derivation "/test.drv")
                  (item "/test.drv.output"))
          (output (name "foo2") (derivation "/test.drv")
                  (item "/test.drv.output.2")))
    (with-fibers
     (match (db-get-builds `((order . build-id)
                             (status . failed)))
       ((build)
        (build-outputs build)))))

  (test-equal "db-get-builds job-name"
    "/foo.drv"
    (with-fibers
     (match (db-get-builds `((order . build-id)
                             (job . "job")))
       ((build)
        (build-derivation build)))))

  (test-equal "db-get-build"
    "/foo.drv"
    (with-fibers
     (build-derivation (db-get-build 1))))

  (test-equal "db-get-build derivation"
    1
    (with-fibers
     (build-id (db-get-build "/foo.drv"))))

  (test-equal "db-get-pending-derivations"
    '("/foo.drv")
    (with-fibers
      (db-get-pending-derivations)))

  (test-equal "db-get-checkouts"
    '("fakesha1" "fakesha2")
    (with-fibers
      (make-dummy-instances "fakesha1" "fakesha2")
      (map checkout-commit (db-get-checkouts 2))))

  (test-equal "db-get-evaluation"
    "guix"
    (with-fibers
      (let ((evaluation (db-get-evaluation 2)))
        (evaluation-specification-name evaluation))))

  (test-equal "db-get-evaluations"
    '("guix" "guix")
    (with-fibers
      (map (lambda (eval)
             (evaluation-specification-name eval))
           (db-get-evaluations 2))))

  (test-equal "db-get-evaluations-build-summary"
    '((0 0 0 0) (0 0 0 0) (0 1 0 1))
    (with-fibers
      (let ((summaries
             (db-get-evaluations-build-summary "guix" 3 #f #f)))
        (map (lambda (summary)
               (list (build-summary-succeeded summary)
                     (build-summary-failed summary)
                     (build-summary-newly-failed summary)
                     (build-summary-scheduled summary)))
             summaries))))

  (test-equal "db-get-evaluation-absolute-summary"
    (list 0 1 0 0 (evaluation-status succeeded))
    (with-fibers
      (let ((summary
             (db-get-evaluation-absolute-summary
              (db-get-latest-evaluation "guix"))))
        (list (evaluation-summary-succeeded summary)
              (evaluation-summary-failed summary)
              (evaluation-summary-newly-failed summary)
              (evaluation-summary-scheduled summary)
              (evaluation-summary-status summary)))))

  (test-equal "db-get-evaluations-absolute-summary"
    '((0 1 0 0) (0 0 0 0) (0 1 0 0))
    (with-fibers
      (let* ((evaluations
              (db-get-evaluations-build-summary "guix" 3 #f #f))
             (summaries
              (db-get-evaluations-absolute-summary evaluations)))
        (map (lambda (summary)
               (list (evaluation-summary-succeeded summary)
                     (evaluation-summary-failed summary)
                     (evaluation-summary-newly-failed summary)
                     (evaluation-summary-scheduled summary)))
             summaries))))

  (test-equal "db-get-evaluations-id-min"
    1
    (with-fibers
      (db-get-evaluations-id-min "guix")))

  (test-equal "db-get-evaluations-id-min"
    #f
    (with-fibers
      (db-get-evaluations-id-min "foo")))

  (test-equal "db-get-evaluations-id-max"
    4
    (with-fibers
      (db-get-evaluations-id-max "guix")))

  (test-equal "db-get-evaluations-id-max"
    #f
    (with-fibers
      (db-get-evaluations-id-max "foo")))

  (test-equal "db-get-latest-evaluation"
    4
    (with-fibers
      (db-get-latest-evaluation "guix")))

  (test-equal "db-get-latest-evaluations"
    4
    (with-fibers
      (match (db-get-latest-evaluations)
        ((eval)
         (evaluation-id eval)))))

  (test-equal "db-get-latest-evaluations 2"
    4
    (with-fibers
      (match (db-get-latest-evaluations #:status #f)
        ((eval)
         (evaluation-id eval)))))

  (test-equal "db-get-evaluation-summary"
    '(2 0 1 0 1)
    (with-fibers
      (let* ((summary (db-get-evaluation-summary 2))
             (total (evaluation-summary-total summary))
             (succeeded (evaluation-summary-succeeded summary))
             (failed (evaluation-summary-failed summary))
             (newly-failed (evaluation-summary-newly-failed summary))
             (scheduled (evaluation-summary-scheduled summary)))
        (list total succeeded failed newly-failed scheduled))))

  (test-equal "db-get-evaluation-summary empty"
    '(0 0 0 0)
    (with-fibers
      (let* ((summary (db-get-evaluation-summary 3))
             (total (evaluation-summary-total summary))
             (succeeded (evaluation-summary-succeeded summary))
             (failed (evaluation-summary-failed summary))
             (scheduled (evaluation-summary-scheduled summary)))
        (list total succeeded failed scheduled))))

  (test-equal "db-get-builds-query-min"
    '(1)
    (with-fibers
      (db-get-builds-query-min "spec:guix foo")))

  (test-equal "db-get-builds-query-max"
    '(3)
    (with-fibers
      (db-get-builds-query-min "spec:guix status:failed test")))

  (test-equal "db-get-builds-min"
    3
    (with-fibers
      (match (db-get-builds-min 2 "failed")
        ((timestamp id)
         id))))

  (test-equal "db-get-builds-max"
    1
    (with-fibers
      (match (db-get-builds-max 2 "pending")
        ((timestamp id)
         id))))

  (test-equal "db-get-evaluation-specification"
    "guix"
    (with-fibers
      (db-get-evaluation-specification 2)))

  (test-equal "db-get-build-products"
    (list (build-product (id 1)
                         (build-id 1)
                         (type "1")
                         (file-size 1)
                         (checksum "sum")
                         (file "path")))
    (with-fibers
     (db-get-build-products 1)))

  (test-equal "db-get-build-product-path"
    "path"
    (with-fibers
      (db-get-build-product-path 1)))

  (test-equal "db-add-or-update-worker"
    1
    (with-fibers
      (db-add-or-update-worker %dummy-worker)
      (db-add-or-update-worker %dummy-worker)))

  (test-equal "db-get-worker"
    %dummy-worker
    (with-fibers
      (db-get-worker "worker")))

  (test-equal "db-get-workers"
    (list %dummy-worker)
    (with-fibers
      (db-get-workers)))

  (test-assert "db-remove-unresponsive-workers"
    (with-fibers
      (let ((drv "/foo.drv"))
        (db-update-build-worker! drv "worker")
        (db-update-build-status! drv (build-status started))
        (db-remove-unresponsive-workers 50)
        (and (eq? (db-get-workers) '())
             (let* ((build (db-get-build drv))
                    (worker (build-worker build))
                    (status (build-current-status build)))
               (and (not worker)
                    (eq? status (build-status scheduled))))))))

  (test-equal "db-clear-workers"
    '()
    (with-fibers
      (db-clear-workers)
      (db-get-workers)))

  (test-equal "db-update-build-status!"
    (list (build-status scheduled)
          (build-status started)
          (build-status succeeded)
          "/foo2.log")
    (with-fibers
      (let* ((derivation "/foo2.drv")
             (get-status (lambda* (#:optional (field build-current-status))
                           (field (db-get-build derivation)))))
        (db-add-build
         (make-dummy-build derivation 2
                           #:outputs
                           (list (output
                                  (derivation derivation)
                                  (item "/foo")))))
        (let ((status0 (get-status)))
          (db-update-build-status! "/foo2.drv" (build-status started)
                                   #:log-file "/foo2.log")
          (let ((status1 (get-status)))
            (db-update-build-status! "/foo2.drv" (build-status succeeded))

            ;; Second call shouldn't make any difference.
            (db-update-build-status! "/foo2.drv" (build-status succeeded))

            (let ((status2 (get-status))
                  (start   (get-status build-start-time))
                  (end     (get-status build-completion-time))
                  (log     (get-status build-log)))
              (and (> start 0) (>= end start)
                   (list status0 status1 status2 log))))))))

  (test-equal "db-get-builds"
    '(("/baa.drv" "/bar.drv" "/baz.drv")          ;ascending order
      ("/baz.drv" "/bar.drv" "/baa.drv")          ;descending order
      ("/baz.drv" "/bar.drv" "/baa.drv")          ;ditto
      ("/baz.drv")                                ;nr = 1
      ("/bar.drv" "/baa.drv" "/baz.drv"))         ;status+submission-time
    (with-fibers
      (exec-query (%db) "DELETE FROM Builds;")
      (db-add-build (make-dummy-build "/baa.drv" 2
                                      #:outputs
                                      (list (output
                                             (item "/baa")
                                             (derivation "/baa.drv")))))
      (db-add-build (make-dummy-build "/bar.drv" 2
                                      #:outputs
                                      (list (output
                                             (item "/bar")
                                             (derivation "/bar.drv")))))
      (db-add-build (make-dummy-build "/baz.drv" 2
                                      #:outputs
                                      (list (output
                                             (item "/baz")
                                             (derivation "/baz.drv")))))
      (db-update-build-status! "/bar.drv" (build-status started)
                               #:log-file "/bar.drv.log")
      (list (map build-derivation (db-get-builds '((nr . 3) (order . build-id))))
            (map build-derivation (db-get-builds '()))
            (map build-derivation (db-get-builds '((jobset . "guix"))))
            (map build-derivation (db-get-builds '((nr . 1))))
            (map build-derivation
                 (db-get-builds '((order . status+submission-time)))))))

  (test-equal "db-get-pending-derivations"
    '("/bar.drv" "/foo.drv")
    (with-fibers
      (exec-query (%db) "DELETE FROM Builds;")
      (db-add-build (make-dummy-build "/foo.drv" 1
                                      #:outputs
                                      (list (output
                                             (item "/foo")
                                             (derivation "/foo.drv")))))
      (db-add-build (make-dummy-build "/bar.drv" 2
                                      #:outputs
                                      (list (output
                                             (item "/bar")
                                             (derivation "/bar.drv")))))
      (sort (db-get-pending-derivations) string<?)))

  (test-equal "db-get-build-percentages"
    '("/cur.drv" #t)
    (with-fibers
      (let* ((ts (time-second (current-time time-utc)))
             (old (build (derivation "/last.drv")
                         (evaluation-id 2)
                         (job-name "job")
                         (specification-name "whatever")
                         (creation-time (- ts 10))
                         (status 0)
                         (start-time 10)
                         (completion-time 20)
                         (system "x86_64-linux")
                         (nix-name "foo")
                         (log "log")
                         (outputs
                          (list (output
                                 (item "/old-percentage")
                                 (derivation "/last.drv"))))))
             (new (build (derivation "/cur.drv")
                         (evaluation-id 2)
                         (job-name "job")
                         (specification-name "whatever")
                         (creation-time (- ts 5))
                         (start-time (- ts 5))
                         (system "x86_64-linux")
                         (nix-name "foo")
                         (log "log")
                         (outputs
                          (list (output
                                 (item "/new-percentage")
                                 (derivation "/cur.drv")))))))
        (db-add-build old)
        (db-add-build new)
        (match (db-get-build-percentages
                (list (db-get-build (build-derivation new))))
          (((b . percentage))
           (list (build-derivation b)
                 (>= percentage 50)))))))

  (test-equal "db-update-build-status!"
    (list #f (build-status failed))
    (with-fibers
      (db-add-evaluation "guix"
                         (make-dummy-instances "fakesha5" "fakesha6"))
      (db-add-build (make-dummy-build "/old-build.drv" 3
                                      #:job-name "job-1"
                                      #:outputs
                                      (list (output
                                             (item "/old")
                                             (derivation "/old-build.drv")))))
      (db-add-build (make-dummy-build "/new-build.drv" 4
                                      #:job-name "job-1"
                                      #:outputs
                                      (list (output
                                             (item "/new")
                                             (derivation "/new-build.drv")))))

      (db-update-build-status! "/old-build.drv" (build-status failed))
      (db-update-build-status! "/new-build.drv" (build-status succeeded))
      (map build-last-status
           (list (db-get-build "/old-build.drv")
                 (db-get-build "/new-build.drv")))))

  (test-equal "db-get-builds weather"
    (build-weather new-success)
    (with-fibers
      (build-current-weather (db-get-build "/new-build.drv"))))

  (test-assert "mail notification"
    (with-fibers
      (spawn-notification-fiber)
      (retry
       (lambda ()
         (and (file-exists? tmp-mail)
              (let ((str (call-with-input-file tmp-mail
                           get-string-all)))
                (string-contains str "Build job-1 on guix is fixed."))))
       #:times 5
       #:delay 1)))

  (test-equal "db-get-builds weather"
    (build-weather new-failure)
    (with-fibers
      (db-update-build-status! "/old-build.drv" 0)
      (db-update-build-status! "/new-build.drv" 1)
      (build-current-weather (db-get-build "/new-build.drv"))))

  (test-assert "mail notification, broken job"
    (with-fibers
      (spawn-notification-fiber)
      (retry
       (lambda ()
         (and (file-exists? tmp-mail)
              (let ((str (call-with-input-file tmp-mail
                           get-string-all)))
                (string-contains str "Build job-1 on guix is broken."))))
       #:times 5
       #:delay 1)))

  (test-equal "db-get-builds weather"
    (build-weather still-succeeding)
    (with-fibers
      (db-update-build-status! "/old-build.drv" 0)
      (db-update-build-status! "/new-build.drv" 0)
      (build-current-weather (db-get-build "/new-build.drv"))))

  (test-equal "db-get-builds weather"
    (build-weather still-failing)
    (with-fibers
      (db-update-build-status! "/old-build.drv" 1)
      (db-update-build-status! "/new-build.drv" 1)
      (build-current-weather (db-get-build "/new-build.drv"))))

  (test-assert "db-restart-build!"
    (with-fibers
      (let ((build (db-get-build "/new-build.drv")))
        (db-restart-build! (build-id build))
        (= (build-current-status (db-get-build "/new-build.drv"))
           (build-status scheduled)))))

  (test-assert "db-restart-evaluation!"
    (with-fibers
      (let ((build (db-get-build "/old-build.drv")))
        (db-restart-evaluation! (build-evaluation-id build))
        (= (build-current-status (db-get-build "/old-build.drv"))
           (build-status scheduled)))))

  (test-assert "db-retry-evaluation!"
    (with-fibers
      (db-retry-evaluation! 4)
      (null? (db-get-checkouts 4))))

  (test-assert "db-cancel-pending-builds!"
    (with-fibers
      (let* ((drv "/old-build.drv")
             (build (db-get-build drv))
             (eval-id (build-evaluation-id build)))
        (db-update-build-status! drv (build-status started))
        (db-cancel-pending-builds! eval-id)
        (= (build-current-status (db-get-build drv))
           (build-status canceled)))))

  (test-assert "db-push-notification"
    (with-fibers
      (let ((build (db-get-build "/new-build.drv")))
        (db-push-notification
         (email
          (from "from")
          (to "to")
          (server (mailer)))
         (build-id build)))))

  (test-assert "db-pop-notification"
    (with-fibers
      (let ((build (db-get-build "/new-build.drv")))
        (match (db-pop-notification)
          ((notif . notif-build)
           (and (email? notif)
                ;; <build> records cannot be compared with 'equal?' because
                ;; they contain procedures, hence this comparison.
                (string=? (build-derivation build)
                          (build-derivation notif-build))
                (eqv? (build-id build)
                      (build-id notif-build))))))))

  (test-assert "set-build-successful!"
    (with-fibers
      (let* ((name "/foo5.drv")
             (build
              (make-dummy-build name
                                #:outputs
                                (list (output
                                       (item (getcwd))
                                       (derivation "/foo5.drv")))))
             (drv (build-derivation build)))
        (db-add-build build)
        (set-build-successful! drv)
        (match (build-products (db-get-build name))
          ((product)
           (equal? (build-product-file product) (getcwd)))))))

  (test-assert "db-worker-current-builds"
    (with-fibers
      (let ((drv-1 "/build-1.drv")
            (drv-2 "/build-2.drv"))
        (db-add-build (make-dummy-build drv-1))
        (db-add-build (make-dummy-build drv-2))
        (db-add-or-update-worker %dummy-worker)
        (db-update-build-worker! drv-1 "worker")
        (db-update-build-worker! drv-2 "worker")
        (db-update-build-status! drv-1 (build-status started))
        (db-update-build-status! drv-2 (build-status started))
        (match (db-worker-current-builds)
          ((build)
           (= (build-id (db-get-build drv-2))
              (build-id build)))))))

  (test-equal "db-register-dashboard"
    "guix"
    (with-fibers
      (let ((id (db-register-dashboard "guix" "emacs")))
        (dashboard-specification-name (db-get-dashboard id)))))

  (test-assert "db-add-build-dependencies"
    (with-fibers
      (db-add-build-dependencies "/build-1.drv"
                                 (list "/build-2.drv"))))

  (test-assert "db-get-build-dependencies"
    (with-fibers
      (let* ((drv1 "/build-1.drv")
             (drv2 "/build-2.drv")
             (id1 (build-id (db-get-build drv1)))
             (id2 (build-id (db-get-build drv2))))
        (match (db-get-build-dependencies id1)
          ((id) (= id id2))))))

  (test-equal "build-dependencies"
    '("/build-2.drv")
    (with-fibers
     (build-dependencies (db-get-build "/build-1.drv"))))

  (test-equal "db-get-pending-build"
    '("/pending-build-3.drv"                      ;high-priority first
      "/pending-build-4.drv"
      "/pending-build-1.drv"                      ;older first
      "/pending-build-2.drv"
      #f)                                         ;no more builds!
    (with-fibers
     (db-add-build (make-dummy-build "/pending-build-1.drv"
                                     #:system "riscv-gnu"
                                     #:priority 9 ;low priority
                                     #:timestamp 1))
     (db-add-build (make-dummy-build "/pending-build-2.drv"
                                     #:system "riscv-gnu"
                                     #:priority 9
                                     #:timestamp 2))
     (db-add-build (make-dummy-build "/pending-build-3.drv"
                                     #:system "riscv-gnu"
                                     #:priority 1 ;high priority
                                     #:timestamp 3))
     (db-add-build (make-dummy-build "/pending-build-4.drv"
                                     #:system "riscv-gnu"
                                     #:priority 1
                                     #:timestamp 4))
     (for-each (lambda (drv)
                 (db-update-build-status! drv (build-status scheduled)))
               '("/pending-build-1.drv"
                 "/pending-build-2.drv"
                 "/pending-build-3.drv"
                 "/pending-build-4.drv"))
     (let loop ((i 0)
                (lst '()))
       (if (= i 5)
           (reverse lst)
           (loop (+ 1 i)
                 (let ((drv (and=> (pk (db-get-pending-build "riscv-gnu"))
                                   build-derivation)))
                   (when drv
                     (db-update-build-status! drv (build-status succeeded)))
                   (cons drv lst)))))))

  (test-assert "dependencies trigger"
    (with-fibers
      (let ((drv-1 "/build-dep-1.drv")
            (drv-2 "/build-dep-2.drv")
            (drv-3 "/build-dep-3.drv")
            (drv-4 "/build-dep-4.drv")
            (drv-5 "/build-dep-5.drv")
            (drv-6 "/build-dep-6.drv")
            (drv-7 "/build-dep-7.drv")
            (status (lambda (drv)
                      (build-current-status (db-get-build drv)))))
        (for-each (compose db-add-build make-dummy-build)
                  (list drv-1 drv-2 drv-3 drv-4
                        drv-5 drv-6 drv-7))
        (db-add-build-dependencies "/build-dep-2.drv"
                                   (list "/build-dep-1.drv"))
        (db-add-build-dependencies "/build-dep-4.drv"
                                   (list "/build-dep-1.drv"
                                         "/build-dep-3.drv"))
        (db-add-build-dependencies "/build-dep-6.drv"
                                   (list "/build-dep-4.drv"
                                         "/build-dep-5.drv"))
        (db-add-build-dependencies "/build-dep-7.drv"
                                   (list "/build-dep-4.drv"))
        (db-update-build-status! drv-1 (build-status failed))
        (db-update-build-status! drv-2 (build-status succeeded))
        (db-update-build-status! drv-5 (build-status canceled))
        (let loop ()
          (unless (eq? (db-update-failed-builds!) 0)
            (loop)))
        (and (eq? (status drv-4) (build-status failed-dependency))
             (eq? (status drv-6) (build-status failed-dependency))
             (eq? (status drv-7) (build-status failed-dependency))))))

  (test-equal "db-get-first-build-failure"
    '("/thing.drv2"                               ;last success
      "/thing.drv3")                              ;first failure
    (with-fibers
     (let ((derivation "/thing.drv")
           (job "thing-that-starts-failing"))
       (for-each (lambda (status n)
                   (let ((id (db-add-evaluation "guix"
                                                (make-dummy-instances
                                                 (number->string n)
                                                 "fakesha2")))
                         (drv (string-append derivation (number->string n))))
                     (db-add-build (make-dummy-build drv id
                                                     #:jobset "guix"
                                                     #:job-name job))
                     (db-update-build-status! drv status)))
                 (list (build-status failed)      ;0
                       (build-status succeeded)   ;1
                       (build-status succeeded)   ;2
                       (build-status failed)      ;3
                       (build-status failed))     ;4
                 (iota 5))
       (let ((last (db-get-build (string-append derivation "4")))
             (all  (db-get-builds `((job . ,job)
                                    (order . evaluation)))))
         (and (= (build-id last) (build-id (first all)))
              (map build-derivation
                   (list (db-get-previous-successful-build last)
                         (db-get-first-build-failure last))))))))

  (test-assert "db-close"
    (begin
      (false-if-exception (delete-file tmp-mail))
      (db-close (%db))
      #t)))

;; Local Variables:
;; eval: (put 'with-fibers 'scheme-indent-function 0)
;; End
