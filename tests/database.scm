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

(use-modules (cuirass base)
             (cuirass database)
             (cuirass notification)
             (cuirass parameters)
             (cuirass remote)
             (cuirass specification)
             (cuirass utils)
             (tests common)
             (guix channels)
             ((guix utils) #:select (call-with-temporary-output-file))
             (rnrs io ports)
             (squee)
             (ice-9 match)
             (srfi srfi-19)
             (srfi srfi-64))

(define (mailer)
  (string-append "sendmail://" (getcwd) "/tests/mail.sh"))

;; The above bash program will be invoked by mailutils.  It copies what's
;; passed on the standard input to the following file.
(define tmp-mail ".tmp-mail")

(false-if-exception (delete-file tmp-mail))

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
                           (job-name "job")
                           (outputs
                            `(("foo" . ,(format #f "~a.output" drv)))))
  `((#:derivation . ,drv)
    (#:eval-id . ,eval-id)
    (#:job-name . ,job-name)
    (#:timestamp . ,(time-second (current-time time-utc)))
    (#:system . "x86_64-linux")
    (#:nix-name . "foo")
    (#:log . "log")
    (#:outputs . ,outputs)))

(define %dummy-worker
  (worker
   (name "worker")
   (address "address")
   (machine "machine")
   (systems '("a" "b"))
   (last-seen 1)))

(define* (retry f #:key times delay)
  (let loop ((attempt 1))
    (let ((result (f)))
      (cond
       (result result)
       (else
        (if (>= attempt times)
            #f
            (begin
              (sleep delay)
              (loop (+ 1 attempt)))))))))

(test-group-with-cleanup "database"
  (test-assert "db-init"
    (begin
      (test-init-db!)
      (start-notification-thread)
      #t))

  (test-equal "db-add-or-update-specification"
    "guix"
    (db-add-or-update-specification example-spec))

  (test-equal "db-add-or-update-specification 2"
    'core
    (begin
      (db-add-or-update-specification
       (specification
        (inherit example-spec)
        (build 'core)))
      (specification-build
       (db-get-specification "guix"))))

  (test-assert "exec-query"
    (begin
      (exec-query (%db) "\
INSERT INTO Evaluations (specification, status,
timestamp, checkouttime, evaltime) VALUES ('guix', 0, 0, 0, 0);")
      (exec-query (%db) "SELECT * FROM Evaluations;")))

  (test-assert "db-get-specification"
    (let* ((spec (db-get-specification "guix"))
           (channels (specification-channels spec))
           (build-outputs (specification-build-outputs spec)))
      (and (string=? (specification-name spec) "guix")
           (equal? (map channel-name channels) '(guix my-channel))
           (equal? (map build-output-job build-outputs) '("job")))))

  (test-equal "db-add-evaluation"
    '(2 3)
    (list
     (db-add-evaluation "guix"
                        (make-dummy-instances "fakesha1" "fakesha2"))
     (db-add-evaluation "guix"
                        (make-dummy-instances "fakesha3" "fakesha4"))))

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

  (test-equal "db-get-pending-derivations"
    '("/foo.drv")
    (db-get-pending-derivations))

  (test-equal "db-get-checkouts"
    '("fakesha1" "fakesha2")
    (begin
      (make-dummy-instances "fakesha1" "fakesha2")
      (map (cut assq-ref <> #:commit) (db-get-checkouts 2))))

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

  (test-equal "db-add-or-update-worker"
    1
    (begin
      (db-add-or-update-worker %dummy-worker)
      (db-add-or-update-worker %dummy-worker)))

  (test-equal "db-get-worker"
    %dummy-worker
    (db-get-worker "worker"))

  (test-equal "db-get-workers"
    (list %dummy-worker)
    (db-get-workers))

  (test-assert "db-remove-unresponsive-workers"
    (begin
      (let ((drv "/foo.drv"))
        (db-update-build-worker! drv "worker")
        (db-update-build-status! drv (build-status started))
        (db-remove-unresponsive-workers 50)
        (and (eq? (db-get-workers) '())
             (let* ((build (db-get-build drv))
                    (worker (assq-ref build #:worker))
                    (status (assq-ref build #:status)))
               (and (not worker)
                    (eq? status (build-status scheduled))))))))

  (test-equal "db-clear-workers"
    '()
    (begin
      (db-clear-workers)
      (db-get-workers)))

  (test-equal "db-update-build-status!"
    (list (build-status scheduled)
          (build-status started)
          (build-status succeeded)
          "/foo2.log")
    (let* ((derivation (db-add-build
                        (make-dummy-build "/foo2.drv" 2
                                          #:outputs '(("out" . "/foo")))))
           (get-status (lambda* (#:optional (key #:status))
                         (assq-ref (db-get-build derivation) key))))
      (let ((status0 (get-status)))
        (db-update-build-status! "/foo2.drv" (build-status started)
                                 #:log-file "/foo2.log")
        (let ((status1 (get-status)))
          (db-update-build-status! "/foo2.drv" (build-status succeeded))

          ;; Second call shouldn't make any difference.
          (db-update-build-status! "/foo2.drv" (build-status succeeded))

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

  (test-assert "db-get-build-percentages"
    (begin
      (let* ((ts (time-second (current-time time-utc)))
             (old `((#:derivation . "/last.drv")
                    (#:eval-id . 2)
                    (#:job-name . "job")
                    (#:timestamp . ,(- ts 10))
                    (#:status . 0)
                    (#:starttime . 10)
                    (#:stoptime . 20)
                    (#:system . "x86_64-linux")
                    (#:nix-name . "foo")
                    (#:log . "log")
                    (#:outputs . (("out" . "/old-percentage")))))
             (new `((#:derivation . "/cur.drv")
                    (#:eval-id . 2)
                    (#:job-name . "job")
                    (#:timestamp . ,(- ts 5))
                    (#:starttime . ,(- ts 5))
                    (#:system . "x86_64-linux")
                    (#:nix-name . "foo")
                    (#:log . "log")
                    (#:outputs . (("out" . "/new-percentage"))))))
        (db-add-build old)
        (db-add-build new)
        (match (db-get-build-percentages
                (list (db-get-build (assq-ref new #:derivation))))
          ((percentage)
           (>= percentage 50))))))

  (test-equal "db-update-build-status!"
    (list #f 1)
    (begin
      (db-add-evaluation "guix"
                         (make-dummy-instances "fakesha5" "fakesha6"))
      (db-add-build (make-dummy-build "/old-build.drv" 3
                                      #:job-name "job-1"
                                      #:outputs `(("out" . "/old"))))
      (db-add-build (make-dummy-build "/new-build.drv" 4
                                      #:job-name "job-1"
                                      #:outputs `(("out" . "/new"))))
      (db-update-build-status! "/old-build.drv" 1)
      (db-update-build-status! "/new-build.drv" 0)
      (map (cut assq-ref <> #:last-status)
           (list (db-get-build "/old-build.drv")
                 (db-get-build "/new-build.drv")))))

  (test-equal "db-get-builds weather"
    (build-weather new-success)
    (begin
      (assq-ref (db-get-build "/new-build.drv") #:weather)))

  (test-assert "mail notification"
    (retry
     (lambda ()
       (and (file-exists? tmp-mail)
            (let ((str (call-with-input-file tmp-mail
                         get-string-all)))
              (string-contains str "Build job-1 on guix is fixed."))))
     #:times 5
     #:delay 1))

  (test-equal "db-get-builds weather"
    (build-weather new-failure)
    (begin
      (db-update-build-status! "/old-build.drv" 0)
      (db-update-build-status! "/new-build.drv" 1)
      (assq-ref (db-get-build "/new-build.drv") #:weather)))

  (test-assert "mail notification"
    (retry
     (lambda ()
       (and (file-exists? tmp-mail)
            (let ((str (call-with-input-file tmp-mail
                         get-string-all)))
              (string-contains str "Build job-1 on guix is broken."))))
     #:times 5
     #:delay 1))

  (test-equal "db-get-builds weather"
    (build-weather still-succeeding)
    (begin
      (db-update-build-status! "/old-build.drv" 0)
      (db-update-build-status! "/new-build.drv" 0)
      (assq-ref (db-get-build "/new-build.drv") #:weather)))

  (test-equal "db-get-builds weather"
    (build-weather still-failing)
    (begin
      (db-update-build-status! "/old-build.drv" 1)
      (db-update-build-status! "/new-build.drv" 1)
      (assq-ref (db-get-build "/new-build.drv") #:weather)))

  (test-assert "db-restart-build!"
    (let ((build (db-get-build "/new-build.drv")))
      (db-restart-build! (assq-ref build #:id))
      (eq? (assq-ref (db-get-build "/new-build.drv") #:status)
           (build-status scheduled))))

  (test-assert "db-restart-evaluation!"
    (let ((build (db-get-build "/old-build.drv")))
      (db-restart-evaluation! (assq-ref build #:eval-id))
      (eq? (assq-ref (db-get-build "/old-build.drv") #:status)
           (build-status scheduled))))

  (test-assert "db-retry-evaluation!"
    (begin
      (db-retry-evaluation! 4)
      (null? (db-get-checkouts 4))))

  (test-assert "db-cancel-pending-builds!"
    (let* ((drv "/old-build.drv")
           (build (db-get-build drv))
           (eval-id (assq-ref build #:eval-id)))
      (db-update-build-status! drv (build-status started))
      (db-cancel-pending-builds! eval-id)
      (eq? (assq-ref (db-get-build drv) #:status)
           (build-status canceled))))

  (test-assert "db-push-notification"
    (let ((build (db-get-build "/new-build.drv")))
      (db-push-notification
       (email
        (from "from")
        (to "to")
        (server (mailer)))
       (assq-ref build #:id))))

  (test-assert "db-pop-notification"
    (let ((build (db-get-build "/new-build.drv")))
      (match (db-pop-notification)
        ((notif . notif-build)
         (and (email? notif)
              (equal? build notif-build))))))

  (test-assert "set-build-successful!"
    (let* ((name "/foo5.drv")
           (build
            (make-dummy-build name #:outputs `(("out" . ,(getcwd)))))
           (drv (assq-ref build #:derivation)))
      (db-add-build build)
      (set-build-successful! drv)
      (match (assq-ref (db-get-build name) #:buildproducts)
        ((product)
         (equal? (assq-ref product #:path) (getcwd))))))

  (test-assert "db-worker-current-builds"
    (begin
      (let ((drv-1
             (db-add-build (make-dummy-build "/build-1.drv")))
            (drv-2
             (db-add-build (make-dummy-build "/build-2.drv"))))
        (db-add-or-update-worker %dummy-worker)
        (db-update-build-worker! drv-1 "worker")
        (db-update-build-worker! drv-2 "worker")
        (db-update-build-status! drv-1 (build-status started))
        (db-update-build-status! drv-2 (build-status started))
        (match (db-worker-current-builds)
          ((build)
           (eq? (assq-ref (db-get-build drv-2) #:id)
                (assq-ref build #:id)))))))

  (test-assert "db-close"
    (begin
      (db-close (%db))
      #t)))
