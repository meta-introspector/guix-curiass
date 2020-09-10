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
             ((guix utils) #:select (call-with-temporary-output-file))
             (cuirass utils)
             (srfi srfi-64))

(define example-spec
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
                 ((#:name . "maintenance")
                  (#:url . "git://git.savannah.gnu.org/guix/maintenance.git")
                  (#:load-path . ".")
                  (#:branch . "master")
                  (#:tag . #f)
                  (#:commit . #f)
                  (#:no-compile? . #f))))
    (#:build-outputs . ())))

(define (make-dummy-checkouts fakesha1 fakesha2)
  `(((#:commit . ,fakesha1)
     (#:input . "guix")
     (#:directory . "foo"))
    ((#:commit . ,fakesha2)
     (#:input . "packages")
     (#:directory . "bar"))))

(define* (make-dummy-build drv
                           #:optional (eval-id 42)
                           #:key (outputs
                                  `(("foo" . ,(format #f "~a.output" drv)))))
  `((#:derivation . ,drv)
    (#:eval-id . ,eval-id)
    (#:job-name . "job")
    (#:system . "x86_64-linux")
    (#:nix-name . "foo")
    (#:log . "log")
    (#:outputs . ,outputs)))

(define-syntax-rule (with-temporary-database body ...)
  (call-with-temporary-output-file
   (lambda (file port)
     (parameterize ((%package-database file))
       (db-init file)
       (with-database
         body ...)))))

(define %db
  ;; Global Slot for a database object.
  (make-parameter #t))

(define database-name
  ;; Use an empty and temporary database for the tests.
  (string-append (getcwd) "/" (number->string (getpid)) "-tmp.db"))

(test-group-with-cleanup "database"
  (test-assert "db-init"
    (begin
      (%db (db-init database-name))
      (%db-channel (make-worker-thread-channel
                    (lambda ()
                      (list (%db)))))
      #t))

  (test-assert "sqlite-exec"
    (begin
      (sqlite-exec (%db) "\
INSERT INTO Evaluations (specification, status,
timestamp, checkouttime, evaltime) VALUES (1, 0, 0, 0, 0);")
      (sqlite-exec (%db) "\
INSERT INTO Evaluations (specification, status,
timestamp, checkouttime, evaltime) VALUES (2, 0, 0, 0, 0);")
      (sqlite-exec (%db) "\
INSERT INTO Evaluations (specification, status,
timestamp, checkouttime, evaltime) VALUES (3, 0, 0, 0, 0);")
      (sqlite-exec (%db) "SELECT * FROM Evaluations;")))

  (test-equal "db-add-specification"
    example-spec
    (begin
      (db-add-specification example-spec)
      (car (db-get-specifications))))

  (test-equal "db-get-specification"
    example-spec
    (db-get-specification "guix"))

  (test-equal "db-add-build"
    #f
    (let ((build (make-dummy-build "/foo.drv")))
      (db-add-build build)

      ;; Should return #f when adding a build whose derivation is already
      ;; there, see <https://bugs.gnu.org/28094>.
      (db-add-build build)))

  (test-equal "db-add-build-with-fixed-output"
    #f
    (let ((build1 (make-dummy-build "/fixed1.drv"
                                    #:outputs '(("out" . "/fixed-output"))))
          (build2 (make-dummy-build "/fixed2.drv"
                                    #:outputs '(("out" . "/fixed-output")))))
      (db-add-build build1)

      ;; Should return #f because the outputs are the same.
      (db-add-build build2)))

  (test-equal "db-update-build-status!"
    (list (build-status scheduled)
          (build-status started)
          (build-status succeeded)
          "/foo.drv.log")
    (with-temporary-database
      (let* ((derivation (db-add-build
                          (make-dummy-build "/foo.drv" 1
                                            #:outputs '(("out" . "/foo")))))
             (get-status (lambda* (#:optional (key #:status))
                           (assq-ref (db-get-build derivation) key))))
        (db-add-evaluation "guix" (make-dummy-checkouts "fakesha1"
                                                        "fakesha2"))
        (db-add-specification example-spec)

        (let ((status0 (get-status)))
          (db-update-build-status! "/foo.drv" (build-status started))
          (let ((status1 (get-status)))
            (db-update-build-status! "/foo.drv" (build-status succeeded)
                                     #:log-file "/foo.drv.log")

            ;; Second call shouldn't make any difference.
            (db-update-build-status! "/foo.drv" (build-status succeeded)
                                     #:log-file "/foo.drv.log")

            (let ((status2 (get-status))
                  (start   (get-status #:starttime))
                  (end     (get-status #:stoptime))
                  (log     (get-status #:log)))
              (and (> start 0) (>= end start)
                   (list status0 status1 status2 log))))))))

  (test-equal "db-get-builds"
    #(((1 "/foo.drv") (2 "/bar.drv") (3 "/baz.drv")) ;ascending order
      ((3 "/baz.drv") (2 "/bar.drv") (1 "/foo.drv")) ;descending order
      ((3 "/baz.drv") (2 "/bar.drv") (1 "/foo.drv")) ;ditto
      ((3 "/baz.drv"))                               ;nr = 1
      ((2 "/bar.drv") (1 "/foo.drv") (3 "/baz.drv"))) ;status+submission-time
    (with-temporary-database
      ;; Populate the 'Builds'', 'Evaluations', and
      ;; 'Specifications' tables in a consistent way, as expected by the
      ;; 'db-get-builds' query.
      (db-add-build (make-dummy-build "/foo.drv" 1
                                      #:outputs `(("out" . "/foo"))))
      (db-add-build (make-dummy-build "/bar.drv" 2
                                      #:outputs `(("out" . "/bar"))))
      (db-add-build (make-dummy-build "/baz.drv" 3
                                      #:outputs `(("out" . "/baz"))))
      (db-add-evaluation "guix" (make-dummy-checkouts "fakesha1" "fakesha2"))
      (db-add-evaluation "guix" (make-dummy-checkouts "fakesha1" "fakesha3"))
      (db-add-evaluation "guix" (make-dummy-checkouts "fakssha2" "fakesha3"))
      (db-add-specification example-spec)

      (db-update-build-status! "/bar.drv" (build-status started)
                               #:log-file "/bar.drv.log")

      (let ((summarize (lambda (alist)
                         (list (assq-ref alist #:id)
                               (assq-ref alist #:derivation)))))
        (vector (map summarize (db-get-builds '((nr . 3) (order . build-id))))
                (map summarize (db-get-builds '()))
                (map summarize (db-get-builds '((jobset . "guix"))))
                (map summarize (db-get-builds '((nr . 1))))
                (map summarize
                     (db-get-builds '((order . status+submission-time))))))))

  (test-equal "db-get-pending-derivations"
    '("/bar.drv" "/foo.drv")
    (with-temporary-database
      ;; Populate the 'Builds', 'Evaluations', and
      ;; 'Specifications' tables.  Here, two builds map to the same derivation
      ;; but the result of 'db-get-pending-derivations' must not contain any
      ;; duplicate.
      (db-add-build (make-dummy-build "/foo.drv" 1
                                      #:outputs `(("out" . "/foo"))))
      (db-add-build (make-dummy-build "/bar.drv" 2
                                      #:outputs `(("out" . "/bar"))))
      (db-add-build (make-dummy-build "/foo.drv" 3
                                      #:outputs `(("out" . "/foo"))))
      (db-add-evaluation "guix" (make-dummy-checkouts "fakesha1" "fakesha2"))
      (db-add-evaluation "guix" (make-dummy-checkouts "fakesha1" "fakesha3"))
      (db-add-evaluation "guix" (make-dummy-checkouts "fakssha2" "fakesha3"))
      (db-add-specification example-spec)

      (sort (db-get-pending-derivations) string<?)))

  (test-assert "db-close"
    (db-close (%db)))

  (begin
    (%db-channel #f)
    (delete-file database-name)))

;;; Local Variables:
;;; eval: (put 'with-temporary-database 'scheme-indent-function 0)
;;; End:
