;;;; database.scm - tests for (cuirass database) module
;;;
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
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
                  (#:no-compile? . #f))))))

(define* (make-dummy-eval #:optional (commits '("cabba3e 61730ea")))
  `((#:specification . "guix")
    (#:commits . ,commits)))

(define* (make-dummy-build drv
                           #:optional (eval-id 42)
                           #:key (outputs '(("foo" . "/foo"))))
  `((#:derivation . ,drv)
    (#:eval-id . ,eval-id)
    (#:job-name . "job")
    (#:system . "x86_64-linux")
    (#:nix-name . "foo")
    (#:log . "log")
    (#:outputs . (("foo" . "/foo")))))

(define-syntax-rule (with-temporary-database db body ...)
  (call-with-temporary-output-file
   (lambda (file port)
     (parameterize ((%package-database file))
       (db-init file)
       (with-database db
         body ...)))))

(define %db
  ;; Global Slot for a database object.
  (make-parameter #t))

(define database-name
  ;; Use an empty and temporary database for the tests.
  (string-append (getcwd) "/" (number->string (getpid)) "-tmp.db"))

(test-group-with-cleanup "database"
  (test-assert "db-init"
    (%db (db-init database-name)))

  (test-assert "sqlite-exec"
    (begin
      (sqlite-exec (%db) "\
INSERT INTO Evaluations (specification, commits) VALUES (1, 1);")
      (sqlite-exec (%db) "\
INSERT INTO Evaluations (specification, commits) VALUES (2, 2);")
      (sqlite-exec (%db) "\
INSERT INTO Evaluations (specification, commits) VALUES (3, 3);")
      (sqlite-exec (%db) "SELECT * FROM Evaluations;")))

  (test-equal "db-add-specification"
    example-spec
    (begin
      (db-add-specification (%db) example-spec)
      (car (db-get-specifications (%db)))))

  (test-equal "db-add-build"
    #f
    (let ((build (make-dummy-build "/foo.drv")))
      (db-add-build (%db) build)

      ;; Should return #f when adding a build whose derivation is already
      ;; there, see <https://bugs.gnu.org/28094>.
      (db-add-build (%db) build)))

  (test-equal "db-update-build-status!"
    (list (build-status scheduled)
          (build-status started)
          (build-status succeeded)
          "/foo.drv.log")
    (with-temporary-database db
      (let* ((derivation (db-add-build
                          db
                          (make-dummy-build "/foo.drv" 1
                                            #:outputs '(("out" . "/foo")))))
             (get-status (lambda* (#:optional (key #:status))
                           (assq-ref (db-get-build db derivation) key))))
        (db-add-evaluation db (make-dummy-eval))
        (db-add-specification db example-spec)

        (let ((status0 (get-status)))
          (db-update-build-status! db "/foo.drv" (build-status started))
          (let ((status1 (get-status)))
            (db-update-build-status! db "/foo.drv" (build-status succeeded)
                                     #:log-file "/foo.drv.log")

            ;; Second call shouldn't make any difference.
            (db-update-build-status! db "/foo.drv" (build-status succeeded)
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
    (with-temporary-database db
      ;; Populate the 'Builds'', 'Evaluations', and
      ;; 'Specifications' tables in a consistent way, as expected by the
      ;; 'db-get-builds' query.
      (db-add-build db (make-dummy-build "/foo.drv" 1
                                         #:outputs `(("out" . "/foo"))))
      (db-add-build db (make-dummy-build "/bar.drv" 2
                                         #:outputs `(("out" . "/bar"))))
      (db-add-build db (make-dummy-build "/baz.drv" 3
                                         #:outputs `(("out" . "/baz"))))
      (db-add-evaluation db (make-dummy-eval))
      (db-add-evaluation db (make-dummy-eval))
      (db-add-evaluation db (make-dummy-eval))
      (db-add-specification db example-spec)

      (db-update-build-status! db "/bar.drv" (build-status started)
                               #:log-file "/bar.drv.log")

      (let ((summarize (lambda (alist)
                         (list (assq-ref alist #:id)
                               (assq-ref alist #:derivation)))))
        (vector (map summarize (db-get-builds db '((nr . 3)
                                                   (order . build-id))))
                (map summarize (db-get-builds db '()))
                (map summarize (db-get-builds db '((jobset . "guix"))))
                (map summarize (db-get-builds db '((nr . 1))))
                (map summarize
                     (db-get-builds
                      db '((order . status+submission-time))))))))

  (test-equal "db-get-pending-derivations"
    '("/bar.drv" "/foo.drv")
    (with-temporary-database db
      ;; Populate the 'Builds', 'Evaluations', and
      ;; 'Specifications' tables.  Here, two builds map to the same derivation
      ;; but the result of 'db-get-pending-derivations' must not contain any
      ;; duplicate.
      (db-add-build db (make-dummy-build "/foo.drv" 1
                                         #:outputs `(("out" . "/foo"))))
      (db-add-build db (make-dummy-build "/bar.drv" 2
                                         #:outputs `(("out" . "/bar"))))
      (db-add-build db (make-dummy-build "/foo.drv" 3
                                         #:outputs `(("out" . "/foo"))))
      (db-add-evaluation db (make-dummy-eval))
      (db-add-evaluation db (make-dummy-eval))
      (db-add-evaluation db (make-dummy-eval))
      (db-add-specification db example-spec)

      (sort (db-get-pending-derivations db) string<?)))

  (test-assert "db-close"
    (db-close (%db)))

  (delete-file database-name))

;;; Local Variables:
;;; eval: (put 'with-temporary-database 'scheme-indent-function 1)
;;; End:
