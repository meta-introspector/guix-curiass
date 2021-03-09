;;; common.scm -- Common test helpers.
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (tests common)
  #:use-module (cuirass database)
  #:use-module (cuirass parameters)
  #:use-module (cuirass utils)
  #:export (%db
            test-init-db!))

(define %db
  (make-parameter #f))

(define (test-init-db!)
  "Initialize the test database."
  (%create-database? #t)
  (%cuirass-database "test_tmp")
  (%db (db-open))
  (%db-channel (make-worker-thread-channel
                (lambda ()
                  (list (%db))))))
