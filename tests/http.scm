;;; http.scm -- tests for (cuirass http) module
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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
             (json)
             (srfi srfi-1)
             (srfi srfi-64))

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

(test-begin "http")

(test-assert "spec->json-string"
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
       (spec->json-string '((#:number . 1)
                            (string . "guix")
                            ("symbol" . hydra-jobs)
                            (#:alist (subset . "hello"))
                            (list 1 "2" #:three)
                            ("boolean" . #f)))
     json->scm)))

(test-end)
