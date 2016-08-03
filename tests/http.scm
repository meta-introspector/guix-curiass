;;; http.scm -- tests for (cuirass http) module
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
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
             (srfi srfi-64))

(test-begin "http")

(test-equal "spec->json-string"
  (string-append "{"
                 "\"boolean\" : false,"
                 "\"string\" : \"guix\","
                 "\"alist\" : {\"subset\" : \"hello\"},"
                 "\"list\" : [1, \"2\", \"three\"],"
                 "\"symbol\" : \"hydra-jobs\","
                 "\"number\" : 1"
                 "}")
  (spec->json-string '((#:number . 1)
                       (string . "guix")
                       ("symbol" . hydra-jobs)
                       (#:alist (subset . "hello"))
                       (list 1 "2" #:three)
                       ("boolean" . #f))))

(test-end)
