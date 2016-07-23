;;; utils.scm -- helper procedures
;;; Copyright © 2012, 2013, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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

(define-module (cuirass utils)
  #:use-module (ice-9 match)
  #:export (;; Procedures
            mkdir-p
            make-user-module
            ;; Macros.
            λ*
            with-directory-excursion))

(define-syntax-rule (λ* formals body ...)
  (lambda* formals body ...))

(define mkdir-p
  (let ((not-slash (char-set-complement (char-set #\/))))
    (λ* (dir #:optional mode)
      "Create directory DIR and all its ancestors."
      (let ((absolute? (string-prefix? "/" dir)))
        (let loop ((components (string-tokenize dir not-slash))
                   (root       (if absolute? "" ".")))
          (match components
            ((head tail ...)
             (let ((dir-name (string-append root "/" head)))
               (catch 'system-error
                 (λ ()
                   (if mode
                       (mkdir dir-name mode)
                       (mkdir dir-name))
                   (loop tail dir-name))
                 (λ args
                   ;; On GNU/Hurd we can get EROFS instead of EEXIST here.
                   ;; Thus, if we get something other than EEXIST, check
                   ;; whether DIR-NAME exists.  See
                   ;; <https://lists.gnu.org/archive/html/guix-devel/2016-02/msg00049.html>.
                   (if (or (= EEXIST (system-error-errno args))
                           (let ((st (stat dir-name #f)))
                             (and st (eq? 'directory (stat:type st)))))
                       (loop tail dir-name)
                       (apply throw args))))))
            (() #t)))))))

(define-syntax-rule (with-directory-excursion dir body ...)
  "Run BODY with DIR as the process's current directory."
  (let ((init (getcwd)))
    (dynamic-wind
      (λ () (chdir dir))
      (λ () body ...)
      (λ () (chdir init)))))

(define* (make-user-module #:optional (modules '()))
  "Return a new user module with the additional MODULES loaded."
  ;; Module in which the machine description file is loaded.
  (let ((module (make-fresh-user-module)))
    (for-each (lambda (iface)
                (module-use! module (resolve-interface iface)))
              modules)
    module))
