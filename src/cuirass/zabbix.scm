;;; zabbix.scm -- Zabbix API connection.
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
;;;
;;; This file is part of Cuirass.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (cuirass zabbix)
  #:use-module (guix import json)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:export (%zabbix-uri
            zabbix-api-version
            zabbix-available?
            zabbix-login
            zabbix-logout
            with-zabbix-connection
            zabbix-host-id
            zabbix-host-enabled?
            zabbix-item-id
            zabbix-item-value
            zabbix-history))

(define %zabbix-auth
  (make-parameter #f))

(define %zabbix-uri
  (make-parameter
   (getenv "CUIRASS_ZABBIX_URI")))

(define %zabbix-user
  (make-parameter
   (or (getenv "CUIRASS_ZABBIX_USER") "Admin")))

(define %zabbix-password
  (make-parameter
   (or (getenv "CUIRASS_ZABBIX_PASSWORD") "zabbix")))

(define* (zabbix-request params)
  (let ((headers `((User-Agent . "Cuirass")
                   (Accept . "application/json")
                   (Content-Type . "application/json"))))
    (let-values (((response port)
                  (http-post (%zabbix-uri)
                             #:headers headers
                             #:body (string->utf8
                                     (scm->json-string params))
                             #:streaming? #t)))
      (cond ((= 200 (response-code response))
             (let ((result (json->scm port)))
               (close-port port)
               (and result (assoc-ref result "result"))))
            (else
             (close-port port)
             #f)))))

(define* (zabbix-params method #:optional extra-params)
  (let ((auth (%zabbix-auth)))
    `(("jsonrpc" . "2.0")
      ("method" . ,method)
      ,@(if auth
            `(("auth" . ,auth))
            '())
      ("params" . ,(or extra-params (vector)))
      ("id" . 1))))

(define (zabbix-type type)
  (case type
    ((float) 0)
    ((character) 1)
    ((log) 2)
    ((unsigned) 3)
    ((text) 4)))

(define (zabbix-api-version)
  (let* ((params (zabbix-params "apiinfo.version"))
         (result (zabbix-request params)))
    result))

(define (zabbix-available?)
  (and (%zabbix-uri)
       (string? (zabbix-api-version))))

(define (zabbix-login)
  (let* ((params (zabbix-params "user.login"
                                `(("user" . ,(%zabbix-user))
                                  ("password" . ,(%zabbix-password)))))
         (result (zabbix-request params)))
    (%zabbix-auth result)
    result))

(define (zabbix-logout)
  (let* ((params (zabbix-params "user.logout"))
         (result (zabbix-request params)))
    (%zabbix-auth #f)
    result))

(define-syntax-rule (with-zabbix-connection exp ...)
  (dynamic-wind
    (lambda ()
      (zabbix-login))
    (lambda ()
      exp ...)
    (lambda ()
      (zabbix-logout))))

(define (zabbix-host-search host)
  (let* ((params (zabbix-params "host.get"
                                `(("filter"
                                   . (("host" . ,(vector host)))))))
         (result (zabbix-request params)))
    (match (vector->list result)
      ((host) host)
      (else #f))))

(define (zabbix-host-id host)
  (let ((host (zabbix-host-search host)))
    (assoc-ref host "hostid")))

(define (zabbix-host-enabled? host)
  (let* ((host (zabbix-host-search host))
         (status (assoc-ref host "status")))
    (and status
         (eq? (string->number status) 0))))

(define (zabbix-item-search key host-id)
  (let* ((params (zabbix-params "item.get"
                                `(("hostids" . ,host-id)
                                  ("search"
                                   . (("key_" . ,key))))))
         (result (zabbix-request params)))
    (match (vector->list result)
      ((item) item )
      (else #f))))

(define (zabbix-item-id key host-id)
  (let ((item (zabbix-item-search key host-id)))
    (assoc-ref item "itemid")))

(define (zabbix-item-value key host-id)
  (let ((item (zabbix-item-search key host-id)))
    (assoc-ref item "lastvalue")))

(define* (zabbix-history item-id #:key limit type)
  (define (format-item item)
    (let ((clock (assoc-ref item "clock"))
          (value (assoc-ref item "value")))
      (cons (string->number clock) (string->number value))))

  (let* ((params (zabbix-params "history.get"
                                `(("history" . ,(zabbix-type type))
                                  ("itemids" . ,item-id)
                                  ("sortfield" . "clock")
                                  ("sortorder" . "DESC")
                                  ("limit" . ,limit))))
         (result (zabbix-request params)))
    (map format-item (vector->list result))))
