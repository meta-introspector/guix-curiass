;;; parameters.scm -- Cuirass parameters.
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

(define-module (cuirass parameters)
  #:use-module ((cuirass logging) #:select (current-logging-level))
  #:re-export (current-logging-level)
  #:export (%cuirass-database
            %cuirass-host
            %cuirass-url

            %zabbix-url
            %zabbix-user
            %zabbix-password

            %mastodon-instance-name
            %mastodon-instance-url
            %mastodon-instance-token))

;; This variable is looked up by 'mu-message-send'.
(define-public mu-debug 0)

;; The Cuirass PostgreSQL database name.
(define %cuirass-database
  (make-parameter "cuirass"))

;; The Cuirass PostgreSQL database host.
(define %cuirass-host
  (make-parameter "/var/run/postgresql"))

;; The URL of the Cuirass web server.  This is useful to send absolute links
;; within notifications.
(define %cuirass-url
  (make-parameter #f))

;; The URL of the Zabbix monitoring server providing the workers status,
;; if supported.
(define %zabbix-url
  (make-parameter #f))

;; The user for Zabbix API authentication.
(define %zabbix-user
  (make-parameter "Admin"))

;; The password for Zabbix API authentication.
(define %zabbix-password
  (make-parameter "zabbix"))

;; The name of the Mastodon instance used to send build notifications.
(define %mastodon-instance-name
  (make-parameter #f))

;; The URL of the Mastodon instance.
(define %mastodon-instance-url
  (make-parameter #f))

;; The token used to authenticate on the Mastodon instance.
(define %mastodon-instance-token
  (make-parameter #f))
