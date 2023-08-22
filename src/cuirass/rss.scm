;;; rss.scm -- RSS feed builder.
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

(define-module (cuirass rss)
  #:use-module (cuirass database)
  #:use-module (cuirass parameters)
  #:use-module (cuirass utils)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (sxml simple)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:export (rss-feed))

;; This module is inspired by the (haunt builder rss) module that is part of
;; the Haunt static site generator and writen by Christopher Lemmer Webber.

(define %void-elements
  '(area
    base
    br
    col
    command
    embed
    hr
    img
    input
    keygen
    link
    meta
    param
    source
    track
    wbr))

(define (void-element? tag)
  "Return #t if TAG is a void element."
  (pair? (memq tag %void-elements)))

(define %escape-chars
  (alist->hash-table
   '((#\" . "quot")
     (#\& . "amp")
     (#\< . "lt")
     (#\> . "gt"))))

(define (string->escaped-html s port)
  "Write the HTML escaped form of S to PORT."
  (define (escape c)
    (let ((escaped (hash-ref %escape-chars c)))
      (if escaped
          (format port "&~a;" escaped)
          (display c port))))
  (string-for-each escape s))

(define (object->escaped-html obj port)
  "Write the HTML escaped form of OBJ to PORT."
  (string->escaped-html
   (call-with-output-string (cut display obj <>))
   port))

(define (attribute-value->html value port)
  "Write the HTML escaped form of VALUE to PORT."
  (if (string? value)
      (string->escaped-html value port)
      (object->escaped-html value port)))

(define (attribute->html attr value port)
  "Write ATTR and VALUE to PORT."
  (format port "~a=\"" attr)
  (attribute-value->html value port)
  (display #\" port))

(define (element->html tag attrs body port)
  "Write the HTML TAG to PORT, where TAG has the attributes in the
list ATTRS and the child nodes in BODY."
  (format port "<~a" tag)
  (for-each (match-lambda
              ((attr value)
               (display #\space port)
               (attribute->html attr value port)))
            attrs)
  (if (and (null? body) (void-element? tag))
      (display " />" port)
      (begin
        (display #\> port)
        (for-each (cut sxml->html <> port) body)
        (format port "</~a>" tag))))

(define (doctype->html doctype port)
  (format port "<!DOCTYPE ~a>" doctype))

(define* (sxml->html tree #:optional (port (current-output-port)))
  "Write the serialized HTML form of TREE to PORT."
  (match tree
    (() *unspecified*)
    (('doctype type)
     (doctype->html type port))
    (((? symbol? tag) ('@ attrs ...) body ...)
     (element->html tag attrs body port))
    (((? symbol? tag) body ...)
     (element->html tag '() body port))
    ((nodes ...)
     (for-each (cut sxml->html <> port) nodes))
    ((? string? text)
     (string->escaped-html text port))
    ;; Render arbitrary Scheme objects, too.
    (obj (object->escaped-html obj port))))

(define (sxml->html-string sxml)
  "Render SXML as an HTML string."
  (call-with-output-string
    (lambda (port)
      (sxml->html sxml port))))

(define (build-details-url build)
  "Return the build details URL for BUILD."
  (let ((id (build-id build))
        (url (or (%cuirass-url) "")))
    (string-append url "/build/" (number->string id) "/details")))

(define* (build->rss-item build)
  "Convert BUILD into an RSS <item> node."
  (let* ((url (build-details-url build))
         (job-name (build-job-name build))
         (specification (build-specification-name build))
         (weather (build-current-weather build))
         (weather-text (cond
                        ((= weather (build-weather new-success))
                         "fixed")
                        ((= weather (build-weather new-failure))
                         "broken")))
         (stoptime (build-completion-time build)))
    `(item
      (guid ,url)
      (title
       ,(format #f "Build ~a on ~a is ~a."
                job-name specification weather-text))
      (author "cuirass@gnu.org (Cuirass)")
      (pubDate ,(date->rfc822-str
                 (time-utc->date
                  (make-time time-utc 0 stoptime))))
      (link ,url)
      (description
       ,(sxml->html-string
         `(p "The build " (b ,job-name) " for specification "
             (b ,specification) " is " ,weather-text ".
You can find the detailed information about this build "
             (a (@ (href ,url))
                "here")
             "."))))))

(define* (rss-feed builds #:key params)
  (let* ((specification (and params
                             (assq-ref params 'specification)))
         (cuirass-url (or (%cuirass-url)
                          "https://cuirass.org"))
         (url (format #f "~a/events/rss/~a"
                      cuirass-url
                      (if specification
                          (string-append "?specification=" specification)
                          ""))))
    `(rss (@ (version "2.0")
             (xmlns:atom "http://www.w3.org/2005/Atom"))
          (channel
           (title "GNU Guix continuous integration system build events.")
           (atom:link (@ (href ,url)
                         (rel "self")
                         (type "application/rss+xml")))
           (description
            ,(string-append
              "Build events for "
              (if specification
                  (string-append "specification " specification ".")
                  "all specifications.")))
           (pubDate ,(date->rfc822-str (current-date)))
           (link ,cuirass-url)
           ,@(map build->rss-item builds)))))
