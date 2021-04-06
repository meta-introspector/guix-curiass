;;; templates.scm -- HTTP API
;;; Copyright © 2018 Tatiana Sholokhova <tanja201396@gmail.com>
;;; Copyright © 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (cuirass templates)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (json)
  #:use-module (web uri)
  #:use-module (guix channels)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix progress)
  #:use-module (guix store)
  #:use-module ((guix utils) #:select (string-replace-substring
                                       version>?))
  #:use-module (cuirass config)
  #:use-module ((cuirass database) #:select (build-status
                                             build-weather
                                             evaluation-status))
  #:use-module (cuirass remote)
  #:use-module (cuirass specification)
  #:export (html-page
            specifications-table
            specification-edit
            evaluation-info-table
            build-eval-table
            build-search-results-table
            build-details
            evaluation-build-table
            running-builds-table
            global-metrics-content
            workers-status
            machine-status))

(define (navigation-items navigation)
  (match navigation
    (() '())
    ((item . rest)
     (cons `(li (@ (class "nav-item"))
                (a (@ (class "nav-link" ,(if (null? rest) " active" ""))
                      (href ,(assq-ref item #:link)))
                   ,(assq-ref item #:name)))
           (navigation-items rest)))))

(define (search-form query)
  `(form (@ (id "search")
            (class "form-inline")
            (action "/search"))
         (div
          (@ (class "input-group"))
          (input (@ (type "text")
                    (class "form-control")
                    (id   "query")
                    (name "query")
                    ,(if query
                         `(value ,query)
                         '(placeholder "search for builds"))))
          (span (@ (class "input-group-append"))
                (button
                 (@ (type "submit")
                    (class "btn btn-primary"))
                 "Search")))
         (div
          (@ (id "search-hints"))
          (p "You can limit the search results with the following keywords:")
          (ul
           (li (code "spec")
               ", a " (em "specification") " such as " (code "guix-master"))
           (li (code "system")
               ", a build for the given " (em "target system")
               " such as " (code "x86_64-linux"))
           (li (code "status")
               ", to limit the results to builds with the given status.  "
               "This should be one of "
               (code "success") ", "
               (code "failed") ", "
               (code "failed-dependency") ", "
               (code "failed-other") ", or "
               (code "canceled") "."))
          (p "For example, the following query will list successful builds of
the " (code "guix-master") " specification for the " (code "i686-linux") "
system whose names start with " (code "guile-") ":" (br)
(code "spec:guix-master system:i686-linux status:success guile-")))))

(define* (html-page title body navigation #:optional query)
  "Return HTML page with given TITLE and BODY."
  `(html (@ (xmlns "http://www.w3.org/1999/xhtml")
            (xml:lang "en")
            (lang "en"))
         (head
          (meta (@ (charset "utf-8")))
          (meta (@ (name "viewport")
                   (content ,(string-join '("width=device-width"
                                            "initial-scale=1"
                                            "shrink-to-fit=no")
                                          ", "))))
          (link (@ (rel "stylesheet")
                   (href "/static/css/bootstrap.css")))
          (link (@ (rel "stylesheet")
                   (href "/static/css/datatables.min.css")))
          (link (@ (rel "stylesheet")
                   (href "/static/css/open-iconic-bootstrap.css")))
          (link (@ (rel "stylesheet")
                   (href "/static/css/choices.min.css")))
          (link (@ (rel "stylesheet")
                   (href "/static/css/cuirass.css")))
          (link (@ (rel "icon") (type "image/png")
                   (href "/static/images/icon.png")))
          (script (@ (src "/static/js/jquery-3.6.0.min.js")))
          (script (@ (src "/static/js/datatables.min.js")))
          (script (@ (src "/static/js/d3.v6.min.js")))
          (script "
$(document).ready(function() {
  var default_opts = {
paging: false,
searching: false,
info: false,
order: [],
};
  var spec_table = $('#spec-table');
  if (spec_table.find('th').length > 1) {
    spec_table.DataTable({
...default_opts,
/* Do not sort the 'Action' column. */
columnDefs: [
    { orderable: false, targets: 5 }
  ],
});
}
  var eval_table = $('#eval-table');
  if (eval_table.find('th').length > 1) {
    eval_table.DataTable({
...default_opts,
columnDefs: [
    { orderable: false, targets: 0 },
    { orderable: false, targets: 1 },
    { orderable: false, targets: 8 }
  ],
});
}
});")
          (title ,title))
         (body
          (nav (@ (class "navbar navbar-expand-lg navbar-light bg-light"))
               (a (@ (class "navbar-brand pt-0")
                     (href "/"))
                  (img (@ (src "/static/images/guix.png")
                          (alt "logo")
                          (height "25")
                          (style "margin-top: -12px"))))
               (div (@ (class "collapse navbar-collapse"))
                    (ul (@ (class "navbar-nav mr-auto"))
                        (li (@ (class "nav-item dropdown"))
                            (a (@ (class "nav-link dropdown-toggle")
                                  (data-toggle "dropdown")
                                  (href "#")
                                  (role "button")
                                  (aria-haspopup "true")
                                  (aria-expanded "false"))
                               "Status")
                            (div (@ (class "dropdown-menu")
                                    (aria-labelledby "navbarDropdow"))
                                 (a (@ (class "dropdown-item")
                                       (href "/metrics"))
                                    "Global metrics")
                                 (a (@ (class "dropdown-item")
                                       (href "/workers"))
                                    "Workers status")
                                 (a (@ (class "dropdown-item")
                                       (href "/status"))
                                    "Running builds")))
                        (li (@ (class "nav-item"))
                            (a (@ (class "nav-link" ,(if (null? navigation)
                                                         " active" ""))
                                  (href "/"))
                               Home))
                        ,@(navigation-items navigation)))
               ,(search-form query))
          (div (@ (class "container content"))
               ,body)
          (footer
           (@ (class "footer text-center"))
           (p (@ (class "mt-3"))
              (a (@ (href "http://guix.gnu.org/cuirass/"))
                 ,(string-append "Cuirass " %package-version))
              " — Copyright © 2016 - 2021 by the GNU Guix community.")))))

(define (status-class status)
  (cond
   ((= (build-status submitted) status)
    "oi oi-clock text-warning")
   ((= (build-status scheduled) status)
    "oi oi-clock text-warning")
   ((= (build-status started) status)
    "oi oi-reload text-warning")
   ((= (build-status succeeded) status)
    "oi oi-check text-success")
   ((= (build-status failed) status)
    "oi oi-x text-danger")
   ((= (build-status failed-dependency) status)
    "oi oi-warning text-danger")
   ((= (build-status failed-other) status)
    "oi oi-warning text-danger")
   ((= (build-status canceled) status)
    "oi oi-question-mark text-warning")
   (else
    "oi oi-warning text-danger")))

(define (status-title status)
  (cond
   ((= (build-status submitted) status)
    "Submitted")
   ((= (build-status scheduled) status)
    "Scheduled")
   ((= (build-status started) status)
    "Started")
   ((= (build-status succeeded) status)
    "Succeeded")
   ((= (build-status failed) status)
    "Failed")
   ((= (build-status failed-dependency) status)
    "Failed (dependency)")
   ((= (build-status failed-other) status)
    "Failed (other)")
   ((= (build-status canceled) status)
    "Canceled")
   (else
    "Invalid status")))

(define (specifications-table specs)
  "Return HTML for the SPECS table."
  `((p (@ (class "lead")) "Specifications"
       (a (@ (href "/events/rss/"))
          (button (@ (class "btn btn-outline-warning float-right")
                     (type "button"))
                  (span (@(class "oi oi-rss text-warning align-right")
                         (title "RSS")
                         (aria-hidden "true"))
                        "")))
       (a (@ (class "btn btn-outline-primary mr-1 float-right")
             (href "/specification/add/")
             (role "button"))
          (span (@(class "oi oi-plus text-primary align-right")
                 (title "Add")
                 (aria-hidden "true"))
                "")))
    (table
     (@ (id "spec-table")
        (class "table table-sm table-hover"))
     ,@(if (null? specs)
           `((th (@ (scope "col")) "No elements here."))
           `((thead (tr (th (@ (scope "col")) Name)
                        (th (@ (scope "col")) Build)
                        (th (@ (scope "col")) Channels)
                        (th (@ (scope "col")) Priority)
                        (th (@ (scope "col")) Systems)
                        (th (@ (scope "col")) Action)))
             (tbody
              ,@(map
                 (lambda (spec)
                   `(tr (td (a (@ (href "/jobset/"
                                        ,(specification-name spec)))
                               ,(specification-name spec)))
                        (td ,(match (specification-build spec)
                               ((? symbol? build)
                                (symbol->string build))
                               ((build _ ...)
                                (symbol->string build))))
                        (td ,(string-join
                              (map (lambda (channel)
                                     (format #f "~a (on ~a)"
                                             (channel-name channel)
                                             (channel-branch channel)))
                                   (specification-channels spec)) ", "))
                        (td ,(number->string
                              (specification-priority spec)))
                        (td ,(string-join
                              (sort (specification-systems spec)
                                    string<?)
                              ", "))
                        (td
                         (div
                          (@ (class "dropdown"))
                          (a (@ (class "oi oi-menu dropdown-toggle no-dropdown-arrow")
                                (href "#")
                                (data-toggle "dropdown")
                                (role "button")
                                (aria-haspopup "true")
                                (aria-expanded "false"))
                             " ")
                          (div (@ (class "dropdown-menu"))
                               (a (@ (class "dropdown-item")
                                     (href "/specification/edit/"
                                           ,(specification-name spec)))
                                  " Edit")
                               (a (@ (class "dropdown-item")
                                     (href "/admin/specifications/delete/"
                                           ,(specification-name spec)))
                                  " Delete"))))))
                 specs)))))))

(define* (specification-edit #:optional spec)
  "Return HTML to add a new specification if no argument is passed, or to edit
the existing SPEC otherwise."
  (define (channels->html channels)
    (let ((html
           (fold
            (lambda (channel html)
              (let ((first-row? (null? html))
                    (name (channel-name channel))
                    (url (channel-url channel))
                    (branch (channel-branch channel)))
                (cons
                 `(div (@ (class ,(if first-row?
                                      "form-group row channel"
                                      "form-group row channel-new")))
                       (label (@ (for "name")
                                 (class "col-sm-2 col-form-label"))
                              ,(if first-row? "Channels" ""))
                       (div (@ (class "col-sm-2"))
                            (input
                             (@ (type "text")
                                (class "form-control channel-name")
                                (name "channel-name")
                                (placeholder "name")
                                (value ,name)
                                (required))))
                       (div (@ (class "col-sm-4"))
                            (input
                             (@ (type "text")
                                (class "form-control channel-url")
                                (name "channel-url")
                                (placeholder "url")
                                (value ,url)
                                (required))))
                       (div (@ (class "col-sm-2"))
                            (input
                             (@ (type "text")
                                (class "form-control channel-branch")
                                (name "channel-branch")
                                (placeholder "branch")
                                (value ,branch)
                                (required))))
                       ,@(if first-row?
                             '((a (@ (class "btn btn-success add-channel")
                                     (href "#")
                                     (role "button"))
                                  "Add"))
                             '((a (@ (class "btn btn-danger remove-channel")
                                     (href "#")
                                     (role "button"))
                                  "Remove"))))
                 html)))
            '()
            channels)))
      (match (reverse html)
        ((first . rest)
         (list first `(div (@ (class "channels")) ,@rest))))))

  (let ((name (and spec (specification-name spec)))
        (build (and spec (match (specification-build spec)
                           ((? symbol? build) build)
                           ((build _ ...) build))))
        (channels (and spec (specification-channels spec)))
        (priority (and spec (specification-priority spec)))
        (systems (and spec (specification-systems spec))))
    `(span
      (p (@ (class "lead"))
         ,(if spec
              (format #f "Edit ~a specification" name)
              "Create a new specification"))
      (script (@ (src "/static/js/choices.min.js")))
      (script "
$(document).ready(function() {
$('.remove-channel').click(function() {
   $(this).parent().remove();
});
$('.add-channel').click(function() {
  var clone = $('.channel').clone();
  clone.attr('class', 'form-group row channel-new');
  clone.find('.col-form-label').text('');

  var new_button = clone.find('.add-channel');
  new_button.attr('class', 'btn btn-danger remove-channel');
  new_button.text('Remove');
  new_button.click(function() {
   $(this).parent().remove();
  });
  clone.appendTo('.channels');
});
var cbs = $('.system');
cbs.change(function(){
  if(cbs.is(':checked')) {
    cbs.removeAttr('required');
  } else {
    cbs.attr('required', 'required');
  }
});
var checked_cbs = $('.system:checkbox:checked').length;
if (checked_cbs == 0) {
  cbs.attr('required', 'required');
}

(function () {
  'use strict'
  var forms = document.querySelectorAll('.needs-validation')
  Array.prototype.slice.call(forms)
    .forEach(function (form) {
      form.addEventListener('submit', function (event) {
        if (!form.checkValidity()) {
          event.preventDefault()
          event.stopPropagation()
        }
        form.classList.add('was-validated')
      }, false)
    })
})();

const select_choices = new Choices($('.build-param-select')[0], {
  removeItemButton: true,
  duplicateItemsAllowed: false,
});
const input_choices = new Choices($('.build-param-input')[0], {
  removeItemButton: true,
  duplicateItemsAllowed: false,
});
$('.build-param-select').on('showDropdown', function() {
  var names = $('.channel-name').map(function() {
    var name = $(this).val();
    return { 'value': name, 'label': name};
  }).toArray();
  select_choices.setChoices(names, 'value', 'label', true);
});
var param_select = $('.build-select');
var param_select_cb = function(){
  var val = param_select.val();
  if (['packages', 'manifests'].indexOf(val) >= 0) {
    input_choices.clearStore();
    $('.param-input-row').show();
  } else {
    $('.param-input-row').hide();
  }

  if (['channels'].indexOf(val) >= 0) {
    $('.param-select-row').show();
  } else {
    $('.param-select-row').hide();
  }
};
param_select_cb();
param_select.change(param_select_cb);

const default_param = $('.default-build-param');
if (default_param.length) {
var items = default_param.text().split(',').map(function(name) {
  return { 'value': name, 'label': name, selected: true};
});
if ($('.param-select-row').is(':visible')) {
  select_choices.setChoices(items, 'value', 'label', true);
} else if ($('.param-input-row').is(':visible')) {
  input_choices.setValue(items);
}}
});")
      (form (@ (id "add-specification")
               (class "needs-validation")
               ,@(if spec
                     '((action "/admin/specification/edit"))
                     '((action "/admin/specification/add")))
               (method "POST"))
            (div (@ (class "form-group row"))
                 (label (@ (for "name")
                           (class "col-sm-2 col-form-label"))
                        "Name")
                 (div (@ (class "col-sm-4"))
                      (input (@ (type "text")
                                (class "form-control")
                                (id "name")
                                (name "name")
                                (value ,(or name ""))
                                ,@(if spec
                                      '((readonly))
                                      '())
                                (required)))))
            (div (@ (class "form-group row"))
                 (label (@ (for "build")
                           (class "col-sm-2 col-form-label"))
                        "Build")
                 (div (@ (class "col-sm-4"))
                      (select
                       (@ (class "form-control build-select")
                          (id "build")
                          (name "build"))
                       ,@(map (lambda (type)
                                `(option (@ ,@(if (eq? type build)
                                                  '((selected))
                                                  '()))
                                         ,(symbol->string type)))
                              %build-types))))
            ,(if spec
                 (match (specification-build spec)
                   ((build . rest)
                    `((span (@ (class "default-build-param")
                               (hidden "true"))
                            ,(string-join
                              (map (lambda (param)
                                     (if (symbol? param)
                                         (symbol->string param)
                                         param))
                                   rest)
                              ","))))
                   (else ""))
                 '())
            (div (@ (class "form-group row param-select-row"))
                 (label (@(class "col-sm-2 col-form-label"))
                        "Parameter")
                 (div (@ (class "col-sm-4"))
                      (select (@ (type "text")
                                 (class "form-control build-param-select")
                                 (name "param-select")
                                 (multiple)))))
            (div (@ (class "form-group row param-input-row"))
                 (label (@(class "col-sm-2 col-form-label"))
                        "Parameter")
                 (div (@ (class "col-sm-4"))
                      (input (@ (type "text")
                                (name "param-input")
                                (class "form-control build-param-input")))))
            ,@(channels->html
               (if spec channels (list %default-guix-channel)))
            (div (@ (class "form-group row"))
                 (label (@ (for "priority")
                           (class "col-sm-2 col-form-label"))
                        "Priority")
                 (div (@ (class "col-sm-4"))
                      (input
                       (@ (type "number")
                          (class "form-control")
                          (id "priority")
                          (name "priority")
                          (min 0)
                          (max 9)
                          (value ,(or priority 9))))))
            (div (@ (class "form-group row"))
                 (label (@ (for "systems")
                           (class "col-sm-2 col-form-label"))
                        "Systems")
                 ,@(map (lambda (system)
                          `(div (@ (class "form-check form-check-inline"))
                                (input (@ (class "form-check-input system")
                                          (type "checkbox")
                                          (id ,system)
                                          (name ,system)
                                          ,@(if (and systems
                                                     (member system systems))
                                                '((checked))
                                                '())))
                                (label (@ (class "form-check-label")
                                          (for ,system))
                                       ,system)))
                        %cuirass-supported-systems))
            (div (@ (class "form-group row"))
                 (div (@ (class "col-sm-4"))
                      (button
                       (@ (type "submit")
                          (class "btn btn-primary"))
                       " Submit")))))))

(define (build-details build products history)
  "Return HTML showing details for the BUILD."
  (define status (assq-ref build #:status))
  (define weather (assq-ref build #:weather))
  (define blocking-outputs
    (or (and-let* (((= (build-status failed-dependency) status))
                   (drv (false-if-exception
                         (read-derivation-from-file
                          (assq-ref build #:derivation)))))
          (append-map (lambda (drv)
                        (match (derivation->output-paths drv)
                          (((_ . items) ...)
                           items)))
                      (filter (compose derivation-log-file
                                       derivation-file-name)
                              (with-store store
                                (derivation-build-plan
                                 store (list (derivation-input drv))
                                 #:substitutable-info (const #f))))))
        '()))

  (define completed?
    (or (= (build-status succeeded) status)
        (= (build-status failed) status)))

  (define evaluation
    (assq-ref build #:eval-id))

  (define (history-table-row build)
    (define status
      (assq-ref build #:status))

    `(tr
      (td (span (@ (class ,(status-class status))
                   (title ,(status-title status))
                   (aria-hidden "true"))
                ""))
      (th (@ (scope "row"))
          (a (@ (href "/build/" ,(assq-ref build #:id) "/details"))
             ,(assq-ref build #:id)))
      (td ,(assq-ref build #:nix-name))
      (td ,(time->string (assq-ref build #:stoptime)))))

  `((p (@ (class "lead")) "Build details"
       (div (@ (class "dropdown float-right"))
            (a (@ (class "btn btn-warning dropdown-toggle")
                  (href "#")
                  (data-toggle "dropdown")
                  (role "button")
                  (aria-haspopup "true")
                  (aria-expanded "false"))
               "Action")
            (div (@ (class "dropdown-menu"))
                 (a (@ (class "dropdown-item")
                       (href "/admin/build/"
                             ,(assq-ref build #:id) "/restart"))
                    " Restart"))))
    (table
     (@ (class "table table-sm table-hover"))
     (tbody
      (tr (th "Build ID")
          (td ,(assq-ref build #:id)))
      (tr (th "Evaluation")
          (td (a (@ (href ,(string-append "/eval/"
                                          (number->string evaluation))))
                 ,(number->string evaluation))))
      (tr (th "Status")
          (td (span (@ (class ,(status-class status))
                       (title ,(status-title status)))
                    ,(string-append " " (status-title status)))
              ,@(map (lambda (output)
                       `((br)
                         (a (@ (href ,(string-append "/log/" (basename output))))
                            ,output)))
                     blocking-outputs)))
      (tr (th "System")
          (td ,(assq-ref build #:system)))
      (tr (th "Name")
          (td ,(assq-ref build #:nix-name)))
      (tr (th "Duration")
          (td ,(let ((timestamp (time-second (current-time time-utc)))
                     (start (assq-ref build #:starttime))
                     (stop  (assq-ref build #:stoptime)))
                 (cond
                  ((and (> start 0) (> stop 0))
                   (string-append (number->string (- stop start))
                                  " seconds"))
                  ((> start 0)
                   (string-append (number->string (- timestamp start))
                                  " seconds"))
                  (else "—")))))
      (tr (th "Finished")
          (td ,(if completed?
                   (time->string (assq-ref build #:stoptime))
                   "—")))
      (tr (th "Weather")
          (td (span (@ (class ,(weather-class weather))
                       (title ,(weather-title weather))
                       (aria-hidden "true"))
                    "")))
      (tr (th "Log file")
          (td (a (@ (href "/build/" ,(assq-ref build #:id) "/log/raw"))
                 "raw")))
      (tr (th "Derivation")
          (td (pre ,(assq-ref build #:derivation))))
      (tr (th "Outputs")
          (td ,(map (match-lambda ((out (#:path . path))
                                   `(pre ,path)))
                    (assq-ref build #:outputs))))
      ,@(if (null? products)
            '()
            (let ((product-items
                   (map
                    (lambda (product)
                      (let* ((id (assq-ref product #:id))
                             (size (assq-ref product #:file-size))
                             (type (assq-ref product #:type))
                             (path (assq-ref product #:path))
                             (href (format #f "/download/~a" id)))
                        `(a (@ (href ,href))
                            (li (@ (class "list-group-item"))
                                (div
                                 (@ (class "container"))
                                 (div
                                  (@ (class "row"))
                                  (div
                                   (@ (class "col-md-auto"))
                                   (span
                                    (@ (class "oi oi-data-transfer-download")
                                       (title "Download")
                                       (aria-hidden "true"))))
                                  (div (@ (class "col-md-auto"))
                                       ,path)
                                  (div (@ (class "col-md-auto"))
                                       "(" ,type ")")
                                  (div (@ (class "col-md-auto"))
                                       ,(byte-count->string size))))))))
                    products)))
              `((tr (th "Build outputs")
                    (td
                     (ul (@ (class "list-group d-flex flex-row"))
                         ,product-items))))))))
    ,@(if (null? history)
          '()
          `((h6 "Build history")
            (table
             (@ (class "table table-sm table-hover table-striped"))
             (thead
              (tr
               (th (@ (scope "col") (class "border-0")) ())
               (th (@ (scope "col") (class "border-0")) "ID")
               (th (@ (scope "col") (class "border-0")) "Name")
               (th (@ (scope "col") (class "border-0")) "Completion time")))
             (tbody ,@(map history-table-row history)))))))

(define (pagination first-link prev-link next-link last-link)
  "Return html page navigation buttons with LINKS."
  `(div (@ (class row))
        (nav
         (@ (class "mx-auto") (aria-label "Page navigation"))
         (ul (@ (class "pagination"))
             (li (@ (class "page-item"
                      ,(if (string-null? prev-link) " disabled")))
                 (a (@ (class "page-link")
                       (href ,first-link))
                    "<< First"))
             (li (@ (class "page-item"
                      ,(if (string-null? prev-link) " disabled")))
                 (a (@ (class "page-link")
                       (href ,prev-link))
                    "< Previous"))
             (li (@ (class "page-item"
                      ,(if (string-null? next-link) " disabled")))
                 (a (@ (class "page-link")
                       (href ,next-link))
                    "Next >"))
             (li (@ (class "page-item"
                      ,(if (string-null? next-link) " disabled")))
                 (a (@ (class "page-link")
                       (href ,last-link))
                    "Last >>"))))))

(define (input-changes checkouts)
  (let ((changes
         (string-join
          (map (lambda (checkout)
                 (let ((input (assq-ref checkout #:channel))
                       (commit (assq-ref checkout #:commit)))
                   (format #f "~a → ~a" input (substring commit 0 7))))
               checkouts)
          ", ")))
    (if (string=? changes "") '(em "None") changes)))

(define (evaluation-badges evaluation)
  (let ((status (assq-ref evaluation #:status)))
    (if (= status (evaluation-status started))
        '((em "In progress…"))
        (cond
         ((= status (evaluation-status failed))
          `((a (@ (href "/eval/" ,(assq-ref evaluation #:id) "/log/raw")
                  (class "oi oi-x text-danger")
                  (title "Failed")
                  (aria-hidden "true"))
               "")))
         ((= status (evaluation-status aborted))
          `((a (@ (href "/eval/" ,(assq-ref evaluation #:id) "/log/raw")
                  (class "oi oi-x text-warning")
                  (title "Aborted")
                  (aria-hidden "true"))
               "")))
         ((= status (evaluation-status succeeded))
          `((a (@ (href "/eval/" ,(assq-ref evaluation #:id)
                        "?status=succeeded")
                  (class "badge badge-success")
                  (title "Succeeded"))
               ,(assq-ref evaluation #:succeeded))
            (a (@ (href "/eval/" ,(assq-ref evaluation #:id)
                        "?status=failed")
                  (class "badge badge-danger")
                  (title "Failed"))
               ,(assq-ref evaluation #:failed))
            (a (@ (href "/eval/" ,(assq-ref evaluation #:id)
                        "?status=pending")
                  (class "badge badge-secondary")
                  (title "Scheduled"))
               ,(assq-ref evaluation #:scheduled))))))))

(define (evaluation-info-table name evaluations id-min id-max)
  "Return HTML for the EVALUATION table NAME. ID-MIN and ID-MAX are
  global minimal and maximal id."
  `((p (@ (class "lead")) "Evaluations of " ,name
       (a (@ (href "/events/rss/?specification=" ,name))
          (button (@ (class "btn btn-outline-warning float-right")
                     (type "button"))
                  (span (@(class "oi oi-rss text-warning align-right")
                         (title "RSS")
                         (aria-hidden "true"))
                        ""))))
    (table
     (@ (class "table table-sm table-hover table-striped"))
     ,@(if (null? evaluations)
           `((th (@ (scope "col")) "No elements here."))
           `((thead
              (tr
               (th (@ (scope "col")) "#")
               (th (@ (scope "col")) "Channel changes")
               (th (@ (scope "col")) Success)
               (th (@ (scope "col")) Action)))
             (tbody
              ,@(map
                 (lambda (row)
                   `(tr (th (@ (scope "row"))
                            (a (@ (href "/eval/" ,(assq-ref row #:id)))
                               ,(assq-ref row #:id)))
                        (td ,(input-changes (assq-ref row #:checkouts)))
                        (td ,@(evaluation-badges row))
                        (td
                         (div
                          (@ (class "dropdown"))
                          (a (@ (class "oi oi-menu dropdown-toggle no-dropdown-arrow")
                                (href "#")
                                (data-toggle "dropdown")
                                (role "button")
                                (aria-haspopup "true")
                                (aria-expanded "false"))
                             " ")
                          (div (@ (class "dropdown-menu"))
                               (a (@ (class "dropdown-item")
                                     (href "/admin/evaluation/"
                                           ,(assq-ref row #:id)
                                           "/cancel"))
                                  " Cancel pending builds")
                               (a (@ (class "dropdown-item")
                                     (href "/admin/evaluation/"
                                           ,(assq-ref row #:id)
                                           "/restart"))
                                  " Restart all builds")
                               (a (@ (class "dropdown-item")
                                     (href "/admin/evaluation/"
                                           ,(assq-ref row #:id)
                                           "/retry"))
                                  " Retry the evaluation"))))))
                 evaluations)))))
    ,(if (null? evaluations)
         (pagination "" "" "" "")
         (let* ((eval-ids (map (cut assq-ref <> #:id) evaluations))
                (page-id-min (last eval-ids))
                (page-id-max (first eval-ids)))
           (pagination
            (format #f "?border-high=~d" (1+ id-max))
            (if (= page-id-max id-max)
                ""
                (format #f "?border-low=~d" page-id-max))
            (if (= page-id-min id-min)
                ""
                (format #f "?border-high=~d" page-id-min))
            (format #f "?border-low=~d" (1- id-min)))))))

(define (time->string time)
  "Return a string representing TIME in a concise, human-readable way."
  (define now*
    (current-time time-utc))

  (define now
    (time-second now*))

  (define elapsed
    (- now time))

  (cond ((< elapsed 120)
         "seconds ago")
        ((< elapsed 7200)
         (let ((minutes (inexact->exact
                         (round (/ elapsed 60)))))
           (format #f "~a minutes ago" minutes)))
        ((< elapsed (* 48 3600))
         (let ((hours (inexact->exact
                       (round (/ elapsed 3600)))))
           (format #f "~a hours ago" hours)))
        (else
         (let* ((time    (make-time time-utc 0 time))
                (date    (time-utc->date time))
                (year    (date-year date))
                (current (date-year (time-utc->date now*)))
                (format  (if (= year current)
                             "~e ~b ~H:~M ~z"
                             "~e ~b ~Y ~H:~M")))
           (date->string date format)))))

(define (weather-class status)
  (cond
   ((= (build-weather unknown) status)
    "oi oi-media-record text-primary mt-1")
   ((= (build-weather new-success) status)
    "oi oi-arrow-thick-top text-success mt-1")
   ((= (build-weather new-failure) status)
    "oi oi-arrow-thick-bottom text-danger mt-1")
   ((= (build-weather still-succeeding) status)
    "oi oi-media-record text-success mt-1")
   ((= (build-weather still-failing) status)
    "oi oi-media-record text-danger mt-1")))

(define (weather-title status)
  (cond
   ((= (build-weather unknown) status) "Unknown")
   ((= (build-weather new-success) status) "New success")
   ((= (build-weather new-failure) status) "New failure")
   ((= (build-weather still-succeeding) status) "Still succeeding")
   ((= (build-weather still-failing) status) "Still failing")))

(define (build-eval-table eval-id builds build-min build-max status)
  "Return HTML for the BUILDS table evaluation with given STATUS.  BUILD-MIN
and BUILD-MAX are global minimal and maximal (stoptime, rowid) pairs."
  (define (table-header)
    `(thead
      (tr
       (th (@ (scope "col") (class "border-0")) '())
       (th (@ (scope "col") (class "border-0")) '())
       (th (@ (scope "col") (class "border-0")) "ID")
       (th (@ (scope "col") (class "border-0")) "Specification")
       (th (@ (scope "col") (class "border-0")) "Completion time")
       (th (@ (scope "col") (class "border-0")) "Job")
       (th (@ (scope "col") (class "border-0")) "Name")
       (th (@ (scope "col") (class "border-0")) "System")
       (th (@ (scope "col") (class "border-0")) "Log"))))

  (define (table-row build)
    (define status
      (assq-ref build #:buildstatus))

    (define weather
      (assq-ref build #:weather))

    (define completed?
      (or (= (build-status succeeded) status)
          (= (build-status failed) status)))

    `(tr
      (td (span (@ (class ,(status-class status))
                   (title ,(status-title status))
                   (aria-hidden "true"))
                ""))
      (td (span (@ (class ,(weather-class weather))
                   (title ,(weather-title weather))
                   (aria-hidden "true"))
                ""))
      (th (@ (scope "row"))
          (a (@ (href "/build/" ,(assq-ref build #:id) "/details"))
             ,(assq-ref build #:id)))
      (td ,(assq-ref build #:jobset))
      (td ,(if completed?
               (time->string (assq-ref build #:stoptime))
               "—"))
      (td ,(assq-ref build #:job))
      (td ,(assq-ref build #:nixname))
      (td ,(assq-ref build #:system))
      (td (a (@ (href "/build/" ,(assq-ref build #:id) "/log/raw"))
               "raw"))))

  (define (build-id build)
    (match build
      ((stoptime id) id)))

  (define (build-stoptime build)
    (match build
      ((stoptime id) stoptime)))

  `((table
     (@ (id "eval-table")
        (class "table table-sm table-hover table-striped"))
     ,@(if (null? builds)
           `((th (@ (scope "col") (class "border-0")) "No elements here."))
           `(,(table-header)
             (tbody ,@(map table-row builds)))))
    ,(if (null? builds)
         (pagination "" "" "" "")
         (let* ((build-time-ids (map (lambda (row)
                                       (list (assq-ref row #:stoptime)
                                             (assq-ref row #:id)))
                                     builds))
                (page-build-min (last build-time-ids))
                (page-build-max (first build-time-ids)))
           (pagination
            (format
             #f "?border-high-time=~d&border-high-id=~d~@[&status=~a~]"
             (build-stoptime build-max)
             (1+ (build-id build-max))
             status)
            (if (equal? page-build-max build-max)
                ""
                (format
                 #f "?border-low-time=~d&border-low-id=~d~@[&status=~a~]"
                 (build-stoptime page-build-max)
                 (build-id page-build-max)
                 status))
            (if (equal? page-build-min build-min)
                ""
                (format
                 #f "?border-high-time=~d&border-high-id=~d~@[&status=~a~]"
                 (build-stoptime page-build-min)
                 (build-id page-build-min)
                 status))
            (format
             #f "?border-low-time=~d&border-low-id=~d~@[&status=~a~]"
             (build-stoptime build-min)
             (1- (build-id build-min))
             status))))))

;; FIXME: Copied from (guix scripts describe).
(define %vcs-web-views
  ;; Hard-coded list of host names and corresponding web view URL templates.
  ;; TODO: Allow '.guix-channel' files to specify a URL template.
  (let ((labhub-url (lambda (repository-url commit)
                      (string-append
                       (if (string-suffix? ".git" repository-url)
                           (string-drop-right repository-url 4)
                           repository-url)
                       "/commit/" commit))))
    `(("git.savannah.gnu.org"
       ,(lambda (repository-url commit)
          (string-append (string-replace-substring repository-url
                                                   "/git/" "/cgit/")
                         "/log/?id=" commit)))
      ("notabug.org" ,labhub-url)
      ("framagit.org" ,labhub-url)
      ("gitlab.com" ,labhub-url)
      ("gitlab.inria.fr" ,labhub-url)
      ("github.com" ,labhub-url))))

(define (commit-hyperlink url commit)
  "Return, if possibly, a hyperlink for COMMIT of the repository at URL."
  (let* ((uri  (string->uri url))
         (host (uri-host uri)))
    (match (assoc-ref %vcs-web-views host)
      (#f     commit)
      ((link) `(a (@ (href ,(link url commit))) ,commit)))))

(define (nearest-exact-integer x)
  "Given a real number X, return the nearest exact integer, with ties going to
the nearest exact even integer."
  (inexact->exact (round x)))

(define (seconds->string duration)
  (if (< duration 60)
      (format #f "~a second~:p" duration)
      (format #f "~a minute~:p" (nearest-exact-integer
                                 (/ duration 60)))))

(define* (evaluation-build-table evaluation
                                 #:key
                                 channels
                                 (checkouts '())
                                 status builds
                                 builds-id-min builds-id-max)
  "Return HTML for an evaluation page, containing a table of builds for that
evaluation."
  (define id        (assq-ref evaluation #:id))
  (define total     (assq-ref evaluation #:total))
  (define succeeded (assq-ref evaluation #:succeeded))
  (define timestamp (assq-ref evaluation #:timestamp))
  (define evaltime  (assq-ref evaluation #:evaltime))
  (define failed    (assq-ref evaluation #:failed))
  (define scheduled (assq-ref evaluation #:scheduled))
  (define spec      (assq-ref evaluation #:spec))

  (define duration  (- evaltime timestamp))

  `((script "
$(document).ready(function() {
  var url = new URL(window.location.href);
  var params = url.searchParams;
  var paginate = params.get('paginate');
  var href;
  console.log(paginate);
  if (!paginate || paginate == '1') {
    params.set('paginate', 0);
    $('#paginate').attr('href', url.toString());
  } else if (paginate == '0') {
    params.set('paginate', 1);
    $('#paginate').attr('class', 'oi oi-collapse-up');
    $('#paginate').attr('href', url.toString());
  }
});
")
    (p (@ (class "lead"))
       ,(format #f "Evaluation #~a" id))
    ,@(if (= timestamp 0)
          '()
          `((p ,(if (= evaltime 0)
                     (format #f "Evaluation started ~a."
                             (time->string timestamp))
                     (format #f "Evaluation completed ~a in ~a."
                             (time->string evaltime)
                             (seconds->string duration))))))
    (table (@ (class "table table-sm table-hover"))
           (thead
            (tr (th (@ (class "border-0") (scope "col")) "Channel")
                (th (@ (class "border-0") (scope "col")) "Commit")))
           (tbody
            ,@(map (lambda (checkout)
                     (let* ((name  (assq-ref checkout #:channel))
                            (channel (find (lambda (channel)
                                           (eq? (channel-name channel)
                                                name))
                                         channels))
                            (url   (channel-url channel))
                            (commit (assq-ref checkout #:commit)))
                       ;; Some checkout entries may refer to removed
                       ;; inputs.
                       (if channel
                           `(tr (td ,url)
                                (td (code ,(commit-hyperlink url commit))))
                           '())))
                   checkouts)))

    (p (@ (class "lead"))
       ,(format #f "~@[~a~] ~:[B~;b~]uilds of evaluation #~a"
                (and=> status string-capitalize)
                status
                id)
       "  "
       (a (@ (id "paginate")
             (class "oi oi-collapse-down")
             (style "font-size:0.7em")
             (href "")
             (role "button"))))
    (ul (@ (class "nav nav-tabs"))
        (li (@ (class "nav-item"))
            (a (@ (class ,(string-append "nav-link "
                                         (match status
                                           (#f "active")
                                           (_ ""))))
                  (href "?all="))
               "All "
               (span (@ (class "badge badge-light badge-pill"))
                     ,total)))
        (li (@ (class "nav-item"))
            (a (@ (class ,(string-append "nav-link "
                                         (match status
                                           ("pending" "active")
                                           (_ ""))))
                  (href "?status=pending"))
               (span (@ (class "oi oi-clock text-warning")
                        (title "Scheduled")
                        (aria-hidden "true"))
                     "")
               " Scheduled "
               (span (@ (class "badge badge-light badge-pill"))
                     ,scheduled)))
        (li (@ (class "nav-item"))
            (a (@ (class ,(string-append "nav-link "
                                         (match status
                                           ("succeeded" "active")
                                           (_ ""))))
                  (href "?status=succeeded"))
               (span (@ (class "oi oi-check text-success")
                        (title "Succeeded")
                        (aria-hidden "true"))
                     "")
               " Succeeded "
               (span (@ (class "badge badge-light badge-pill"))
                     ,succeeded)))
        (li (@ (class "nav-item"))
            (a (@ (class ,(string-append "nav-link "
                                         (match status
                                           ("failed" "active")
                                           (_ ""))))
                  (href "?status=failed"))
               (span (@ (class "oi oi-x text-danger")
                        (title "Failed")
                        (aria-hidden "true"))
                     "")
               " Failed "
               (span (@ (class "badge badge-light badge-pill"))
                     ,failed))))
    (div (@ (class "tab-content pt-3"))
         (div (@ (class "tab-pane show active"))
              ,(build-eval-table
                id
                builds
                builds-id-min
                builds-id-max
                status)))))

(define (build-search-results-table query builds build-min build-max)
  "Return HTML for the BUILDS table evaluation matching QUERY.  BUILD-MIN
and BUILD-MAX are global minimal and maximal row identifiers."
  (define (table-header)
    `(thead
      (tr
       (th (@ (scope "col")) '())
       (th (@ (scope "col")) "ID")
       (th (@ (scope "col")) "Specification")
       (th (@ (scope "col")) "Completion time")
       (th (@ (scope "col")) "Job")
       (th (@ (scope "col")) "Name")
       (th (@ (scope "col")) "System")
       (th (@ (scope "col")) "Log"))))

  (define (table-row build)
    (define status
      (assq-ref build #:buildstatus))

    (define completed?
      (or (= (build-status succeeded) status)
          (= (build-status failed) status)))

    `(tr
      (td (span (@ (class ,(status-class status))
                   (title ,(status-title status))
                   (aria-hidden "true"))
                ""))
      (th (@ (scope "row"))
          (a (@ (href "/build/" ,(assq-ref build #:id) "/details"))
             ,(assq-ref build #:id)))
      (td ,(assq-ref build #:jobset))
      (td ,(if completed?
               (time->string (assq-ref build #:stoptime))
               "—"))
      (td ,(assq-ref build #:job))
      (td ,(assq-ref build #:nixname))
      (td ,(assq-ref build #:system))
      (td ,(if completed?
               `(a (@ (href "/build/" ,(assq-ref build #:id) "/log/raw"))
                   "raw")
               "—"))))

  `((p (@ (class "lead"))
       "Builds matching " (em ,query))
    (table
     (@ (class "table table-sm table-hover table-striped"))
     ,@(if (null? builds)
           `((th (@ (scope "col")) "No elements here."))
           `(,(table-header)
             (tbody ,@(map table-row builds)))))

    ,(if (null? builds)
         (pagination "" "" "" "")
         (let* ((build-ids (map (lambda (row) (assq-ref row #:id)) builds))
                (page-build-min (last build-ids))
                (page-build-max (first build-ids)))
           (pagination
            (format
             #f "?query=~a&border-high-id=~d"
             query
             (1+ (first build-max)))
            (if (equal? page-build-max (first build-max))
                ""
                (format
                 #f "?query=~a&border-low-id=~d"
                 query
                 page-build-max))
            (if (equal? page-build-min (first build-min))
                ""
                (format
                 #f "?query=~a&border-high-id=~d"
                 query
                 page-build-min))
            (format
             #f "?query=~a&border-low-id=~d"
             query
             (1- (first build-min))))))))

(define (running-builds-table builds)
  "Return HTML for the running builds table."
  (define (build-row build)
    `(tr
      (th (@ (scope "row"))
          (a (@ (href "/build/" ,(assq-ref build #:id) "/details"))
             ,(assq-ref build #:id)))
      (td ,(assq-ref build #:job-name))
      (td ,(time->string
            (assq-ref build #:starttime)))
      (td ,(assq-ref build #:system))
      (td (a (@ (href "/build/" ,(assq-ref build #:id) "/log/raw"))
             "raw"))))

  `((p (@ (class "lead")) "Running builds")
    (table
     (@ (class "table table-sm table-hover table-striped"))
     ,@(if (null? builds)
           `((th (@ (scope "col")) "No elements here."))
           `((thead (tr (th (@ (scope "col")) "ID")
                        (th (@ (scope "col")) "Job")
                        (th (@ (scope "col")) "Queued at")
                        (th (@ (scope "col")) "System")
                        (th (@ (scope "col")) "Log")))
             (tbody
              ,(map build-row builds)))))))

(define* (make-line-chart id datasets
                          #:key
                          (interpolation? #t)
                          (legend? #f)
                          (time-x-axes? #f)
                          xaxes-labels
                          x-label
                          y-label
                          (x-unit "day")
                          title
                          labels
                          colors)
  (let* ((normal-xAxes (vector `((type . "category")
                                 (labels . ,xaxes-labels)
                                 (display . #t)
                                 (scaleLabel
                                  . ((display . #t)
                                     (labelString . ,x-label))))))
         (time-xAxes (vector `((type . "time")
                               (time . ((unit . ,x-unit)))
                               (display . #t)
                               (distribution . "series")
                               (scaleLabel
                                . ((display . #t)
                                   (labelString . ,x-label))))))
         (scales  `((xAxes . ,(if time-x-axes?
                                  time-xAxes
                                  normal-xAxes))
                    (yAxes
                     . ,(vector `((display . #t)
                                  (scaleLabel
                                   . ((display . #t)
                                      (labelString . ,y-label))))))))
         (chart `((type . "line")
                  (data . ((datasets
                            . ,(apply vector
                                      (map (lambda (dataset label color)
                                             `((fill . #f)
                                               (label . ,label)
                                               ,@(if interpolation?
                                                     '()
                                                     '((lineTension . 0)))
                                               (borderColor . ,color)
                                               (data . ,dataset)))
                                           datasets labels colors)))))
                  (options . ((responsive . #t)
                              (tooltips . ((enabled . #f)))
                              (legend . ((display . ,legend?)))
                              (title . ((display . #f)
                                        (text . ,title)))
                              (scales . ,scales))))))
    `((script ,(format #f "window.addEventListener(\"load\",
function(event) {\
window.~a = new Chart\
(document.getElementById('~a').getContext('2d'), ~a);\
});" id id (scm->json-string chart))))))

(define* (global-metrics-content #:key
                                 avg-eval-durations
                                 avg-eval-build-start-time
                                 builds-per-day
                                 builds-per-machine
                                 eval-completion-speed
                                 new-derivations-per-day
                                 pending-builds
                                 percentage-failed-eval)
  (define (avg-eval-duration-row . eval-durations)
    (let ((spec (match eval-durations
                  (((spec . _) . rest) spec))))
      `(tr (td ,spec)
           ,@(map (lambda (duration)
                    `(td ,(number->string
                           (nearest-exact-integer duration))))
                  (map cdr eval-durations)))))

  (define (percentage-failed-eval-row . percentages)
    (let ((spec (match percentages
                  (((spec . _) . rest) spec))))
      `(tr (td ,spec)
           ,@(map (lambda (duration)
                    `(td ,(number->string
                           (nearest-exact-integer duration))
                         "%"))
                  (map cdr percentages)))))

  (define (builds-per-machine-rows builds)
    (map (match-lambda
           ((field . value)
            `(tr (td ,field)
                 (td ,(number->string
                       (nearest-exact-integer value))))))
         builds))

  (define (builds->json-scm builds)
    (apply vector
           (map (match-lambda
                  ((field . value)
                   `((x . ,(* field 1000)) (y . ,value))))
                builds)))

  (define (evals->json-scm evals)
    (apply vector
           (map (match-lambda
                  ((field . value)
                   `((x . ,(number->string field)) (y . ,value))))
                evals)))

  (define (evals->labels evals)
    (apply vector
           (map (match-lambda
                  ((field . value) field))
                evals)))

  (let ((builds-chart "builds_per_day")
        (build-start-chart "avg_eval_build_start_time")
        (evaluation-speed-chart "eval_speed_chart")
        (pending-builds-chart "pending_builds"))
    `((p (@ (class "lead")) "Global metrics")
      (h6 "Average evaluation duration per specification (seconds).")
      (table
       (@ (class "table table-sm table-hover table-striped"))
       (thead (tr (th (@ (scope "col")) "Specification")
                  (th (@ (scope "col")) "10 last evaluations")
                  (th (@ (scope "col")) "100 last evaluations")
                  (th (@ (scope "col")) "All evaluations")))
       (tbody
        ,(apply map avg-eval-duration-row avg-eval-durations)))
      (br)
      (h6 "Builds completion.")
      (p "This shows the difference between newly added derivations and built
derivations per day.")
      (canvas (@ (id ,builds-chart)))
      (br)
      (h6 "Evaluation average build start time.")
      (p "This is the average time required for an evaluation to start its
builds.")
      (br)
      (canvas (@ (id ,build-start-chart)))
      (br)
      (h6 "Evaluation completion speed.")
      (p "The evaluation completion speed is the sum of an evaluation
completed builds divided by the time required to build them.")
      (br)
      (canvas (@ (id ,evaluation-speed-chart)))
      (br)
      (h6 "Pending builds.")
      (p "This is the sum of all the currently pending builds.")
      (br)
      (canvas (@ (id ,pending-builds-chart)))
      (br)
      (h6 "Builds per machine.")
      (p "This is the builds count per machine during the last day.")
      (table
       (@ (class "table table-sm table-hover table-striped"))
       (thead (tr (th (@ (scope "col")) "Machine")
                  (th (@ (scope "col")) "Builds (last 24 hours)")))
       (tbody
        ,(builds-per-machine-rows builds-per-machine)))
      (br)
      (h6 "Percentage of failed evaluations.")
      (table
       (@ (class "table table-sm table-hover table-striped"))
       (thead (tr (th (@ (scope "col")) "Specification")
                  (th (@ (scope "col")) "10 last evaluations")
                  (th (@ (scope "col")) "100 last evaluations")
                  (th (@ (scope "col")) "All evaluations")))
       (tbody
        ,(apply map percentage-failed-eval-row percentage-failed-eval)))
      ;; Scripts.
      (script (@ (src "/static/js/chart.js")))
      ,@(make-line-chart builds-chart
                         (list (builds->json-scm new-derivations-per-day)
                               (builds->json-scm builds-per-day))
                         #:interpolation? #f
                         #:time-x-axes? #t
                         #:x-label "Day"
                         #:y-label "Builds"
                         #:title "Builds per day"
                         #:legend? #t
                         #:labels '("New derivations"
                                    "Builds completed")
                         #:colors (list "#f6dd27" "#3e95cd"))
      ,@(make-line-chart build-start-chart
                         (list (evals->json-scm avg-eval-build-start-time))
                         #:xaxes-labels (evals->labels
                                         avg-eval-build-start-time)
                         #:x-label "Evaluations"
                         #:y-label "Time (s)"
                         #:title "Evaluation average build start time"
                         #:labels '("Build start time")
                         #:colors (list "#3e95cd"))
      ,@(make-line-chart evaluation-speed-chart
                         (list (evals->json-scm eval-completion-speed))
                         #:xaxes-labels (evals->labels
                                         eval-completion-speed)
                         #:x-label "Evaluations"
                         #:y-label "Speed (builds/hour)"
                         #:title "Evaluation completion speed"
                         #:labels '("Completion speed")
                         #:colors (list "#3e95cd"))
      ,@(make-line-chart pending-builds-chart
                         (list (builds->json-scm pending-builds))
                         #:time-x-axes? #t
                         #:x-label "Day"
                         #:y-label "Builds"
                         #:title "Pending builds"
                         #:labels '("Pending builds")
                         #:colors (list "#3e95cd")))))

(define (workers-status workers builds)
  (define (machine-row machine)
    (let* ((workers (sort (filter-map
                           (lambda (worker)
                             (and (string=? (worker-machine worker)
                                            machine)
                                  (worker-name worker)))
                           workers)
                          string<?))
           (builds
            (map (lambda (worker)
                   (match (filter
                           (lambda (build)
                             (let ((build-worker
                                    (assq-ref build #:worker)))
                               (and build-worker
                                    (string=? build-worker worker))))
                           builds)
                     (() #f)
                     ((build _ ...) build)))
                 workers)))
      `(div (@ (class "col-sm-4 mt-3"))
            (a (@(href "/machine/" ,machine))
               (h6 ,machine))
            ,(map (lambda (build)
                    (let ((style (format #f
                                         "width: ~a%"
                                         (if build
                                             (assq-ref build #:percentage)
                                             0))))
                      `(div (@ (class "progress mt-1")
                               (style "height: 20px"))
                            (div (@ (class "progress-bar")
                                    (role "progressbar")
                                    (style ,style)
                                    (aria-valuemin "0")
                                    (aria-valuemax "100"))
                                 ,(if build
                                      `(strong
                                        (@ (class "justify-content-center
d-flex position-absolute w-100"))
                                        (a (@ (class "text-dark text-truncate")
                                              (style "max-width: 150px")
                                              (href "/build/"
                                                    ,(assq-ref build #:id)
                                                    "/details"))
                                           ,(assq-ref build #:job-name)))
                                      '(em
                                        (@ (class "justify-content-center
text-dark d-flex position-absolute w-100"))
                                        "idle"))))))
                  builds))))

  (let ((machines (reverse
                   (sort (delete-duplicates
                          (map worker-machine workers))
                         version>?))))
    `((p (@ (class "lead")) "Workers status")
      (div (@ (class "container"))
           (div (@ (class "row"))
                ,@(map machine-row machines))))))

(define* (machine-status name workers builds info)
  (define (history->json-scm history)
    (apply vector
           (map (match-lambda
                  ((field . value)
                   `((x . ,(* field 1000)) (y . ,value))))
                history)))

  (define (ram-available->json-scm history)
    (apply vector
           (map (match-lambda
                  ((field . value)
                   `((x . ,(* field 1000))
                     (y . ,(/ value (expt 2 30))))))
                history)))

  `((p (@ (class "lead")) "Machine " ,name)
    ,@(if (null? info)
          '()
          `((table
             (@ (class "table table-sm table-hover table-striped"))
             (tbody
              (tr (th "Hostname")
                  (td ,(assq-ref info #:hostname)))
              (tr (th "Info")
                  (td ,(assq-ref info #:info)))
              (tr (th "Boot time")
                  (td ,(time->string
                        (assq-ref info #:boottime))))
              (tr (th "Total RAM")
                  (td ,(assq-ref info #:ram)))
              (tr (th "Total root disk space")
                  (td ,(assq-ref info #:root-space)))
              (tr (th "Total store disk space")
                  (td ,(assq-ref info #:store-space)))))))
    (h6 "Workers")
    (table
     (@ (class "table table-sm table-hover table-striped"))
     ,@(if (null? workers)
           `((th (@ (scope "col")) "No elements here."))
           `((thead
              (tr
               (th (@ (scope "col")) "Name")
               (th (@ (scope "col")) "Systems")
               (th (@ (scope "col")) "Building")
               (th (@ (scope "col")) "Last seen")))
             (tbody
              ,@(map
                 (lambda (worker build)
                   `(tr (td ,(worker-name worker))
                        (td ,(string-join (worker-systems worker)
                                          ", "))
                        (td ,(match build
                               (() "idle")
                               ((build _ ...)
                                `(a (@ (class "text-truncate")
                                       (style "max-width: 150px")
                                       (href "/build/"
                                             ,(assq-ref build #:id)
                                             "/details"))
                                    ,(assq-ref build #:job-name)))))
                        (td ,(time->string
                              (worker-last-seen worker)))))
                 workers builds)))))
    ,@(if (null? info)
          '((div (@ (class "alert alert-danger"))
                 "Could not find machine information using Zabbix."))
          `((h6 "CPU idle time")
            ,@(let ((cpu-idle (assq-ref info #:cpu-idle))
                    (cpu-idle-chart "cpu_idle_chart"))
                `((script (@ (src "/static/js/chart.js")))
                  (br)
                  (canvas (@ (id ,cpu-idle-chart)))
                  ,@(make-line-chart cpu-idle-chart
                                     (list (history->json-scm cpu-idle))
                                     #:time-x-axes? #t
                                     #:x-label "Time"
                                     #:y-label "Percentage"
                                     #:x-unit "minute"
                                     #:title "CPU idle time"
                                     #:labels '("CPU idle time")
                                     #:colors (list "#3e95cd"))))
            (br)
            (h6 "Available memory")
            ,@(let ((ram-available (assq-ref info #:ram-available))
                    (ram-available-chart "ram_available_chart"))
                `((script (@ (src "/static/js/chart.js")))
                  (br)
                  (canvas (@ (id ,ram-available-chart)))
                  ,@(make-line-chart ram-available-chart
                                     (list
                                      (ram-available->json-scm ram-available))
                                     #:time-x-axes? #t
                                     #:x-label "Time"
                                     #:y-label "GiB"
                                     #:x-unit "minute"
                                     #:title
                                     "Available memory"
                                     #:labels
                                     '("Available memory")
                                     #:colors (list "#3e95cd"))))
            (br)
            (h6 "Free store disk space percentage")
            ,@(let ((store-free (assq-ref info #:store-free))
                    (store-free-chart "store_free_chart"))
                `((script (@ (src "/static/js/chart.js")))
                  (br)
                  (canvas (@ (id ,store-free-chart)))
                  ,@(make-line-chart store-free-chart
                                     (list (history->json-scm store-free))
                                     #:time-x-axes? #t
                                     #:x-label "Time"
                                     #:y-label "Percentage"
                                     #:x-unit "minute"
                                     #:title
                                     "Free store disk space percentage"
                                     #:labels
                                     '("Free store disk space percentage")
                                     #:colors (list "#3e95cd"))))))))
