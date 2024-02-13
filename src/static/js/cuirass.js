/**
 *
 * @source:
 * https://git.savannah.gnu.org/cgit/guix/guix-cuirass.git/tree/src/static/js/cuirass.js
 *
 * @licstart  The following is the entire license notice for the
 *  JavaScript code in this page.
 *
 * Copyright (C) 2021 - Mathieu Othacehe <othacehe@gnu.org>
 * Copyright (C) 2024 - Ricardo Wurmus <rekado@elephly.net>
 *
 *
 * The JavaScript code in this page is free software: you can
 * redistribute it and/or modify it under the terms of the GNU
 * General Public License (GNU GPL) as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option)
 * any later version.  The code is distributed WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.
 *
 * As additional permission under GNU GPL version 3 section 7, you
 * may distribute non-source (e.g., minimized or compacted) forms of
 * that code without the copy of the GNU GPL normally required by
 * section 4, provided you include this license notice and a URL
 * through which recipients can access the Corresponding Source.
 *
 * @licend  The above is the entire license notice
 * for the JavaScript code in this page.
 *
 */

var ready = (callback) => {
  if (document.readyState != "loading") callback();
  else document.addEventListener("DOMContentLoaded", callback);
}

ready(() => {
    /* Specifications page. */
    var default_opts = {
        paging: false,
        searching: false,
        info: false,
        order: [],
    };
    var spec_table = document.querySelector('#spec-table');
    if ((spec_table !== null) &&
        (spec_table.querySelectorAll('th').length > 1)) {
        new DataTable('#spec-table', {
            ...default_opts,
            /* Do not sort the 'Action' column. */
            columnDefs: [
                { orderable: false, targets: 5 },
                { orderable: false, targets: 6 }
            ],
        });
    }

    var job_toggle = document.querySelector('.job-toggle');
    if (job_toggle !== null) {
        job_toggle.addEventListener("click", (e) => {
            job_abs = document.querySelectorAll('.job-abs'),
            job_rel = document.querySelectorAll('.job-rel');
            
            job_abs.forEach((el) => el.classList.toggle('d-none'));
            job_rel.forEach((el) => el.classList.toggle('d-none'));
        });
    }

    /* Evaluations page. */
    var eval_table = document.querySelector('#eval-table');
    if ((eval_table !== null) &&
        (eval_table.querySelectorAll('th').length > 1)) {
        new DataTable('#eval-table', {
            ...default_opts,
            columnDefs: [
                { orderable: false, targets: 0 },
                { orderable: false, targets: 1 },
                { orderable: false, targets: 8 }
            ],
        });
    }

    (function(){
        var paginateElement = document.querySelector("#paginate");
        if (paginateElement !== null) {
            var url = new URL(window.location.href);
            var params = url.searchParams;
            var paginate = params.get('paginate');
            var href;
            if (!paginate || paginate == '1') {
                params.set('paginate', 0);
                paginateElement.setAttribute('href', url.toString());
            } else if (paginate == '0') {
                params.set('paginate', 1);
                paginateElement.setAttribute('class', 'oi oi-collapse-up');
                paginateElement.setAttribute('href', url.toString());
            }
        }
    })();
    /* Build details page. */
    (function(){
        var dependencies = document.querySelector('.dependencies'),
            collapse_dep_btn = document.querySelector('#collapse-dep-btn');

        if (dependencies !== null) {
            // TODO: replace collapse with details/summary
            dependencies.collapse({ toggle: false });
            dependencies.addEventListener('hide.bs.collapse', (e) =>
                collapse_dep_btn.textContent = "Show more");
            dependencies.addEventListener('show.bs.collapse', (e) =>
                collapse_dep_btn.textContent = "Show less");
        }
    })();

    /* Dashboard page. */
    (function(){
        var cache_data;

        if (document.querySelector("#get-dashboard") !== null) {
            function enableLoadButton() {
                var button = document.querySelector('#load-btn');
                button.removeAttribute('disabled');
                button.textContent = 'Go';
            }

            function disableLoadButton() {
                var button = document.querySelector('#load-btn');
                button.setAttribute('disabled', 'true');
                button.textContent = ' Loading';
            }

            function radius(count) {
                if (count < 100)
                    return 15;
                else if (count < 1000)
                    return 10;
                else
                    return 5;
            }

	    /* FIXME-someday: these should be CSS classes, so users can
	       feasibly customise them. */
            function color(status) {
                switch (status) {
                case -3:
                case -2:
                    return 'transparent';
                case -1:
                    return 'orange';
                case 0:
                    return '#9f9';	/* high contrast with others */
                case 1:
                case 2:
                case 3:
                case 4:
                    return 'red';
                }
            }

            function svgWidth() {
                var width = d3.select('#content').style('width').slice(0, -2);
                return Math.round(Number(width));
            }

            function drawPage(data) {
                var width = svgWidth();
                var circle_radius = radius(data.length);
                var margin_x = circle_radius;
                var margin_y = circle_radius;
                var margin_circle_x = 3;
                var margin_circle_y = (2.5 * circle_radius);
                var circle_count_x =
                    Math.floor((width - 2 * margin_x) /
                               ((2 * circle_radius) + margin_circle_x));
                var height = ((data.length / circle_count_x) *
                              margin_circle_y) +
                    circle_radius + 2 * margin_y;

                var div = d3.select('body').append('div')
                    .attr('class', 'tooltip')
                    .style('opacity', 0);
                var svg = d3.select('#content').append('svg')
                    .attr('width', width)
                    .attr('height', height);

                var circles = svg.append('g')
                    .selectAll('circle')
                    .data(data)
                    .enter()
                    .append('a')
                    .attr('xlink:href', d => '/build/' + d.build + '/details')
                    .append('circle')
                    .attr('r', circle_radius)
                    .attr('cx', function(d, i) {
                        return margin_x +
                            (i % circle_count_x)
                            * (circle_radius * 2 + margin_circle_x);
                    })
                    .attr('cy', function (d, i) {
                        return margin_y + Math.floor(i / circle_count_x)
                            * margin_circle_y;
                    })
                    .style('fill', d => color(d.status))
                    .on('mouseover', function(event, d) {
                        var circle = d3.select(this)
                            .style('fill', 'steelblue');
                        div.style('opacity', .9);
                        div.html(d.name)
                            .style('left', (event.pageX + 30) + 'px')
                            .style('top', (event.pageY - 30) + 'px');
                    })
                    .on('mouseout', function(event, d) {
                        var circle = d3.select(this)
                            .style('fill', color(d.status));
                        div.style('opacity', 0);
                        div.html('')
                            .style('left', '0px')
                            .style('top', '0px');
                    });
                enableLoadButton();
            }

            function filterSvg(str) {
                d3.selectAll("svg").remove();
                drawPage(cache_data.filter(function (c, i) {
                    return c.name.includes(str);
                }));
            }

            function initSvg(jobs) {
                d3.json(jobs).then(function (data) {
                    cache_data = data;
                    drawPage(data);
                });
            }

            document.querySelector('#query-jobs').addEventListener('input', (e) =>
                filterSvg(e.target.value));

            var dashboard = document.querySelector('#dashboard');
            initSvg(dashboard.getAttribute('url'));

            window.filterSvg = filterSvg;
        }
    })();

    /* Specification edition page. */
    (function(){
        if (document.querySelector(".edit-channel") !== null) {
            document.querySelector('.remove-channel').addEventListener('click', (e) =>
                e.target.parentNode.remove());
            document.querySelector('.add-channel').addEventListener('click', (e) => {
                var clone = document.querySelector('.channel').cloneNode(true);
                clone.setAttribute('class', 'form-group row channel-new');
                clone.querySelector('.col-form-label').textContent = '';

                var new_button = clone.querySelector('.add-channel');
                new_button.setAttribute('class', 'btn btn-danger remove-channel');
                new_button.textContent = 'Remove';
                new_button.addEventListener('click', (e) =>
                    e.target.parentNode.remove());

                var channels = document.querySelector('.channels');
                channels.append(clone);
            });

            /* require that at least one .system checkbox is checked */
            var cbs = document.querySelectorAll('.system'),
                require_system_check = function () {
                    var checked_cbs = document.querySelectorAll('.system:checked').length;
                    if (checked_cbs == 0) {
                        cbs.forEach((cb) => cb.setAttribute('required', 'required'));
                    } else {
                        cbs.forEach((cb) => cb.removeAttribute('required'));
                    }
                };
            cbs.forEach((cb) =>
                cb.addEventListener('change', (e) => require_system_check()));
            require_system_check();

            (function () {
                'use strict';
                var forms = document.querySelectorAll('.needs-validation');
                Array.prototype.slice.call(forms)
                    .forEach(function (form) {
                        form.addEventListener('submit', function (event) {
                            if (!form.checkValidity()) {
                                event.preventDefault();
                                event.stopPropagation();
                            }
                            form.classList.add('was-validated');
                        }, false);
                    });
            })();

            const select_choices = new Choices(document.querySelectorAll('.build-param-select')[0], {
                removeItemButton: true,
                duplicateItemsAllowed: false,
            });
            const input_choices = new Choices(document.querySelectorAll('.build-param-input')[0], {
                removeItemButton: true,
                duplicateItemsAllowed: false,
            });
            document.querySelector('.build-param-select').addEventListener('showDropdown', (e) => {
                var names = Array.prototype.map.call(document.querySelectorAll('.channel-name'), (el) => {
                    return {'value': el.value, 'label': el.value};
                });
                select_choices.setChoices(names, 'value', 'label', true);
            });
            var param_select = document.querySelector('.build-select');
            var param_select_cb = function(){
                var val = param_select.value,
                    input_row = document.querySelector('.param-input-row'),
                    select_row = document.querySelector('.param-select-row');
                if (['custom', 'packages', 'manifests'].indexOf(val) >= 0) {
                    input_choices.clearStore();
                    input_row.style.display = '';
                } else {
                    input_row.style.display = 'none';
                }

                if (['channels'].indexOf(val) >= 0) {
                    select_row.style.display = '';
                } else {
                    select_row.style.display = 'none';
                }
            };
            param_select_cb();
            param_select.addEventListener('change', param_select_cb);

            const default_param = document.querySelector('.default-build-param');
            if (default_param) {
                var items = default_param.textContent.split(',').map((name) => {
                    return { 'value': name, 'label': name, selected: true};
                });
                var select_row = document.querySelector('.param-select-row'),
                    input_row = document.querySelector('.param-input-row');
                if (select_row.checkVisibility()) {
                    select_choices.setChoices(items, 'value', 'label', true);
                } else if (input_row.checkVisibility()) {
                    input_choices.setValue(items);
                }}
        }
    })();
});
