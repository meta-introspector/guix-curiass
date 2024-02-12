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
    var spec_table = $('#spec-table');
    if (spec_table.find('th').length > 1) {
        spec_table.DataTable({
            ...default_opts,
            /* Do not sort the 'Action' column. */
            columnDefs: [
                { orderable: false, targets: 5 },
                { orderable: false, targets: 6 }
            ],
        });
    }

    $('.job-toggle').click(function() {
        $('.job-abs').toggleClass('d-none');
        $('.job-rel').toggleClass('d-none');
    })

    /* Evaluations page. */
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

    $(function(){
        if ($("#paginate").length > 0) {
            var url = new URL(window.location.href);
            var params = url.searchParams;
            var paginate = params.get('paginate');
            var href;
            if (!paginate || paginate == '1') {
                params.set('paginate', 0);
                $('#paginate').attr('href', url.toString());
            } else if (paginate == '0') {
                params.set('paginate', 1);
                $('#paginate').attr('class', 'oi oi-collapse-up');
                $('#paginate').attr('href', url.toString());
            }
        }
    });
    /* Build details page. */
    $('.dependencies').collapse({
        toggle: false
    })
    $('.dependencies').on('hide.bs.collapse', function () {
        $('#collapse-dep-btn').text("Show more");
    })
    $('.dependencies').on('show.bs.collapse', function () {
        $('#collapse-dep-btn').text("Show less");
    })

    /* Dashboard page. */
    $(function(){
        var cache_data;

        if ($("#get-dashboard").length > 0) {
            function enableLoadButton() {
                $('#load-btn').removeAttr('disabled');
                $('#load-btn').text('Go');
            }

            function disableLoadButton() {
                $('#load-btn').attr('disabled', 'true');
                $('#load-btn').text(' Loading');
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
                    })
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

            $('#query-jobs').on('input', function(e){
                filterSvg($(this).val());
            });

            var dashboard = $('#dashboard');
            initSvg(dashboard.attr('url'));

            window.filterSvg = filterSvg;
        }
    });

    /* Specification edition page. */
    $(function(){
        if ($(".edit-channel").length > 0) {
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
                if (['custom', 'packages', 'manifests'].indexOf(val) >= 0) {
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
        }
    });
});
