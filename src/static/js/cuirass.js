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
                { orderable: false, targets: 5 },
                { orderable: false, targets: 6 }
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
});
