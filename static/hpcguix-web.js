// @license magnet:?xt=urn:btih:0b31508aeb0634b347b8270c7bee4d411b5d4109&dn=agpl-3.0.txt AGPL-v3-or-later
function feed_table(packages) {
    var dt = $('#packages-table').DataTable({
                sDom: 'lrtip',
                data: packages,
                processing: true,
                createdRow: function (row, data, index) {
                                $('#stand-by').hide();
                                $('#packages-table').show();
                            },
                columns: [
                    { data: 'name',
                      mRender: function (data, type, full) {
                                  return '<a href="package/' + data + '">' + data + '</a>';
                               }
                    },
                    { data: 'version' },
                    { data: 'synopsis' },
                    { data: 'module',
                      visible: false
                    },
                    { data: 'homepage',
                      mRender: function (data, type, full) {
                                  return '<a href="' + data + '">' + data + '</a>';
                               }
                    }
                ]});
    $('#search-field').on('keyup', function() {
        dt.search(this.value).draw();
    });
    $('#search-field').on('keydown', function(event) {
        if (event.which == 13) return false;
    });
}
$(document).ready(function() {
    $('#packages-table').hide();
    $.getJSON('/packages.json', feed_table);
});
// @license-end
