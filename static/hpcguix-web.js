// @license magnet:?xt=urn:btih:0b31508aeb0634b347b8270c7bee4d411b5d4109&dn=agpl-3.0.txt AGPL-v3-or-later
var hpcguix = (function () {
    // Return a regular expression string without boundary slashes.
    function makeRegex (terms) {
        var search = '^(?=.*?' + terms.join( ')(?=.*?' ) + ').*$';
	    return new RegExp(search, 'i').toString().slice(1,-2);
    }

    // Split a search string into a list of global search terms and
    // per-column search terms for any term that has a column name
    // followed by a colon as its prefix.
    function parseQuery(search, columns) {
        var queries = {},
            global = [];

        search.split(' ').forEach(function (word) {
            var term = word.split(':'),
                columnName = false;

            // Restrict search to specified column
            if ((term.length > 1) &&
                (columns.findIndex(function(el) { return el.data === term[0]; }) > -1)) {
                columnName = term[0];
                term = term.slice(1).join(':');

                queries[columnName] ? queries[columnName].push(term) : queries[columnName] = [term];
            } else {
                global.push(word);
            }
        });

        return [global, queries];
    }

    function feed_table(packages) {
        var dt = $('#packages-table').DataTable({
            sDom: 'lrtip',
            data: packages,
            processing: true,
	    autoWidth: false,
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
                { data: 'version',
		  width: '10%' },
		{ data: 'channel',
		  width: '10%',
		  mRender: function (data, type, full) {
		      return '<a href="channel/' + data.name + '">' + data.name + '</a>';
		  }
		},
                { data: 'synopsis',
		  width: '60%' }
            ]});

        // Filter the table on each input change. Debounce for 200ms.
        var debounce = null,
            debounceTimeout = 500;
        $('#search-field').on('input', function(e) {
            if (debounce !== null) {
                clearTimeout(debounce);
            }
            debounce = setTimeout(function() {
                var columns = dt.settings().init().columns,
                    query = parseQuery(e.target.value, columns),
                    global = query[0],
                    perColumn = query[1];

                // Set (or reset) per-column filters.
                dt.columns().every(function (index) {
                    var name = columns[index].data;
                    if (perColumn.hasOwnProperty(name)) {
                        dt.column(index).search(makeRegex(perColumn[name]), true, false);
                    } else {
                        dt.column(index).search('', true, false);
                    }
                });

                // Set global filter.
                dt.search(makeRegex(global), true, false).draw();
            }, debounceTimeout);
        });

        $('#search-field').on('keydown', function(event) {
            if (event.which == 13) return false;
        });

	// The search field might already have a value: take it into
	// account.
	$('#search-field').trigger('keyup');
    }

    // Initialize the packages table.
    function init () {
        $('#packages-table').hide();

	// Honor "?q=TERM" URL parameters.
	var params = new URLSearchParams(document.location.search);
	$('#search-field').val(params.get("q"));

        $.getJSON('/packages.json', feed_table);
    }

    return {'init': init };
})();

$(document).ready(hpcguix.init);
// @license-end
