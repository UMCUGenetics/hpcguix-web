;;; Copyright © 2017  Roel Janssen <roel@gnu.org>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(define-module (www pages welcome)
  #:use-module (www pages)
  #:export (page-welcome))

(define (page-welcome request-path)
  (page-root-template "Search" request-path
   `((h2 "Find software packages")

     (form
      (input (@ (type "search")
                (id "search-field")
                (class "search-field")
                (aria-controls "packages-table")
                (placeholder "Search"))))
     (hr)
     (div (@ (id "stand-by")) (p "Please wait for the package data to load..."))
     (div (@ (id "packages-table-wrapper"))
          (table (@ (id "packages-table")
                    (class "display"))
                 (thead
                  (tr
                   (th "Name")
                   (th "Version")
                   (th "Synopsis")
                   (th (@ (style "width: 250pt")) "Homepage")))))
     (script (@ (type "text/javascript")) "
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
                                  return '" (a (@ (href "package/' + data + '")) "' + data + '") "';
                               }
                    },
                    { data: 'version' },
                    { data: 'synopsis' },
                    { data: 'homepage',
                      mRender: function (data, type, full) {
                                  return '" (a (@ (href "' + data + '")) "' + data + '") "';
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
"))
   #:dependencies '(datatables)))
