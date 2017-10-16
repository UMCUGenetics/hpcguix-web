;;; Copyright © 2017  Ricardo Wurmus <rekado@elephly.net>
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

(define-module (www pages javascript)
  #:use-module (www pages)
  #:export (page-javascript))

(define (page-javascript request-path)
  (page-root-template
   "JavaScript license information" request-path
   `((h2 "JavaScript license information")
     (table (@ (id "jslicense-labels1"))
            (tr (td (a (@ (href "/static/datatables.min.js"))
                       "datatables.min.js"))
                (td (a (@ (href "http://datatables.net/license"))
                       "Expat")))
            (tr (td (a (@ (href "/static/highlight/highlight.min.js"))
                       "highlight.min.js"))
                (td (a (@ (href "https://raw.githubusercontent.com/isagalaev/highlight.js/master/LICENSE"))
                       "BSD-3")))
            (tr (td (a (@ (href "./static/jquery-2.2.4.min.js"))
                       "jquery-2.2.4.js"))
                (td (a (@ (href "https://raw.githubusercontent.com/jquery/jquery/master/LICENSE.txt"))
                       "Expat")))))))

