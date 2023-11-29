;;; Copyright © 2017  Roel Janssen <roel@gnu.org>
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

(define-module (www pages welcome)
  #:use-module (www pages)
  #:export (page-welcome))

(define (page-welcome request-path site-config)
  (page-root-template "Search" request-path site-config
   `((h2 "Find software packages")
     (p (strong "ProTip:") " The search field takes regular expressions."
        "  For example, when searching for R, " (code "^r") " provides far"
        " better results than “just” " (code "r") ".  "
        "You can also restrict results to a specific channel—e.g., "
        (code "channel:guix-science") ".")
     (p "")
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
                   (th "Channel")
                   (th "Synopsis")))))
     (script (@ (type "text/javascript")
                (src "/static/hpcguix-web.min.js")) ""))
   #:dependencies '(datatables)))
