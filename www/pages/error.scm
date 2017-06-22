;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
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

(define-module (www pages error)
  #:use-module (www pages)
  #:use-module (www config)
  #:export (page-error-404
            page-error-filesize
            page-error))

(define (page-error-404 request-path)
  (page-root-template "Oops!" request-path
   `(p "The page you tried to reach cannot be found.")))

(define (page-error-filesize request-path)
  (page-root-template "Oops!" request-path
   `(p ,(format #f "The maximum file size has been set to ~a megabytes."
                (/ %www-max-file-size 1000000)))))

(define page-error page-error-404)
