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
  #:use-module ((web request) #:select (request-uri))
  #:use-module ((web response) #:select (build-response))
  #:use-module ((web uri) #:select (uri-path))
  #:use-module ((sxml simple) #:select (sxml->xml))
  #:export (page-error-404
            page-error-filesize
            page-error
            respond-404))

(define (page-error-404 request-path site-config)
  (page-root-template "Oops!" request-path site-config
   `(p "The page you tried to reach cannot be found.")))

(define (page-error-filesize request-path site-config)
  (page-root-template "Oops!" request-path site-config
   `(p ,(format #f "The maximum file size has been set to ~a megabytes."
                (/ %www-max-file-size 1000000)))))

(define page-error page-error-404)

(define (respond-404 config request)
  "Return 404 with a pretty HTML page."
  (values (build-response #:code 404
                          #:headers '((content-type . (text/html))))
          (with-output-to-string
            (lambda _
              (sxml->xml
               (page-error-404 (uri-path (request-uri request))
                               config))))))
