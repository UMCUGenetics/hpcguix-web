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

(define-module (www pages build-status)
  #:use-module (www pages)
  #:use-module (ice-9 ftw)
  #:export (page-build-status))

(define (page-build-status request-path)
  (let ((build-items (scandir "/tmp"
                       (lambda (item)
                         (and (> (string-length item) 10)
                              (string= (string-take item 10) "guix-build"))))))
    (page-root-template "Build status" request-path
     `((h2 "Build status")
       ,(if (> (length build-items) 0)
            `((p "The following build jobs are currently running:")
              (ul
               ,(map (lambda (item)
                       `(li ,(substring
                              (string-take item (- (string-length item) 6))
                              11)))
                     build-items)))
            `(p "Currently, no build jobs are running."))))))
