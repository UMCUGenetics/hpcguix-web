;;; Copyright © 2016  Roel Janssen <roel@gnu.org>
;;; Copyright © 2016  Ricardo Wurmus <rekado@elephly.net>
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

(define-module (www util)
  #:use-module (srfi srfi-1)
  #:export (file-extension
            string-replace-occurrence
            module-path))

(define (file-extension file-name)
  (last (string-split file-name #\.)))

(define (string-replace-occurrence str occurrence alternative)
  (string-map (lambda (x) (if (eq? x occurrence) alternative x)) str))

(define (module-path prefix elements)
  "Returns the module path so it can be loaded."
  (if (> (length elements) 1)
      (module-path
       (append prefix (list (string->symbol (car elements))))
       (cdr elements))
      (append prefix (list (string->symbol (car elements))))))
