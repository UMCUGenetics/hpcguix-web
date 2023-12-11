;;; Copyright © 2023 Ludovic Courtès <ludo@gnu.org>
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

(define-module (www pages channels)
  #:use-module (hpcweb-configuration)
  #:use-module (www pages)
  #:autoload   (www pages error) (page-error-404)
  #:autoload   (www util) (manual-url)
  #:use-module (guix channels)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (page-channels))

(define (channel-list-shtml channels)
  (define (channel-row channel)
    (let ((name (symbol->string (channel-name channel))))
      `(tr (a (@ (href ,(string-append "/channel/" name)))
              (code ,name)))))

  `(table ,@(map channel-row channels)))

(define %not-slash
  (char-set-complement (char-set #\/)))

(define (page-channels request-path config)
  (match (string-tokenize request-path %not-slash)
    (("channels")
     (page-root-template
      "Channels"
      request-path config
      `((h2 "Channels")

        "This service knows about packages provided by the following "
        (a (@ (href ,(manual-url "Channels")))
           (em "channels"))
        ":"

        (p
         ,(channel-list-shtml (hpcweb-configuration-channels config))))))
    (_                                            ;invalid URI path
     (page-error-404 request-path config))))