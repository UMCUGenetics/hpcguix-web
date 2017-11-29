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

(define-module (www pages)
  #:use-module (srfi srfi-1)
  #:use-module (site-specific config)
  #:use-module (www util)
  #:export (page-root-template))

(define pages
  '(("/" "Search")
    ("/getting-started" "Get started")
    ("/solutions" "Common problems and solutions")
    ("/build-status" "Build status")
    ("/help" "Help")))

(define (page-partial-main-menu request-path)
  `(ul
    ,(map
      (lambda (item)
        (cond
         ((string= (substring (car item) 1) (car (string-split (substring request-path 1) #\/)))
          `(li (@ (class "active")) ,(cadr item)))
         ((and (string= "package" (car (string-split (substring request-path 1) #\/)))
               (string= (car item) "/"))
          `(li (@ (class "active"))
               (a (@ (href "/")
                     (onclick "history.go(-1); return false;"))
                  "← back to search")))
         (else
          `(li (a (@ (href ,(car item))) ,(cadr item))))))
      pages)))

(define* (page-root-template title request-path content-tree #:key (dependencies '(test)))
  `((html (@ (lang "en"))
     (head
      (title ,(string-append (if (defined? '%site-title-prefix)
                                 %site-title-prefix
                                 "hpcguix | ")
                             title))
      (meta (@ (http-equiv "Content-Type") (content "text/html; charset=utf-8")))
      (link (@ (rel "icon")
               (type "image/x-icon")
               (href "/static/images/favicon.png")))
      ,(if (memq 'highlight dependencies)
         `((link (@ (rel "stylesheet") (href "/static/highlight/styles/github.css")))
           (script (@ (src "/static/highlight/highlight.min.js")) "")
           (script "hljs.initHighlightingOnLoad();"))
         `())
      ,(if (memq 'datatables dependencies)
           `((link (@ (rel "stylesheet") (type "text/css") (href "/static/datatables.min.css")))
             (script (@ (type "text/javascript") (src "/static/jquery-2.2.4.min.js")) "")
             (script (@ (type "text/javascript") (src "/static/datatables.min.js")) ""))
           `())
      (link (@ (rel "stylesheet")
               (href "/static/css/main.css")
               (type "text/css")
               (media "screen"))))
     (body
      (div (@ (id "header"))
           (div (@ (id "header-inner")
                   (class "width-control"))
                (a (@ (href "/"))
                   (img (@ (class "logo")
                           (src "/static/images/logo-small.png")
                           (alt "GuixHPC"))))
                (div (@ (class "baseline"))
                     "Reproducible software deployment for high-performance computing.")))
      (div (@ (id "menubar")
              (class "width-control"))
           ,(page-partial-main-menu request-path))
      (div (@ (id "content")
              (class "width-control"))
           ,content-tree)
      (div (@ (id "footer-box")
              (class "width-control"))
           (p "Made with λ by the GNU Guix community — Copyright © 2017. "
              (a (@ (href "https://github.com/UMCUGenetics/hpcguix-web"))
                 "Download the source code of this page") ".")
           (p (a (@ (href "/javascript")
             (data-jslicense "1"))
                 "JavaScript license information")))))))
