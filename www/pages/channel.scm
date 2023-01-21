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

(define-module (www pages channel)
  #:use-module (hpcweb-configuration)
  #:use-module (www pages)
  #:autoload   (www pages error) (page-error-404)
  #:autoload   (www packages) (channel-home-page-url)
  #:use-module (guix channels)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (web uri)
  #:export (page-channel))

(define (manual-url page)
  "Return the complete URL to PAGE in the Guix reference manual."
  (string-append "https://guix.gnu.org/manual/en/html_node/"
                 page ".html"))

(define (channel-description-shtml channel)
  `(p ,@(if (guix-channel? channel)
            `("The " (code "guix") " channel is the main Guix channel, "
              "providing many packages but also the core Guix modules "
              "and commands.  It is provided by default and "
              (a (@ (href ,(manual-url "Channels")))
                 "defined")
              " as follows:")
            `("The " (code ,(channel-name channel)) " channel can be obtained "
              "by "
              (a (@ (href ,(manual-url "Specifying-Additional-Channels")))
                 "adding the following definition")
              " to " (code "~/.config/guix/channels.scm") " and then running "
              (a (@ (href ,(manual-url "Invoking-guix-pull")))
                 (code "guix pull"))
              ":"))

      (pre (code (@ (class "scheme"))
                 ,(call-with-output-string
                    (lambda (port)
                      (pretty-print (channel->code channel) port)))))

      "You can also " (a (@ (href ,(channel-home-page-url channel)))
                         "browse its source repository")
      ".  "

      ,@(if (guix-channel? channel)
            '()
            `("Additional settings may be needed to obtain "
              (a (@ (href ,(manual-url
                            "Getting-Substitutes-from-Other-Servers")))
                 "substitutes")
              " (pre-built binaries) for packages in that channel—check "
              "out the channel’s documentation."))))

(define %not-slash
  (char-set-complement (char-set #\/)))

(define (page-channel request-path config)
  (match (string-tokenize request-path %not-slash)
    (("channel" (= string->symbol name))
     (let ((channel (find (lambda (channel)
                            (eq? (channel-name channel) name))
                          (hpcweb-configuration-channels config))))
       (if channel
           (page-root-template
            (string-append "Channel " (symbol->string name))
            request-path config
            `((h2 "The " (code (@ (class "h2-title")) ,name)
                  " channel")
              ,(channel-description-shtml channel)))
           (page-error-404 request-path config))))
    (_                                            ;invalid URI path
     (page-error-404 request-path config))))