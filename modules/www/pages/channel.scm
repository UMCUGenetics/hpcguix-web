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
  #:autoload   (www util) (manual-url)
  #:autoload   (www pages error) (page-error-404)
  #:autoload   (www packages) (channel-home-page-url)
  #:use-module (guix channels)
  #:autoload   (syntax-highlight) (highlights->sxml highlight)
  #:autoload   (syntax-highlight scheme) (make-scheme-lexer
                                          %default-special-prefixes
                                          %default-special-symbols)
  #:autoload   (texinfo) (texi-fragment->stexi)
  #:autoload   (texinfo html) (stexi->shtml)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (web uri)
  #:export (page-channel))

(define %scheme-lexer
  (delay (make-scheme-lexer (cons "channel" %default-special-symbols)
                            %default-special-prefixes)))

(define (scheme->sxml code)
  "Take the sexp CODE, format it, syntax-highlight it, and return the result as
SXML."
  (let ((str (call-with-output-string
               (lambda (port)
                 (pretty-print code port)))))
    (highlights->sxml
     (highlight (force %scheme-lexer) str))))

(define (channel-description-shtml channel config)
  (define main
    (hpcweb-configuration-main-page config))

  (define description
    (find (lambda (description)
            (eq? (channel-description-name description)
                 (channel-name channel)))
          (hpcweb-configuration-channel-descriptions config)))

  (define (link . args)
    (let ((home (or (and description
                         (channel-description-home-page description))
                    (channel-url channel))))
      `(a (@ (href ,home)) ,@args)))

  `(p ,@(if (guix-channel? channel)
            `("The " (code "guix") " channel is the main Guix channel, "
              "providing "
              (a (@ (href ,(string-append main "?q=channel:guix")))
                 "many packages")
              " but also the core Guix modules "
              "and commands.  It is provided by default and "
              (a (@ (href ,(manual-url "Channels")))
                 "defined")
              " as follows:"

              (pre (code (@ (class "scheme"))
                         ,(scheme->sxml (channel->code channel)))))
            `((h3 ,(or (and description
                            (and=> (channel-description-synopsis description)
                                   (compose stexi->shtml
                                            texi-fragment->stexi)))
                       ""))

              ("The "
               ,(link `(code ,(channel-name channel)) " channel")
               " provides "
               (a (@ (href ,(string-append main "?q=channel:"
                                           (symbol->string
                                            (channel-name channel)))))
                  "additional packages") ".  "
               "It can be obtained "
               "by writing a "
               (a (@ (href ,(manual-url "Specifying-Additional-Channels")))
                  "snippet")
               " along these lines"
               " to " (code "~/.config/guix/channels.scm") " and then running "
               (a (@ (href ,(manual-url "Invoking-guix-pull")))
                  (code "guix pull"))
               ":"

               (pre (code (@ (class "scheme"))
                          ,(scheme->sxml
                            `(append (list ,(channel->code channel))
                                     %default-channels)))))))

      (p "You can also "
         (a (@ (href ,(channel-home-page-url channel)))
            "browse its source repository")
         ".  ")

      ,(if (guix-channel? channel)
           ""
           (match (if description
                      (channel-description-substitutes description)
                      '())
             (()
              `(p "Additional settings may be needed to obtain "
                  (a (@ (href ,(manual-url
                                "Getting-Substitutes-from-Other-Servers")))
                     "substitutes")
                  " (pre-built binaries) for packages in that channel—check "
                  "out the channel’s documentation."))
             (((urls . keys) ...)
              `(p "To obtain " (em "substitutes") " (pre-built binaries) "
                  "for the packages provided by this channel, "
                  (a (@ (href ,(manual-url
                                "Getting-Substitutes-from-Other-Servers")))
                     "configure your system")
                  " to fetch substitutes from the URLs below "
                  "and authorize the corresponding keys:"
                  (table
                   ,@(map (lambda (url key)
                            `(tr (td (code ,url))
                                 (td (pre
                                      (code (@ (class "scheme"))
                                            ,(highlights->sxml
                                              (highlight (force %scheme-lexer)
                                                         key)))))))
                          urls keys))))))

      ,(if (and description
                (channel-description-ci-url description))
           `(p "Check out this channel's "
               (a (@ (href ,(channel-description-ci-url description)))
                  "continuous integration status")
               ".")
           "")))

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
              ,(channel-description-shtml channel config)))
           (page-error-404 request-path config))))
    (_                                            ;invalid URI path
     (page-error-404 request-path config))))
