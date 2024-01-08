;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
;;; Copyright © 2017-2019, 2021, 2023-2024 Ludovic Courtès <ludo@gnu.org>
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

(define-module (www pages package)
  #:use-module (hpcweb-configuration)
  #:use-module (www pages)
  #:use-module (www pages error)
  #:use-module (www packages)
  #:use-module (www config)
  #:use-module (guix memoization)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 match)
  #:use-module (web uri)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:export (page-package))

(define (package-blurb-shtml blurb)
  (lambda (package)
    "Return an SXML representation of PACKAGE description or synopsis with HTML
vocabulary."
    ;; 'texi-fragment->stexi' uses 'call-with-input-string', so make sure
    ;; those string ports are Unicode-capable.
    (with-fluids ((%default-port-encoding "UTF-8"))
      (and=> (blurb package)
             (compose stexi->shtml texi-fragment->stexi)))))

(define package-synopsis-shtml
  (package-blurb-shtml inferior-package-synopsis))
(define package-description-shtml
  (package-blurb-shtml inferior-package-description))

(define %vcs-web-views
  ;; Hard-coded list of host names and corresponding web view URL templates.
  (let ((labhub-url (lambda (repository-url commit location)
                      (string-append
                       (if (string-suffix? ".git" repository-url)
                           (string-drop-right repository-url 4)
                           repository-url)
                       "/blob/" commit "/" (location-file location)
                       "#L" (number->string (location-line location))))))
    `(("git.savannah.gnu.org"
       ,(lambda (repository-url commit location)
          (string-append (string-replace-substring repository-url
                                                   "/git/" "/cgit/")
                         "/tree/" (location-file location) "?id=" commit
                         "#n" (number->string (location-line location)))))
      ("notabug.org" ,labhub-url)
      ("framagit.org" ,labhub-url)
      ("gitlab.com" ,labhub-url)
      ("gitlab.inria.fr" ,labhub-url)
      ("github.com" ,labhub-url))))

(define* (channel-file-url channel location)
  "Return the URL to the web view of LOCATION in CHANNEL."
  (let* ((url  (channel-url channel))
         (uri  (string->uri url))
         (host (and uri (uri-host uri))))
    (and host
         (match (assoc host %vcs-web-views)
           (#f #f)
           ((_ template)
            (template url (channel-commit channel) location))))))

(define inferior-package-field
  (@@ (guix inferior) inferior-package-field))

(define (inferior-package-archival-shtml package)
  "Return SHTML representing the archival status of PACKAGE's source code or
at least its URL."
  (define (swh-revision-badge ref)
    `(let ((commit (git-reference-commit ,ref))
           (alt-text
            "Badge showing source code archival at Software Heritage."))
       (if (commit-id? commit)
           `(a (@ (href ,(string-append
                          "https://archive.softwareheritage.org/browse/revision/"
                          commit "/")))
               (img (@ (src
                        ,(string-append
                          "https://archive.softwareheritage.org/badge/revision/"
                          commit "/"))
                       (alt ,alt-text))))
           (let ((query (string-append
                         "?origin_url=" (uri-encode (git-reference-url ,ref))
                         "&branch=refs/tags/" (uri-encode commit))))
             `(a (@ (href ,(string-append
                            "https://archive.softwareheritage.org/browse/origin/"
                            query)))
                 ;; XXX: The badge is an approximation: it doesn't tell us
                 ;; whether this specific revision is archived.
                 (img (@ (src
                          ,(string-append
                            "https://archive.softwareheritage.org/badge/origin/"
                            (git-reference-url ,ref))) ;unencoded!
                         (alt ,alt-text))))))))

  (define (origin-url-link ref)
    ;; TODO: Check whether REF is on disarchive.guix.gnu.org and on SWH.
    `(let ((url (match ,ref
                  ((? string? url) url)
                  (((? string? url) _ ...) url)
                  (_ #f))))
       (if url
           `(tt ,url)
           "—")))

  (inferior-package-field package
                          `(lambda (package)
                             (let ((source (package-source package)))
                               (match (and (origin? source) (origin-uri source))
                                 ((? git-reference? ref)
                                  ,(swh-revision-badge 'ref))
                                 (ref
                                  ,(origin-url-link 'ref)))))))

(define (inferior-package-location-shtml package)
  "Return SHTML denoting the source code location of PACKAGE, an inferior
package."
  (match (inferior-package-location package)
    ((? location? location)
     (let* ((channel (inferior-package-primary-channel package))
            (url     (and channel (channel-file-url channel location)))
            (body    `(code (@ (class "nobg"))
                            ,(location-file location))))
       `(span
         (code (@ (class "nobg"))
               ,(if url
                    `(a (@ (href ,url)) ,body)
                    body)))))
    (#f
     "unknown location")))

(define %not-slash
  (char-set-complement (char-set #\/)))

(define (page-package request-path site-config)
  (define (version-history-link package)
    (let ((url (string-append
                "https://data.guix.gnu.org"
                "/repository/1/branch/master/package/"
                package)))
      `(a (@ (href ,url)) "history")))

  (match (string-tokenize request-path %not-slash)
    (("package" name)
     (let ((packages (sort (vhash-fold* cons '() name
                                        (atomic-box-ref current-packages))
                           (lambda (package1 package2)
                             (version>? (inferior-package-version package1)
                                        (inferior-package-version package2))))))
       (if (null? packages)
           (page-root-template "Oops!" request-path site-config
                               (if (vlist-null?
                                    (atomic-box-ref current-packages))
                                   `((h2 "Please be patient...")
                                     (p "Package data is being cooked and
will be ready soon!"))
                                   `((h2 "Uh-oh...")
                                     (p "The package is gone!"))))
           (page-root-template
            (string-append "Details for " name) request-path
            site-config
            `((h2 ,(package-synopsis-shtml (car packages)))
              (p ,(package-description-shtml (car packages)))
              ,@(match packages
                  ((one)
                   '())
                  ((one two ...)
                   `((p "There are " ,(length packages) " versions"
                        " available for this package."))))
              (hr)

              ,(map
                (lambda (instance)
                  (define channel
                    (inferior-package-primary-channel instance))

                  (define description
                    (and channel
                         (find (lambda (description)
                                 (eq? (channel-description-name description)
                                      (channel-name channel)))
                               (hpcweb-configuration-channel-descriptions
                                site-config))))

                  (define build-status-url
                    (and description
                         (channel-description-ci-package-url description)))

                  `((table (@ (style "width: 100%"))
                           (tr
                            (td (strong "Package"))
                            (td (b ,(inferior-package-name instance))))
                           (tr
                            (td (strong "Version"))
                            (td ,(inferior-package-version instance)
                                ,@(if (and=> channel guix-channel?)
                                      `(" (" ,(version-history-link name) ")")
                                      '())))
                           ,@(if channel
                                 `((tr (td (strong "Channel"))
                                       (td (a (@ (href
                                                  ,(string-append
                                                    "/channel/"
                                                    (symbol->string
                                                     (channel-name channel)))))
                                              ,(channel-name channel)))))
                                 '())
                           (tr
                            (td (strong "Definition"))
                            (td ,(inferior-package-location-shtml instance)))

                           ,@(let ((url (and build-status-url
                                             (build-status-url
                                              (inferior-package-name
                                               instance)
                                              (inferior-package-version
                                               instance)))))
                               (if url
                                   `((tr
                                      (td (strong "Build status"))
                                      (td (a (@ (href ,url))
                                             "view 🚧"))))
                                   '()))
                           (tr
                            (td (strong "Home page"))
                            (td (a (@ (href ,(inferior-package-home-page
                                              instance)))
                                   ,(inferior-package-home-page instance))))
                           (tr
                            (td (strong "Source"))
                            (td ,(inferior-package-archival-shtml instance)))
                           (tr
                            (td (@ (style "width: 150pt")) (strong "Installation command"))
                            (td (pre (code (@ (class "bash"))
                                           (string-append
                                            ,(if (not (null? site-config))
                                                 (hpcweb-configuration-guix-command site-config)
                                                 "guix")
                                            " install "
                                            ,name
                                            ,(if (> (length packages) 1)
                                                 (string-append
                                                  "@"
                                                  (inferior-package-version instance))
                                                 "")))))))
                    (hr)))
                packages)

              ,(if (not (null? site-config))
                   (let ((func (hpcweb-configuration-package-page-extension-proc site-config)))
                     (func request-path))
                   ""))
            #:dependencies '(highlight)))))
    (_                                            ;invalid URI path
     (page-error-404 request-path site-config))))
