;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
;;; Copyright © 2017-2019, 2021, 2023-2025 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (srfi srfi-34)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 match)
  #:autoload   (ice-9 binary-ports) (get-bytevector-all)
  #:autoload   (ice-9 textual-ports) (get-string-all)
  #:autoload   (guix base16) (bytevector->base16-string)
  #:autoload   (guix base32) (nix-base32-string->bytevector)
  #:autoload   (guix http-client) (http-fetch/cached
                                   http-get-error?
                                   http-get-error-uri
                                   http-get-error-code
                                   http-get-error-reason)
  #:autoload   (gnutls) (error->string)
  #:use-module (web uri)
  #:use-module ((web request) #:select (request-uri))
  #:use-module ((web response) #:select (build-response))
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:autoload   (zlib) (call-with-gzip-input-port)
  #:export (page-package
            request-package-source-badge-handler))

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
                       "#L" (number->string (location-line location)))))
        (forgejo-url (lambda (repository-url commit location)
                       (string-append
                        (if (string-suffix? ".git" repository-url)
                            (string-drop-right repository-url 4)
                            repository-url)
                        "/src/commit/" commit "/" (location-file location)
                        "#L" (number->string (location-line location))))))
    `(("git.savannah.gnu.org"
       ,(lambda (repository-url commit location)
          (string-append (string-replace-substring repository-url
                                                   "/git/" "/cgit/")
                         "/tree/" (location-file location) "?id=" commit
                         "#n" (number->string (location-line location)))))
      ("notabug.org" ,labhub-url)
      ("framagit.org" ,labhub-url)
      ("codeberg.org" ,forgejo-url)
      ("git.guix.gnu.org" ,forgejo-url)
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
  (define alt-text
    "Source code archival status at Software Heritage.")

  (define (swh-revision-badge ref)
    `(let ((commit (git-reference-commit ,ref))
           (alt-text ,alt-text))
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
                         ;; XXX: '&release=' only works for Git annotated tags.
                         ;; For "plain" tags, '&branch=refs/tags/NAME' works.
                         "&release=" (uri-encode commit))))
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

  (define (disarchive+swh-badge hash)
    `(let ((algo+hash
            (string-append (symbol->string
                            (content-hash-algorithm ,hash))
                           "/"
                           (bytevector->nix-base32-string
                            (content-hash-value ,hash)))))
       `(a (@ (href ,(string-append "/package/source/" algo+hash
                                    "/browse")))
           (img (@ (src ,(string-append "/package/source/" algo+hash
                                        "/badge"))
                   (alt ,,alt-text))))))

  (inferior-package-field package
                          `(lambda (package)
                             (let ((source (package-source package)))
                               (match (and (origin? source) (origin-uri source))
                                 ((? git-reference? ref)
                                  ,(swh-revision-badge 'ref))
                                 ((? string?)
                                  ,(disarchive+swh-badge
                                    '(origin-hash source)))
                                 (((? string?) ...)
                                  ,(disarchive+swh-badge
                                    '(origin-hash source)))
                                 (_
                                  "—"))))))

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

(define (disarchive-sexp->swhid sexp)
  "Return the SWHID of the directory referenced by SEXP, a Disarchive sexp.
Return #f if the SWHID could not be extracted."
  ;; XXX: It would be cleaner to use Disarchive's interface for that, but in
  ;; practice it seems to be good enough.
  (match sexp
    (('disarchive ('version 0)
                  (_  ;(or 'gzip-member 'xz-file)
                   _ ...
                   ('input
                    ('tarball
                     _ ...
                     ('input ('directory-ref
                              ('version 0)
                              ('name _)
                              ('addresses ('swhid swhid) _ ...)
                              _ ...))))))
     swhid)
    (_ #f)))

(define exception-with-kind-and-args?
  (exception-predicate &exception-with-kind-and-args))

(define (gnutls-error? c)
  (and (exception-with-kind-and-args? c)
       (eq? (exception-kind c) 'gnutls-error)))

(define %disarchive-base-url
  "https://disarchive.guix.gnu.org")

(define (tarball-sha256->swhid sha256)
  "Return the SWHID corresponding to SHA256, the hash of a tarball as a
bytevector.  This works by looking up SHA256 on disarchive.guix.gnu.org."
  (define (write-swhid-to-cache input output)
    (let ((swhid (disarchive-sexp->swhid (read input))))
      (when swhid
        (display swhid output))))

  (guard (c ((http-get-error? c)
             (format (current-error-port) "GET ~s returned ~a (~s)~%"
                     (uri->string (http-get-error-uri c))
                     (http-get-error-code c)
                     (http-get-error-reason c))
             #f)
            ((gnutls-error? c)
             (format (current-error-port) "TLS error while talking to ~a: ~a~%"
                     %disarchive-base-url
                     (error->string (car (exception-args c))))
             #f)
            (else
             (format (current-error-port) "error while talking to ~a: ~s~%"
                     %disarchive-base-url
                     c)
             #f))
    (let* ((url   (string-append %disarchive-base-url "/sha256/"
                                 (bytevector->base16-string sha256)))
           (port  (http-fetch/cached (string->uri url)
                                     #:timeout 3
                                     #:write-cache write-swhid-to-cache))
           (swhid (get-string-all port)))
      (close-port port)
      (and (string-prefix? "swh:" swhid) swhid))))

(define (request-package-source-badge-handler config request)
  "Handle REQUEST, a GET request under /package/source.  Return two values:
the response and its body."
  (define (redirect url)
    (let ((uri (string->uri url)))
      (values (build-response #:code 301
                              #:headers `((location . ,uri)))
              "Redirecting to Software Heritage.")))

  (match (string-tokenize (uri-path (request-uri request))
                          %not-slash)
    (("package" "source" "sha256" (= nix-base32-string->bytevector hash)
      "badge")
     (match (tarball-sha256->swhid hash)
       (#f
        ;; XXX: Would be nice to redirect to /badge/content/HASH but
        ;; apparently that only works for SHA1 Git hashes.
        (values (build-response #:code 200
                                #:headers '((content-type
                                             . (image/svg+xml))))
                (call-with-input-file (string-append %www-static-root
                                                     "/images/not-archived.svg")
                  get-bytevector-all)))
       (swhid
        (redirect (string-append
                   "https://archive.softwareheritage.org/badge/"
                   swhid)))))
    (("package" "source" "sha256" (= nix-base32-string->bytevector hash)
      "browse")
     (match (tarball-sha256->swhid hash)
       (#f
        (respond-404 config request))
       (swhid
        (redirect (string-append
                   "https://archive.softwareheritage.org/browse/"
                   swhid)))))
    (_
     (respond-404 config request))))

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
            (string-append "Package " name) request-path
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
                            (td (span (@ (class "package-field")) "Package"))
                            (td (b ,(inferior-package-name instance) " "
                                   ,(inferior-package-version instance))
                                ,@(if (and=> channel guix-channel?)
                                      `(" (" ,(version-history-link name) ")")
                                      '())))
                           ,@(if channel
                                 `((tr (td (span (@ (class "package-field")) "Channel"))
                                       (td (a (@ (href
                                                  ,(string-append
                                                    "/channel/"
                                                    (symbol->string
                                                     (channel-name channel)))))
                                              ,(channel-name channel)))))
                                 '())
                           (tr
                            (td (span (@ (class "package-field")) "Definition"))
                            (td ,(inferior-package-location-shtml instance)))

                           ,@(let ((url (and build-status-url
                                             (build-status-url
                                              (inferior-package-name
                                               instance)
                                              (inferior-package-version
                                               instance)))))
                               (if url
                                   `((tr
                                      (td (span (@ (class "package-field")) "Build status"))
                                      (td (a (@ (href ,url))
                                             "view 🚧"))))
                                   '()))
                           (tr
                            (td (span (@ (class "package-field")) "Home page"))
                            (td (a (@ (href ,(inferior-package-home-page
                                              instance)))
                                   ,(inferior-package-home-page instance))))
                           (tr
                            (td (span (@ (class "package-field")) "Source"))
                            (td ,(inferior-package-archival-shtml instance)))
                           (tr
                            (td (@ (style "width: 150pt")) (span (@ (class "package-field")) "Installation command"))
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
