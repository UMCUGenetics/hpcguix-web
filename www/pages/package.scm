;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
;;; Copyright © 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix utils)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:export (page-package))

(define (read-at-location file line-number)
  "Call 'read' at LINE-NUMBER in FILE and return its result."
  (call-with-input-file file
    (lambda (port)
      (let loop ((line 0))
        (cond ((= line (- line-number 1))
               (read port))
              ((>= line line-number)
               #f)
              (else
               (begin
                 (read-line port)
                 (loop (+ 1 line)))))))))

(define (package->variable-name package)
  "Return the name of the variable that defines PACKAGE, a package object,
or #f if we failed to find it."
  (define path
    (match (inferior-eval '(%package-module-path)
                          (atomic-box-ref current-inferior))
      ((lst ...)
       (map (match-lambda
              ((? string? str) str)
              ((directory . sub-directory) directory))
            lst))
      (_ '())))

  ;; XXX: There are many cases where this doesn't work, such as computed
  ;; packages, or simply packages where the 'package' form is not strictly on
  ;; the line that follows 'define'.
  (match (inferior-package-location package)
    ((? location? location)
     (let ((file (search-path path (location-file location))))
       (and file
            (match (read-at-location file (- (location-line location) 1))
              (((or 'define 'define-public) (? symbol? name) _ ...)
               name)
              (_
               #f)))))
    (#f #f)))

(define (package-description-shtml package)
  "Return an SXML representation of PACKAGE description field with HTML
vocabulary."
  ;; 'texi-fragment->stexi' uses 'call-with-input-string', so make sure
  ;; those string ports are Unicode-capable.
  (with-fluids ((%default-port-encoding "UTF-8"))
    (and=> (inferior-package-description package)
           (compose stexi->shtml texi-fragment->stexi))))

(define %not-slash
  (char-set-complement (char-set #\/)))

(define (page-package request-path site-config)
  (match (string-tokenize request-path %not-slash)
    (("package" name)
     (let ((packages (vhash-fold* cons '() name
                                  (atomic-box-ref current-packages))))
       (if (null? packages)
           (page-root-template "Oops!" request-path site-config
                               `((h2 "Uh-oh...")
                                 (p "The package is gone!")))
           (page-root-template
            (string-append "Details for " name) request-path
            site-config
            `((h2 "Package details of " (code (@ (class "h2-title")) ,name))
              (p ,(package-description-shtml (car packages)))
              (p "There " ,(if (> (length packages) 1) "are " "is ")
                 ,(length packages) " version"
                 ,(if (> (length packages) 1) "s" "")
                 " available for this package.")
              (hr)

              ,(map
                (lambda (instance)
                  (let ((location (inferior-package-location instance)))
                    `((table (@ (style "width: 100%"))
                             (tr
                              (td (strong "Version"))
                              (td ,(inferior-package-version instance)))
                             (tr
                              (td (strong "Defined at"))
                              (td (code (@ (class "nobg"))
                                        ,(string-append (location-file location) ":"
                                                        (number->string
                                                         (location-line location))))))
                             (tr
                              (td (strong "Symbol name"))
                              (td (code (@ (class "nobg"))
                                        ,(match (package->variable-name
                                                 instance)
                                           (#f "?")
                                           (sym (symbol->string sym))))))
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
                                                   ""))))))
                             (tr
                              (td (strong "Homepage"))
                              (td (a (@ (href ,(inferior-package-home-page
                                                instance)))
                                     ,(inferior-package-home-page instance)))))
                      (hr))))
                packages)
              ,(if (not (null? site-config))
                   (let ((func (hpcweb-configuration-package-page-extension-proc site-config)))
                     (func request-path))
                   ""))
            #:dependencies '(highlight)))))
    (_                                            ;invalid URI path
     (page-error-404 request-path site-config))))
