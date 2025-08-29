;;; hpcguix-web - Web interface for Guix
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2023, 2025 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of hpcguix-web.
;;;
;;; hpcguix-web is free software; see COPYING file for details.

;;; Run the following command to enter a development environment for
;;; hpcguix-web:
;;;
;;;   guix shell -CPNW
;;;
;;; You can also build hpcguix-web itself with:
;;;
;;;   guix build -f guix.scm

(use-modules ((guix licenses) #:prefix license:)
             (guix packages)
             (guix download)
             (guix utils)
             (guix gexp)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages lisp-xyz)
             (gnu packages nss)
             (gnu packages package-management)
             (gnu packages pkg-config))

(define %version
  (symbol->string (with-input-from-file "VERSION" read)))

(define vcs-file?
  ;; Return true if the given file is under version control.
  (or (git-predicate (current-source-directory))
      (const #t)))                                ;not in a Git checkout

(package
  (name "hpcguix-web")
  (version %version)
  (source (local-file "." "hpcguix-web-checkout"
                      #:recursive? #t
                      #:select? vcs-file?))
  (build-system gnu-build-system)
  (native-inputs
   (list autoconf
         automake
         uglify-js
         pkg-config

         ;; NSS certificates are only needed when running hpcguix-web in
         ;; development environments so it can check out Git repository over
         ;; HTTPS.
         nss-certs))
  (inputs
   (list guix
         (lookup-package-input guix "guile")
         guile-json-4
         guile-commonmark
         guile-syntax-highlight))
  (home-page "https://github.com/UMCUGenetics/hpcguix-web")
  (synopsis "Web interface for cluster deployments of Guix")
  (description "This package provides a web interface to the list of
packages provided by Guix.  The list of packages is searchable and
provides instructions on how to use Guix in a shared HPC
environment.")
  (license license:agpl3+))
