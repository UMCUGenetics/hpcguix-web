;;; hpcguix-web - Web interface for Guix
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; This file is part of hpcguix-web.
;;;
;;; hpcguix-web is free software; see COPYING file for details.

;;; Run the following command to enter a development environment for
;;; HPC Guix web:
;;;
;;;  $ guix environment -l environment.scm

(use-modules ((guix licenses) #:prefix license:)
             (guix packages)
             (guix download)
             (guix utils)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages lisp-xyz)
             (gnu packages package-management)
             (gnu packages pkg-config))

(define %version
  (symbol->string (with-input-from-file "VERSION" read)))

(package
  (name "hpcguix-web")
  (version %version)
  (source (string-append (getcwd) "/hpcguix-web-" version ".tar.gz"))
  (build-system gnu-build-system)
  (arguments
   `(#:phases
     (modify-phases %standard-phases
       (add-before 'configure 'autoconf
         (lambda _
           (zero? (system* "autoreconf" "-vif")))))))
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("uglify-js" ,uglify-js)
     ("pkg-config" ,pkg-config)))
  (inputs
   `(("guix" ,guix)
     ("guile" ,@(assoc-ref (package-inputs guix) "guile"))
     ("guile-json" ,guile-json-4)
     ("guile-commonmark" ,guile-commonmark)
     ("guile-syntax-highlighting" ,guile-syntax-highlighting)))
  (home-page "https://github.com/UMCUGenetics/hpcguix-web")
  (synopsis "Web interface for cluster deployments of Guix")
  (description "This package provides a web interface to the list of
packages provided by Guix.  The list of packages is searchable and
provides instructions on how to use Guix in a shared HPC
environment.")
  (license license:agpl3+))
