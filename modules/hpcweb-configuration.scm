(define-module (hpcweb-configuration)
  #:use-module (guix records)
  #:use-module (guix channels)
  #:export (hpcweb-configuration
            hpcweb-configuration?
            hpcweb-configuration-title-prefix
            hpcweb-configuration-main-page
            hpcweb-configuration-guix-command
            hpcweb-configuration-package-filter-proc
            hpcweb-configuration-package-page-extension-proc
            hpcweb-configuration-menu
            hpcweb-configuration-channels
            hpcweb-configuration-package-list-expiration))

(define-record-type* <hpcweb-configuration>
  hpcweb-configuration make-hpcweb-configuration
  hpcweb-configuration?

  (title-prefix                 hpcweb-configuration-title-prefix
                                (default "hpcguix | "))

  ;; Path of the main page/welcome page.
  (main-page                    hpcweb-configuration-main-page
                                (default "/"))

  (guix-command                 hpcweb-configuration-guix-command
                                (default "guix"))
  (package-filter-proc          hpcweb-configuration-package-filter-proc
                                (default (const #t)))
  (package-page-extension-proc  hpcweb-configuration-package-page-extension-proc
                                (default (const '())))
  (menu                         hpcweb-configuration-menu
                                (default '()))
  (channels                     hpcweb-configuration-channels
                                (default %default-channels))
  (package-list-expiration      hpcweb-configuration-package-list-expiration
                                (default (* 12 3600))))
