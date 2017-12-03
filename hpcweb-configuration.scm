(define-module (hpcweb-configuration)
  #:use-module (guix records)
  #:export (hpcweb-configuration
            hpcweb-configuration?
            hpcweb-configuration-title-prefix
            hpcweb-configuration-guix-command
            hpcweb-configuration-blacklist
            hpcweb-configuration-package-page-extension
            hpcweb-configuration-menu))

(define-record-type* <hpcweb-configuration>
  hpcweb-configuration make-hpcweb-configuration
  hpcweb-configuration?

  (title-prefix            hpcweb-configuration-title-prefix
                           (default "hpcguix | "))
  (guix-command            hpcweb-configuration-guix-command
                           (default "guix"))
  (blacklist               hpcweb-configuration-blacklist
                           (default '()))
  (package-page-extension  hpcweb-configuration-package-page-extension
                           (default '()))
  (menu                    hpcweb-configuration-menu
                           (default '())))
