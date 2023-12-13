(define-module (hpcweb-configuration)
  #:use-module (guix records)
  #:use-module (guix channels)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (hpcweb-configuration
            hpcweb-configuration?
            hpcweb-configuration-title-prefix
            hpcweb-configuration-main-page
            hpcweb-configuration-guix-command
            hpcweb-configuration-package-filter-proc
            hpcweb-configuration-package-page-extension-proc
            hpcweb-configuration-menu
            hpcweb-configuration-channels
            hpcweb-configuration-channel-descriptions
            hpcweb-configuration-package-list-expiration

            channel-description
            channel-description?
            channel-description-name
            channel-description-synopsis
            channel-description-home-page
            channel-description-logo-url
            channel-description-ci-url
            channel-description-ci-badge
            channel-description-substitutes
            channel-description-license

            %guix-channel-description))

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
  (channel-descriptions         hpcweb-configuration-channel-descriptions
                                (default (list %guix-channel-description)))
  (package-list-expiration      hpcweb-configuration-package-list-expiration
                                (default (* 12 3600))))

(define-record-type* <channel-description>
  channel-description make-channel-description
  channel-description?
  (name           channel-description-name)
  (synopsis       channel-description-synopsis)
  (home-page      channel-description-home-page
                  (default #f))
  (logo-url       channel-description-logo-url
                  (default "/static/images/favicon.png"))
  (ci-badge       channel-description-ci-badge
                  (default #f))
  (ci-url         channel-description-ci-url
                  (default #f))
  (substitutes    channel-description-substitutes
                  (default '()))                  ;list of URL/key pairs
  (license        channel-description-license
                  (default license:gpl3+)))

(define %guix-channel-description
  ;; Description of the official 'guix' channel.
  (channel-description
   (name 'guix)
   (synopsis "Main channel, providing Guix itself and a large collection of packages")
   (home-page "https://guix.gnu.org")
   (ci-url "https://ci.guix.gnu.org/eval/latest/dashboard?spec=master")
   (ci-badge "https://ci.guix.gnu.org/jobset/master/badge.svg")
   (substitutes
    `(("https://ci.guix.gnu.org" . "\
(public-key
 (ecc
  (curve Ed25519)
  (q #8D156F295D24B0D9A86FA5741A840FF2D24F60F7B6C4134814AD55625971B394#)
  )
 )")
      ("https://bordeaux.guix.gnu.org" . "\
(public-key
 (ecc
  (curve Ed25519)
  (q #7D602902D3A2DBB83F8A0FB98602A754C5493B0B778C8D1DD4E0F41DE14DE34F#)
  )
 )")))))
