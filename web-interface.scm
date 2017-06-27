(define-module (web-interface)
  #:use-module (web server)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (json)
  #:use-module (www util)
  #:use-module (www config)
  #:use-module (www pages)
  #:use-module (www pages error)
  #:use-module (www pages package)
  #:use-module (www pages welcome)

  #:export (run-web-interface))

(define %package-blacklist
  '("cataclysm-dda"                "freedoom"          "gnubg"
    "gnubik"                       "gnushogi"          "prboom-plus"
    "retux"                        "xshogi"            "abbaye"
    "angband"                      "pingus"            "talkfilters"
    "cmatrix"                      "chess"             "freedink-engine"
    "freedink-data"                "freedink"          "xboard"
    "xboing"                       "gtypist"           "irrlicht"
    "mars"                         "minetest-data"     "minetest"
    "glkterm"                      "glulxe"            "fizmo"
    "retroarch"                    "gnugo"             "extremetuxracer"
    "supertuxkart"                 "gnujump"           "wesnoth"
    "dosbox"                       "gamine"            "raincat"
    "manaplus"                     "mupen64plus-core"
    "mupen64plus-audio-sdl"        "mupen64plus-input-sdl"
    "mupen64plus-rsp-hle"          "mupen64plus-rsp-z64"
    "mupen64plus-video-arachnoid"  "mupen64plus-video-glide64"
    "mupen64plus-video-glide64mk2" "mupen64plus-video-rice"
    "mupen64plus-video-z64"        "mupen64plus-ui-console"
    "nestopia-ue"                  "emulation-station" "openttd-engine"
    "openttd-opengfx"              "openttd"           "pinball"
    "pioneers"                     "desmume"           "einstein"
    "powwow"                       "red-eclipse"       "higan"
    "grue-hunter"                  "lierolibre"        "warzone2100"
    "starfighter"                  "chromium-bsu"      "tuxpaint"
    "tuxpaint-stamps"              "tuxpaint-config"   "supertux"
    "tintin++"                     "laby"              "bambam"
    "mrrescue"                     "hyperrogue"        "kobodeluxe"
    "freeciv"                      "no-more-secrets"   "megaglest-data"
    "megaglest"                    "freegish"          "cdogs-sdl"
    "kiki"                         "teeworlds"         "enigma"
    "fillets-ng"                   "crawl"             "crawl-tiles"
    "lugaru"                       "0ad-data"          "0ad"
    "open-adventure"               "aisleriot"))

;; ----------------------------------------------------------------------------
;; HANDLERS
;; ----------------------------------------------------------------------------
;;
;; The way a request is handled varies upon the nature of the request.  It can
;; be as simple as serving a pre-existing file, or as complex as finding a
;; Scheme module to use for handling the request.
;;
;; In this section, the different handlers are implemented.
;;

(define (request-packages-json-handler)
  (let ((all-packages (fold-packages cons '()))
        (package->json (lambda (package)
                         (json (object
                                ("name"     ,(package-name package))
                                ("version"  ,(package-version package))
                                ("synopsis" ,(package-synopsis package))
                                ("homepage" ,(package-home-page package)))))))
    (values '((content-type . (application/javascript)))
            (with-output-to-string
              (lambda _
                (scm->json
                 (map package->json
                      (lset-difference
                       (lambda (a b)
                         (string= (package-name a) b))
                       all-packages %package-blacklist))))))))

(define (request-file-handler path)
  "This handler takes data from a file and sends that as a response."

  (define (response-content-type path)
    "This function returns the content type of a file based on its extension."
    (let ((extension (substring path (1+ (string-rindex path #\.)))))
      (cond [(string= extension "css")  '(text/css)]
            [(string= extension "js")   '(application/javascript)]
            [(string= extension "json") '(application/javascript)]
            [(string= extension "html") '(text/html)]
            [(string= extension "png")  '(image/png)]
            [(string= extension "svg")  '(image/svg+xml)]
            [(string= extension "ico")  '(image/x-icon)]
            [(string= extension "pdf")  '(application/pdf)]
            [(string= extension "ttf")  '(application/font-sfnt)]
            [(#t '(text/plain))])))

  (let ((full-path (string-append %www-root "/" path)))
    (if (not (file-exists? full-path))
        (values '((content-type . (text/html)))
                (with-output-to-string (lambda _ (sxml->xml (page-error-404 path)))))
        ;; Do not handle files larger than %maximum-file-size.
        ;; Please increase the file size if your server can handle it.
        (let ((file-stat (stat full-path)))
          (if (> (stat:size file-stat) %www-max-file-size)
              (values '((content-type . (text/html)))
                      (with-output-to-string 
                        (lambda _ (sxml->xml (page-error-filesize path)))))
              (values `((content-type . ,(response-content-type full-path)))
                      (with-input-from-file full-path
                        (lambda _
                          (get-bytevector-all (current-input-port))))))))))

(define (request-package-handler request-path)
  (values '((content-type . (text/html)))
          (call-with-output-string
            (lambda (port)
              (sxml->xml (page-package request-path) port)))))

(define (request-scheme-page-handler request request-body request-path)

  (define (module-path prefix elements)
    "Returns the module path so it can be loaded."
    (if (> (length elements) 1)
        (module-path
         (append prefix (list (string->symbol (car elements))))
         (cdr elements))
        (append prefix (list (string->symbol (car elements))))))
  (values '((content-type . (text/html)))
          (call-with-output-string
            (lambda (port)
              (set-port-encoding! port "utf8")
              (format port "<!DOCTYPE html>~%")
              (if (< (string-length request-path) 2)
                  (sxml->xml (page-welcome "/") port)
                  (let* ((function-symbol (string->symbol
                                           (string-map
                                            (lambda (x)
                                              (if (eq? x #\/) #\- x))
                                            (substring request-path 1))))
                         (module (resolve-module
                                  (module-path
                                   '(www pages)
                                   (string-split (substring request-path 1) #\/))
                                  #:ensure #f))
                         (page-symbol (symbol-append 'page- function-symbol)))
                    (if module
                        (let ((display-function
                               (module-ref module page-symbol)))
                          (if (eq? (request-method request) 'POST)
                              (sxml->xml (display-function
                                          request-path
                                          #:post-data
                                          (utf8->string
                                           request-body)) port)
                              (sxml->xml (display-function request-path) port)))
                        (sxml->xml (page-error-404 request-path) port))))))))


;; ----------------------------------------------------------------------------
;; ROUTING & HANDLERS
;; ----------------------------------------------------------------------------
;;
;; Requests can have different handlers.
;; * Static objects (images, stylesheet, javascript files) have their own
;;   handler.
;; * Package pages are generated dynamically, so they have their own handler.
;; * The 'regular' Scheme pages have their own handler that resolves the
;;   module dynamically.
;;
;; Feel free to add your own handler whenever that is necessary.
;;
;; ----------------------------------------------------------------------------
(define (request-handler request request-body)
  (let ((request-path (uri-path (request-uri request))))
    (format #t "~a ~a~%" (request-method request) request-path)
    (cond
     ((string= request-path "/packages.json")
      (request-packages-json-handler))
     ((and (> (string-length request-path) 7)
           (string= (string-take request-path 8) "/static/"))
      (request-file-handler request-path))
     ((and (> (string-length request-path) 8)
           (string= (string-take request-path 9) "/package/"))
      (request-package-handler request-path))
     (else
      (request-scheme-page-handler request request-body request-path)))))


;; ----------------------------------------------------------------------------
;; RUNNER
;; ----------------------------------------------------------------------------
;;
;; This code runs the web server.

(define (run-web-interface)
  (run-server request-handler 'http
              `(#:port ,%www-listen-port
                #:addr ,INADDR_ANY)))

(run-web-interface)
