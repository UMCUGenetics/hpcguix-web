;;; Copyright © 2017-2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <contact@parouby.fr>
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

(define-module (www packages)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (guix ui)
  #:use-module ((guix utils)
                #:select (location-file string-replace-substring))
  #:autoload   (guix build utils) (mkdir-p)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 atomic)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:use-module (sxml simple)
  #:use-module (web uri)
  #:use-module (json)
  #:use-module (zlib)
  #:export (current-packages
            inferior-package-channels
            channel-home-page-url
            maybe-update-package-file))

(define current-packages
  ;; Current package set as a vhash that maps package names to inferior
  ;; packages.
  (make-atomic-box vlist-null))

(define current-inferior
  ;; Current inferior.  It is kept around so that requests such as
  ;; 'inferior-package-description' that involve communication with the
  ;; inferior can succeed.
  (make-atomic-box #f))

(define (package-synopsis-shtml package)
  "Return an SXML representation of PACKAGE synopsis field with HTML
vocabulary."
  ;; 'texi-fragment->stexi' uses 'call-with-input-string', so make sure
  ;; those string ports are Unicode-capable.
  (with-fluids ((%default-port-encoding "UTF-8"))
    (and=> (inferior-package-synopsis package)
           (compose stexi->shtml texi-fragment->stexi))))

(define (inferior-package-channels package)
  "Return the list of channels PACKAGE, an inferior package, comes from."
  (map sexp->channel (inferior-package-provenance package)))

(define (channel-home-page-url channel)
  "Return the home page of CHANNEL."
  (let* ((url  (channel-url channel))
         (uri  (string->uri url))
         (host (and uri (uri-host uri))))
    (if (and host (string=? host "git.savannah.gnu.org")) ;the only exception
        (string-replace-substring url "/git/" "/cgit/")
        url)))

(define (channel->json channel)
  "Return identifying information about CHANNEL suitable for JSON
serialization."
  `((name   . ,(channel-name channel))
    (url    . ,(channel-home-page-url channel))))

(define (inferior-package->json package)
  "Return meta-data for PACKAGE as an alist that can be converted to JSON."
  `(("name"     ,@(inferior-package-name package))
    ("version"  ,@(inferior-package-version package))
    ("synopsis" ,@(call-with-output-string
                    (lambda (port)
                      (sxml->xml (match (package-synopsis-shtml package)
                                   (('div ('p text)) text)
                                   (tree tree))
                                 port))))
    ("channel"  ,@(and=> (match (inferior-package-channels package)
                           (() #f)
                           ((guix) guix)
                           (lst (find (negate guix-channel?) lst)))
                         channel->json))))

(define* (update-package-file file channels #:key (select? (const #t)))
  "Atomically update FILE with the a JSON representation of the latest set of
Guix packages."
  (let* ((inferior (inferior-for-channels channels))
         (packages (inferior-packages inferior))
         (pivot    (string-append file ".part")))
    (mkdir-p (dirname file))
    (call-with-output-file pivot
      (lambda (port)
        (call-with-gzip-output-port port
          (lambda (port)
            (set-port-encoding! port "UTF-8")
            (scm->json (list->vector
                        (filter-map (lambda (package)
                                      (and (select? package)
                                           (inferior-package->json package)))
                                    packages))
                       port))
          #:level 9
          #:buffer-size (expt 2 17))))
    (rename-file pivot file)
    (atomic-box-set! current-packages
                     (fold (lambda (package table)
                             (vhash-cons (inferior-package-name package)
                                         package table))
                           vlist-null
                           packages))

    ;; Keep INFERIOR around so that operations on CURRENT-PACKAGES, such as
    ;; calls to 'inferior-package-description', succeed (there's a time
    ;; window during which this is out-of-sync compared to CURRENT-PACKAGES,
    ;; but it doesn't matter much.)
    (let ((previous (atomic-box-swap! current-inferior inferior)))
      (when previous
        (close-inferior previous)))))

(define* (maybe-update-package-file file channels
                                    #:key (select? (const #t))
                                    (expiration (* 3600 12)))
  "If FILE, a 'packages.json' meta-data file, does not exist, of it it's
older than EXPIRATION seconds, then spawn a new thread to update it and
return immediately."
  (define lock
    (string-append file ".lock"))

  (let ((st (stat file #f)))
    (when (or (not st)
              (> (- ((@ (guile) current-time)) (stat:mtime st))
                 expiration)
              (vlist-null? (atomic-box-ref current-packages)))
      (let ((lock-port
             (catch 'system-error
               (lambda ()
                 (open lock (logior O_CREAT O_WRONLY O_EXCL)))
               (lambda args
                 (if (= EEXIST (system-error-errno args))
                     #f
                     (apply throw args))))))
        ;; If we have the lock, then go ahead; otherwise do nothing.
        (when lock-port
          (format (current-error-port) "updating package cache '~a'...~%"
                  file)
          (call-with-new-thread
           (lambda ()
             (dynamic-wind
               (const #t)
               (lambda ()
                 (update-package-file file channels #:select? select?))
               (lambda ()
                 (close-port lock-port)
                 (delete-file lock))))))))))
