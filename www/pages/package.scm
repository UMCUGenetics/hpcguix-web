;;; Copyright © 2016, 2017  Roel Janssen <roel@gnu.org>
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
  #:use-module (www pages)
  #:use-module (www config)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 rdelim)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:export (page-package))

(define (scheme-variable-name file line)

  (define (search-path* path file)
    "Like 'search-path' but exit if FILE is not found."
    (let ((absolute-file-name (search-path path file)))
      (unless absolute-file-name
        ;; Shouldn't happen unless somebody fiddled with the 'location' field.
        (leave (G_ "file '~a' not found in search path ~s~%")
               file path))
      absolute-file-name))
  
  (define (get-definition port current-line target-line)
    (let ((line (read-line port)))
      (if (< current-line target-line)
          (get-definition port (1+ current-line) target-line)
          (cadr (string-split line #\ )))))

  (let ((file (search-path* %load-path file)))
    (call-with-input-file file
      (lambda (port)
        (get-definition port 2 line)))))

(define (package-description-shtml package)
  "Return an SXML representation of PACKAGE description field with HTML
vocabulary."
  ;; 'texi-fragment->stexi' uses 'call-with-input-string', so make sure
  ;; those string ports are Unicode-capable.
  (with-fluids ((%default-port-encoding "UTF-8"))
    (and=> (package-description package)
           (compose stexi->shtml texi-fragment->stexi))))

(define (page-package request-path)
  (let* ((name (list-ref (string-split request-path #\/) 2))
         (packages (find-packages-by-name name)))
    (if (eqv? packages '())
        (page-root-template "Oops!" request-path
         `((h2 "Uh-oh...")
           (p "The package is gone!")))
        (page-root-template (string-append "Details for " name) request-path
         `((h2 "Package details of " (code (@ (class "h2-title")) ,name))
           (p ,(package-description-shtml (car packages)))
           (p "There " ,(if (> (length packages) 1) "are " "is ") ,(length packages) " version"
              ,(if (> (length packages) 1) "s" "") " available for this package.")
           (hr)
           ,(map (lambda (instance)
                   (let ((location (package-location instance)))
                     `((table (@ (style "width: 100%"))
                        (tr
                         (td (strong "Version"))
                         (td ,(package-version instance)))
                        (tr
                         (td (strong "Defined at"))
                         (td (code (@ (class "nobg"))
                                   ,(string-append (location-file location) ":"
                                                   (number->string
                                                    (location-line location))))))
                        (tr
                         (td (strong "Symbol name"))
                         (td (code (@ (class "nobg"))
                                   ,(scheme-variable-name
                                     (location-file location)
                                     (location-line location)))))
                        (tr
                         (td (@ (style "width: 150pt")) (strong "Installation command"))
                         (td (pre (code (@ (class "bash"))
                                        (string-append "guixr package -i "
                                                       ,name ,(if (> (length packages) 1)
                                                                  (string-append
                                                                   "@" (package-version instance)) ""))))))
                        (tr
                         (td (strong "Homepage"))
                         (td (a (@ (href ,(package-home-page instance))) ,(package-home-page instance)))))
                       (hr))))
                 packages)

           (h2 "After installation")
           (p "After running the installation command, your package has been "
              "installed into a profile.  By default, this is your “user” "
              "profile.  You can change the default by appending "
              (code "--profile=/path/to/profile") " to the installation command, "
              "where " (code "/path/to/profile") " can be any filesystem "
              "location.")

           (p "To use the newly installed program, the shell needs to know where "
              "to find the programs in your profile.  This is what we do when we "
              "“load a profile”.")

           (h2 "Using the package: Loading a profile")

           (p "To load your profile, run the following command: ")
           (pre (code (@ (class "bash")) "guixr load-profile ~/.guix-profile"))

           (p "You can go back to the state before loading the profile by running:")
           (pre (code (@ (class "bash")) "exit"))

           (p "If you used a non-default profile, append the filesystem path to "
              "the command, like so:")
           (pre (code (@ (class "bash")) "guixr load-profile /path/to/profile"))

           (p "")
           (h2 "When writing a job submission script")

           (p "There is one little caveat for writing scripts.  In your job "
              "submission script, you cannot simply do: ")
           (pre (code (@ (class "bash"))
                      "guixr load-profile /path/to/profile
# Run your programs here.
exit"))

           (p "... because the " (code "load-profile") " subcommand starts a "
              "new shell, which waits for you to enter a command.  Instead, "
              "use the following snippet:")
           (pre (code (@ (class "bash"))
                      "guixr load-profile /path/to/profile -- <<EOF
# Run your programs here.
EOF"))

           (p "This will run everything between " (code "EOF") " in the "
              "shell that lives in the specified profile environment.  Make sure "
              "the second " (code "EOF") " is on a new line, without anything "
              "else on the same line."))
         #:dependencies '(highlight)))))
