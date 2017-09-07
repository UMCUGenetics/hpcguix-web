;;; Copyright © 2017  Roel Janssen <roel@gnu.org>
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

(define-module (www pages solutions)
  #:use-module (www pages)
  #:export (page-solutions))

(define (page-solutions request-path)
  (page-root-template
   "Common problems and solutions" request-path
   `((h2 "Common problems and solutions")

     (p "In the following section, we address problems we have encountered "
        "while test-driving the GNU Guix deployment, and provide solutions "
        "to these problems.")

     (h3 "Permission denied upon browsing a shared profile directory")

     (p "When attempting to browse a directory in " (code "/gnu/profiles")
        ", a permissions error occurs.")

     (p "Loading such a profile, however, does work:")
     (pre (code (@ (class "bash"))
                "guixr load-profile /gnu/profiles/per-language/r"))

     (p "To look into the profile directory, use the " (code "/gnu/store")
        " path instead.  So, in the case of "
        (code "/gnu/profiles/per-language/r") ", we need to find what "
        "this points to by using: ")

     (pre (code (@ (class "bash"))
                "ls -lh /gnu/profiles/per-language/r"))

     (p "The linked " (code "/gnu/store") " path is accessible.")

     (h3 "Connection failure")
     (p "The deployment of GNU Guix is a bit special on the HPC. Therefore, "
        "you may end up receiving this error:")

     (pre (code (@ (class "bash"))
            "guix package: error: failed to connect to `/var/guix/daemon-socket/socket':
  No such file or directory"))

     (p "In this case, you should replace " (code "guix") " with " (code "guixr")
        " in your command.")

     (p "Further explanation: " (code "guix") " talks to another program called "
        (code "guix-daemon") " to handle the building of packages and profiles. "
        "On the HPC, this " (code "guix-daemon") " runs on a separate machine. "
        "Therefore, we created a wrapper program called " (code "guixr") " to handle "
        "the remote communication.  This wrapper creates a temporary socket which will "
        " then be used by " (code "guix") ".")

     (h3 "Substitute failures")

     (p "You may run into a problem similar to this:")

     (pre (code (@ (class "bash"))
            "guix environment: error: build failed: some substitutes for the outputs of
  derivation XXX failed (usually happens due to networking issues); try
  `--fallback' to build derivation from source"))

     (p "The suggestion provided by the error message is correct; append " (code "--fallback")
        " to your previous command. Note that it will take more time to install the program."))
   #:dependencies '(highlight)))
