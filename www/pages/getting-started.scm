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

(define-module (www pages getting-started)
  #:use-module (www pages)
  #:export (page-getting-started))

(define (page-getting-started request-path)
  (page-root-template
   "Getting started with GNU Guix" request-path
   `((h2 "Getting started with GNU Guix on the cluster")

     (p "GNU Guix is a software package manager available on our computing "
        "cluster that helps you get your software tools to work, and helps "
        "others reproducing your usage of these tools.")

     (p "To use GNU Guix, you need to add the following lines to your "
        (code "$HOME/.bashrc") " file:")

     (pre (code (@ (class "bash"))
                "export GUIX_LOCPATH=\"/gnu/profiles/base/lib/locale\"
export PATH=$PATH:\"/gnu/profiles/base/bin\""))

     (p "This page contains the instructions to get started with GNU "
        "Guix.  We try not to bore you with unneccessary technicalities.  So, "
        "please choose whatever is most applicable to you:")

     (ul
      (li (a (@ (href "/getting-started/scientists"))
             "GNU Guix for scientists"))
      (li (a (@ (href "/getting-started/bioinformaticians"))
             "GNU Guix for (bio)informaticians"))
      (li (a (@ (href "/solutions")) "Common problems and solutions")))

     (p "Of course, feel free to read whatever is not applicable to you."))
   #:dependencies '(highlight)))

