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

(define-module (www pages getting-started bioinformaticians)
  #:use-module (www pages)
  #:export (page-getting-started-bioinformaticians))

(define (page-getting-started-bioinformaticians request-path)
  (page-root-template
   "Getting started with GNU Guix for busy scientists" request-path
   `((h2 "Introduction")
    (p "GNU Guix is a software package manager, available on the HPC, that 
 enables you to install and use software packages in a reproducible way.")

    (p "To use GNU Guix, you need to add the following lines to your "
       (code "$HOME/.bashrc") " file:")

    (pre (code (@ (class "bash"))
               "export GUIX_LOCPATH=\"/gnu/profiles/base/lib/locale\"
export PATH=$PATH:\"/gnu/profiles/base/bin\""))

    (p "")
    (h2 "Package management with GNU Guix")

    (p "Like most package managers, you can install, remove and update "
       "packages.  In the remainder of this section, examples of the commands "
       "to do this are provided. For a more complete guide, see the "
       (a (@ (href "https://www.gnu.org/software/guix/manual/html_node/Package-Management.html"))
          "GNU Guix manual") ".")

    (h3 "Finding programs")
    (p "We can find packages by using the "
       (a (@ (href "/")) "web interface's search function") " . Additionally, "
       "we can search using the command line:")

    (pre (code (@ (class "bash")) "$ guixr package --list-available | less
$ guixr package --search=samtools
$ guixr package -s ^samtools$"))

    (p "This output can be a bit verbose, so an alternative is to use the "
       (code "--list-available") " option, or its short-hand " (code "-A") ":")

    (pre (code (@ (class "bash"))
           "$ guixr package -A ^samtools$"))

    (p "")
    (h3 "Installing programs")
    (p "To install packages, for example " (code bwa) ", we can use the "
       "following command:")

    (p "The newly installed programs install into " (code "$HOME/.guix-profile")
       ". To be able to use them, you need to set the environment variables. To "
       "obtain an overview of which environment variables to set, use:")

    (pre (code (@ (class "bash"))
           "$ guixr package --search-paths"))

    (p "")
    (h3 "Upgrading packages")
    (p "The package recipes GNU Guix looks for can be found in "
       (code "/gnu/repositories/guix") " and "
       (code "/gnu/repositories/guix-additions") ". When these package recipes "
       "change, the packages in your profile may be upgradeable.")

    (p "View which packages would be upgraded before actually upgrading them:")
    (pre (code (@ (class "bash"))
           "$ guixr package --upgrade -n"))

    (p "Perform the upgrade:")
    (pre (code (@ (class "bash"))
           "$ guixr package --upgrade"))

    (p "")

    (h3 "Rolling back")
    (p "Is there a problem with the upgraded packages? No problem, it’s easy "
       "to go back to the pre-upgrade state:")

    (pre (code (@ (class "bash"))
           "$ guixr package --roll-back"))

    (p (strong "Note:") " When we roll back, the later profile generations "
       "will be overwritten by future install/remove/upgrade actions.  If we "
       "just want to have a look back, make sure to switch back to the latest "
       "generation before performing install/remove/upgrade actions.")

    (h3 "More on profile states and history")

    (p "With the following command, you can view each state to which you can "
       "roll back to:")

    (pre (code (@ (class "bash"))
           "$ guixr package --list-generations"))

    (p "Rolling back to a specific generation (let’s say " (code "12")
       ") can be done using:")

    (pre (code (@ (class "bash"))
           "$ guixr package --switch-generation=12"))

    (p "")
    (h2 "Running programs")

    (h3 "Program environment")

    (p "Programs read environment variables to find additional modules or "
       "files required to run.  For example, " (code "R") " reads the "
       "environment variable " (code "R_LIBS_SITE") " to find additionally "
       "installed packages.")

    (p "These environment variables make up the environment of a program. "
       "Setting the environment for a program, can be done using:")

    (pre (code (@ (class "bash"))
           "$ guixr environment --ad-hoc r r-ggplot2"))

    (p "To prevent previous environment settings from leaking into the "
       "environment, we need to add the " (code "--pure") " option, which "
       "unsets previous settings:")

    (pre (code (@ (class "bash"))
           "$ guixr environment --pure --ad-hoc r r-ggplot2"))

    (p (strong "Note:") " The environment subcommand does not take your "
       "profile into account.  It only provides a minimal environment to "
       "correctly run the program you specified.")

    (h2 "Profile environment")

    (p "Similar to " (code "guixr environment") ", which works on a "
       (em "package") " level, there is " (code "guixr load-profile")
       " that works on a " (em "profile") " level.")

    (pre (code (@ (class "bash"))
           "$ guixr load-profile /gnu/profiles/per-language/r"))

    (p "So, basically you call " (code "guixr load-profile")
       " followed by the path to a profile created by GNU Guix.")

    (h2 "Environments in " (code (@ (class "h2-title")) "qsub") " scripts")

    (p "The following code snippet can be used to run parts of your "
       (code "qsub") " script in a protected environment where the "
       "specified program is available:")

    (p "For packages, using " (code "guixr environment") ":")

    (pre (code (@ (class "bash"))
           "guixr environment --pure --ad-hoc samtools -- <<EOF
  echo \"Running inside a protected environment\";
  samtools # Specify any command-line arguments you'd like..
  exit
EOF"))

    (p "For profiles, using " (code "guixr load-profile") ":")

    (pre (code (@ (class "bash"))
           "guixr load-profile /gnu/profiles/per-program/samtools -- <<EOF
  echo \"Running inside a protected environment\";
  samtools # Specify any command-line arguments you'd like..
  exit
EOF"))

    (p "Inside the environment, all relevant environment variables, like "
       (code "PATH") ", will be adjusted to be able to run the programs in "
       "the specified packages.")

    (p "So, inside an environment for " (code "R") " packages, it will set "
       (code "R_LIBS_SITE") " and " (code "PATH") ":")

    (pre (code (@ (class "bash"))
           "guixr environment --pure --ad-hoc r r-ggplot2 r-deseq2 -- <<EOF
  echo \"Running inside a protected environment\";
  Rscript my-script.R
  exit
EOF"))

    (p "The same applies for the " (code "guixr load-profile") " command. "
       "To see which environment variables will be set, use the following "
       "command:")

    (pre (code (@ (class "bash"))
           "$ guixr package --search-paths -p /path/to/profile")))
   #:dependencies '(highlight)))
