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

(define-module (www pages getting-started scientists)
  #:use-module (www pages)
  #:export (page-getting-started-scientists))

(define (page-getting-started-scientists request-path)
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
    (h2 "Recommended project setup")

    (p "To be able to describe how the entire data analysis of your project "
       "can be reproduced, we need to collect all software into one place.  "
       "We assume to have a project folder on the HPC and refer to this "
       "folder as " (code "$PROJECT_ROOT") " in the remainder of this section.")

    (h3 "Finding programs")

    (p "We can find packages by using the "
       (a (@ (href "/")) "web interface's search function") " . Additionally, "
       "we can search using the command line:")

    (pre (code (@ (class "bash")) "$ guixr package --list-available | less
$ guixr package --search=samtools
$ guixr package -s ^samtools$"))

    (p "This output can be a bit verbose, so an alternative is to use the "
       (code "--list-available") " option, or its short-hand " (code "-A") ":")
    (pre (code (@ (class "bash")) "$ guixr package -A ^samtools$"))

    (p "")
    (h3 "Installing programs")
    (p "To install packages, for example " (code bwa) ", we can use the "
       "following command:")
    (pre
     (code (@ (class "bash"))
       "$ guixr package --install=bwa --profile=$PROJECT_ROOT/.guix-profile"))

    (p "Or its equivalent using “shorthand” notation:")

    (pre
     (code (@ (class "bash"))
           "$ guixr package -i bwa -p $PROJECT_ROOT/.guix-profile"))

    (p "To get an overview of the tools in the project’s profile, we can use:")

    (pre
     (code (@ (class "bash"))
       "$ guixr package --list-installed -p $PROJECT_ROOT/.guix-profile"))

    (p "")
    (h2 "Running installed programs")

    (p "Once all required programs have been installed in one " (em profile)
       ", we need to tell the system to use the programs in that profile. We "
       "do this by " (em "loading") " a profile, using:")
    (pre
     (code "$ guixr load-profile $PROJECT_ROOT/.guix-profile"))

    (p "This command will provide a new shell prompt from which those (and "
       "only those) programs in the profile are available to us.")

    (p (strong "Note") ": You may notice that even basic commands like "
       (code "ls") " and " (code "grep") " may not be available inside an "
       "environment created with the " (code "load-profile") " command.  This "
       "environment ensures that we are running exactly the software we expect "
       "to be running (that inside the profile we loaded). If you need these "
       "programs, install the " (code "coreutils") " and " (code "grep")
       " package into the profile.")

    (p "A common task is to run one or more commands on a compute node using a "
       "specially crafted " (em "job script") ". This is how it can be done in "
       "combination with the " (code "load-profile") " command:")

    (pre
     (code (@ (class "bash"))
       "guixr load-profile $PROJECT_ROOT/.guix-profile --<<EOF
  Rscript $PROJECT_ROOT/scripts/analyse.R
EOF"))

    (p "So, any command available inside the profile environment can be placed "
       "between the " (code "EOF") " markers, on separate lines.")

    (h2 "Dealing with software updates")

    (p "To make it easy for someone else to reproduce the programs, and "
       "therefore, the essential tools to reproduce the entire data analysis, "
       "we would like to have a single set of instructions to perform, like:")

    (blockquote
     (p "All software needed to perform this data analysis can be "
        "installed by running "
        (code "guix package -i bwa samtools delly gatk r r-mutationalpatterns r-ggplot2")
        ", after installing GNU Guix."))

    (p "This instruction implies all software was installed in a single point "
       "in time, or more accurately, using a single snapshot of the GNU Guix "
       "package recipes state.")

    (p "So, when upgrading a single program, we should upgrade all programs in "
       "the profile, so that the entire profile can be installed using a single "
       "installation instruction.")

    (h2 "Trying out programs without installing them")
    (p "When we would just like to experiment with some packages, but are "
       "unsure whether this will be useful for your data analysis in the long "
       "run, we can create a so-called " (em "environment") " in which packages "
       "are available to us, without needing to install them in our profile. "
       "Here’s an example for " (code "R") " and " (code "QDNAseq") ":")
    (pre
     (code (@ (class "bash"))
       "$ guixr environment --ad-hoc r r-qdnaseq"))

    (p "This command will give us a new shell prompt in which we can run "
       (code "R") " and load " (code "QDNAseq") " from that R shell.")

    (h2 "Transforming your scripts into a pipeline")
    (p "As an extension to GNU Guix, there’s a " (em "workflow language")
       " that attempts to make running scripts and programs on computing "
       "clusters fast and easy.")

    (p "To get started with this workflow language, please visit the project’s "
       "page at: " (a (@ (href "https://www.guixwl.org/")) "https://www.guixwl.org/")
       ".")

    (h2 "Where to go with feedback and/or problems")

    (p "Please do not hesitate to contact me by e-mailing " (a (@ (href "R.R.E.Janssen-10@umcutrecht.nl")) "R.R.E.Janssen-10@umcutrecht.nl") "."))
   #:dependencies '(highlight)))
