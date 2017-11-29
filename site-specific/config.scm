(define-module (site-specific config))

;; ----------------------------------------------------------------------------
;; The following variable overrides the site's title prefix.
;; The default value is "hpcguix | ".
;(define-public %site-title-prefix "hpcguix | ")

;; ----------------------------------------------------------------------------
;; If you use a wrapper around guix which changes the command, you can define
;; it here.  This command will be used in the package page for the
;; installation command example.  The default value is 'guix'.
;;
;; Comment the next line to use the default.
(define-public %guix-command "guixr")

;; ----------------------------------------------------------------------------
;; Unfortunately, some sites might want to remove packages from the search
;; results.  Note that this does not remove the packages from Guix, and users
;; can still find and install them using the command-line.
;;
;; The following blacklist is an example of the blacklist used by UMC Utrecht.
(define-public %package-blacklist
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
;; The following variable can be used to extend the package page.
;; If %package-page-extension is not defined, only a table with a few
;; properties is shown.  The contents below are an example of the extended
;; version used by UMC Utrecht.
(define-public %package-page-extension
  `((h2 "After installation")
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
      "else on the same line.")))
