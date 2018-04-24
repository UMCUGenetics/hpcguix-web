# Copyright © 2017 Roel Janssen <roel@gnu.org>
#
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU Affero General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

include guile.am

moddir=@guilemoduledir@
godir=@guileobjectdir@
datadir=@datarootdir@

SOURCES =                                               \
  www/util.scm                                          \
  www/pages.scm                                         \
  www/pages/javascript.scm                              \
  www/pages/package.scm                                 \
  www/pages/error.scm                                   \
  www/pages/welcome.scm                                 \
  hpcweb-configuration.scm

SUFFIXES += .js .min.js

if HAVE_UGLIFYJS
.js.min.js:
	$(UGLIFYJS) "$<" > "$@"
else
.js.min.js:
	cp "$<" > "$@"
endif

assetsdir = $(datadir)/@PACKAGE@
nobase_dist_assets_DATA =                               \
  static/css/main.css                                   \
  static/fonts/FiraMono-Regular.ttf                     \
  static/fonts/Roboto-Bold.ttf                          \
  static/fonts/Roboto-LightItalic.ttf                   \
  static/fonts/Roboto-Light.ttf                         \
  static/fonts/OFL.txt                                  \
  static/images/g16.png                                 \
  static/images/grid.png                                \
  static/images/grid.svg                                \
  static/images/sort_asc.png                            \
  static/images/sort_asc_disabled.png                   \
  static/images/sort_both.png                           \
  static/images/sort_desc.png                           \
  static/images/logo.png                                \
  static/images/logo-small.png                          \
  static/images/cubes.png                               \
  static/hpcguix-web.min.js                             \
  static/highlight/highlight.min.js                     \
  static/highlight/LICENSE                              \
  static/highlight/styles/androidstudio.css             \
  static/highlight/styles/github.css                    \
  static/datatables.min.css                             \
  static/datatables.min.js                              \
  static/jquery-2.2.4.min.js

EXTRA_DIST +=					\
  run.in					\
  www/config.scm.in

CLEANFILES +=					\
  $(wildcard static/*.min.js)			\
  run						\
  www/config.scm				\
  www/config.go

nobase_nodist_bin_SCRIPTS = run

BUILT_SOURCES = www/config.scm
nobase_nodist_mod_DATA = www/config.scm
nobase_nodist_go_DATA = www/config.go

# Flags for 'guild compile'.
GUILD_COMPILE_FLAGS = -Wformat -Wunbound-variable -Warity-mismatch

# Create the 'www' directory on build path and relocate static file
# path
www/config.scm: www/config.scm.in
	$(MKDIR_P) www
	$(SED) -e 's,@''datarootdir''@,$(abs_top_srcdir),g' \
	       < $< > $@.tmp
	mv $@.tmp $@

# Relocate module and compiled module path in 'run'.
# Make it executable.
run: run.in
	$(SED) -e 's,@''guilemoduledir''@\(.*\)%load-path,$(abs_top_srcdir)\1%load-path,g' \
	       -e 's,@''guileobjectdir''@\(.*\)%load-compiled-path,$(abs_top_builddir)\1%load-compiled-path,g' \
	       -e 's,@''GUILE''@,$(GUILE),g' \
	       < $< > $@.tmp
	chmod +x $@.tmp
	mv $@.tmp $@

# Relocate module and compiled module path in 'run'.
install-exec-hook:
	$(SED) -e 's,$(abs_top_srcdir),$(moddir),g'  \
	       -e 's,$(abs_top_builddir),$(godir),g' \
	       $(DESTDIR)$(bindir)/run -i

# Relocate static file path in 'www/config.scm'.
install-data-hook:
	$(SED) -e 's,$(abs_top_srcdir),$(datadir)/hpcguix-web,g' \
	       $(DESTDIR)$(moddir)/www/config.scm -i

# Build 'run' by default.  Compile it so we get warnings.
all-local: run
	$(AM_V_GEN)$(GUILD) compile $(GUILD_COMPILE_FLAGS) \
	  -L "$(top_srcdir)" -L "$(top_builddir)"  run
