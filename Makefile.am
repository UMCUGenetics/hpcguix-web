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

moddir=$(prefix)/share/guile/site/2.2
godir=$(libdir)/guile/2.2/ccache

SOURCES =                                               \
  web-interface.scm                                     \
  www/config.scm                                        \
  www/util.scm                                          \
  www/pages.scm                                         \
  www/pages/error.scm                                   \
  www/pages/faq.scm                                     \
  www/pages/getting-started.scm                         \
  www/pages/welcome.scm

WWW_STATIC_RESOURCES =                                  \
  static/css/main.css                                   \
  static/fonts/FiraMono-Regular.ttf                     \
  static/fonts/Roboto-Bold.ttf                          \
  static/fonts/Roboto-LightItalic.ttf                   \
  static/fonts/Roboto-Light.ttf                         \
  static/fonts/OFL.txt                                  \
  static/graphs/index.html                              \
  static/images/g16.png                                 \
  static/images/grid.png                                \
  static/images/grid.svg                                \
  static/images/sort_asc.png                            \
  static/images/sort_asc_disabled.png                   \
  static/images/sort_both.png                           \
  static/images/sort_desc.png                           \
  static/logo.png                                       \
  static/highlight/highlight.pack.js                    \
  static/highlight/LICENSE                              \
  static/highlight/styles/androidstudio.css             \
  static/datatables.min.css                             \
  static/datatables.min.js

EXTRA_DIST += env.in $(WWW_STATIC_RESOURCES)
