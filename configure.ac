dnl Copyright © 2017 Roel Janssen <roel@gnu.org>
dnl
dnl This program is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program. If not, see <http://www.gnu.org/licenses/>.
dnl -*- Autoconf -*-

AC_INIT([hpcguix-web], [m4_translit(m4_esyscmd([cat VERSION]),m4_newline)])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

AC_PROG_SED

GUILE_PKG([3.0 2.2])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your Guile installation.])
fi

GUILE_MODULE_AVAILABLE([have_guile_zlib], [(zlib)])
if test "x$have_guile_zlib" != "xyes"; then
  AC_MSG_ERROR([Guile-Zlib not found; please install it first.])
fi

GUILE_MODULE_AVAILABLE([have_syntax_highlight], [(syntax-highlight)])
if test "x$have_syntax_highlight" != "xyes"; then
  AC_MSG_ERROR([Syntax-Highlight not found; please install it first.])
fi

GUILE_MODULE_AVAILABLE([have_guix_inferior], [(guix inferior)])
GUILE_MODULE_AVAILABLE([have_guix_channels], [(guix channels)])
if test "x$have_guix_inferior$have_guix_channels" = "xnono"; then
  AC_MSG_ERROR([Guix appears to be too old; please upgrade to Guix > 0.15.0.])
fi

AM_MISSING_PROG([UGLIFYJS], [uglify-js])
AM_CONDITIONAL([HAVE_UGLIFYJS], [test "x$UGLIFYJS" != "x"])

guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([env], [chmod +x env])

AC_OUTPUT
