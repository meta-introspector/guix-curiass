## Process this file with autoconf to produce a configure script.

# Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
# Copyright © 2017, 2018, 2020 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2017 Mathieu Othacehe <othacehe@gnu.org>
#
# This file is part of Cuirass.
#
# Cuirass is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# Cuirass is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Cuirass.  If not, see <http://www.gnu.org/licenses/>.

AC_PREREQ([2.61])
AC_INIT([Cuirass],
  m4_esyscmd([build-aux/git-version-gen .tarball-version]),
  [bug-guix@gnu.org], [cuirass],
  [https://guix.gnu.org/en/cuirass/])
AC_CONFIG_SRCDIR([bin/cuirass.in])
AC_CONFIG_AUX_DIR([build-aux])
AC_REQUIRE_AUX_FILE([git-version-gen])
AC_REQUIRE_AUX_FILE([test-driver.scm])
AM_INIT_AUTOMAKE([foreign subdir-objects -Wall])
AM_SILENT_RULES([yes])		# enables silent rules by default

AC_CANONICAL_HOST
AC_PROG_MKDIR_P
AC_PROG_SED

GUILE_PKG([3.0])

AC_PATH_PROG([GUILE], [guile])
AC_PATH_PROG([GUILD], [guild])
AS_IF([test -z "$ac_cv_path_GUILD"],
  [AC_MSG_ERROR(['guild' program cannot be found.])])

GUILE_MODULE_REQUIRED([avahi])
GUILE_MODULE_REQUIRED([fibers])
GUILE_MODULE_REQUIRED([guix])
GUILE_MODULE_REQUIRED([guix git])
GUILE_MODULE_REQUIRED([guix config])
GUILE_MODULE_REQUIRED([git])
GUILE_MODULE_REQUIRED([json])
GUILE_MODULE_REQUIRED([mailutils mailutils])
GUILE_MODULE_REQUIRED([mastodon])
GUILE_MODULE_REQUIRED([simple-zmq])
GUILE_MODULE_REQUIRED([squee])
GUILE_MODULE_REQUIRED([zlib])

# We depend on new Guile-Git errors.
GUILE_MODULE_REQUIRED_EXPORT([(git)], git-error-message)

guix_localstatedir="$($GUILE -c '(import (guix config)) (display %localstatedir)')"
AC_SUBST(guix_localstatedir)

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in],
  [chmod +x pre-inst-env])
AC_OUTPUT
