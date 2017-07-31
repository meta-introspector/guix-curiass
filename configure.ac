## Process this file with autoconf to produce a configure script.

# Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
# Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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
  [https://www.gnu.org/software/guix/])
AC_CONFIG_SRCDIR([bin/cuirass.in])
AC_CONFIG_AUX_DIR([build-aux])
AC_REQUIRE_AUX_FILE([git-version-gen])
AC_REQUIRE_AUX_FILE([test-driver.scm])
AM_INIT_AUTOMAKE([foreign subdir-objects -Wall])
AM_SILENT_RULES([yes])		# enables silent rules by default

AC_CANONICAL_HOST
AC_PROG_MKDIR_P
AC_PROG_SED

GUILE_PKG([2.2 2.0])

if test "x$GUILE_EFFECTIVE_VERSION" = "x2.0"; then
  PKG_CHECK_MODULES([GUILE], [guile-2.0 >= 2.0.7])
fi

AC_PATH_PROG([GUILE], [guile])
AC_PATH_PROG([GUILD], [guild])
AS_IF([test -z "$ac_cv_path_GUILD"],
  [AC_MSG_ERROR(['guild' program cannot be found.])])

GUILE_MODULE_REQUIRED([guix])
GUILE_MODULE_REQUIRED([guix git])
GUILE_MODULE_REQUIRED([git])
GUILE_MODULE_REQUIRED([json])
GUILE_MODULE_REQUIRED([sqlite3])

# We depend on new Guile-Git errors.
GUILE_MODULE_REQUIRED_EXPORT([(git)], git-error-message)

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in],
  [chmod +x pre-inst-env])
AC_OUTPUT
