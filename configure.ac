AC_PREREQ([2.61])
AC_INIT([Cuirass], [0.0.1], [bug-guix@gnu.org])
AC_CONFIG_SRCDIR([bin/cuirass.in])
AC_CONFIG_AUX_DIR([build-aux])
AC_REQUIRE_AUX_FILE([test-driver.scm])
AM_INIT_AUTOMAKE([foreign subdir-objects -Wall])
AM_SILENT_RULES([yes])		# enables silent rules by default

AC_CANONICAL_HOST

PKG_CHECK_MODULES([GUILE], [guile-2.0 >= 2.0.7])
AC_PATH_PROG([GUILE], [guile])
AC_PATH_PROG([GUILD], [guild])
AS_IF([test -z "$ac_cv_path_GUILD"],
  [AC_MSG_ERROR(['guild' program cannot be found.])])

moduledir="${datarootdir}/guile/site/2.0"
pkgmoduledir="${moduledir}/cuirass"
AC_SUBST([moduledir])
AC_SUBST([pkgmoduledir])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([bin/cuirass], [chmod +x bin/cuirass])
AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in],
  [chmod +x pre-inst-env])
AC_OUTPUT
