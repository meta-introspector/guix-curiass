AC_PREREQ([2.61])
AC_INIT([Cuirass], [0.0.1], [bug-guix@gnu.org])
AC_CONFIG_SRCDIR([bin/cuirass.in])
AC_CONFIG_AUX_DIR([build-aux])
AC_REQUIRE_AUX_FILE([test-driver.scm])
AM_INIT_AUTOMAKE([foreign subdir-objects -Wall])
AM_SILENT_RULES([yes])		# enables silent rules by default

AC_CANONICAL_HOST

# Prepare a version of installation directories that does not contain
# references to shell variables.
expanded_prefix="`eval echo $prefix | sed -e "s|NONE|/usr/local|g"`"
AC_SUBST([expanded_datadir],
  [`eval echo $datarootdir | sed -e "s|NONE|$expanded_prefix|g"`])
AC_SUBST([expanded_localstatedir],
  [`eval echo $localstatedir | sed -e "s|NONE|$expanded_prefix|g"`])

PKG_CHECK_MODULES([GUILE], [guile-2.0 >= 2.0.7])
AC_PATH_PROG([GUILE], [guile])
AC_PATH_PROG([GUILD], [guild])
AS_IF([test -z "$ac_cv_path_GUILD"],
  [AC_MSG_ERROR(['guild' program cannot be found.])])

GUILE_MODULE_REQUIRED([guix])
GUILE_MODULE_REQUIRED([sqlite3])

AC_CONFIG_FILES([Makefile
                 src/cuirass/config.scm])
AC_CONFIG_FILES([bin/cuirass], [chmod +x bin/cuirass])
AC_CONFIG_FILES([bin/evaluate], [chmod +x bin/evaluate])
AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in],
  [chmod +x pre-inst-env])
AC_OUTPUT
