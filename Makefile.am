## Process this file with automake to produce Makefile.in.

# Copyright © 1995-2016 Free Software Foundation, Inc.
# Copyright © 2016, 2017 Mathieu Lirzin <mthl@gnu.org>
# Copyright © 2018, 2023 Ludovic Courtès <ludo@gnu.org>
# Copyright © 2018 Clément Lassieur <clement@lassieur.org>
# Copyright © 2018 Tatiana Sholokhova <tanja201396@gmail.com>
# Copyright © 2020 Mathieu Othacehe <othacehe@gnu.org>
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

bin_SCRIPTS =                    \
  bin/cuirass

noinst_SCRIPTS = pre-inst-env

guilesitedir = $(datarootdir)/guile/site/@GUILE_EFFECTIVE_VERSION@
guileobjectdir = $(libdir)/guile/@GUILE_EFFECTIVE_VERSION@/site-ccache

dist_guilesite_DATA = src/cuirass.scm
nodist_guileobject_DATA = $(dist_guilesite_DATA:.scm=.go)

pkgmoduledir = $(guilesitedir)/$(PACKAGE)
pkgobjectdir = $(guileobjectdir)/$(PACKAGE)
webmoduledir = $(guilesitedir)/web/server
webobjectdir = $(guileobjectdir)/web/server
scriptsmoduledir = $(guilesitedir)/$(PACKAGE)/scripts
scriptsobjectdir = $(guileobjectdir)/$(PACKAGE)/scripts
sqldir = $(pkgdatadir)/sql
staticdir = $(pkgdatadir)/static
cssdir = $(staticdir)/css
fontsdir = $(staticdir)/fonts
imagesdir = $(staticdir)/images
jsdir = $(staticdir)/js
systemdservicedir = $(libdir)/systemd/system

dist_pkgmodule_DATA =				\
  src/cuirass/base.scm				\
  src/cuirass/database.scm			\
  src/cuirass/http.scm				\
  src/cuirass/logging.scm			\
  src/cuirass/mail.scm				\
  src/cuirass/mastodon.scm			\
  src/cuirass/metrics.scm			\
  src/cuirass/notification.scm			\
  src/cuirass/parameters.scm			\
  src/cuirass/remote.scm			\
  src/cuirass/rss.scm				\
  src/cuirass/specification.scm			\
  src/cuirass/ui.scm				\
  src/cuirass/utils.scm		                \
  src/cuirass/templates.scm                     \
  src/cuirass/watchdog.scm			\
  src/cuirass/zabbix.scm

dist_scriptsmodule_DATA =			\
  src/cuirass/scripts/evaluate.scm              \
  src/cuirass/scripts/register.scm              \
  src/cuirass/scripts/remote-server.scm         \
  src/cuirass/scripts/remote-worker.scm         \
  src/cuirass/scripts/web.scm

nodist_pkgmodule_DATA = \
  src/cuirass/config.scm

nodist_pkgobject_DATA = \
  $(dist_pkgmodule_DATA:.scm=.go) \
  src/cuirass/config.go

dist_webmodule_DATA =				\
  src/web/server/fiberized.scm

nodist_scriptsobject_DATA =				\
  $(dist_scriptsmodule_DATA:.scm=.go)

nodist_webobject_DATA =				\
  $(dist_webmodule_DATA:.scm=.go)

dist_pkgdata_DATA = src/schema.sql

dist_sql_DATA = 				\
  src/sql/upgrade-1.sql				\
  src/sql/upgrade-2.sql				\
  src/sql/upgrade-3.sql				\
  src/sql/upgrade-4.sql				\
  src/sql/upgrade-5.sql				\
  src/sql/upgrade-6.sql				\
  src/sql/upgrade-7.sql				\
  src/sql/upgrade-8.sql				\
  src/sql/upgrade-9.sql				\
  src/sql/upgrade-10.sql			\
  src/sql/upgrade-11.sql			\
  src/sql/upgrade-12.sql			\
  src/sql/upgrade-13.sql			\
  src/sql/upgrade-14.sql			\
  src/sql/upgrade-15.sql

dist_css_DATA =					\
  src/static/css/choices.min.css		\
  src/static/css/cuirass.css			\
  src/static/css/bootstrap.min.css		\
  src/static/css/datatables.min.css		\
  src/static/css/open-iconic-bootstrap.css
dist_fonts_DATA =				\
  src/static/fonts/open-iconic.eot		\
  src/static/fonts/open-iconic.otf		\
  src/static/fonts/open-iconic.svg		\
  src/static/fonts/open-iconic.ttf		\
  src/static/fonts/open-iconic.woff
dist_images_DATA =				\
  src/static/images/badge-error.svg		\
  src/static/images/badge-spec-percentage.svg	\
  src/static/images/badge-percentage.svg	\
  src/static/images/guix.png			\
  src/static/images/icon.png
dist_js_DATA =					\
  src/static/js/bootstrap.min.js	        \
  src/static/js/cuirass.js                      \
  src/static/js/chart.js                        \
  src/static/js/choices.min.js                  \
  src/static/js/d3.v6.min.js	                \
  src/static/js/datatables.min.js               \
  src/static/js/jquery-3.3.1.min.js		\
  src/static/js/popper.min.js

dist_systemdservice_DATA =			\
  etc/cuirass-remote-worker.service

TEST_EXTENSIONS = .scm .sh
AM_TESTS_ENVIRONMENT = \
  env GUILE_AUTO_COMPILE='0' \
  testsrcdir='$(abs_top_srcdir)/tests' \
  testbuilddir='$(abs_top_builddir)/tests'

SCM_LOG_DRIVER =				\
  $(top_builddir)/pre-inst-env $(GUILE)		\
  -L "$(abs_top_srcdir)" -e main		\
  $(top_srcdir)/build-aux/test-driver.scm

SH_LOG_COMPILER = $(top_builddir)/pre-inst-env $(SHELL)
AM_SH_LOG_FLAGS = -x -e

TESTS = \
  tests/base.scm \
## tests/basic.sh # takes too long to execute
  tests/database.scm \
  tests/http.scm \
  tests/metrics.scm \
  tests/remote.scm \
  tests/utils.scm

# Unset 'GUILE_LOAD_COMPILED_PATH' altogether while compiling.  Otherwise, if
# $GUILE_LOAD_COMPILED_PATH contains $(pkgmoduledir), we may find .go files in
# there that are newer than the local .scm files (for instance because the
# user ran 'make install' recently).  When that happens, we end up loading
# those previously-installed .go files, which may be stale, thereby breaking
# the whole thing.  Set GUILE_AUTO_COMPILE to 0 to avoid auto-compiling guild.
#
# XXX: Use the C locale for when Guile lacks
# <http://git.sv.gnu.org/cgit/guile.git/commit/?h=stable-2.0&id=e2c6bf3866d1186c60bacfbd4fe5037087ee5e3f>.
.scm.go:
	$(guilec_verbose)$(MKDIR_P) `dirname "$@"`; \
	export GUILE_AUTO_COMPILE=0 ; unset GUILE_LOAD_COMPILED_PATH; \
	LC_ALL=C \
	$(top_builddir)/pre-inst-env $(GUILD) compile \
	  --load-path="$(top_builddir)/src" \
	  --load-path="$(top_srcdir)/src" \
	  --warn=format --warn=unbound-variable --warn=arity-mismatch \
	  --target="$(host)" --output="$@" "$<" $(devnull_verbose)

CLEANFILES =					\
  $(nodist_guilesite_DATA)			\
  $(dist_pkgmodule_DATA:.scm=.go)		\
  $(nodist_scriptsmodule_DATA)			\
  $(nodist_webobject_DATA)			\
  $(nodist_guileobject_DATA)			\
  src/cuirass/config.go

## -------------- ##
## Distribution.  ##
## -------------- ##

EXTRA_DIST =					\
  .dir-locals.el				\
  bin/cuirass.in				\
  bootstrap					\
  guix.scm					\
  build-aux/guix/cuirass-package.scm		\
  src/cuirass/config.scm.in			\
  etc/cuirass-remote-worker.service.in		\
  $(TESTS)

dist-hook: gen-ChangeLog
	$(AM_V_GEN)echo $(VERSION) > $(distdir)/.tarball-version

.PHONY: gen-ChangeLog
gen-ChangeLog:
	$(AM_V_GEN)if test -d $(srcdir)/.git; then \
	  log_fix="$(srcdir)/build-aux/git-log-fix"; \
	  test -e "$$log_fix" \
	    && amend_git_log="--amend=$$log_fix" \
	    || amend_git_log=; \
	  $(top_srcdir)/build-aux/gitlog-to-changelog \
	    $$amend_git_log > $(distdir)/cl-t && \
	    { rm -f $(distdir)/ChangeLog && \
	      mv $(distdir)/cl-t $(distdir)/ChangeLog; } \
	fi

## ----------------- ##
## Generated files.  ##
## ----------------- ##

# 'AC_CONFIG_FILES' doesn't fully expand variables that depend on ${prefix}.
# To use such variables in source files and scripts while following GNU Coding
# Standards, let 'make' manage their replacement.

# Use config.status to substitute the remainder where a single expansion is
# sufficient.  We use a funny notation here to avoid configure substitutions
# in our text.
do_subst = ( $(SED) \
  -e "s,@configure_input[@],Generated from $$in.  Do not edit by hand.,g" \
  -e 's,@datadir[@],$(datadir),g' \
  -e 's,@GUILE[@],$(GUILE),g' \
  -e 's,@localstatedir[@],$(localstatedir),g' \
  -e 's,@PACKAGE_LOAD_COMPILED_PATH[@],$(guileobjectdir),g' \
  -e 's,@PACKAGE_LOAD_PATH[@],$(guilesitedir),g' \
  | $(SHELL) ./config.status --file=- )

# Generic instructions to perform the substitution.  Generated files shouldn't
# contain unexpanded '@substitutions@', and should be made read-only, to
# prevent them from being edited by mistake instead of the file they are
# generated from.
generate_file = \
  $(AM_V_GEN)rm -f $@ $@-t; \
  $(MKDIR_P) $(@D); \
  in=$@.in && $(do_subst) <$(srcdir)/$$in >$@-t; \
  if LC_ALL=C grep '@[a-zA-Z0-9_][a-zA-Z0-9_]*@' $@-t; then \
    echo "$@ contains unexpanded substitution (see lines above)"; \
    exit 1; \
  fi; \
  chmod a-w $@-t; mv -f $@-t $@

# These files depend on Makefile so they are rebuilt if $(VERSION),
# $(datadir) or other do_subst'ituted variables change.
bin/cuirass: $(srcdir)/bin/cuirass.in
$(bin_SCRIPTS): Makefile
	$(generate_file); chmod +x $@
src/cuirass/config.scm: $(srcdir)/src/cuirass/config.scm.in Makefile
	$(generate_file)
etc/cuirass-remote-worker.service: $(srcdir)/etc/cuirass-remote-worker.service.in \
					Makefile
	$(generate_file)

# Guile modules require 'src/cuirass/config.scm' to exist before their
# compilation.
BUILT_SOURCES = src/cuirass/config.scm

MOSTLYCLEANFILES = $(bin_SCRIPTS) src/cuirass/config.scm

## --------------- ##
## Documentation.  ##
## --------------- ##

info_TEXINFOS = doc/cuirass.texi
doc_cuirass_TEXINFOS = doc/fdl-1.3.texi

## -------------- ##
## Installation.  ##
## -------------- ##

# Ensure that the scripts will find their module directories.
install-exec-hook:
	@find $(DESTDIR)$(bindir) -type f -exec \
	$(SED) -i -e 's,^#GUILE_LOAD,GUILE_LOAD,g' {} ';'

## -------------- ##
## Silent rules.  ##
## -------------- ##

guilec_verbose = $(guilec_verbose_@AM_V@)
guilec_verbose_ = $(guilec_verbose_@AM_DEFAULT_V@)
guilec_verbose_0 = @echo "  GUILEC  " $@;

devnull_verbose = $(devnull_verbose_@AM_V@)
devnull_verbose_ = $(devnull_verbose_@AM_DEFAULT_V@)
devnull_verbose_0 = >/dev/null
