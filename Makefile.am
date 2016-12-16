## Process this file with automake to produce Makefile.in.

# Copyright © 1995-2016 Free Software Foundation, Inc.
# Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
#
# This file is part of Cuirass.
#
# Cuirass is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# Cuirass is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Cuirass.  If not, see <http://www.gnu.org/licenses/>.

# Guile local load paths.
local_load_path = $(abs_top_builddir)/src:$(abs_top_srcdir)/src
local_load_compiled_path = $(abs_top_builddir)/src

bin_SCRIPTS = bin/cuirass bin/evaluate
noinst_SCRIPTS = pre-inst-env

guilesitedir=$(datarootdir)/guile/site/2.0
dist_guilesite_DATA = src/cuirass.scm
nodist_guilesite_DATA = $(dist_guilesite_DATA:%.scm=%.go)

pkgmoduledir=$(guilesitedir)/$(PACKAGE)
dist_pkgmodule_DATA = \
  src/cuirass/base.scm \
  src/cuirass/database.scm \
  src/cuirass/http.scm \
  src/cuirass/ui.scm \
  src/cuirass/utils.scm

nodist_pkgmodule_DATA = \
  $(dist_pkgmodule_DATA:%.scm=%.go) \
  src/cuirass/config.scm \
  src/cuirass/config.go

dist_pkgdata_DATA = src/schema.sql

TEST_EXTENSIONS = .scm .sh
AM_TESTS_ENVIRONMENT = \
  env GUILE_AUTO_COMPILE='0' \
  testsrcdir='$(abs_top_srcdir)/tests' \
  testbuilddir='$(abs_top_builddir)/tests' \
  GUILE_LOAD_PATH="$(local_load_path)$${GUILE_LOAD_PATH:+:}$$GUILE_LOAD_PATH" \
  GUILE_LOAD_COMPILED_PATH="$(local_load_compiled_path)$${GUILE_LOAD_COMPILED_PATH:+:}$$GUILE_LOAD_COMPILED_PATH"

SCM_LOG_DRIVER = \
  $(builddir)/pre-inst-env $(GUILE) \
  $(srcdir)/build-aux/test-driver.scm

SH_LOG_COMPILER = $(top_builddir)/pre-inst-env $(SHELL)
AM_SH_LOG_FLAGS = -x -e

TESTS = \
  tests/base.scm \
## tests/basic.sh # takes too long to execute
  tests/database.scm \
  tests/http.scm \
  tests/ui.scm \
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

dist-hook: gen-ChangeLog

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

.PHONY:	sql-check
sql-check: src/schema.sql
	@echo "$<"
	$(AM_V_at)sqlite3 tmp-$$$.db < $< ; \
	rm tmp-$$$.db

EXTRA_DIST = \
  .dir-locals.el \
  bin/cuirass.in \
  bin/evaluate.in \
  bootstrap \
  src/cuirass/config.scm.in \
  $(TESTS)

MOSTLYCLEANFILES = $(bin_SCRIPTS) src/cuirass/config.scm
CLEANFILES = \
  $(nodist_guilesite_DATA) \
  $(dist_pkgmodule_DATA:%.scm=%.go) \
  src/cuirass/config.go

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
  -e 's,@PACKAGE_LOAD_COMPILED_PATH[@],$(local_load_compiled_path),g' \
  -e 's,@PACKAGE_LOAD_PATH[@],$(local_load_path),g' \
  | $(SHELL) ./config.status --file=- )

# Generic instructions to perform the substitution.  Generated files shouldn't
# contain unexpanded '@substitutions@', and should be made read-only, to
# prevent them from being edited by mistake instead of the file the are
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
bin/evaluate: $(srcdir)/bin/evaluate.in
$(bin_SCRIPTS): Makefile
	$(generate_file); chmod +x $@
src/cuirass/config.scm: $(srcdir)/src/cuirass/config.scm.in Makefile
	$(generate_file)

# Guile modules require 'src/cuirass/config.scm' to exist before their
# compilation.
BUILT_SOURCES = src/cuirass/config.scm

## --------------- ##
## Documentation.  ##
## --------------- ##

info_TEXINFOS = doc/cuirass.texi
doc_cuirass_TEXINFOS = doc/fdl-1.3.texi

# Install the examples.
nobase_dist_pkgdata_DATA = \
  examples/gnu-system.scm \
  examples/guix-jobs.scm \
  examples/hello-singleton.scm \
  examples/hello-subset.scm

## -------------- ##
## Installation.  ##
## -------------- ##

# Relocate the script---i.e., have them refer to the installed module
# directory.
install-exec-hook:
	$(SED) \
## XXX: Beware of the order of these two substitutions, because one is a
## substring of the other.
	  -e 's,$(local_load_path),$(guilesitedir),g' \
	  -e 's,$(local_load_compiled_path),$(guilesitedir),g' \
	  -i $(DESTDIR)$(bindir)/cuirass

## -------------- ##
## Silent rules.  ##
## -------------- ##

guilec_verbose = $(guilec_verbose_@AM_V@)
guilec_verbose_ = $(guilec_verbose_@AM_DEFAULT_V@)
guilec_verbose_0 = @echo "  GUILEC  " $@;

devnull_verbose = $(devnull_verbose_@AM_V@)
devnull_verbose_ = $(devnull_verbose_@AM_DEFAULT_V@)
devnull_verbose_0 = >/dev/null
