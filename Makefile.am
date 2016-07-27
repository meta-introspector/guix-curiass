## Process this file with automake to produce Makefile.in.

bin_SCRIPTS = bin/cuirass bin/evaluate
noinst_SCRIPTS = pre-inst-env

pkgmoduledir=$(datarootdir)/guile/site/2.0/$(PACKAGE)
dist_pkgmodule_DATA = \
  src/cuirass/base.scm \
  src/cuirass/database.scm \
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
  testbuilddir='$(abs_top_builddir)/tests'

SCM_LOG_DRIVER = \
  $(builddir)/pre-inst-env $(GUILE) \
  $(srcdir)/build-aux/test-driver.scm

SH_LOG_COMPILER = $(top_builddir)/pre-inst-env $(SHELL)
AM_SH_LOG_FLAGS = -x -e

TESTS = \
  tests/base.scm \
## tests/basic.sh # takes too long to execute
  tests/database.scm \
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
  tests/gnu-system.scm \
  tests/guix-jobs.scm \
  tests/hello-singleton.scm \
  tests/hello-subset.scm \
  $(TESTS)

DISTCLEANFILES = src/cuirass/config.scm
CLEANFILES = \
  $(dist_pkgmodule_DATA:%.scm=%.go) \
  src/cuirass/config.go

## -------------- ##
## Silent rules.  ##
## -------------- ##

guilec_verbose = $(guilec_verbose_@AM_V@)
guilec_verbose_ = $(guilec_verbose_@AM_DEFAULT_V@)
guilec_verbose_0 = @echo "  GUILEC  " $@;

devnull_verbose = $(devnull_verbose_@AM_V@)
devnull_verbose_ = $(devnull_verbose_@AM_DEFAULT_V@)
devnull_verbose_0 = >/dev/null
