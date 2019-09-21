# This is a common Makefile included by subfolder Makefiles.

# 6.5 Setting Variables
# https://www.gnu.org/software/make/manual/html_node/Setting.html
# If you’d like a variable to be set to a value only if it’s
# not already set, then you can use the shorthand operator
# ‘?=’ instead of ‘=’.
PYTHON ?= python3
SED ?= sed
CASK ?= cask
EMACS ?= emacs
export EMACS
#
# Appendix C Command Line Arguments for Emacs Invocation
# C.1 Action Arguments
# https://www.gnu.org/software/emacs/draft/manual/html_node/emacs/Action-Arguments.html#Action-Arguments
# ‘-L dir’
# ‘--directory=dir’
#     Prepend directory dir to the variable load-path.
#
# GNU make: 8.3 Functions for File Names
# $(abspath names…)
# For each file name in names return an absolute name that does not
# contain any . or .. components, nor any repeated path separators (/).
# Note that, in contrast to realpath function, abspath does not
# resolve symlinks and does not require the file names to refer
# to an existing file or directory. Use the wildcard function to test
# for existence.
#
# Note this common.mk is included from each project subdirectory Makefile.
# Thus, .. takes make out of the project directory to the parent directory.
EMACSFLAGS ?=
#
# 6.5 Setting Variables
# https://www.gnu.org/software/make/manual/html_node/Setting.html
# Variables defined with ‘=’ are recursively expanded variables.
# Variables defined with ‘:=’ or ‘::=’ are simply expanded variables.
#
# https://cask.readthedocs.io/en/latest/guide/usage.html#cask-version
# cask [GLOBAL-OPTIONS] version
#   Print version of the current package.
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
#
# https://cask.readthedocs.io/en/latest/guide/usage.html#cask-package-directory
# cask [GLOBAL-OPTIONS] package-directory
#   Print path to package directory, where all dependencies are installed.
# Currently, this is .cask/emacs-version/elpa), where emacs-version is the
# value of the emacs-version variable in Emacs.
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)
#
DISTDIR = dist
#
# Appendix C Command Line Arguments for Emacs Invocation
# C.2 Initial Options
# https://www.gnu.org/software/emacs/manual/html_node/emacs/Initial-Options.html
# ‘-Q’
# ‘--quick’
#     Start Emacs with minimum customizations. This is similar to using
# ‘-q’, ‘--no-site-file’, ‘--no-site-lisp’, ‘--no-x-resources’,
# and ‘--no-splash’ together.
#
# ‘--batch’
#     Run Emacs in batch mode. Batch mode is used for running programs
# written in Emacs Lisp from shell scripts, makefiles, and so on.
# To invoke a Lisp program, use the ‘-batch’ option in conjunction with
# one or more of ‘-l’, ‘-f’ or ‘--eval’ (see Action Arguments).
EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)


# 4.6 Phony Targets
# https://www.gnu.org/software/make/manual/html_node/Phony-Targets.html
# A phony target is a name for a recipe to be executed upon a request.
# If you write a rule whose recipe will not create the target file,
# the recipe will be executed every time the target comes up for remaking.
.PHONY : test lint lint-package lint-elisp compile dist deps dist clean clean-elc clean-deps clean-dist show


# EMACSFLAGS should be an appropriate package-specific load-path entries.
# For example, testing of company-stan should have access to the latest
# local stan-mode. Check the package-specific subdirectory Makefile.
#
# Appendix C Command Line Arguments for Emacs Invocation
# C.1 Action Arguments
# https://www.gnu.org/software/emacs/draft/manual/html_node/emacs/Action-Arguments.html#Action-Arguments
# ‘-L dir’
# ‘--directory=dir’
#     Prepend directory dir to the variable load-path.
#
# GNU make: 8.3 Functions for File Names
# $(abspath names…)
# For each file name in names return an absolute name that does not
# contain any . or .. components, nor any repeated path separators (/).
# Note that, in contrast to realpath function, abspath does not
# resolve symlinks and does not require the file names to refer
# to an existing file or directory. Use the wildcard function to test
# for existence.
#
# -L $(abspath .): Add the expanded current directory to load-path.
test : compile build-src
	$(CASK) exec buttercup $(EMACSFLAGS) -L $(abspath .)

lint : lint-package lint-elisp

# Appendix C Command Line Arguments for Emacs Invocation
# C.1 Action Arguments
# https://www.gnu.org/software/emacs/draft/manual/html_node/emacs/Action-Arguments.html#Action-Arguments
#
# ‘--eval=expression’
# ‘--execute=expression’
#     Evaluate Lisp expression expression.
#
# ‘-l file’
# ‘--load=file’
#     Load a Lisp library named file with the function load. If file is
# not an absolute file name, Emacs first looks for it in the current
# directory, then in the directories listed in load-path (see Lisp Libraries).
#
# ‘-f function’
# ‘--funcall=function’
#     Call Lisp function function. If it is an interactive function
# (a command), it reads the arguments interactively just as if you had
# called the same function with a key sequence. Otherwise, it calls the
# function with no arguments.
#
# Note Melpa needs to be added to avoid "stan-mode uninstallable".
# https://github.com/purcell/package-lint/issues/55
# https://github.com/DamienCassou/klassified.el/pull/9/files#diff-b67911656ef5d18c4ae36cb6741b7965
# $(SRCS) should contain all *.el files except the *-pkg.el file.
# Exclude *-autoloads.el files to avoid false positives.
#
# local-melpa local package archive
# https://www.gnu.org/software/emacs/manual/html_node/elisp/Package-Archives.html
# https://emacs.stackexchange.com/questions/33627/how-to-generate-and-activate-autoloads-for-local-packages
lint-package :
	@echo "Linting for elisp package metadata!"
	$(CASK) exec $(EMACS) --batch \
	--eval "(require 'package)" \
	--eval "(setq package-archives '((\"melpa\" . \"http://melpa.org/packages/\")))" \
	--eval "(push '(\"local-melpa\" . \"$(abspath ../local-melpa/packages/)\") package-archives)" \
	--eval "(package-initialize)" \
	--eval "(package-refresh-contents)" \
	$(EMACSFLAGS) -L $(abspath .) \
	-l package-lint \
	-f package-lint-batch-and-exit $(filter-out %-autoloads.el, $(SRCS))

# https://github.com/gonewest818/elisp-lint
# Use - to allow the recipe to proceed even with error in the statement.
# https://www.gnu.org/software/make/manual/html_node/Errors.html
lint-elisp :
	@echo "Linting for elisp!"
	-$(CASK) exec $(EMACS) --batch \
	--eval "(require 'package)" \
	--eval "(setq package-archives '((\"melpa\" . \"http://melpa.org/packages/\")))" \
	--eval "(push '(\"local-melpa\" . \"$(abspath ../local-melpa/packages/)\") package-archives)" \
	--eval "(package-initialize)" \
	--eval "(package-refresh-contents)" \
	--eval "(setq make-backup-files nil)" \
	$(EMACSFLAGS) -L $(abspath .) \
	-l elisp-lint \
	-f elisp-lint-files-batch --no-indent-character --no-fill-column \
	$(filter-out %-autoloads.el, $(SRCS))
	rm -rf *-autoloads.el


# $(OBJECTS) is a list of *.elc files to be generated defined
# in the calling Makefile.
compile : $(OBJECTS)

dist :
	$(CASK) package

deps : $(PKGDIR)

# clean-src is package-specific and defined in each package Makefile.
clean : clean-deps clean-src clean-elc clean-dist

# Reverses deps
clean-deps :
	rm -rf .cask/

# Reverses compile
# https://cask.readthedocs.io/en/latest/guide/usage.html#cask-clean-elc
# cask [GLOBAL-OPTIONS] clean-elc
# Remove byte compiled files generated by cask build.
# Above does not clean all .elc files.
# $(OBJECTS) are names of the relevant *.elc files.
clean-elc :
	$(CASK) clean-elc
	rm -rf $(OBJECTS)

# Reverses dist
clean-dist :
	rm -rf $(DISTDIR)

# cask update is necessary to utilize local developmental repos,
# which are somehow not used upon cask install.
# See the developmental dependencies in company-mode/Cask.
$(PKGDIR) : Cask
	$(CASK) install
	$(CASK) update
	touch $(PKGDIR)

# (batch-byte-compile &optional NOFORCE)
# Run byte-compile-file on the files remaining on the command line.
# Use this from the command line, with -batch;
#
# 10.5.3 Automatic Variables
# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html
# $< The name of the first prerequisite.
%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -L $(abspath .) \
	-f batch-byte-compile $<

# Just show SRCS and OBJECTS defined in the project-specific Makefile.
show :
	@echo "Showing SRCS"
	@echo " $(SRCS)"
	@echo "Showing OBJECTS"
	@echo " $(OBJECTS)"
