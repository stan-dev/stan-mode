STAN_LANG = ../stan-language-definitions/stan_lang.json

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
# Single quote to protect against a abspath name with spaces in it.
# This single quote trick can only work with a single name.
#
# This adds the local stan-mode package to be in the load-path,
# which allows access to the latest definitions, not the MELPA version.
EMACSFLAGS = -L '$(abspath ../stan-mode)' -L '$(abspath ../stan-snippets)'

# GNU Make: 8.2 Functions for String Substitution and Analysis
# https://www.gnu.org/software/make/manual/html_node/Text-Functions.html
# $(filter-out pattern…,text)
#   Returns all whitespace-separated words in text that do not match any
# of the pattern words, removing the words that do match one or more.
# This is the exact opposite of the filter function.
# Here include all *.el files, except the *-pkg.el file.
#
# GNU Make: 4.4.3 The Function wildcard
# https://www.gnu.org/software/make/manual/html_node/Wildcard-Function.html
# $(wildcard pattern…)
# This string, used anywhere in a makefile, is replaced by a
# space-separated list of names of existing files that match one
# of the given file name patterns.
SRCS = $(filter-out %-pkg.el, $(wildcard *.el))
#
# 6.3.1 Substitution References
# https://www.gnu.org/software/make/manual/html_node/Substitution-Refs.html
# Take each word in $SRCS, replace .el with .elc.
# The $OBJECTS variale is used in ../common.mk.
OBJECTS = $(SRCS:.el=.elc)

$(OBJECTS) : build-src

.PHONY : build-src clean-src link

build-src : ac-dict/stan-mode
clean-src :
	rm -rf ac-dict/stan-mode

# 10.5.3 Automatic Variables
# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html
# $^ The names of all the prerequisites, with spaces between them.
# $@ The file name of the target of the rule.
ac-dict/stan-mode : create_ac_dict.py $(STAN_LANG)
	$(PYTHON) $^ $@

link :
	$(CASK) link stan-mode ../stan-mode
	$(CASK) link stan-snippets ../stan-snippets

-include ../common.mk
