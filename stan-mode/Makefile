STAN_LANG = ../stan-language-definitions/stan_lang.json

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

# 4.6 Phony Targets
# https://www.gnu.org/software/make/manual/html_node/Phony-Targets.html
# A phony target is a name for a recipe to be executed upon a request.
# If you write a rule whose recipe will not create the target file,
# the recipe will be executed every time the target comes up for remaking.
.PHONY : build-src clean-src link

# If stan-keywords.el exists and newer than $(STAN_LANG), do nothing.
# If not, the recipe under stan-keywords.el as a target is invoked.
build-src : stan-keywords.el
clean-src :
	rm -rf stan-keywords.el

# Empty for compatibility with the main Makefile
link :

# 10.5.3 Automatic Variables
# https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html
# $^ The names of all the prerequisites, with spaces between them.
# $@ The file name of the target of the rule.
stan-keywords.el : create_stan_keywords.py $(STAN_LANG)
	$(PYTHON) $^ $@

# 3.3 Including Other Makefiles
# https://www.gnu.org/software/make/manual/html_node/Include.html
# If you want make to simply ignore a makefile which does not exist
# or cannot be remade, with no error message, use the -include
# directive instead of include.
-include ../common.mk
