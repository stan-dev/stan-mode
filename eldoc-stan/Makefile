STAN_LANG = ../stan-language-definitions/stan_lang.json
ELDOC_STAN_JSON = eldoc-stan.json

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
EMACSFLAGS = -L '$(abspath ../stan-mode)'

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
# This package depends on this extra data file, which may need building.
build-src : $(ELDOC_STAN_JSON)
clean-src :
	rm -rf $(ELDOC_STAN_JSON)

link :
	$(CASK) link stan-mode ../stan-mode

# Target eldoc-stan.json requires $(STAN_LANG) file.
# If the target does not exists or older than $(STAN_LANG),
# the create-eldoc-stan-json.el file is loaded and the following
# is executed.
# http://ergoemacs.org/emacs/elisp_running_script_in_batch_mode.html
$(ELDOC_STAN_JSON) : $(STAN_LANG) eldoc-stan-create-json.el
	$(CASK) exec $(EMACS) --batch \
	--load eldoc-stan-create-json.el \
	--eval '(eldoc-stan-create-json--write-args-json (eldoc-stan-create-json--create-function-hash-table "$(STAN_LANG)" (quote given)) "$(ELDOC_STAN_JSON)")'

# 3.3 Including Other Makefiles
# https://www.gnu.org/software/make/manual/html_node/Include.html
# If you want make to simply ignore a makefile which does not exist
# or cannot be remade, with no error message, use the -include
# directive instead of include.
-include ../common.mk
