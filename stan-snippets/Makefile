STAN_LANG = ../stan-lang/stan_lang.json

SNIPPETS_DIR = snippets
SNIPPETS = $(shell find $(SNIPPETS_DIR) -type f -name "*.yasnippet")
COMPILED_SNIPPETS = $(SNIPPETS_DIR)/stan-mode/.yas-compiled-snippets.el

SRCS = stan-snippets.el
OBJECTS = $(SRCS:.el=.elc)

-include ../common.mk

.PHONY : build-src

build-src : $(COMPILED_SNIPPETS)

# This will run every time since deps touches the PKGDIR
$(COMPILED_SNIPPETS) : create_snippets.py $(STAN_LANG) $(SNIPPETS) deps
	$(PYTHON) $< $(STAN_LANG) $(SNIPPETS_DIR)
	$(CASK) exec $(EMACS) --batch -l yasnippet --eval '(yas-compile-directory "$(SNIPPETS_DIR)")'
	# better for version control
	$(SED) -i '/^;;; Do not edit!/d' $(COMPILED_SNIPPETS)


