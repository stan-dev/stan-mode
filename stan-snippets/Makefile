SHELL = bash
STAN_LANG = ../stan-language-definitions/stan_lang.json
SED = sed

SNIPPETS_DIR = snippets
SNIPPETS = $(shell find $(SNIPPETS_DIR) -type f -name "*.yasnippet")
COMPILED_SNIPPETS = $(SNIPPETS_DIR)/stan-mode/.yas-compiled-snippets.el

SRCS = stan-snippets.el
OBJECTS = $(SRCS:.el=.elc)

-include ../common.mk

.PHONY : build-src

build-src : build-snippets

# This will run every time since deps touches the PKGDIR
build-snippets :
	$(PYTHON) create_snippets.py $(STAN_LANG) $(SNIPPETS_DIR)
	$(CASK) exec $(EMACS) --batch -l yasnippet --eval '(yas-compile-directory "$(SNIPPETS_DIR)")'
	# better for version control to remove dates
	# gnu-sed and unix sed have different options
	if $(SED) --version  > /dev/null; \
	then $(SED) -i -e '/^;;; Do not edit!/d' $(COMPILED_SNIPPETS); \
	else $(SED) -i .bak -e '/^;;; Do not edit!/d' $(COMPILED_SNIPPETS) ; \
	fi

.PHONY: $(COMPILED_SNIPPETS)
