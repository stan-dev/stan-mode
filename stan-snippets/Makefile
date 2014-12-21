STAN_LANG = ../stan-lang/stan_lang.json

SRC_SNIPPETS_DIR = src
SNIPPETS = $(shell find $(SRC_SNIPPETS_DIR) -type f -name "*.yasnippet")
COMPILED_SNIPPETS = snippets/stan-mode/.yas-compiled-snippets.el

SRCS = stan-snippets.el $(COMPILED_SNIPPETS)
OBJECTS = $(SRCS:.el=.elc)

-include ../common.mk

.PHONY : snippets
snippets : $(COMPILED_SNIPPETS)

$(COMPILED_SNIPPETS) : create_snippets.py $(STAN_LANG) $(SNIPPETS)
	$(PYTHON) $< $(STAN_LANG) $(SRC_SNIPPETS_DIR)
	$(CASK) exec $(EMACS) --batch -l yasnippet --eval '(yas-compile-directory "$(SRC_SNIPPETS_DIR)")'
	# better for version control
	-mkdir -p snippets/stan-mode/
	$(SED) -i '/^;;; Do not edit!/d' $(SRC_SNIPPETS_DIR)/stan-mode/.yas-compiled-snippets.el
	cp $(SRC_SNIPPETS_DIR)/stan-mode/.yas-compiled-snippets.el $(COMPILED_SNIPPETS)
	cp $(SRC_SNIPPETS_DIR)/stan-mode/.yas-make-groups snippets/stan-mode/
	cp $(SRC_SNIPPETS_DIR)/stan-mode/.yas-parents snippets/stan-mode/


