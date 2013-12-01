PYTHON = python3
EMACS = emacs
## text file of Stan reference manual created with pdftotext --layout 
REFERENCE = scripts/stan-reference-2.0.1.txt

yasnippets = $(shell find snippets -type f -name "*.yasnippet")

all: stan-keywords-lists.el snippets/stan-mode/.yas-compiled-snippets.el

stan_lang.json: scripts/create_stan_lang.py $(REFERENCE)
	$(PYTHON) $^ > $@

stan-keywords-lists.el: scripts/create_stan_keywords_lists.py stan_lang.json
	$(PYTHON) $^ > $@

snippets/stan-mode/.yas-compiled-snippets.el: $(yasnippets) stan_lang.json
	$(PYTHON) scripts/create_snippets.py stan_lang.json snippets
	$(EMACS) --batch -L lib -l yasnippet --eval '(yas-compile-directory "snippets")'
	# better for version control
	$(SED) -i '/^;;; Do not edit!/d' $@

.PHONY: snippets
