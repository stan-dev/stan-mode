PYTHON = python3
EMACS = emacs
## text file of Stan reference manual created with pdftotext --layout 
REFERENCE = scripts/stan-reference-2.0.1.txt

all: stan-keywords-lists.el snippets

stan_lang.json: scripts/create_stan_lang.py $(REFERENCE)
	$(PYTHON) $^ > $@

stan-keywords-lists.el: scripts/create_stan_keywords_lists.py stan_lang.json
	$(PYTHON) $^ > $@

snippets:
	$(PYTHON) scripts/create_snippets.py stan_lang.json snippets
	$(EMACS) --batch -L lib -l yasnippet --eval '(yas-compile-directory "snippets")'

.PHONY: snippets
