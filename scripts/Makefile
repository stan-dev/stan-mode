PYTHON = python3
## text file of Stan reference manual created with pdftotext --layout 
REFERENCE = stan-reference-2.0.1.txt

all: ../stan-keywords-lists.el

stan_lang.json: create_stan_lang.py $(REFERENCE)
	$(PYTHON) $^ > $@

../stan-keywords-lists.el: create_stan_keywords_lists.py stan_lang.json
	$(PYTHON) $^ > $@
