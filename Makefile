PYTHON = python3
SED = sed

CASK = cask
EMACS = emacs
EMACSFLAGS = 
EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS = stan-keywords-lists.el stan-mode.el  stan-snippets.el
OBJECTS = $(SRCS:%.el=%.elc)

## text file of Stan reference manual created with pdftotext --layout 
REFERENCE = scripts/stan-reference.txt

yasnippets = $(shell find snippets -type f -name "*.yasnippet")

all: stan-keywords-lists.el ac-dict/stan-mode snippets/stan-mode/.yas-compiled-snippets.el

stan_lang.json: scripts/create_stan_lang.py $(REFERENCE)
	$(PYTHON) $^ $@
stan-keywords-lists.el: scripts/create_stan_keywords_lists.py stan_lang.json
	$(PYTHON) $^ > $@

ac-dict/stan-mode: scripts/create_ac_dict.py stan_lang.json
	$(PYTHON) $^ $@

snippets/stan-mode/.yas-compiled-snippets.el: scripts/create_snippets.py $(yasnippets) stan_lang.json
	$(PYTHON) $< stan_lang.json snippets-src
	$(EMACS) --batch -L lib -l yasnippet --eval '(yas-compile-directory "snippets-src")'
	# better for version control
	-mkdir -p snippets/stan-mode/
	$(SED) -i '/^;;; Do not edit!/d' snippets-src/stan-mode/.yas-compiled-snippets.el
	cp snippets-src/stan-mode/.yas-compiled-snippets.el snippets/stan-mode/
	cp snippets-src/stan-mode/.yas-make-groups snippets/stan-mode/
	cp snippets-src/stan-mode/.yas-parents snippets/stan-mode/

snippets: snippets/stan-mode/.yas-compiled-snippets.el

.PHONY: snippets

compile : $(OBJECTS)

deps : $(PKGDIR)

clean-elc :
	rm -rf $(OBJECTS)

clean-deps :
	rm -rf .cask/

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -L . -f batch-byte-compile $<
