PYTHON = python3
EMACS = emacs
EMACSFLAGS =
CASK = cask
SED = sed
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

export EMACS

SRCS = stan-mode.el stan-keywords-lists.el
OBJECTS = $(SRCS:.el=.elc)

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

## text file of Stan reference manual created with pdftotext --layout 
REFERENCE = scripts/stan-reference.txt

YASNIPPETS = $(shell find snippets -type f -name "*.yasnippet")

.PHONY: all src compile dist deps build-src build-keywords build-ac build-snippets \
	clean-elc clean-dist clean-doc clean-deps

all: $(objects)

compile: $(OBJECTS) build-src

dist: build-src
	$(CASK) package

build-src: build-keywords build-ac build-snippets

build-keywords: stan-keywords-lists.el

stan-keywords-lists.el: scripts/create_stan_keywords_lists.py stan_lang.json
	$(PYTHON) $^ > $@

build-ac: ac-dict/stan-mode

ac-dict/stan-mode: scripts/create_ac_dict.py stan_lang.json
	$(PYTHON) $^ $@

build-snippets: snippets/stan-mode/.yas-compiled-snippets.el

snippets/stan-mode/.yas-compiled-snippets.el: scripts/create_snippets.py $(yasnippets) stan_lang.json
	$(PYTHON) $< stan_lang.json snippets-src
	$(EMACS) --batch -L lib -l yasnippet --eval '(yas-compile-directory "snippets-src")'
	# better for version control
	-mkdir -p snippets/stan-mode/
	$(SED) -i '/^;;; Do not edit!/d' snippets-src/stan-mode/.yas-compiled-snippets.el
	cp snippets-src/stan-mode/.yas-compiled-snippets.el snippets/stan-mode/
	cp snippets-src/stan-mode/.yas-make-groups snippets/stan-mode/
	cp snippets-src/stan-mode/.yas-parents snippets/stan-mode/

stan_lang.json: scripts/create_stan_lang.py $(REFERENCE)
	$(PYTHON) $^ $@

clean-elc: 
	rm -rf $(OBJECTS)

clean-dist: 
	rm -rf $(DIST)

clean-doc:
	rm -rf $(DOCBUILDER)

clean-deps:
	rm -rf .cask/

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -L . -f batch-byte-compile $<
