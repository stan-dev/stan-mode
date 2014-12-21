PYTHON ?= python3
CASK ?= cask
EMACS ?= emacs
export EMACS
EMACSFLAGS ?=
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)
DISTDIR = dist

STAN_LANG = ../stan-lang/stan_lang.json

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

SRCS = stan-mode.el stan-keywords-lists.el
OBJECTS = $(SRCS:.el=.elc)

.PHONY : compile clean-elc 

compile: $(OBJECTS)

stan-keyword-lists.el: create_stan_keywords_lists.py $(STAN_LANG)
	$(PYTHON) $^ $@

dist :
	$(CASK) package

clean-elc :
	rm -rf $(OBJECTS)

clean-deps:
	rm -rf .cask/

clean-dist: 
	rm -rf $(DISTDIR)

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -L . -f batch-byte-compile $<

