PYTHON ?= python3
CASK ?= cask
EMACS ?= emacs
export EMACS
EMACSFLAGS ?=
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

STAN_LANG = ../stan-lang/stan_lang.json

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

SRCS = flycheck-stan.el
OBJECTS = $(SRCS:.el=.elc)

.PHONY : compile clean-elc 

compile: $(OBJECTS)

clean-elc :
	rm -rf $(OBJECTS)

clean-deps:
	rm -rf .cask/

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -L . -f batch-byte-compile $<

