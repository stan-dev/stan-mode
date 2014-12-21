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

SRCS = ac-stan.el
OBJECTS = $(SRCS:.el=.elc)

all: $(objects) ac-dict/stan-mode

.PHONY : compile dist deps clean-elc clean-deps clean-dist

compile: $(OBJECTS)

dist : 
	$(CASK) package

ac-dict/stan-mode: create_ac_dict.py $(STAN_LANG)
	$(PYTHON) $^ $@

clean-elc : 
	rm -rf $(OBJECTS)

clean-deps :
	rm -rf .cask/

clean-dist :
	rm -rf $(DISTDIR)

deps : $(PKGDIR)

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -L . -f batch-byte-compile $<

