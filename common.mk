PYTHON ?= python3
SED ?= sed
CASK ?= cask
EMACS ?= emacs
export EMACS
EMACSFLAGS ?= -L $(abspath ../lib)
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)
DISTDIR = dist

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

.PHONY : compile dist deps dist clean-elc clean-deps clean-dist \
	checkdoc

compile : $(OBJECTS)

dist : 
	$(CASK) package

deps : $(PKGDIR)

clean-elc :
	rm -rf $(OBJECTS)

clean-deps :
	rm -rf .cask/

clean-dist :
	rm -rf $(DISTDIR)

checkdoc : 
	$(foreach file,$(SRCS),$(CASK) exec $(EMACSBATCH) -l checkdoc-batch.el --eval "(setq vc-handled-backends ())" -f checkdoc-batch-commandline $(file);)

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -L . -f batch-byte-compile $<
