PYTHON ?= python3
SED ?= sed
CASK ?= cask
EMACS ?= emacs
export EMACS
EMACSFLAGS ?=
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)
EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

build : $(PKGDIR)

clean :
	rm -rf .cask/

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

