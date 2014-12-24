STAN_LANG = ../stan-lang/stan_lang.json

SRCS = stan-mode.el stan-keywords-lists.el
OBJECTS = $(SRCS:.el=.elc)

.PHONY : build-src

build-src : stan-keywords-lists.el

stan-keywords-lists.el : create_stan_keywords_lists.py $(STAN_LANG)
	$(PYTHON) $^ $@

-include ../common.mk
