STAN_LANG = ../stan-lang/stan_lang.json

SRCS = stan-mode.el stan-keywords-lists.el
OBJECTS = $(SRCS:.el=.elc)

stan-keyword-lists.el: create_stan_keywords_lists.py $(STAN_LANG)
	$(PYTHON) $^ $@

-include ../common.mk
