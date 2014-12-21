STAN_LANG = ../stan-lang/stan_lang.json

SRCS = ac-stan.el
OBJECTS = $(SRCS:.el=.elc)

$(OBJECTS) : ac-dict/stan-mode

.PHONY : build-src
build-src : ac-dict/stan-mode

ac-dict/stan-mode : create_ac_dict.py $(STAN_LANG)
	$(PYTHON) $^ $@

-include ../common.mk

