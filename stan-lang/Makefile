PYTHON ?= python3
STAN_LANG = stan_lang.json
STAN_FUNCTIONS = $(wildcard stan-functions-*.txt)

all : $(STAN_LANG)

$(STAN_LANG) : create_stan_lang.py $(STAN_FUNCTIONS)
	$(PYTHON) $^ $@


