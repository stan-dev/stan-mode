.PHONY: stan-mode ac-mode flycheck-stan stan-snippets

all: stan-mode ac-mode flycheck-stan stan-snippets

stan-mode:
	make -C stan-mode compile

flycheck-stan:
	make -C flycheck-stan compile

stan-snippets:
	make -C stan-snippets compile

ac-stan:
	make -C ac-stan compile
