.PHONY: stan-mode ac-mode flycheck-stan stan-snippets

all: stan-lang stan-mode ac-mode flycheck-stan stan-snippets 

stan-lang:
	make -C stan-lang

stan-mode:
	make -C stan-mode compile

flycheck-stan:
	make -C flycheck-stan compile

stan-snippets:
	make -C stan-snippets compile

ac-stan: 
	make -C ac-stan compile
