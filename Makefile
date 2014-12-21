PACKAGES := stan-lang stan-mode ac-mode flycheck-stan stan-snippets

build-stan-lang :
	make -C stan-lang

build : build-stan-lang
	$(foreach pkg,$(PACKAGES),make -C $(pkg) build-src ; )

compile : build
	$(foreach pkg,$(PACKAGES),make -C $(pkg) compile ; )

dist : 
	$(foreach pkg,$(PACKAGES),make -C $(pkg) dist ; )

