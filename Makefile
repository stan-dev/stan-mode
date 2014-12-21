PACKAGES := stan-lang stan-mode ac-mode stan-snippets

build-stan-lang :
	make -C stan-lang

build : build-stan-lang
	$(foreach pkg,$(PACKAGES),make -C $(pkg) build-src ; )

compile : build
	$(foreach pkg,$(PACKAGES),make -C $(pkg) compile ; )

dist : 
	$(foreach pkg,$(PACKAGES),make -C $(pkg) dist ; )

clean : clean-dist clean-deps clean-elc

clean-dist : 
	$(foreach pkg,$(PACKAGES),make -C $(pkg) clean-dist ; )

clean-deps : 
	$(foreach pkg,$(PACKAGES),make -C $(pkg) clean-deps ; )

clean-elc : 
	$(foreach pkg,$(PACKAGES),make -C $(pkg) clean-elc ; )
