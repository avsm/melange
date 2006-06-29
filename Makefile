SUBDIRS=dist lib tools apps

world:
	$(MAKE) clean
	$(MAKE) depend
	$(MAKE) native


%:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) $@); done

install:
	@echo Not done yet, sorry!
