SUBDIRS=dist lib tools apps

world:
	$(MAKE) clean
	$(MAKE) depend
	$(MAKE) native

clean:
	rm -f config.h config.log config.mk config.status
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) $@); done

%:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) $@); done

install:
	@echo Not done yet, sorry!
