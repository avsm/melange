SUBDIRS=dist lib tools apps

world:
	$(MAKE) clean
	$(MAKE) depend
	$(MAKE) native

clean:
	rm -f config.log config.status
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) $@); done

distclean: clean
	rm -f config.h config.mk

%:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) $@); done

install:
	@echo Not done yet, sorry!

