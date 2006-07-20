SUBDIRS=dist tools lib apps

world:
	@if [ ! -f config.mk ]; then echo Run ./configure first; exit 1; fi
	$(MAKE) clean
	$(MAKE) depend
	$(MAKE) all

clean:
	rm -f config.log config.status
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) $@); done

distclean: clean
	rm -f config.h config.mk

%:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) $@); done

install:
	@echo Not done yet, sorry!

