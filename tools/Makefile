SUBDIRS=mpl spl

all:
%:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) $@); done
