SUBDIR=cryptokit

all:
%:
	for i in $(SUBDIR); do (cd $$i && $(MAKE) -f Makefile.wrapper $@); done
