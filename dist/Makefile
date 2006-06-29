SUBDIR=cryptokit getopt

all:
%:
	for i in $(SUBDIR); do (cd $$i && $(MAKE) -f Makefile.wrapper $@); done
