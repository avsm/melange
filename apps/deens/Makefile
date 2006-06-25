# $Id$

OCAMLMAKEFILE = ../../OCamlMakefile
 
SOURCES = deens.ml
RESULT = deens
LIBS= unix str ounix mpl_stdlib dns
LIBDIRS=../../lib/dns ../../tools/mpl ../../lib/ounix
INCDIRS=../../lib/dns ../../tools/mpl ../../lib/ounix

all: nc

depend:
	cd ../../lib/dns && $(MAKE) depend all


include $(OCAMLMAKEFILE)
