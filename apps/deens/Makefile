# $Id$

OCAMLMAKEFILE = ../../OCamlMakefile
 
SOURCES = deens.ml
RESULT = deens
LIBS= unix str ounix mpl_stdlib dns
LIBDIRS=../../lib/dns ../../tools/mpl ../../lib/ounix
INCDIRS=../../lib/dns ../../tools/mpl ../../lib/ounix

depend:
	cd ../../lib/dns && $(MAKE) depend all

all: nc

include $(OCAMLMAKEFILE)
