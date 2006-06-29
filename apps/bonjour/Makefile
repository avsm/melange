# $Id$

OCAMLMAKEFILE = ../../OCamlMakefile

depend:
	cd ../../lib/dns && LIBS= $(MAKE) depend native bytecode

 
SOURCES = bonjour_app.ml
RESULT = bonjour
LIBS= unix str ounix mpl_stdlib dns
LIBDIRS=../../lib/dns ../../tools/mpl ../../lib/ounix
INCDIRS=../../lib/dns ../../tools/mpl ../../lib/ounix

native: nc
bytecode: bc


include $(OCAMLMAKEFILE)
