# $Id$

OCAMLMAKEFILE = ../../OCamlMakefile

depend:
	cd ../../lib/dns && LIBS= $(MAKE) depend native bytecode

 
SOURCES = deens.ml
RESULT = deens
LIBS= unix str ounix mpl_stdlib dns
LIBDIRS=../../lib/dns ../../tools/mpl ../../lib/ounix
INCDIRS=../../lib/dns ../../tools/mpl ../../lib/ounix

native: nc
bytecode: bc


include $(OCAMLMAKEFILE)
