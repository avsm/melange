OCAMLMAKEFILE= ../../OCamlMakefile
MPL=../../tools/mpl
MPLC=$(MPL)/mplc

SOURCES= ipv4.ml
RESULT= ipv4
LIBDIRS= ../ounix $(MPL)
INCDIRS= ../ounix $(MPL)
TRASH= ipv4.ml

native: ncl
bytecode: bcl

depend:
	cd ../../tools/mpl && ${MAKE} depend all
	${MPLC} ipv4.mpl > ipv4.ml

include $(OCAMLMAKEFILE)

