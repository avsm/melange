OCAMLMAKEFILE= ../../OCamlMakefile
MPL=../../tools/mpl
MPLC=$(MPL)/mplc

SOURCES= ethernet.ml
RESULT= ethernet
LIBDIRS= ../ounix $(MPL)
INCDIRS= ../ounix $(MPL)
TRASH= ethernet.ml

all: ncl bcl

depend:
	cd ../../tools/mpl && ${MAKE} depend all
	${MPLC} ethernet.mpl > ethernet.ml

include $(OCAMLMAKEFILE)

