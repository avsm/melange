OCAMLMAKEFILE= ../../OCamlMakefile
MPL=../../tools/mpl
MPLC=$(MPL)/mplc

SOURCES= udp.ml
RESULT= udp
LIBDIRS= ../ounix $(MPL)
INCDIRS= ../ounix $(MPL)
TRASH= udp.ml

all: ncl bcl

depend:
	cd ../../tools/mpl && ${MAKE} depend all
	${MPLC} udp.mpl > udp.ml

include $(OCAMLMAKEFILE)

