OCAMLMAKEFILE= ../../OCamlMakefile
MPL=../../tools/mpl
MPLC=$(MPL)/mplc

SOURCES= dhcp.ml dhcplib.ml
RESULT= dhcp
LIBDIRS= ../ounix $(MPL)
INCDIRS= ../ounix $(MPL)
TRASH= dhcp.ml

native: ncl
bytecode: bcl

depend:
	cd ../../tools/mpl && ${MAKE} depend all
	${MPLC} dhcp.mpl > dhcp.ml

include $(OCAMLMAKEFILE)

