OCAMLMAKEFILE= ../../OCamlMakefile
MPL=../../tools/mpl
MPLC=$(MPL)/mplc

SOURCES= pcap_entry.ml pcap.ml
RESULT= pcap
LIBDIRS= ../ounix $(MPL)
INCDIRS= ../ounix $(MPL)
TRASH= pcap_entry.ml

all: ncl bcl

depend:
	cd ../../tools/mpl && ${MAKE} depend all
	${MPLC} pcap_entry.mpl > pcap_entry.ml

include $(OCAMLMAKEFILE)

