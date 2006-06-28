# $Id$

OCAMLMAKEFILE= ../../OCamlMakefile
MPLC= ../../tools/mpl/mplc

SOURCES= dns_rr.ml dns.ml hashcons.mli hashcons.ml \
	dnsparameters.ml dnsrr.mli dnstrie.mli dnsrr.ml dnstrie.ml \
	dnsquery.ml dnsloader.ml dnsparser.mli dnslexer.mll dnsparser.mly \
	dnsserver.mli dnsserver.ml
RESULT= dns
#LIBS = unix str ounix mpl_stdlib
LIBDIRS= ../ounix ../../tools/mpl
INCDIRS= ../ounix ../../tools/mpl
TRASH=dns.ml dns_rr.ml

native: ncl
bytecode: bcl

depend:
	cd ../../tools/mpl && ${MAKE} depend all
	${MPLC} dns.mpl > dns.ml
	${MPLC} dns_rr.mpl > dns_rr.ml

include $(OCAMLMAKEFILE)
