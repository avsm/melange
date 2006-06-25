# $Id$

OCAMLMAKEFILE = ../../OCamlMakefile
 
SOURCES = server.ml ssh_openssh_formats.ml sshd.ml
RESULT = sshd
LIBS= unix nums cryptokit str ounix mpl_stdlib ssh
LIBDIRS=../../lib/ssh ../../tools/mpl ../../lib/ounix
INCDIRS=../../lib/ssh ../../tools/mpl ../../lib/ounix

all: nc

depend:
	cd ../../lib/ssh && $(MAKE) depend all


include $(OCAMLMAKEFILE)
