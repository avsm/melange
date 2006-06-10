# $Id$

OCAMLMAKEFILE = ../../OCamlMakefile
 
SOURCES = ssh_openssh_formats.ml sshd.ml
RESULT = sshd
LIBS= unix nums cryptokit str ounix mpl_stdlib ssh
LIBDIRS=../../lib/ssh ../../tools/mpl ../../lib/ounix
INCDIRS=../../lib/ssh ../../tools/mpl ../../lib/ounix

depend:
	cd ../../lib/ssh && $(MAKE) depend all

all: nc

include $(OCAMLMAKEFILE)
