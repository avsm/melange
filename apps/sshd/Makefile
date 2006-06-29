# $Id$

OCAMLMAKEFILE = ../../OCamlMakefile
 
SOURCES = server.ml ssh_openssh_formats.ml sshd.ml
RESULT = sshd

# for Linux/openpty
LIBS= unix nums cryptokit str ounix mpl_stdlib spl_stdlib ssh
LIBDIRS=../../lib/ssh ../../tools/mpl ../../lib/ounix ../../tools/spl \
	../../dist/cryptokit
INCDIRS=../../lib/ssh ../../tools/mpl ../../lib/ounix ../../tools/spl \
	../../dist/cryptokit

native: nc
bytecode: bc

depend:
	cd ../../lib/ssh && $(MAKE) depend native bytecode
	cd ../../dist && $(MAKE) all


include $(OCAMLMAKEFILE)
