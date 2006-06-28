# $Id$

OCAMLMAKEFILE = ../../OCamlMakefile
 
SOURCES = server.ml ssh_openssh_formats.ml sshd.ml
RESULT = sshd

# for Linux/openpty
CLIBS+=util
LIBS= unix nums numerix cryptokit str ounix mpl_stdlib spl_stdlib ssh
LIBDIRS=../../lib/ssh ../../tools/mpl ../../lib/ounix ../../tools/spl cryptokit numerix
INCDIRS=../../lib/ssh ../../tools/mpl ../../lib/ounix ../../tools/spl +cryptokit +numerix

native: nc
bytecode: bc

depend:
	cd ../../lib/ssh && $(MAKE) depend native bytecode


include $(OCAMLMAKEFILE)
