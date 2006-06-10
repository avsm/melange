# $Id$

OCAMLMAKEFILE = ../../OCamlMakefile

SPLC=../../tools/spl/splc
MPLC=../../tools/mpl/mplc

SOURCES= olog.mli olog.ml  ssh_statecalls.ml ssh_message.ml \
	ssh_classify.ml \
	ssh_server_automaton.mli ssh_server_automaton.ml \
	ssh_server_channel.mli ssh_server_channel.ml \
	ssh_utils.ml ssh_pty.ml ssh_pty_stubs.c \
	ssh_tty.mli ssh_tty.ml ssh_algorithms.ml \
	ssh_transport.ml ssh_keys.ml \
	ssh_kex.ml ssh_channel.mli ssh_channel.ml \
	ssh_userauth.ml ssh_config.ml \
	ssh_env.ml ssh_server.ml server.ml
RESULT= ssh
#LIBS= unix nums cryptokit str ounix mpl_stdlib
INCDIRS= ../ounix ../../tools/mpl
LIBDIRS= ../ounix ../../tools/mpl
SPL_SRC= ssh_server_automaton.spl ssh_server_channel.spl
TRASH=	ssh_server_automaton.ml ssh_server_automaton.mli \
	ssh_client_automaton.ml ssh_client_automaton.mli \
	ssh_server_channel.ml ssh_server_channel.mli \
	ssh_statecalls.ml

all: ncl bcl

include $(OCAMLMAKEFILE)

depend:
	cd ../ounix && ${MAKE} all
	cd ../../tools/spl && ${MAKE} all
	${SPLC} -t ocaml -s ssh_statecalls -hdir ../../tools/spl -d false ${SPL_SRC}
	cd ../../tools/mpl && ${MAKE} all
	${MPLC} -s ssh.mpl > ssh_message.ml

