SOURCES= ssh_classify.ml \
	ssh_utils.ml ssh_pty.ml ssh_pty_stubs.c \
	ssh_tty.mli ssh_tty.ml ssh_algorithms.ml ssh_version.ml ssh_pool.ml \
	ssh_keys.ml ssh_kex.ml ssh_channel.mli ssh_channel.ml \
	ssh_env_t.ml ssh_transport.ml ssh_userauth.ml ssh_config.ml \
	ssh_env.ml ssh_server.ml
RESULT= ssh
SPL_AUTOGEN0=	ssh_server_automaton ssh_server_channel
SPL_STATECALL0= ssh_statecalls
MPL_FLAGS=	-s
MPL_AUTOGEN=	ssh_message
MELANGE_LIBS=	ounix
DISTS=		cryptokit

include ../../mk/lib.mk
