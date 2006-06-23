SUBDIRS=deens sshd

all:
%:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) $@); done
