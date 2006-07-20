SUBDIRS=deens sshd bonjour mldig

%:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) $@); done
