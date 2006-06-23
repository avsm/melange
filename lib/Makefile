SUBDIRS=ounix ipv4 ethernet udp dhcp pcap ounix ssh dns

all:
%:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) $@); done

