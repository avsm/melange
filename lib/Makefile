SUBDIRS=ounix ipv4 ethernet udp dhcp pcap ounix ssh dns icmp config
BUILD_ROOT=..

all:
%:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) $@); done

