packet pcap_entry {
	sec: uint32;
	usec: uint32;
	caplen: uint32 value(sizeof(data));
	reallen: uint32;
	data: byte[caplen];
}
