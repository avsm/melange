packet ethernet {
    dest_mac: byte[6];
    src_mac: byte[6];
    length: uint16 value(offset(end_of_packet)-offset(length));
    classify (length) {
        |46..1500:"E802_2" ->
            data: byte[length];
        |0x800:"IPv4" ->
            data: byte[remaining()];
        |0x806:"Arp" ->
            data: byte[remaining()];
        |0x86dd:"IPv6" ->
            data: byte[remaining()];
    };
    end_of_packet: label;
}
