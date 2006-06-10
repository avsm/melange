packet transport {
    packet_start: label;
    ptype: byte;
    classify (ptype) {
      |1:"Disconnect" ->
        reason_code: uint32 variant {
           |1 -> Host_not_allowed |2 -> Protocol_error
           |3 -> Kex_failed |4 -> Reserved |5 -> MAC_error
           |6 -> Compression_error |7 -> Service_not_available
           |8 -> Protocol_version_not_supported
           |9 -> Host_key_not_verifiable |10 -> Connection_lost
           |11 -> By_application |12 -> Too_many_connections
           |13 -> Auth_cancelled_by_user |14 -> No_more_auth_methods_available
           |15 -> Illegal_user_name };
        description: string32;
        language: string32;
      |2:"Ignore" ->
        data: string32;
      |3:"Unimplemented" ->
        seq_num: uint32;
      |4:"Debug" ->
        always_display: boolean;
        message: string32;
        language: string32;
      |5:"ServiceReq" ->
        stype: string32;
        classify (stype) {
          |"ssh-userauth":"UserAuth" -> ();
          |"ssh-connection":"Connection" -> ();
        };
      |6:"ServiceAccept" ->
        stype: string32;
        classify (stype) {
          |"ssh-userauth":"UserAuth" -> ();
          |"ssh-connection":"Connection" -> ();
        };
      |20:"KexInit" ->
        cookie: byte[16];
        kex_algorithms: string32;
        server_host_key_algorithms: string32;
        encryption_algorithms_client_to_server: string32;
        encryption_algorithms_server_to_client: string32;
        mac_algorithms_client_to_server: string32;
        mac_algorithms_server_to_client: string32;
        compression_algorithms_client_to_server: string32;
        compression_algorithms_server_to_client: string32;
        languages_client_to_server: string32;
        languages_server_to_client: string32;
        kex_packet_follows: boolean;
        reserved: uint32 const(0);
      |21:"NewKeys" ->
        ();
    };
    packet_end: label;
}

packet dhgroupsha1 {
	ptype: byte;
	classify (ptype) {
	  |30:"Init" ->
        e: mpint;
      |31:"Reply" ->
        k_s: string32;
        f: mpint;
        sig_h: string32;
    };
}

packet dhgexsha1 {
    ptype: byte;
    classify (ptype) {
      |34:"Request" ->
        minv: uint32;
        n: uint32;
        maxv: uint32;
      |31:"Group" ->
        p: mpint;
        g: mpint;
      |32:"Init" ->
        e: mpint;
      |33:"Reply" ->
        k_s: string32;
        f: mpint;
        sig_h: string32;
    };
}

packet auth (bool passwd_ns) {
    ptype: byte;
    classify (ptype) {
      |50:"Req" ->
        user_name: string32;
        service: string32;
        authtype: string32;
        classify (authtype) {
          |"none":"None" -> ();
          |"publickey":"PublicKey" ->
            bcheck: boolean;
            classify (bcheck) {
             |false:"Check" ->
                algorithm: string32;
                blob: string32;
             |true:"Request" ->
                algorithm: string32;
                publickey: string32;
                signature: string32;
            };
          |"password":"Password" ->
            bcheck: boolean;
            classify (bcheck) {
              |false:"Request" ->
                password: string32;
              |true:"Change" ->
                old_password: string32;
                new_password: string32;
            };
        };
      |51:"Failure" ->
        auth_continue: string32;
        partial_success: boolean;
      |52:"Success" -> ();
      |53:"Banner" ->
        banner: string32;
        language: string32;
      |60:"ChangeReq" when (passwd_ns) ->
        prompt: string32;
        language: string32;
      |60:"PublicKey_OK" when (!passwd_ns) ->
        algorithm: string32;
        blob: string32;
    };
}

packet channel (bool expecting_port) {
    ptype: byte;
    classify (ptype) {
      |80:"GlobalRequest" ->
        reqtype: string32;
        want_reply: boolean;
        bind_address: string32;
        port: uint32;
        classify (reqtype) {
          |"tcpip-forward":"TCPForward" -> ();
          |"cancel-tcpip-forward":"CancelTCPForward" -> ();
        };
      |81:"RequestSuccess" when (!expecting_port) -> ();
      |81:"RequestSuccessPort" when (expecting_port) ->
        port:uint32;
      |82:"RequestFailure" -> ();
      |90:"Open" ->
        stype: string32;
        classify (stype) {
          |"session":"Session" ->
            sender_channel: uint32;
            window_size: uint32;
            max_packet_size: uint32;
          |"forwarded-tcpip":"TCPForward" ->
            sender_channel: uint32;
            window_size: uint32;
            max_packet_size: uint32;
            origin_address: string32;
            origin_port: uint32;
          |"x11":"X11" ->
            sender_channel: uint32;
            window_size: uint32;
            max_packet_size: uint32;
            origin_address: string32;
            origin_port: uint32;
          |"direct-tcpip":"DirectTCP" ->
            sender_channel: uint32;
            window_size: uint32;
            max_packet_size: uint32;
            connect_host: string32;
            connect_port: uint32;
            origin_address: string32;
            origin_port: uint32;
        };
      |91:"OpenConfirmation" ->
        recipient_channel: uint32;
        sender_channel: uint32;
        initial_window_size: uint32;
        maximum_packet_size: uint32;
      |92:"OpenFailure" ->
        recipient_channel: uint32;
        reason_code: uint32 variant {
          |1 -> Prohibited |2 -> Connect_failed
          |3 -> Unknown_type |4 -> Resource_shortage };
        information: string32;
        language: string32;
      |93:"WindowAdjust" ->
        recipient_channel: uint32;
        bytes_to_add: uint32;
      |94:"Data" ->
        recipient_channel: uint32;
        datalen: uint32 value(sizeof(data));
        data: byte[datalen];
      |95:"ExtendedData" ->
        recipient_channel: uint32;
        data_type: uint32 variant { |1 => Stderr };
        datalen: uint32 value(sizeof(data));
        data: byte[datalen];
      |96:"EOF" ->
        recipient_channel: uint32;
      |97:"Close" ->
        recipient_channel: uint32;
      |98:"Request" ->
        recipient_channel: uint32;
        reqtype: string32;
        classify (reqtype) {
          |"pty-req":"Pty" ->
            want_reply: boolean;
            term: string32;
            width_chars: uint32;
            height_rows: uint32;
            width_pixels: uint32;
            height_pixels: uint32;
          |"x11-req":"X11" ->
            auth_cookie: string32;
            screen: uint32;
          |"shell":"Shell" ->
            want_reply: boolean;
          |"env":"Env" ->
            want_reply: boolean;
            name: string32;
            value: string32;
          |"exec":"Exec" ->
            want_reply: boolean;
            command: string32;
          |"subsystem":"Subsystem" ->
            want_reply: boolean;
            subsystem: string32;
          |"window-change":"WindowChange" ->
            reserved: boolean const(false);
            col: uint32;
            row: uint32;
            xpixel: uint32;
            ypixel: uint32;
          |"xon-xoff":"LocalFlowControl" ->
            reserved: boolean const(false);
            can_do: boolean;
          |"signal":"Signal" ->
            reserved: boolean const(false);
            signal: string32;
          |"exit-status":"ExitStatus" ->
            reserved: boolean const(false);
            exit_status: uint32;
          |"exit-signal":"ExitSignal" ->
            reserved: boolean const(false);
            signal: string32;
            core_dumped: boolean;
            error: string32;
            language: string32;
        };
      |99:"Success" ->
        recipient_channel: uint32;
      |100:"Failure" ->
        recipient_channel: uint32;
    };
}

packet ssh {
    packet_len: uint32 value(offset(padding) - offset(packet_len));
    padding_len: byte value(sizeof(padding));
    data: byte[packet_len - padding_len - 1];
    padding: byte[padding_len];
    mac_start: label;
    /* mac is added here, but must be determined from connection */
}

packet key {
    name: string32;
    classify (name) {
    |"ssh-dss":"DSS" ->
        p: mpint;
        q: mpint;
        g: mpint;
        y: mpint;
    |"ssh-rsa":"RSA" ->
        e: mpint;
        n: mpint;
    };
}

packet sig {
    name: string32;
    sig_blob: string32;
    classify (name) {
    |"ssh-dss":"DSS" -> ();
    |"ssh-rsa":"RSA" -> ();
    };
}

packet kex_hash {
    v_c: string32;
    v_s: string32;
    i_c: string32;
    i_s: string32;
    k_s: string32;
    e: mpint;
    f: mpint;
    k: mpint;
}
