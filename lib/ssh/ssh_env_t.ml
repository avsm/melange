type t = {
    fd: Ounix.tcp_odescr;
    log: Olog.base_log;
    rng: Cryptokit.Random.rng;
    osel: Ounix.oselect;
    kex_methods: Ssh_kex.Methods.t list;
    mac_methods: Ssh_algorithms.MAC.t list;
    cipher_methods: Ssh_algorithms.Cipher.t list;
    hostkey_algorithms: Ssh_keys.PublicKey.t list;
    debugger: bool;  (* Is the SPL debugger active? *)
}

