open Config_t

let network_port = "network.port"
let network_ip = "network.ip"
let zones_file = "zones.file"
let cache_mode = "cache.mode"

let vt = [
    { t_name = network_port; t_atom=T_int (0,65535);
      t_descr=Some "IPv4 port to listen on";
      t_default=Some (V_int 53) };
      
    { t_name = network_ip; t_atom=T_ip; t_descr=Some "IP addresses to listen on";
      t_default=Some (V_ip Unix.inet_addr_any); };
 
    { t_name = zones_file; t_atom=T_string; t_descr=Some "Filename for zones";
      t_default=Some (V_string "default.zones"); };

    { t_name = cache_mode; t_atom=T_variant ["None"; "Weak"; "Leaky"];
      t_descr=None; t_default=Some (V_variant "None") }
]

let init () =
    let c = new Config.config vt "./deens.conf" in
    c

let default () =
    prerr_endline (Config.default_config vt)
    