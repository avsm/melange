open Config_t

let network_port = "network.port"
let network_ip = "network.ip"
let zones_file = "zones.file"
let cache_mode = "cache.mode"

let vt = [
    { t_name = network_port; t_atom=T_int (0,65535);
      t_descr=Some "IPv4 port to listen on";
      t_default=Some (V_int 53); t_short=Some 'p'; t_long=Some "port" };
      
    { t_name = network_ip; t_atom=T_ip; t_descr=Some "IP addresses to listen on";
      t_default=Some (V_ip Unix.inet_addr_any);
      t_short=Some 'i'; t_long=Some "interface" };
 
    { t_name = zones_file; t_atom=T_string; t_descr=Some "Filename for zones";
      t_default=Some (V_string "default.zones");
      t_short=Some 'z'; t_long=Some "zone-list" };

    { t_name = cache_mode; t_atom=T_variant ["None"; "Weak"; "Leaky"];
      t_descr=None; t_default=Some (V_variant "None");
      t_short=Some 'm'; t_long=Some "mode" }
]

let init () =
    let c = new Config.config vt "./deens.conf" in
    c

let default () =
    prerr_endline (Config.default_config vt)
    