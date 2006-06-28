(** Set or unset the TCP_NODELAY flag on a fd *)
val set_tcp_nodelay : Unix.file_descr -> bool -> unit

(** Set the multicast TTL on an fd to x *)
val set_ip_multicast_ttl : Unix.file_descr -> int -> unit

(** Set the IP_MULTICAST_LOOP on an fd to x *)
val set_ip_multicast_loop : Unix.file_descr -> int -> unit

(** Add the fd to the multicast group with address addr *)
val join_multicast_group : Unix.file_descr -> Unix.inet_addr -> unit

class type odescr =
  object
    method close : unit
    method exception_ready : unit
    method fd : Unix.file_descr
    method read_ready : unit
    method write_ready : unit
  end
class stream_odescr :
  ?rfn:(stream_odescr -> unit) ->
  ?wfn:(stream_odescr -> unit) ->
  ?efn:(stream_odescr -> unit) ->
  Unix.file_descr ->
  object
    method close : unit
    method exception_ready : unit
    method fd : Unix.file_descr
    method in_channel : in_channel
    method out_channel : out_channel
    method read : string -> int -> int -> unit
    method read_buf : int -> string
    method read_ready : unit
    method write : string -> int -> int -> unit
    method write_buf : string -> unit
    method write_ready : unit
  end
class tcp_odescr :
  ?rfn:(stream_odescr -> unit) ->
  ?wfn:(stream_odescr -> unit) ->
  ?efn:(stream_odescr -> unit) ->
  Unix.file_descr ->
  object
    method close : unit
    method exception_ready : unit
    method fd : Unix.file_descr
    method in_channel : in_channel
    method out_channel : out_channel
    method read : string -> int -> int -> unit
    method read_buf : int -> string
    method read_ready : unit
    method set_nodelay : bool -> unit
    method write : string -> int -> int -> unit
    method write_buf : string -> unit
    method write_ready : unit
  end
class dgram_odescr :
  ?rfn:(dgram_odescr -> unit) ->
  ?wfn:(dgram_odescr -> unit) ->
  ?efn:(dgram_odescr -> unit) ->
  Unix.file_descr ->
  object
    method close : unit
    method exception_ready : unit
    method fd : Unix.file_descr
    method read_ready : unit
    method recvfrom :
      string -> int -> int -> Unix.msg_flag list -> int * Unix.sockaddr
    method sendto :
      string -> int -> int -> Unix.msg_flag list -> Unix.sockaddr -> int
    method write_ready : unit
  end
val udp_listener :
  ?interface:Unix.inet_addr ->
  ?rfn:(dgram_odescr -> unit) ->
  ?wfn:(dgram_odescr -> unit) ->
  ?efn:(dgram_odescr -> unit) -> int -> dgram_odescr
val tcp_listener :
  ?interface:Unix.inet_addr ->
  ?rfn:(stream_odescr -> unit) ->
  ?wfn:(stream_odescr -> unit) ->
  ?efn:(stream_odescr -> unit) -> int -> tcp_odescr
class oselect :
  object
    method add_ofd : odescr -> unit
    method read : unit
    method remove_ofd : odescr -> unit
  end
class osignal :
  object
    method add_sigchld_handler : (unit -> unit) -> unit
    method add_sighup_handler : (unit -> unit) -> unit
    method process : unit
  end
