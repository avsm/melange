exception Bad_access
class channel :
  packet_size:int32 ->
  initial_window:int32 ->
  channel:int32 ->
  Ssh_env_t.t ->
  object
    val mutable automaton : Ssh_server_channel.t
    val mutable close : bool
    val mutable exit_status : int option
    val mutable interactive : bool
    val mutable other_channel : int32 option
    val mutable other_initial_window : int32 option
    val mutable other_packet_size : int32 option
    val mutable other_window : int32
    val our_channel : int32
    val our_initial_window : int32
    val our_packet_size : int32
    val mutable our_window : int32
    val mutable pid : int option
    val mutable pty : (Ounix.Pty.pty * Ounix.Pty.pty_window) option
    val mutable stderr : Ounix.stream_odescr option
    val mutable stdin : Ounix.stream_odescr option
    val mutable stdout : Ounix.stream_odescr option
    method automaton : Ssh_server_channel.t
    method clear_pid : unit
    method close : bool
    method close_pty : unit
    method consume_our_window : int32 -> int32 option
    method exit_status : int option
    method private opt_close : Ounix.stream_odescr option -> unit
    method other_id : int32
    method other_initial_window : int32
    method other_packet_size : int32
    method other_window : int32
    method our_id : int32
    method our_initial_window : int32
    method our_packet_size : int32
    method our_window : int32
    method pid : int option
    method pty : (Ounix.Pty.pty * Ounix.Pty.pty_window) option
    method set_close : bool -> unit
    method set_exit_status : int -> unit
    method set_other_id : int32 -> unit
    method set_other_initial_window : int32 -> unit
    method set_other_packet_size : int32 -> unit
    method set_other_window : int32 -> unit
    method set_our_window : int32 -> unit
    method set_pid : int -> unit
    method set_pty : Ounix.Pty.pty * Ounix.Pty.pty_window -> unit
    method set_stderr : Ounix.stream_odescr option -> unit
    method set_stdin : Ounix.stream_odescr option -> unit
    method set_stdout : Ounix.stream_odescr option -> unit
    method stderr : Ounix.stream_odescr option
    method stdin : Ounix.stream_odescr option
    method stdout : Ounix.stream_odescr option
    method tick_automaton : Ssh_statecalls.t -> unit
  end
type pty_req = {
  term : string;
  row : int;
  col : int;
  xpixel : int;
  ypixel : int;
  modes : string;
}
type halfchan = {
  chan : channel;
  pty : pty_req option;
  exec : string option;
  mutable confirm : bool option * bool option;
}
exception Stop_iter
class channel_env :
  object
    val chans : (int32, channel) Hashtbl.t
    val halfchans : (int32, halfchan) Hashtbl.t
    val mutable max_channel : int32
    method del_chan : int32 -> unit
    method find_by_id : int32 -> channel option
    method find_by_pid : int -> channel option
    method find_halfchan_by_id : int32 -> halfchan option
    method new_chan :
      packet_size:int32 ->
      initial_window:int32 -> Ssh_env_t.t -> channel option
    method new_half_chan :
      initial_window:int32 ->
      packet_size:int32 ->
      pty:pty_req option ->
      cmd:string option -> Ssh_env_t.t -> channel option
    method private new_id : int32
    method progress_halfchans :
      channel -> bool -> (channel * bool option * bool) option
  end
