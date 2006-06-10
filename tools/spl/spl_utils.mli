val hashtbl_add_list : ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit
val list_unique : 'a list -> 'a list
val safe_chop : string -> string
val list_filter_map : ('a -> 'b option) -> 'a list -> 'b list
  val may : 'a -> ('b -> 'a) -> 'b option -> 'a
val string_of_file : string -> string

module Logger : sig
    type level = Quiet | Normal | Verbose
    val set_log_level : level -> unit
    val log_quiet : string -> unit
    val log : string -> unit
    val log_verbose : string -> unit
end

module Printer : sig
    type env = {
      fn : int -> string -> unit;
      p : string -> unit;
      i : int;
      nl : unit -> unit;
    }
    val indent : env -> env
    val indent_fn : env -> (env -> 'a) -> 'a
    val list_iter_indent : env -> (env -> 'a -> unit) -> 'a list -> unit
    val hashtbl_iter_indent :
      env -> (env -> 'a -> 'b -> unit) -> ('a, 'b) Hashtbl.t -> unit
    val init_printer : ?header:bool -> ?comment:string * string -> out_channel -> env
end
