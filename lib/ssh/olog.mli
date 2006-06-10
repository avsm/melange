type level = Debug | Warning | Info | Critical
class virtual base_log :
  object
    val mutable dl : level
    method virtual critical : string -> unit
    method critical_active : bool
    method virtual debug : string -> unit
    method debug_active : bool
    method virtual info : string -> unit
    method info_active : bool
    method virtual init : unit
    method set_critical : unit
    method set_debug : unit
    method set_info : unit
    method private set_level : level -> unit
    method set_warning : unit
    method virtual warning : string -> unit
    method warning_active : bool
  end
class stderr_log :
  object
    val mutable dl : level
    method critical : string -> unit
    method critical_active : bool
    method debug : string -> unit
    method debug_active : bool
    method info : string -> unit
    method info_active : bool
    method init : unit
    method set_critical : unit
    method set_debug : unit
    method set_info : unit
    method private set_level : level -> unit
    method set_warning : unit
    method warning : string -> unit
    method warning_active : bool
  end
class null_log :
  object
    val mutable dl : level
    method critical : string -> unit
    method critical_active : bool
    method debug : string -> unit
    method debug_active : bool
    method info : string -> unit
    method info_active : bool
    method init : unit
    method set_critical : unit
    method set_debug : unit
    method set_info : unit
    method private set_level : level -> unit
    method set_warning : unit
    method warning : string -> unit
    method warning_active : bool
  end
