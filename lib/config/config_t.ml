(*
 * Copyright (c) 2006 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Printf

exception Type_error of (Config_location.t * string)
exception Internal_error of string

(** Type of the configuration field contents *)
type type_atom =
    |T_string
    |T_string_list
    |T_int of (int * int)
    |T_int_list of (int * int)
    |T_ip
    |T_ip_list
    |T_boolean
    |T_variant of string list
    |T_variant_list of string list
    |T_unknown

(** Variant type to hold the actual contents of a configuration field *)
type val_atom =
    |V_string of string
    |V_string_list of string list
    |V_int of int
    |V_ip of Unix.inet_addr
    |V_ip_list of Unix.inet_addr list
    |V_int_list of int list
    |V_boolean of bool
    |V_variant of string
    |V_variant_list of string list
    |V_empty_list

(** Record describing a configuration field *)
type var_type = {
    t_name: string;
    t_atom: type_atom;
    t_descr: string option;
    t_default: val_atom option;
    t_short: char option; (** short command line switch e.g. 'p' for -p 1234 *)
    t_long: string option; (** long command line switch e.g. 'port' for --port 1234 *)
}

type var_types = var_type list

type var_val = {
    v_name: string;
    v_ty: var_type;
    v_val: val_atom;
    v_loc: Config_location.t;
}

type var_vals = var_val list

(** Dummy var type, replaced after type checking by something sensible *)
let null_var_type = {
    t_name="(null)"; t_atom=T_unknown; t_descr=None; t_default=None;
    t_short=None; t_long=None
}
    
(** Convert a type_atom to a human-readable string *)
let string_of_type_atom = function
    |T_string -> "string"
    |T_string_list -> "string list"
    |T_int (min,max) -> sprintf "integer (range %d-%d)" min max
    |T_int_list (min,max) -> sprintf "integer list (range %d->%d)]" min max
    |T_ip -> "ip"
    |T_ip_list -> "ip list"
    |T_boolean -> "boolean"
    |T_variant vs -> sprintf "variant (%s)" (String.concat "|" vs)
    |T_variant_list vs -> sprintf "variant list (%s)" (String.concat "|" vs)
    |T_unknown -> raise (Internal_error "T_unknown found in config definition")

(** Convert a var_val to a human-readable string *)
let rec string_of_val_atom = 
    let string_of_list fn xl =
        sprintf "[%s]" (String.concat ", " 
            (List.map (fun x -> string_of_val_atom (fn x)) xl)
        ) in
    function
    |V_string x -> sprintf "\"%s\"" x
    |V_ip x -> sprintf "%s" (Unix.string_of_inet_addr x)
    |V_ip_list xl -> string_of_list (fun x -> V_ip x) xl
    |V_string_list xl -> string_of_list  (fun x -> V_string x) xl
    |V_int x -> sprintf "%d" x
    |V_int_list xl -> string_of_list (fun x -> V_int x) xl
    |V_boolean x -> sprintf "%B" x
    |V_variant v -> v
    |V_variant_list xl -> string_of_list (fun x -> V_variant x) xl
    |V_empty_list -> "[]"

(** Convert a var_type to a human-readable string *)
let string_of_var_type vty =
    let string_of_opt fn = function |None -> "(none)" |Some x -> fn x in
    sprintf "[name=%s; type=%s; descr=%s; default=%s; short=%s; long=%s]"
      vty.t_name (string_of_type_atom vty.t_atom) (string_of_opt (fun x -> x) vty.t_descr)
      (string_of_opt string_of_val_atom vty.t_default)
      (string_of_opt (fun x -> sprintf "%c" x) vty.t_short)
      (string_of_opt (fun x -> x) vty.t_long)

(** Convert a var_val to a human-readable string *)
let string_of_var_val x =
    sprintf "name=%s ty=%s val=%s" x.v_name (string_of_var_type x.v_ty)
        (string_of_val_atom x.v_val)

(** Convert a list of var_val to a human-readable string *)
let string_of_var_vals xs =
    String.concat "\n" (List.map string_of_var_val xs)

(* sanity check a var_val to make sure type and value are consistent *)
let valid_type x =
    match x.v_ty.t_atom, x.v_val with
    |T_string,(V_string _)
    |T_string_list,(V_string_list _)
    |(T_int _),(V_int _) 
    |(T_int_list _),(V_int _)
    |T_boolean, (V_boolean _)
    |T_ip, (V_string _)
    |T_ip_list, (V_string_list _)
    |(T_variant _),(V_variant _)
    |(T_variant_list _),(V_variant_list _)
    |(T_int_list _), V_empty_list
    |T_string_list, V_empty_list
    |(T_variant_list _), V_empty_list
    |T_ip_list, V_empty_list
        -> true
    |_ -> false

let check_int_range loc a b v =
    if v < a || v > b then raise (Type_error (loc,
        (sprintf "Integer out of range: expected %d-%d, got %d" a b v)))

let check_variants loc is vs =
    List.iter (fun v -> if not (List.mem v is) then raise (Type_error (loc,
        (sprintf "Unknown variant '%s', expected (%s)" v (String.concat "|" is))))) vs
    
let resolve_type (ty:var_types) (v:var_val) =
    let id = v.v_name in
    let conf_ty = try List.find (fun t -> t.t_name = id) ty with Not_found ->
        raise (Type_error (v.v_loc, (sprintf "Unknown variable '%s'" id)))
    in
    let v = {v with v_ty=conf_ty} in
    let t_atom = v.v_ty.t_atom in
    (* is the type the type we actually want? *)
    if not (valid_type v) then 
        raise (Type_error (v.v_loc, (sprintf "%s: Expected %s, found %s"
            id (string_of_type_atom t_atom)(string_of_val_atom v.v_val))));
    match t_atom,v.v_val with
    |T_int (a,b), (V_int i) -> check_int_range v.v_loc a b i; v
    |T_int (a,b), (V_int_list il) -> List.iter (check_int_range v.v_loc a b) il; v
    |T_variant t, (V_variant x) -> check_variants v.v_loc t [x]; v
    |T_variant t, (V_variant_list x) -> check_variants v.v_loc t x; v
    |T_ip, (V_string x) -> {v with v_val=V_ip (Unix.inet_addr_of_string x)}
    |T_ip_list, (V_string_list x) -> {v with v_val=V_ip_list
        (List.map Unix.inet_addr_of_string x)}
    |T_int_list _, V_empty_list -> {v with v_val=(V_int_list [])}
    |T_string_list, V_empty_list -> {v with v_val=(V_string_list [])}
    |T_variant_list _, V_empty_list -> {v with v_val=(V_variant_list [])}
    |T_ip_list, V_empty_list -> {v with v_val=(V_ip_list [])}
    |_,_ -> v

(** Parse a getopt command line string to a var_atom *) 
let cmd_string_to_val_atom str vty =
    let loc = Config_location.cmd_location vty.t_name in
    match vty.t_atom with 
    |T_string -> V_string str
    |T_int (a,b) -> begin
        let i = try int_of_string str with _ ->
            raise (Type_error (loc, sprintf "Not a valid int: %s" str)) in
        check_int_range loc a b i;
        V_int i
    end
    |T_ip -> begin
        let ip = try Unix.inet_addr_of_string str with _ ->
            raise (Type_error (loc, sprintf "Not a valid IP: %s" str)) in
        V_ip ip
    end
    |T_variant vs -> begin
        try
          V_variant (List.find ((=) str) vs)
        with
          Not_found -> raise (Type_error (loc, sprintf "Not a valid variant '%s', must be [%s]" str (String.concat " or " vs)))
    end
    |_ -> failwith "getopt_to_var_type: not finished yet"

(** Turn a var_val into a getopt entry *)
let getopt_of_var_ty add_config_fn vty =
    let shortval = match vty.t_short with |None -> Getopt.noshort |Some c -> c in
    let longval = match vty.t_long with |None -> Getopt.nolong |Some s -> s in
    let action = None in (* No default actions yet *)
    let handler = Some (fun str ->
        add_config_fn vty (cmd_string_to_val_atom str vty)
    ) in
    (shortval, longval, action, handler)
   
(** Given a type specification, fill in the value types *)
let rec type_check ty (vals:var_vals) =
    List.map (resolve_type ty) vals
