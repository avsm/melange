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

type type_atom =
    |T_string
    |T_string_list
    |T_int of (int * int)
    |T_int_list of (int * int)
    |T_boolean
    |T_variant of string list
    |T_variant_list of string list
    |T_unknown
	
let string_of_type_atom = function
    |T_string -> "string"
    |T_string_list -> "string list"
    |T_int (min,max) -> sprintf "integer (range %d-%d)" min max
    |T_int_list (min,max) -> sprintf "integer list (range %d->%d)]" min max
    |T_boolean -> "boolean"
    |T_variant vs -> sprintf "variant (%s)" (String.concat "|" vs)
    |T_variant_list vs -> sprintf "variant list (%s)" (String.concat "|" vs)
    |T_unknown -> raise (Internal_error "T_unknown found in config definition")

type var_type = {
    t_name: string;
    t_atom: type_atom;
    t_descr: string option;
    t_default: val_atom option;
}
and val_atom =
    |V_string of string
    |V_string_list of string list
    |V_int of int
    |V_int_list of int list
    |V_boolean of bool
    |V_variant of string
    |V_variant_list of string list
	|V_empty_list

let rec string_of_val_atom = 
    let string_of_list x = sprintf "[%s]" (String.concat ", " x) in
    function
    |V_string x -> sprintf "\"%s\"" x
    |V_string_list xl -> string_of_list 
        (List.map (fun x -> string_of_val_atom (V_string x)) xl)
    |V_int x -> sprintf "%d" x
    |V_int_list xl -> string_of_list
        (List.map (fun x -> string_of_val_atom (V_int x)) xl)
    |V_boolean x -> sprintf "%B" x
    |V_variant v -> v
    |V_variant_list vs -> string_of_list vs
	|V_empty_list -> "[]"

type var_types = var_type list

type var_val = {
    v_name: string;
    v_ty: type_atom;
    v_val: val_atom;
	v_loc: Config_location.t;
}

let string_of_var_val x =
    sprintf "name=%s ty=%s val=%s" x.v_name (string_of_type_atom x.v_ty)
        (string_of_val_atom x.v_val)

type var_vals = var_val list

let string_of_var_vals xs =
    String.concat "\n" (List.map string_of_var_val xs)

(* sanity check a var_val to make sure type and value are consistent *)
let valid_type x =
    match x.v_ty, x.v_val with
    |T_string,(V_string _)
    |T_string_list,(V_string_list _)
    |(T_int _),(V_int _) 
    |(T_int_list _),(V_int _)
    |T_boolean, (V_boolean _)
    |(T_variant _),(V_variant _)
    |(T_variant_list _),(V_variant_list _)
	|(T_int_list _), V_empty_list
	|T_string_list, V_empty_list
	|(T_variant_list _), V_empty_list
        -> true
    |_ -> false

let check_int_range loc a b v =
	if v < a || v > b then raise (Type_error (loc,
		(sprintf "Integer out of range: expected %d-%d, got %d" a b v)))

let check_variants loc is vs =
	List.iter (fun v -> if not (List.mem v is) then raise (Type_error (loc,
		(sprintf "Unknown variant '%s', expected (%s)" v (String.concat "|" is))))) vs
	
let resolve_type (ty:var_types) v =
	let id = v.v_name in
	let conf_ty = try List.find (fun t -> t.t_name = id) ty with Not_found ->
		raise (Type_error (v.v_loc, (sprintf "Unknown variable '%s'" id)))
	in
	let v = {v with v_ty=conf_ty.t_atom} in
	(* is the type the type we actually want? *)
	if not (valid_type v) then 
	    raise (Type_error (v.v_loc, (sprintf "%s: Expected %s, found %s"
	        id (string_of_type_atom v.v_ty)(string_of_val_atom v.v_val))));
	match v.v_ty,v.v_val with
    |T_int (a,b), (V_int i) -> check_int_range v.v_loc a b i; v
    |T_int (a,b), (V_int_list il) -> List.iter (check_int_range v.v_loc a b) il; v
    |T_variant t, (V_variant x) -> check_variants v.v_loc t [x]; v
    |T_variant t, (V_variant_list x) -> check_variants v.v_loc t x; v
	|T_int_list _, V_empty_list -> {v with v_val=(V_int_list [])}
	|T_string_list, V_empty_list -> {v with v_val=(V_string_list [])}
	|T_variant_list _, V_empty_list -> {v with v_val=(V_variant_list [])}
	|_,_ -> v

(* Given a type specification, fill in the value types *)
let rec type_check ty (vals:var_vals)=
	List.map (resolve_type ty) vals
