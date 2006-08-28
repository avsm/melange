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
    |T_string_list -> "[string]"
    |T_int (min,max) -> sprintf "int(%d-%d)" min max
      (* XXX check range *)
    |T_int_list (min,max) -> sprintf "[int(%d->%d)]" min max
      (* XXX check ranges *)
    |T_boolean -> "bool"
    |T_variant vs -> "("^ (String.concat "| " vs) ^ ")"
    |T_variant_list vs -> "[("^ (String.concat "| " vs) ^ ")]"
    |T_unknown -> "?"
    
type var_type = {
    c_name: string;
    c_ty: type_atom;
}

let string_of_var_type x =
    sprintf "name=%s ty=%s" x.c_name (string_of_type_atom x.c_ty)

type var_types = var_type list

type val_atom =
    |V_string of string
    |V_string_list of string list
    |V_int of int
    |V_int_list of int list
    |V_boolean of bool
    |V_variant of string
    |V_variant_list of string list
	|V_empty_list

let string_of_list x = sprintf "[%s]" (String.concat ", " x)
    
let rec string_of_val_atom = function
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

type var_val = {
    v_name: string;
    v_ty: type_atom;
    v_val: val_atom;
}

let string_of_var_val x =
    sprintf "name=%s ty=%s val=%s" x.v_name (string_of_type_atom x.v_ty)
        (string_of_val_atom x.v_val)

type var_vals = var_val list

let string_of_var_vals xs =
    String.concat "\n" (List.map string_of_var_val xs)

(* sanity check a var_var to make sure type and value are consistent *)
let valid_type x =
    match x.v_ty, x.v_val with
    |T_string,(V_string _)
    |T_string_list,(V_string_list _)
    |(T_int _),(V_int _) 
    |(T_int_list _),(V_int _)
    |T_boolean, (V_boolean _)
    |(T_variant _),(V_variant _)
    |(T_variant_list _),(V_variant_list _)
        -> true
    |_ -> false

class config = object(self)
    
end

