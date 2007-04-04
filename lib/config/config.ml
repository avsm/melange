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
open Config_t

exception Error of (int * string)

let do_parse ty fname =
	let fin = open_in fname in
	let lexbuf = Lexing.from_channel fin in
	try
		Config_location.start_parse fname;
		let raw_vals = Config_parser.main Config_lexer.token lexbuf in
		type_check ty raw_vals
    with
	|Config_location.Syntax_error l ->
        raise (Error (l.Config_location.line_num,(sprintf "Syntax error%s near token '%s'"
        	(Config_location.string_of_location l) (Lexing.lexeme lexbuf))))
	|Config_t.Type_error (l,str) ->
        raise (Error (l.Config_location.line_num,(sprintf "Type error%s %s"
        	(Config_location.string_of_location l) str)))

(**
  * Create a string of a default configuration file
  *)
let default_config (ty:var_types) =
    String.concat "\n" (List.flatten (List.map (fun t ->
        let comment = sprintf "# %s [%s]" (match t.t_descr with |None -> "" |Some x -> x)
            (string_of_type_atom t.t_atom) in
        let base = match t.t_default with
        |Some v -> sprintf "# %s = %s;" t.t_name (string_of_val_atom v)
        |None -> sprintf "%s = " t.t_name in
        [comment; base; ""]
    ) ty))
    
(**
  * Generate an ML file with all the variant definitions 
  *)
let generate_ml (ty:var_types) =
    (* convert x.y.z to X_y_z *)
    let ocaml_mod_of_t_name name =
        let bits = Str.split (Str.regexp_string ".") name in
        String.capitalize (String.concat "_" bits) in
    let ind y = "  " ^ y in
    String.concat "\n" (List.flatten (List.map (fun t ->
        match t.t_atom with
        |T_variant vlist |T_variant_list vlist ->
            [ sprintf "module %s = struct" (ocaml_mod_of_t_name t.t_name);
              ind (sprintf "type t = [ %s ]" (String.concat " " (List.map (fun n -> "|`" ^ n) vlist)));
              ind "let of_string (x:string) : t option = match x with"
            ] @ (List.map (fun vname ->
              ind (ind (sprintf "|\"%s\" -> Some `%s" vname vname))
            ) vlist) @ [
              ind (ind "|_ -> None");
              ind "let of_config config (key:string) = of_string (config#get_variant key)";
              "end"
            ]
        |_ -> []
    ) ty))
    
class config (ty:var_types) (fname:string) =
    let internal_error key expty =
      raise (Error (key.v_loc.Config_location.line_num,
        (sprintf "Internal error: expected type %s, found: %s" expty (string_of_var_val key)))) in
	let checked_vals = do_parse ty fname in
	object(self)
		val v = checked_vals
		method v = v
		method dump = print_endline (string_of_var_vals v)
		method get_val name =
		    try List.find (fun s -> s.v_name = name) v
            with Not_found -> raise (Error (0, (sprintf "Unknown config key %s" name)))
		method get_string name =
		    let key = self#get_val name in
		    match key.v_val with V_string str -> str | _ -> internal_error key "string"
		method get_ip name =
		    let key = self#get_val name in
		    match key.v_val with V_ip ip -> ip | _ -> internal_error key "ip"
		method get_int name =
		    let key = self#get_val name in
		    match key.v_val with V_int i -> i | _ -> internal_error key "int"
		method get_variant name =
		    let key = self#get_val name in
		    match key.v_val with V_variant i -> i | _ -> internal_error key "variant"
end
