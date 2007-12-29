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

let error_of_type_error l str =
    raise (Error (l.Config_location.line_num,(sprintf "Type error%s %s"
    	(Config_location.string_of_location l) str)))
    
(** Create a string of a default configuration file *)
let default_config (ty:var_types) =
    String.concat "\n" (List.flatten (List.map (fun t ->
        let comment = sprintf "# %s [%s]" (match t.t_descr with |None -> "" |Some x -> x)
            (string_of_type_atom t.t_atom) in
        let base = match t.t_default with
        |Some v -> sprintf "# %s = %s;" t.t_name (string_of_val_atom v)
        |None -> sprintf "%s = " t.t_name in
        [comment; base; ""]
    ) ty))
    
let print_usage (ty:var_types) =
	print_endline (default_config ty);
	exit(1)
	
(** Generate an ML file with all the variant definitions *)
let generate_ml (ty:var_types) =
    (* convert x.y.z to X_y_z *)
    let ocaml_mod_of_t_name name =
        let bits = Str.split (Str.regexp_string ".") name in
        String.capitalize (String.concat "_" bits) in
    let ind y = "  " ^ y in
    String.concat "\n" (List.flatten (
      List.map (fun t ->
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
      ) ty)
    )

(** Parse a configuration file and return a var_val list *)
let parse_config config_hash vtys fname =
	try
		let fin = open_in fname in
		let lexbuf = Lexing.from_channel fin in
		begin
		try
			Config_location.start_parse fname;
			let raw_vals = Config_parser.main Config_lexer.token lexbuf in
			let var_vals = type_check vtys raw_vals in
			List.iter (fun vval ->
				Hashtbl.add config_hash vval.v_name vval) var_vals;
    	with
		|Config_location.Syntax_error l ->
        	raise (Error (l.Config_location.line_num,(sprintf "Syntax error%s near token '%s'"
        	(Config_location.string_of_location l) (Lexing.lexeme lexbuf))))
		|Type_error (l,str) -> error_of_type_error l str
		end
	with 
	|Sys_error _ -> ()
 
let default_config_getopt_key = "default-config"
       
(** Generate help string of a list of var_vals *)
let helpopt_of_var_tys vtys =
    let help_of_vty vty =
        let opt_string = match vty.t_short,vty.t_long with
        |None,None -> failwith "Invalid config string"
        |Some c, None -> sprintf "-%c" c
        |Some c, Some s -> sprintf "--%s (-%c)" s c
        |None, Some s -> sprintf "--%s" s in
        let opt_descr = match vty.t_descr with
        |None -> ""
        |Some d -> sprintf "%s. " d in
        let opt_default = match vty.t_default with
        |None -> ""
        |Some d -> sprintf " (default: %s)" (string_of_val_atom d) in
        sprintf "%-20s %sType %s%s" opt_string opt_descr (string_of_type_atom vty.t_atom) opt_default
    in
    let help_msg () =
        print_endline "Usage:";
        print_endline (String.concat "\n" (List.map help_of_vty vtys));
		print_endline (sprintf "--%-18s %s" default_config_getopt_key "Output default configuration file to stdout");
        exit 1 in
    ('h', "help", Some help_msg, None)
    
(** Parse command line arguments into a list of var_vals *)
let parse_cmdline config_hash vtys =
	let add_config_entry vty v =
		let vval = { v_name=vty.t_name; v_ty=vty; v_val=v;
			v_loc=Config_location.cmd_location vty.t_name } in
		Hashtbl.add config_hash vty.t_name vval
	in
    try 
		(* Populate config hash with default keys *)
		List.iter (fun vty -> match vty.t_default with
			|Some v -> add_config_entry vty v
			|None -> ()
		) vtys;
		(* Register getopt handlers for each of the config vals *)
        let opts = List.fold_left (fun acc vty ->
          getopt_of_var_ty add_config_entry vty :: acc) [] vtys in
		(* Calculate the help message *)
        let opts = helpopt_of_var_tys vtys :: opts in
        (* XXX catch getopt errors *)
		let default_config = ('c', default_config_getopt_key,
			Some (fun () -> print_usage vtys), None) in
		(* Parse the command line, which populates config_hash *)
        Getopt.parse_cmdline (default_config :: opts) (fun x -> ());
    with
        |Type_error (l,str) -> error_of_type_error l str

(** Given a list of filenames, parse them in order, then parse command line
  and return complete list of variable values *)
let parse_config_and_cmdline flist vtys =
	(* Generate a hashtable to stick config keys in. Newer keys mask older ones,
	   so the order of evaluation determines which config key entry wins *)
	let config_hash = Hashtbl.create 1 in
    parse_cmdline config_hash vtys;
    List.iter (parse_config config_hash vtys) flist;
	config_hash
    
class config (ty:var_types) (fname:string) =
    let internal_error key expty =
      raise (Error (key.v_loc.Config_location.line_num,
        (sprintf "Internal error: expected type %s, found: %s" expty (string_of_var_val key)))) in
	let config_hash = parse_config_and_cmdline [fname] ty in
	object(self)
		val v = config_hash
		
		method v = v
		
		method dump =
			let keys = Hashtbl.fold (fun k v a -> if not (List.mem k a) then k :: a else a) config_hash [] in
			List.iter (fun k ->
				let vals = Hashtbl.find_all config_hash k in
				prerr_endline ("Key: " ^ k);
				List.iter (fun v -> 
					prerr_endline (sprintf "  %s -> %s%s" k (string_of_var_val v) (Config_location.string_of_location v.v_loc))
				) vals;
			) keys
			
		method get_val name =
		    try Hashtbl.find config_hash name
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
