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
   	let () = Config_location.start_parse fname in
	let raw_vals = Config_parser.main Config_lexer.token lexbuf in
	type_check ty raw_vals
    with
	|Config_location.Syntax_error l ->
        raise (Error (l.Config_location.line_num,(sprintf "Syntax error%s near token '%s'"
        	(Config_location.string_of_location l) (Lexing.lexeme lexbuf))))
	|Config_t.Type_error (l,str) ->
        raise (Error (l.Config_location.line_num,(sprintf "Type error%s %s"
        	(Config_location.string_of_location l) str)))

class config (ty:var_types) (fname:string) =
	let checked_vals = do_parse ty fname in
	object(self)
		val v = checked_vals
		method v = v
		method dump = print_endline (string_of_var_vals v)
end
