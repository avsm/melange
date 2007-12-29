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

{
open Config_location
open Config_parser
exception Eof
}

let number = ['0'-'9']+
let ipv4 = ['0'-'9']+'.'['0'-'9']+'.'['0'-'9']+'.'['0'-'9']+
let variant = ['A'-'Z']['_''a'-'z''A'-'Z''0'-'9']*
let identifier = ['a'-'z']['_''.''a'-'z''A'-'Z''0'-'9']*

rule token = parse
| [ ' ' '\t' ] { token lexbuf }
| [ '\n' ] { new_line lexbuf; token lexbuf }
| ';'   { SEMICOLON(next_token lexbuf) }
| ','   { COMMA(next_token lexbuf) }
| '['   { LBRACKET(next_token lexbuf) }
| ']'   { RBRACKET(next_token lexbuf) }
| '='   { EQUALS(next_token lexbuf) }
| "true" { TRUE(next_token lexbuf) }
| "false" { FALSE(next_token lexbuf) }
| identifier { IDENTIFIER(Lexing.lexeme lexbuf, next_token lexbuf) }
| variant { VARIANT(Lexing.lexeme lexbuf, next_token lexbuf) }
| number { INT(int_of_string (Lexing.lexeme lexbuf), next_token lexbuf) }
| ipv4 { STRING(Lexing.lexeme lexbuf, next_token lexbuf) }
| "/*" { ignore(comment lexbuf); token lexbuf }
| "//" { ignore(single_comment lexbuf); token lexbuf }
| "#" { ignore(single_comment lexbuf); token lexbuf }
| "\"" { string_e (Buffer.create 128) lexbuf }
| eof { EOF(next_token lexbuf) }

and comment = parse
| "/*" { ignore(comment lexbuf); comment lexbuf }
| "*/" { true }
| '\n' { ignore(new_line lexbuf); comment lexbuf }
| _ { comment lexbuf }

and single_comment = parse
| '\n' { new_line lexbuf; true }
| _ { single_comment lexbuf }

and string_e s = parse
| '\"' { STRING(Buffer.contents s, next_token lexbuf) }
| _ as x { Buffer.add_char s x; string_e s lexbuf }
