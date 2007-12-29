(*
 * Copyright (c) 2005 Anil Madhavapeddy <anil@recoil.org>
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
 * $Id: spl_lexer.mll,v 1.12 2005/04/24 18:05:48 avsm Exp $
 *)

{
open Spl_location
open Spl_parser
exception Eof
}

let number = ['0'-'9']+
let identifier = ['a'-'z']['_''a'-'z''A'-'Z''0'-'9']*
let statecall = ['A'-'Z']['_''a'-'z''A'-'Z''0'-'9']*

rule token = parse
| [ ' ' '\t' ] { token lexbuf }
| [ '\n' ] { new_line lexbuf; token lexbuf }
| '{'   { LBRACE(next_token lexbuf) }
| '}'   { RBRACE(next_token lexbuf) }
| '('   { LBRACKET(next_token lexbuf) }
| ')'   { RBRACKET(next_token lexbuf) }
| ';'   { SEMICOLON(next_token lexbuf) }
| ','   { COMMA(next_token lexbuf) }
| "="   { ASSIGN(next_token lexbuf) }
| "function" { FUNCTION(next_token lexbuf) }
| "automaton" { AUTOMATON(next_token lexbuf) }
| "multiple" { MULTIPLE(next_token lexbuf) }
| "always_allow" { ALWAYS_ALLOW(next_token lexbuf) }
| "optional" { OPTIONAL(next_token lexbuf) }
| "either" { EITHER(next_token lexbuf) }
| "or" { OR(next_token lexbuf) }
| "if" { IF(next_token lexbuf) }
| "during" { DURING(next_token lexbuf) }
| "handle" { HANDLE(next_token lexbuf) }
| "do" { DO(next_token lexbuf) }
| "until" { UNTIL(next_token lexbuf) }
| "while" { WHILE(next_token lexbuf) }
| "true" { TRUE(next_token lexbuf) }
| "false" { FALSE(next_token lexbuf) }
| "exit" { EXIT(next_token lexbuf) }
| "abort" { ABORT(next_token lexbuf) }
| "&&" { AND(next_token lexbuf) }
| "||" { OR(next_token lexbuf) }
| "!" { NOT(next_token lexbuf) }
| "+" { PLUS(next_token lexbuf) }
| "-" { MINUS(next_token lexbuf) }
| "*" { MULTIPLY(next_token lexbuf) }
| "/" { DIVIDE(next_token lexbuf) }
| ">" { GREATER(next_token lexbuf) }
| ">=" { GREATER_EQUAL(next_token lexbuf) }
| "<" { LESS(next_token lexbuf) }
| "<=" { LESS_EQUAL(next_token lexbuf) }
| "==" { EQUALS(next_token lexbuf) }
| ".." { DOTDOT(next_token lexbuf) }
| "int" { INT_DECL(next_token lexbuf) }
| "bool" { BOOL_DECL(next_token lexbuf) }
| identifier { IDENTIFIER(Lexing.lexeme lexbuf, next_token lexbuf) }
| statecall { STATECALL(Lexing.lexeme lexbuf, next_token lexbuf) }
| number { INT(int_of_string (Lexing.lexeme lexbuf), next_token lexbuf) }
| "/*" { ignore(comment lexbuf); token lexbuf }
| "//" { ignore(single_comment lexbuf); token lexbuf }
| eof { EOF(next_token lexbuf) }

and comment = parse
| "/*" { ignore(comment lexbuf); comment lexbuf }
| "*/" { true }
| '\n' { new_line lexbuf; comment lexbuf }
| _ { comment lexbuf }

and single_comment = parse
| '\n' { new_line lexbuf; true }
| _ { single_comment lexbuf }
