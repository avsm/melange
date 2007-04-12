/*
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
 */

%{
    open Config_location
    open Config_t
    open Printf

    let parse_error msg =
        raise (Syntax_error !Config_location.current_location)
        
    let id (x,_) = x
    let loc (_,x) = x

%}

%token <Config_location.t> EOL EOF TRUE FALSE
%token <Config_location.t> SEMICOLON COMMA LBRACKET RBRACKET EQUALS
%token <string * Config_location.t> IDENTIFIER STRING VARIANT
%token <int * Config_location.t> INT

%start main
%type <Config_t.var_vals> main
%%
main:
| config_lines EOF { $1 }
;
config_lines:
| config_line config_lines { $1 :: $2 }
| config_line { [$1] }
;
config_line:
| IDENTIFIER EQUALS config_value SEMICOLON {
    {Config_t.v_name=(id $1); v_ty=Config_t.null_var_type; v_val=$3; v_loc=(loc $1)}
  }
| IDENTIFIER EQUALS LBRACKET config_list_values RBRACKET SEMICOLON {
	{Config_t.v_name=(id $1); v_ty=Config_t.null_var_type; v_val=$4; v_loc=(loc $1)}
  }
;
config_value:
| TRUE { Config_t.V_boolean true }
| FALSE { Config_t.V_boolean false }
| STRING { Config_t.V_string (id $1) }
| INT { Config_t.V_int (id $1) }
| VARIANT { Config_t.V_variant (id $1) }
;
config_list_values:
| { Config_t.V_empty_list }
| config_variant_list { Config_t.V_variant_list $1 }
| config_string_list { Config_t.V_string_list $1 }
| config_int_list { Config_t.V_int_list $1 }
;
config_variant_list:
| VARIANT { [id $1] }
| VARIANT COMMA config_variant_list { id $1 :: $3 }
;
config_string_list:
| STRING { [id $1] }
| STRING COMMA config_string_list { id $1 :: $3 }
;
config_int_list:
| INT { [id $1] }
| INT COMMA config_int_list { id $1 :: $3 }
;
