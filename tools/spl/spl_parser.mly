/*
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
 * $Id: spl_parser.mly,v 1.18 2006/02/05 21:49:52 avsm Exp $
 */

%{
    open Spl_location
    open Spl_syntaxtree
    open Printf

    let parse_error msg =
        raise (Syntax_error !Spl_location.current_location)
        
    let id (x,_) = x
    let loc (_,x) = x 
%}

%token <Spl_location.t> EOL EOF
%token <string * Spl_location.t> IDENTIFIER
%token <string * Spl_location.t> STATECALL
%token <int * Spl_location.t> INT
%token <Spl_location.t> FUNCTION AUTOMATON
%token <Spl_location.t> BOOL_DECL INT_DECL
%token <Spl_location.t> BOOLEAN
%token <Spl_location.t> EXPORT
%token <Spl_location.t> LBRACE RBRACE LBRACKET RBRACKET
%token <Spl_location.t> MULTIPLE ALWAYS_ALLOW DOTDOT OPTIONAL
%token <Spl_location.t> COMMA SEMICOLON ASSIGN EOF
%token <Spl_location.t> TRUE FALSE
%token <Spl_location.t> AND OR NOT
%token <Spl_location.t> EITHER OR IF
%token <Spl_location.t> DURING HANDLE
%token <Spl_location.t> WHILE
%token <Spl_location.t> DO UNTIL
%token <Spl_location.t> PLUS MINUS MULTIPLY DIVIDE
%token <Spl_location.t> GREATER GREATER_EQUAL LESS LESS_EQUAL EQUALS
%token <Spl_location.t> EXIT ABORT

/* XXX - review these associativities - avsm */
%left OR
%left AND
%right NOT
%left GREATER GREATER_EQUAL LESS LESS_EQUAL EQUALS
%left PLUS MINUS
%left MULTIPLY DIVIDE
%nonassoc UMINUS

%start main
%type <Spl_syntaxtree.funcs> main
%%
main:
  function_list EOF {$1}
;
function_list:
| function_decl function_list {$1::$2}
| function_decl {[$1]}
;
function_decl:
| AUTOMATON IDENTIFIER LBRACKET function_args RBRACKET function_body 
    {{Spl_syntaxtree.name=id $2; args=$4; body=$6; html=""; export=true;
      loc=$1}}
| FUNCTION IDENTIFIER LBRACKET function_args RBRACKET function_body 
    {{Spl_syntaxtree.name=id $2; args=$4; body=$6; html=""; export=false;
      loc=$1}}
;
function_args:
| function_arg COMMA function_args { $1::$3 }
| function_arg { [$1] }
| {[]}
;
function_arg:
| INT_DECL IDENTIFIER { Spl_syntaxtree.Integer (id $2) }
| BOOL_DECL IDENTIFIER { Spl_syntaxtree.Boolean (id $2) }
;
function_call_args:
| function_arg_nodecl COMMA function_call_args { $1::$3 }
| function_arg_nodecl { [$1] }
| {[]}
;
function_arg_nodecl:
| IDENTIFIER { Spl_syntaxtree.Unknown (id $1) }
;
statecall_args:
| STATECALL COMMA statecall_args { (id $1)::$3 }
| STATECALL { [(id $1)] }
| {[]}
;
function_body:
| LBRACE statement RBRACE optional_semi { $2 }
| LBRACE RBRACE optional_semi {[]}
;
optional_semi:
| SEMICOLON { () }
| { () }
;
statement:
| single_statement statement { $1 :: $2 }
| single_statement {[$1]}
;
int_range:
| LBRACKET INT DOTDOT INT RBRACKET { (Some (id $2)), (Some (id $4)) }
| LBRACKET DOTDOT INT RBRACKET { (None, (Some (id $3))) }
| LBRACKET INT DOTDOT RBRACKET { (Some (id $2), None) }
| LBRACKET INT RBRACKET { let a = Some (id $2) in (a,a) }
| { (None, None) }
;
single_statement:
| STATECALL SEMICOLON
    { (loc $1, Spl_syntaxtree.Sequence (id $1)) }
| IDENTIFIER LBRACKET function_call_args RBRACKET SEMICOLON
    { (loc $1, Spl_syntaxtree.Function_call ((id $1), $3)) }
| ALWAYS_ALLOW LBRACKET statecall_args RBRACKET function_body
    { ($1, Spl_syntaxtree.Always_allow ($3, $5)) }
| MULTIPLE int_range function_body
    { let l,h = $2 in ($1, Spl_syntaxtree.Multiple (l, h, $3)) }
| OPTIONAL function_body
    { ($1, Spl_syntaxtree.Multiple (None, Some 1, $2)) }
| EITHER guard_expr function_body or_list
    { ($1, Spl_syntaxtree.Either_or (($2,$3) :: $4)) }
| DO function_body UNTIL guard_expr SEMICOLON
    { ($1, Spl_syntaxtree.Do_until ($4, $2)) }
| WHILE guard_expr function_body
    { ($1, Spl_syntaxtree.While ($2, $3)) }
| IDENTIFIER ASSIGN expr SEMICOLON
    { (loc $1, Spl_syntaxtree.Assign ((id $1), $3)) }
| DURING function_body handle_list
    { ($1, Spl_syntaxtree.During_handle ($2, $3)) }
| EXIT SEMICOLON { ($1, Spl_syntaxtree.Exit) }
| ABORT SEMICOLON { ($1, Spl_syntaxtree.Abort) }
;
or_list:
| OR guard_expr function_body or_list {($2,$3)::$4}
| OR guard_expr function_body {[($2, $3)]}
;
handle_list:
| HANDLE function_body handle_list { $2::$3 }
| { [] }
;
guard_expr:
| LBRACKET expr RBRACKET {$2}
| {Spl_syntaxtree.True}
;
expr:
| INT { Spl_syntaxtree.Int_constant (id $1) }
| IDENTIFIER {Spl_syntaxtree.Identifier (id $1)}
| LBRACKET expr RBRACKET { $2 }
| expr PLUS expr { Spl_syntaxtree.Plus ($1, $3) }
| expr MINUS expr { Spl_syntaxtree.Minus ($1, $3) }
| expr MULTIPLY expr { Spl_syntaxtree.Multiply ($1, $3) }
| expr DIVIDE expr { Spl_syntaxtree.Divide ($1, $3) }
| MINUS expr %prec UMINUS {
    Spl_syntaxtree.Multiply ( Spl_syntaxtree.Int_constant (-1), $2) }
| TRUE {Spl_syntaxtree.True}
| FALSE {Spl_syntaxtree.False}
| expr AND expr {Spl_syntaxtree.And ($1,$3)}
| expr OR expr {Spl_syntaxtree.Or ($1,$3)}
| NOT expr {Spl_syntaxtree.Not $2}
| expr GREATER expr {Spl_syntaxtree.Greater ($1, $3)}
| expr GREATER_EQUAL expr {Spl_syntaxtree.Greater_or_equal ($1, $3)}
| expr LESS expr {Spl_syntaxtree.Less ($1, $3)}
| expr LESS_EQUAL expr {Spl_syntaxtree.Less_or_equal ($1, $3)}
| expr EQUALS expr {Spl_syntaxtree.Equals ($1, $3)}
;
