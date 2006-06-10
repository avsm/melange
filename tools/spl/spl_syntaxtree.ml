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
 * $Id: spl_syntaxtree.ml,v 1.20 2006/02/05 21:49:52 avsm Exp $
 *)

open Printf

type id = string
type ids = string list
type funcname = string

type expr =
    | And of (expr * expr)
    | Or of (expr * expr)
    | Identifier of id
    | Not of expr
    | True
    | False
    | Greater of (expr * expr)
    | Less of (expr * expr)
    | Greater_or_equal of (expr * expr)
    | Less_or_equal of (expr * expr)
    | Equals of (expr * expr)
    | Plus of (expr * expr)
    | Minus of (expr * expr)
    | Multiply of (expr * expr)
    | Divide of (expr * expr)
    | Int_constant of int

(* Types that can be assigned to state variables *)
type var_type =
    | Unknown of id (* should never be present post-typechecking *)
    | Boolean of id
    | Integer of id
type var_types = var_type list

type statement =
    | Sequence of id
    | Multiple of (int option * int option * statements)
    | Always_allow of (ids * statements)
    | Either_or of (guarded_choice list)
    | Do_until of guarded_choice
    | While of guarded_choice
    | Assign of (id * expr)
    | Function_call of (funcname * var_types)
    | During_handle of (statements * statements list)
    | Exit  (* Exit without an error *)
    | Abort (* Immediately raise a Bad_statecall exception *)
and
statements = (Spl_location.t * statement) list
and
guarded_choice = (expr * statements)

type func = {
    name: string;
    args: var_types;
    mutable body: statements;
    mutable html: string;
    export: bool;
    loc: Spl_location.t;
}

type funcs = func list

let var_name_of_arg = function
    |Unknown x -> x
    |Boolean x -> x
    |Integer x -> x
   
exception Syntax_error of Spl_location.t
