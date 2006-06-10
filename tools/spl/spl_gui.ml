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
 * $Id: spl_gui.ml,v 1.1 2005/08/21 19:51:59 avsm Exp $
 *)

open Printf
open Spl_syntaxtree
open Spl_cfg

(* Convert expression to a string *)
let rec string_of_expr = function
    | And (a,b) -> sprintf "(%s && %s)" (string_of_expr a) (string_of_expr b)
    | Or (a,b) -> sprintf "(%s || %s)" (string_of_expr a) (string_of_expr b)
    | Identifier i -> i
    | Not e -> sprintf "(!%s)" (string_of_expr e)
    | True -> "true"
    | False -> "false"
    | Greater (a,b) -> sprintf "(%s > %s)" (string_of_expr a) (string_of_expr b)
    | Less (a,b) -> sprintf "(%s < %s)"  (string_of_expr a) (string_of_expr b)
    | Greater_or_equal (a,b) -> sprintf "(%s >= %s)"  (string_of_expr a) (string_of_expr b)
    | Less_or_equal (a,b) -> sprintf "(%s <= %s)"  (string_of_expr a) (string_of_expr b)
    | Equals (a,b) -> sprintf "(%s == %s)"  (string_of_expr a) (string_of_expr b)
    | Plus (a,b) -> sprintf "(%s + %s)" (string_of_expr a) (string_of_expr b)
    | Minus (a,b) -> sprintf "(%s - %s)" (string_of_expr a) (string_of_expr b)
    | Multiply (a,b) -> sprintf "(%s * %s)" (string_of_expr a) (string_of_expr b)
    | Divide (a,b) -> sprintf "(%s / %s)" (string_of_expr a) (string_of_expr b)
    | Int_constant a -> sprintf "%d" a

let string_of_transition = function
    |Terminate -> "abort=\"1\""
    |Condition expr -> sprintf "condition=\"%s\"" (string_of_expr expr)
    |Assignment (id,expr) -> sprintf "assign=\"%s=%s\"" id (string_of_expr expr)
    |Message id -> sprintf "msg=\"%s\"" id

let string_of_state s =
    sprintf "state { label=\"%s\"; type=\"default\" }" s.label

let string_of_edge s e =
    sprintf "edge { from=\"%s\"; to=\"%s\"; %s}"
        s.label !(e.target).label (string_of_transition e.t)

let generate ochanfn genv =
    Hashtbl.iter (fun fname (env, func) ->
        ochanfn env.func_name (fun outchan ->
            let o x = output_string outchan (x ^ "\n") in
            let funcs = Hashtbl.fold (fun f () a ->
                let e,_ = Hashtbl.find genv.functions f in
                e.blocks :: a) env.functions_called [env.blocks] in
            List.iter (Hashtbl.iter (fun k st ->
                o (string_of_state st);
                List.iter (fun edge -> o (string_of_edge st edge)) st.edges;
            )) funcs;
        );
    ) genv.functions
