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
 * $Id: spl_dot.ml,v 1.14 2006/02/05 21:49:52 avsm Exp $
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

let string_of_transition from t =
    let (ty,str) = match t.t with
    |Terminate -> "color=\"red\"", "abort"
    |Condition expr -> "arrowhead=\"odiamond\",color=\"blue\"", (string_of_expr  expr)
    |Assignment (id,expr) -> "arrowhead=\"tee\",color=\"green\"",
        sprintf "%s=%s" id (string_of_expr expr)
    |Message id -> "", (sprintf "{%s}" id)
    in
    let ln = match t.loc with None -> "" |Some l -> sprintf " (%d)" l.Spl_location.line_num in
    sprintf " \"%s\" -> \"%s\" [label=\"%s%s\",fontsize=10,%s];" from.label !(t.target).label str ln ty

let string_of_state s =
    String.concat "\n" ((List.map (string_of_transition s) s.edges))

let generate ochanfn genv =
    ochanfn "all" (fun outchan ->
        let o x = output_string outchan (x ^ "\n") in
        o "digraph FUNCTION {";
        Hashtbl.iter (fun fname (env, func) ->
            o (sprintf "subgraph %s {" fname);
            Hashtbl.iter (fun k v -> o (string_of_state v)) env.blocks;
            o "}";
        ) genv.functions;
        o "}";
    );
    Hashtbl.iter (fun fname (env, func) ->
        ochanfn env.func_name (fun outchan ->
            let o x = output_string outchan (x ^ "\n") in
            o "digraph FUNCTION {";
            let funcs = Hashtbl.fold (fun f () a ->
                let e,_ = Hashtbl.find genv.functions f in
                e.blocks :: a) env.functions_called [env.blocks] in
(*            List.iter (Hashtbl.iter (fun k v ->
                let ln v = match v.loc with |None -> "" |Some x -> string_of_int (x.Spl_location.line_num) in
                o (sprintf "%s [label=\"%s\"];" v.label v.label (ln v))
            )) funcs; *)
            List.iter (Hashtbl.iter (fun k v ->
                o (string_of_state v)
            )) funcs;
            o "}";
        );
    ) genv.functions
