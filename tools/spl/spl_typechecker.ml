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
 * $Id: spl_typechecker.ml,v 1.9 2006/02/05 21:49:52 avsm Exp $
 *)

open Spl_syntaxtree
open Spl_location
open Printf

exception Type_error of (string * Spl_location.t)
exception Type_internal_error of string

(* Convenience function to raise a type error *)
let terr e l = raise (Type_error (e,l))

(* Test a list to see if there are duplicate values *)
let list_dup fn l =
    let ret = ref None in
    let _ = List.fold_left (fun a b -> let x = fn b in
        if List.mem x a then (ret := Some b; a) else x::a) [] l in
    !ret

(* Sanity check function declarations for errors *)
let check_function_declarations fs =
    (* No duplicate function names allowed *)
    let _ = match (list_dup (fun x -> x.name) fs) with
    |None -> ()
    |Some f -> terr (sprintf "Duplicate function name '%s'" f.name) f.loc in
    (* Variable declarations must have unique names as well *)
    List.iter (fun f -> match (list_dup var_name_of_arg f.args) with
    |None -> ()
    |Some v -> terr (sprintf "Duplicate variable name '%s'" (var_name_of_arg v)) f.loc
    ) fs;
    ()

(* Need at least one top-level automaton *)
let check_top_level_automaton fs =
    let exports = List.fold_left (fun a b ->
        if b.export then succ a else a) 0 fs in
    if exports = 0 then begin
        terr "At least one automaton declaration required" initial_location
    end

(* Lookup a function definition by its name from the list of functions *)
let find_function fs name =
    let ret = ref None in
    List.iter (fun f -> if f.name = name then ret := Some f) fs;
    !ret

(* Lookup an argument by its name from a function definition *)
let find_argument f name =
    let ret = ref None in
    List.iter (fun v -> if (var_name_of_arg v) = name then ret := Some v) f.args;
    !ret

(* Check expected type 'e' against the passed argument 'p' *)
let check_variable_type loc e p =
    let terr x e r = terr (sprintf
        "Argument '%s' has type %s but should be %s" x e r) loc in
    match e with
    |Integer _ -> begin match p with
        |Integer _ -> ()
        |Boolean x -> terr x "bool" "int"
        |Unknown _ -> raise (Type_internal_error "Unknown variable seen")
    end
    |Boolean _ -> begin match p with
        |Integer x -> terr x "int" "bool"
        |Boolean _ -> ()
        |Unknown _ -> raise (Type_internal_error "Unknown variable seen")
    end
    |Unknown _ -> raise (Type_internal_error "Unknown variable seen")

(* Make sure an expression has a return type e_type and correct variables *)
let rec check_expr f loc e_type expr =
    match e_type with
    |Boolean _ ->
        let rec bool_ops e =
            let check_int = check_expr f loc (Integer "") in
            let berr x = terr (sprintf 
                "Expression should have type bool but uses int operator '%s'" x) loc in
            match e with
            |And (a,b) -> bool_ops a; bool_ops b
            |Or (a,b) -> bool_ops a; bool_ops b
            |Not x -> bool_ops x
            |True |False-> ()
            |Greater (a,b) -> check_int a; check_int b
            |Greater_or_equal (a,b) -> check_int a; check_int b
            |Less (a,b) -> check_int a; check_int b
            |Less_or_equal (a,b) -> check_int a; check_int b
            |Equals (a,b) -> check_int a; check_int b
            |Identifier id -> begin
                (* Figure out type of id from function argument list *)
                match find_argument f id with
                |None -> terr (sprintf "Unknown variable '%s'" id) loc
                |Some a -> check_variable_type loc (Boolean "") a
                end
            |Plus _ -> berr "+"
            |Minus _ -> berr "-"
            |Multiply _ -> berr "*"
            |Divide _ -> berr "/"
            |Int_constant x -> terr
                (sprintf "Expression should be type bool but has int constant %d" x) loc
        in bool_ops expr
    |Integer _ ->
        let rec int_ops e =
            let terr x = terr (sprintf
                "Expression should have type int but uses int operator '%s'" x) loc in
            match e with
            |Plus (a,b) -> int_ops a; int_ops b
            |Minus (a,b) -> int_ops a; int_ops b
            |Multiply (a,b) -> int_ops a; int_ops b
            |Divide (a,b) -> int_ops a; int_ops b
            |Int_constant a -> ()
            |Identifier id -> begin
                (* Figure out type of id from function argument list *)
                match find_argument f id with
                |None -> terr (sprintf "Unknown variable '%s'" id)
                |Some a -> check_variable_type loc (Integer "") a
                end
            |And _ -> terr "&&"
            |Or _ -> terr "||"
            |Not _ -> terr "!"
            |True -> terr "true"
            |False -> terr "false"
            |Greater _ -> terr ">"
            |Greater_or_equal _ -> terr ">="
            |Less _ -> terr "<"
            |Less_or_equal _ -> terr "<="
            |Equals _ -> terr "=="
        in int_ops expr
    |Unknown _ -> raise (Type_internal_error "check_expr")

(* Just go through a syntax tree and make sure that Unknowns disappear *)
let resolve_unknown_variables f =
    let rec resolve xsl = List.map (fun (loc,xs) ->
        (loc, (
        match xs with
        |Abort -> Abort
        |Exit -> Exit
        |Sequence _ as p -> p
        |Either_or gcl -> Either_or
            (List.map (fun (expr, xsl) -> (expr, resolve xsl)) gcl)
        |Multiple (l,h,xsl) -> Multiple (l, h, resolve xsl)
        |Always_allow (ids, xsl) -> Always_allow (ids, resolve xsl)
        |While (expr, xsl) -> While (expr, resolve xsl)
        |Do_until (expr, xsl) -> Do_until (expr, resolve xsl)
        |Assign (id, expr) as p -> p
        |During_handle (xsl, xsll) ->
            During_handle(resolve xsl, List.map resolve xsll)
        |Function_call (fname, args) ->
            (* Every Unknown here needs to be resolved or error thrown *)
            let nargs = List.map (function
            |Unknown arg -> begin
                match find_argument f arg with
                |None -> terr (sprintf "Unknown variable '%s' in function call '%s'" arg fname) loc
                |Some a -> a
                end
            |_ -> raise (Type_internal_error "resolve_unknown_vars")
            ) args in
            Function_call (fname, nargs)
        ))
    ) xsl in
    f.body <- resolve f.body

(* Recurse through syntax tree checking expression/function calls *)
let check_syntax_tree fs =
    (* Check each function to make sure function calls are correct *)
    List.iter (fun f ->
        let rec follow_fun_call xsl = List.iter (fun (loc,xs) ->
            let terr x = terr x loc in
            let bool_expr = check_expr f loc (Boolean "") in
            match xs with
            |Abort -> ()
            |Exit -> ()
            |Sequence _ -> ()
            |Either_or gcl ->
                List.iter (fun (expr, xsl) ->
                    bool_expr expr;
                    follow_fun_call xsl) gcl
            |Multiple (_,_,xsl) ->
                follow_fun_call xsl
            |Always_allow (ids, xsl) ->
                follow_fun_call xsl
            |While (expr, xsl) ->
                bool_expr expr;
                follow_fun_call xsl
            |Do_until (expr, xsl) ->
                bool_expr expr;
                follow_fun_call xsl
            |During_handle (xsl, xsll) ->
                follow_fun_call xsl;
                List.iter follow_fun_call xsll
            |Assign (id, expr) -> begin
                match find_argument f id with
                |None -> terr (sprintf "Unknown variable '%s'" id)
                |Some a -> check_expr f loc a expr
                end
            |Function_call (fname, args) ->
                let func_def = match find_function fs fname with
                |None -> terr (sprintf "Unknown function call '%s'" fname)
                |Some x -> x in
                (* Cannot function call to an automaton *)
                if func_def.export then
                    terr (sprintf "Cannot call automaton '%s' as a function" fname);
                (* Make sure argument size is correct *)
                let la1 = List.length args in
                let la2 = List.length func_def.args in
                if la1 != la2 then
                    terr (sprintf "Function '%s' requires %d argument%s, not %d"
                        fname la2 (if la2 > 1 then "s" else "") la1);
                (* Check types of each argument are consistent *)
                List.iter2 (check_variable_type loc) func_def.args args;
                
            ) xsl in
        follow_fun_call f.body
    ) fs
    
let type_checks = [
    check_function_declarations; check_top_level_automaton;
    check_syntax_tree
]

let type_check fs =
    List.iter resolve_unknown_variables fs;
    List.iter (fun x -> x fs) type_checks

