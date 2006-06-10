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
 * $Id: spl_cfg.ml,v 1.30 2006/02/15 18:17:42 avsm Exp $
 *)

open Spl_syntaxtree
open Spl_utils
open Printf

type transition_method =
    |Condition of expr
    |Message of id
    |Assignment of (id * expr)
    |Terminate

type transition_class =
    |T_handle
    |T_normal

type state = {
    label: string;
    mutable edges: transition list;
}
and
transition = {
    t: transition_method;
    target: state ref;
    cl: transition_class;
    loc: Spl_location.t option;
}

type env = {
    func_name: string;
    initial_state: state option ref;
    final_state: state option ref;
    blocks: (string, state) Hashtbl.t;
    registers: (var_type, unit) Hashtbl.t;
    functions_called: (string, unit) Hashtbl.t;
}

type compiled_functions =
    (string, env * Spl_syntaxtree.func) Hashtbl.t

type global_env = {
    filename: string;
    functions: compiled_functions;
    counter: int ref; (* for unique state number *)
    mutable webpage: string;
}

exception Block_not_unique of string
exception Unknown_variable of string
exception Unknown_function of string
exception Type_checking_invariant_failure

let string_of_transition_class = function
    |T_handle -> "handle"
    |T_normal -> "normal"

let gen_label genv prefix =
    incr genv.counter;
    sprintf "S_%s_%d" prefix !(genv.counter)

let gen_return_number genv =
    incr genv.counter;
    !(genv.counter)

(* Add a new basic block to the environment *)
let new_state env label =
    let new_block = {
        label = String.capitalize label;
        edges = [];
    } in
    (* Sanity check to make sure the block really is new *)
    begin
    try
        let _ = Hashtbl.find env.blocks label in
        raise (Block_not_unique label)
    with Not_found ->
        Hashtbl.add env.blocks label new_block
    end;
    new_block

let initial_state_of_env e =
    match !(e.initial_state) with
    |Some x -> x
    |None -> failwith "no initial state recorded for env"

let final_state_of_env e =
    match !(e.final_state) with
    |Some x -> x
    |None -> failwith "no final state recorded for env"

(* Create a new mutable environment for the state list *)
let new_state_env genv name =
    let env = {
        func_name = name;
        blocks = Hashtbl.create 1;
        initial_state = ref None;
        final_state = ref None;
        registers = Hashtbl.create 1;
        functions_called = Hashtbl.create 1;
    } in
    let initial = new_state env (gen_label genv "initial") in
    let final = new_state env (gen_label genv "final") in
    env.initial_state := Some initial;
    env.final_state := Some final;
    env

let create_edge ?(cl=T_normal) ?(loc=None) sfrom sto trans =
    let edge = { t = trans; target = ref sto; cl=cl; loc=loc } in
    sfrom.edges <- (edge :: sfrom.edges)

let list_of_registers e =
    Hashtbl.fold (fun k v a -> k :: a) e.registers []

let reg_name f x = sprintf "%s_%s" f x

let reg_arg f = function
  |Integer x -> Integer (reg_name f x) | Boolean x -> Boolean (reg_name f x)
  |Unknown x -> failwith "type checker invariant failure"

(* Get a flat list of blocks for a function *)
let blocks_of_function env fhash =
    let functions_called = Hashtbl.fold (fun k v a -> k :: a) env.functions_called [] in
    let block_hash_list = List.map (fun fname ->
        let e, f = try Hashtbl.find fhash fname
        with Not_found -> failwith "Internal error, block_list" in
        e.blocks) functions_called @ [env.blocks] in
    let a = List.map (fun x ->
        Hashtbl.fold (fun k v a -> v :: a) x []
    ) block_hash_list in
    List.flatten a

(* Convert any identifiers in an expression into their register equivalents *)
let rec register_expr f = function
    |And (a,b) -> And (register_expr f a, register_expr f b)
    |Or (a,b) -> Or (register_expr f a, register_expr f b)
    |Not x -> Not (register_expr f x)
    |Identifier i -> Identifier (reg_name f.func_name i)
    |True -> True | False -> False
    |Greater (a,b) -> Greater (register_expr f a, register_expr f b)
    |Greater_or_equal (a,b) -> Greater_or_equal (register_expr f a, register_expr f b)
    |Less (a,b) -> Less (register_expr f a, register_expr f b)
    |Less_or_equal (a,b) -> Less_or_equal (register_expr f a, register_expr f b)
    |Equals (a,b) -> Equals (register_expr f a, register_expr f b)
    |Plus (a,b) -> Plus (register_expr f a, register_expr f b)
    |Minus (a,b) -> Minus (register_expr f a, register_expr f b)
    |Multiply (a,b) -> Multiply (register_expr f a, register_expr f b)
    |Divide (a,b) -> Divide (register_expr f a, register_expr f b)
    |Int_constant a as x -> x

let rec generate_states_of_expr ?(cl = T_normal) genv env allows handles si se xsl =
    (* helper function to add during/handle edges to a given state *)
    let insert_during_handles state entry_cond =
        List.iter (fun (reg, si, se) ->
            let ret = gen_return_number genv in
            let retc = Int_constant ret in
            let scond = new_state env (gen_label genv "handle") in
            create_edge state ~cl:T_handle scond entry_cond;
            create_edge scond ~cl:T_handle si (Assignment (reg, retc));
            create_edge se ~cl:T_handle state (Condition (Equals (Identifier reg, retc)));
        ) in
    let insert_always_allows state entry_cond =
        List.iter (fun id ->
            let scond = new_state env (gen_label genv "allow") in
            create_edge state scond entry_cond;
            create_edge scond state (Message id)
        ) in
    (* Fold across our (reversed) list of statements to generate states *)
    let connect, last_state = List.fold_left (fun (_,state) (loc,xs) ->
        let create_edge = create_edge ~loc:(Some loc) in
        match xs with
        |Abort ->
            let final_state = final_state_of_env env in
            create_edge state final_state Terminate;
            (false, final_state)
        |Exit ->
            let final_state = final_state_of_env env in
            create_edge state final_state (Condition True);
            (false, final_state)
        |Sequence id ->
            let ns = new_state env (gen_label genv "seq") in
            create_edge state ns (Message id);
            insert_always_allows state (Condition True) allows;
            insert_during_handles state (Condition True) handles;
            (true, ns)
        |Either_or gcl ->
            let sblockexit = new_state env (gen_label genv "either_or") in
            List.iter (fun (expr, xsl) ->
                let ns = new_state env (gen_label genv "or") in
                create_edge state ns (Condition (register_expr env expr));
                generate_states_of_expr genv env allows handles ns sblockexit xsl
            ) gcl;
            (true, sblockexit)
        |Multiple (l, h, xsl) ->
            let need_reg x =
                if (l = None) && (h = None) then Condition True else x
            in
            let sinit = new_state env (gen_label genv "multinit") in
            let sentry = new_state env (gen_label genv "multentry") in
            let sblockincr = new_state env (gen_label genv "multincr") in
            let sblockexit = new_state env (gen_label genv "multblexit") in
            let smultexit = new_state env (gen_label genv "multexit") in
            let regn = sprintf "multiple_%d" (gen_return_number genv) in
            Hashtbl.replace env.registers (Integer regn) ();
            let reg = Identifier regn in
            create_edge state sinit (need_reg (Assignment (regn, (Int_constant 0))));
            create_edge sblockexit sblockincr (
                need_reg (Assignment (regn,(Plus (reg,Int_constant 1)))));
            create_edge sblockincr sinit (Condition True);
            generate_states_of_expr genv env allows handles sentry sblockexit xsl;
            create_edge sinit smultexit (Condition (match l with
            |None -> True
            |Some l -> 
                if l = 0 then True else
                Greater_or_equal (reg, (Int_constant l))
            ));
            create_edge sinit sentry (Condition (match h with
            |None -> True
            |Some h -> Less (reg, (Int_constant h))
            ));
            (true, smultexit)
        |Always_allow (ids, xsl) ->
            let sexit = new_state env (gen_label genv "always") in
            generate_states_of_expr genv env (ids @ allows) handles state sexit xsl;
            (true, sexit)
        |Assign (id, expr) ->
            let sblockexit = new_state env (gen_label genv "assign") in
            create_edge state sblockexit (Assignment
                (reg_name env.func_name id, register_expr env expr));
            (true, sblockexit)
        |While (expr, xsl) ->
            let alpha = new_state env (gen_label genv "while_a") in
            let beta = new_state env (gen_label genv "while_b") in
            let gamma = new_state env (gen_label genv "while_c") in
            create_edge state alpha (Condition (register_expr env expr));
            generate_states_of_expr genv env allows handles alpha beta xsl;
            create_edge state gamma (Condition (Not (register_expr env expr)));
            create_edge beta state (Condition True);
            (true, gamma)
        |Do_until (expr, xsl) ->
            let scond = new_state env (gen_label genv "do") in
            generate_states_of_expr genv env allows handles state scond xsl;
            let sblockexit = new_state env (gen_label genv "until") in
            create_edge scond state (Condition (Not (register_expr env expr)));
            create_edge scond sblockexit (Condition (register_expr env expr));
            (true, sblockexit)
        |During_handle (xsl, xsll) ->
            let sexit = new_state env (gen_label genv "during") in
            let new_handles = List.map (fun xsl ->
                let reg = sprintf "h_ret_%d" (gen_return_number genv) in
                Hashtbl.replace env.registers (Integer reg) ();
                let sinit = new_state env (gen_label genv "h_init") in
                let sexit = new_state env (gen_label genv "h_exit") in
                generate_states_of_expr ~cl:T_handle genv env allows handles sinit sexit xsl;
                (reg, sinit, sexit)
            ) xsll in
            generate_states_of_expr genv env allows (handles @ new_handles) state sexit xsl;
            (true, sexit)
        |Function_call (fname, args) ->
            (* Function that maps function name, local variable to a register name *)
            (* Lookup the function in our global environment *)
            let f_env, f = try Hashtbl.find genv.functions fname
            with Not_found -> raise (Unknown_function fname) in
            (* Add the function call to the call list of this environment, and also any
               functions that it calls *)
            Hashtbl.replace env.functions_called fname ();
            Hashtbl.iter (fun f () -> Hashtbl.replace env.functions_called f ())
                f_env.functions_called;
            (* Map the function arguments into register names *)
            let target_args = List.map (reg_arg fname) f.args in
            let from_args = List.map (reg_arg env.func_name) args in
            (* Add function registers to the our register list for automaton *)
            Hashtbl.iter (fun f () -> Hashtbl.replace env.registers f ()) f_env.registers;
            (* Add function return register to the register list *)
            let return_reg_name = fname ^ "_return" in
            let return_reg = (Integer return_reg_name) in
            Hashtbl.replace env.registers return_reg ();
            (* Get a unique number for the return register *)
            let return_num = Int_constant (gen_return_number genv) in
            (* Map function arguments into the register variable for this function *)
            let return_push_state = new_state env (gen_label genv "func_push_ret") in
            create_edge state return_push_state (Assignment (return_reg_name, return_num));
            (* Push function arguments into the registers *)
            let func_entry_state = List.fold_left2 (fun st var arg ->
                let assign_state = new_state env (gen_label genv "func_push") in
                let var_name = var_name_of_arg var in
                let arg_name = var_name_of_arg arg in
                create_edge st assign_state (Assignment (var_name, (Identifier arg_name)));
                assign_state) return_push_state target_args from_args in
            (* Jump to the function being called *)
            create_edge func_entry_state (initial_state_of_env f_env) (Condition True);
            (* And add a link condition to the return location *)
            let func_exit_state = new_state env (gen_label genv "func_exit") in
            let func_condition = Condition (Equals ((Identifier return_reg_name), return_num)) in
            create_edge (final_state_of_env f_env) func_exit_state func_condition;
            let func_pop_state = List.fold_left2 (fun st var arg ->
                let assign_state = new_state env (gen_label genv "func_pop") in
                let var_name = var_name_of_arg var in
                let arg_name = var_name_of_arg arg in
                create_edge st assign_state (Assignment (arg_name, (Identifier var_name)));
                assign_state) func_exit_state target_args from_args in
            (* Finally, propagate during/handle and always_allows to the function body
               with a conditional check on the return register *)
            Hashtbl.iter (fun sname block ->
                insert_always_allows block func_condition allows;
                insert_during_handles block func_condition handles;
            ) f_env.blocks;
            (true, func_pop_state)
    ) (true, si) xsl in
    if connect then create_edge ~cl:cl last_state se (Condition True);
    ()
    
let generate_states fname funcs =
    let genv = { filename=fname; functions = Hashtbl.create 1; counter = ref 0; webpage="" } in
    List.iter (fun f ->
        Logger.log (sprintf "Compiling function %s (%s) ... " f.name
            (if f.export then "public" else "private"));
        let env = new_state_env genv f.name in
        (* Add the function arguments to the register list *)
        List.iter (fun x -> Hashtbl.replace env.registers (reg_arg f.name x) ()) f.args;
        let initial_state = initial_state_of_env env in
        let final_state = final_state_of_env env in
        generate_states_of_expr genv env [] [] initial_state final_state f.body;
        Logger.log (sprintf "  %d states" (Hashtbl.length env.blocks));
        (* Add function entry to global environment *)
        Hashtbl.replace genv.functions f.name (env, f)
    ) funcs;
    genv
