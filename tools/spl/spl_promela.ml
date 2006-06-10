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
 * $Id: spl_promela.ml,v 1.12 2006/03/15 11:32:16 avsm Exp $
 *)

open Printf
open Spl_syntaxtree
open Spl_cfg
open Spl_utils
open Spl_utils.Printer

let comment e c = e.nl (); e.p (sprintf "/* %s */" c)

type env = {
    statecalls: (id, unit) Hashtbl.t; (* Statecall name, unit *)
    funcs: (string, (Spl_cfg.env * func)) Hashtbl.t;
}

let rec reduce_expr sym ex =
    let rec fn = function
    | And (a,b) -> And ((fn a), (fn b))
    | Or (a,b) -> Or ((fn a), (fn b))
    | Not a -> Not (fn a)
    | Greater (a,b) -> Greater ((fn a), (fn b))
    | Greater_or_equal (a,b) -> Greater_or_equal ((fn a), (fn b))
    | Less (a,b) -> Less ((fn a), (fn b))
    | Less_or_equal (a,b) -> Less_or_equal ((fn a), (fn b))
    | Equals (a,b) -> Equals ((fn a), (fn b))
    | Plus (a,b) -> Plus ((fn a), (fn b))
    | Minus (a,b) -> Minus ((fn a), (fn b))
    | Multiply (a,b) -> Multiply ((fn a), (fn b))
    | Divide (a,b) -> Divide ((fn a), (fn b))
    | Int_constant a as x -> x
    | True -> True | False -> False
    | Identifier i ->
        try 
            let ex = List.assoc i sym in
            let sym = List.remove_assoc i sym in
            reduce_expr sym ex
        with Not_found -> Identifier i
    in Spl_optimiser.fold (fn ex)

let rec promela_of_expr = function
    | And (a,b) -> sprintf "(%s && %s)"
        (promela_of_expr a) (promela_of_expr b)
    | Or (a,b) -> sprintf "(%s || %s)"
        (promela_of_expr a) (promela_of_expr b)
    | Identifier i -> i
    | Not e -> sprintf "(!%s)" (promela_of_expr e)
    | True -> "true"
    | False -> "false"
    | Greater (a,b) -> sprintf "(%s > %s)" (promela_of_expr a) (promela_of_expr b)
    | Less (a,b) -> sprintf "(%s < %s)"  (promela_of_expr a) (promela_of_expr b)
    | Greater_or_equal (a,b) -> sprintf "(%s >= %s)"  (promela_of_expr a) (promela_of_expr b)
    | Less_or_equal (a,b) -> sprintf "(%s <= %s)"  (promela_of_expr a) (promela_of_expr b)
    | Equals (a,b) -> sprintf "(%s == %s)"  (promela_of_expr a) (promela_of_expr b)
    | Plus (a,b) -> sprintf "(%s + %s)" (promela_of_expr a) (promela_of_expr b)
    | Minus (a,b) -> sprintf "(%s - %s)" (promela_of_expr a) (promela_of_expr b)
    | Multiply (a,b) -> sprintf "(%s * %s)" (promela_of_expr a) (promela_of_expr b)
    | Divide (a,b) -> sprintf "(%s / %s)" (promela_of_expr a) (promela_of_expr b)
    | Int_constant a -> sprintf "%d" a

let initial_promela_of_arg = function
    | Integer x -> sprintf "int %s = 0" x
    | Boolean x -> sprintf "bool %s = 0" x
    | Unknown x -> failwith "type checker invariant failure"

(* Run an iterator over only automata functions *)
let export_fun_iter fn genv =
    Hashtbl.iter (fun fname (env, func) ->
        if func.export then fn fname env func
    ) genv.funcs


let extract_statecalls h func_env =
    Hashtbl.iter (fun k state ->
        List.iter (fun edge ->
            match edge.t with
            |Message id -> Hashtbl.replace h id ()
            |_ -> ()
        ) state.edges
    ) func_env.blocks
   
let msg_of_statecall s =
    s

let chan_of_funcname c =
    "chan_" ^ c

let proc_of_funcname c =
    "p_" ^ (String.lowercase c)

let pp_brace e s fn =
    e.p (sprintf "%s {" s);
    indent_fn e fn;
    e.p "}"
        
let pp_env env e =
    comment e "State definitions";
    let var_counter = ref 0 in
    Hashtbl.iter (fun _ (func_env, func_def) ->
        Hashtbl.iter (fun sname state ->
            e.p (sprintf "#define %s  %d" sname !var_counter);
            incr var_counter;
        ) func_env.blocks;
    ) env.funcs;
    let num_total_states = !var_counter in
    
    comment e "Message definitions";
    let var_counter = ref 0 in
    e.p "mtype = { Null_msg,";
    indent_fn e (fun e -> Hashtbl.iter (fun sname _ ->
        e.p (sprintf "%s, " (msg_of_statecall sname));
        incr var_counter;
    ) env.statecalls);
    e.p "}";
    e.p "bool err = false;";
    e.p "bool gen_done = false;";
    e.p "local mtype r;";
    e.p "mtype p;";
    e.nl ();
    (* Helper function to set state and return to progress_loop *)
    export_fun_iter (fun func_name func_env func_def ->
        let block_list = blocks_of_function func_env env.funcs in
        comment e (sprintf "Automaton for: %s" func_name);
        let cn = chan_of_funcname func_name in 
        let initial_state = initial_state_of_env func_env in
        e.p (sprintf "chan %s = [0] of {mtype}" cn);
        e.p (sprintf "unsigned %s_state : %d = %s;" func_name (snd (frexp (float_of_int num_total_states)) ) initial_state.label);        
        e.p (sprintf "bool %s_done = false;" func_name);

        Hashtbl.iter (fun reg () ->
            match reg with
            |Unknown _ -> failwith "type checking badness";
            |Boolean x -> e.p (sprintf "bool %s = false;" x);
            |Integer x -> e.p (sprintf "byte %s = 0;" x);
        ) func_env.registers;

        let is_sink_state state = List.length state.edges = 0 in
        let rec tickfn e sym targ =
            let condtrans = List.filter (fun x -> match x.t with |Condition _ -> true |_ -> false) targ.edges in
            let asstrans = List.filter (fun x -> match x.t with |Assignment _ -> true |_ -> false) targ.edges in
            let aborttrans = List.filter (fun x -> match x.t with |Terminate -> true |_ -> false) targ.edges in
            if List.length aborttrans > 0 then begin
                e.p (sprintf ":: true -> goto %s_bad_statecall" func_name);
            end else begin
                if is_sink_state targ then begin
                    e.p (sprintf ":: true -> goto %s_end_full" func_name);
                end else begin
                    let sh = Hashtbl.create 1 in
                    List.iter (fun x ->
                        match x.t with
                        |Message id ->
                            if not (Hashtbl.mem sh targ.label) then begin
                                e.p (sprintf ":: %s_state = %s" func_name targ.label);
                                Hashtbl.add sh targ.label ();
                            end;
                        |Assignment (var,expr) -> ()
                        |Condition _ -> ()
                        |Terminate -> ()
                    ) targ.edges;
                    (* Group common assignments together *)
                    let asshash = Hashtbl.create 1 in
                    List.iter (fun x -> match x.t with
                        |Assignment v -> hashtbl_add_list asshash v x
                        |_ -> failwith "err") asstrans;
                    Hashtbl.iter (fun (var,expr) xs ->
                        e.p (sprintf ":: %s = %s -> if" var (promela_of_expr expr));
                        indent_fn e (fun e -> List.iter (fun x -> tickfn e sym !(x.target)) xs);
                        e.p "fi;";                        
                    ) asshash;
                    (* Group common conditionals together *)
                    let condhash = Hashtbl.create 1 in
                    List.iter (fun x -> match x.t with
                        |Condition c -> hashtbl_add_list condhash c x
                        |_ -> failwith "err") condtrans;
                    Hashtbl.iter (fun c xs ->
                        e.p (sprintf ":: %s -> if" (promela_of_expr c));
                        indent_fn e (fun e -> List.iter (fun x -> tickfn e sym !(x.target)) xs);
                        e.p "fi;";
                    ) condhash;
                end;
            end
        in

        pp_brace e (sprintf "active proctype %s ()" (proc_of_funcname func_name)) (fun e ->
            e.p "atomic {";
            let ist = initial_state_of_env func_env in
            e.p "if";
            tickfn e [] ist;
            e.p "fi;";
            e.p "do";
            e.p (sprintf ":: %s?r -> if" cn);
            list_iter_indent e (fun e state ->
                e.p (sprintf ":: %s_state == %s ->" func_name state.label);
                indent_fn e (fun e ->
                    let msghash = Hashtbl.create 1 in
                    List.iter (fun x -> match x.t with
                        |Message id -> hashtbl_add_list msghash id x  |_ -> ()) state.edges;
                    if Hashtbl.length msghash = 0 then e.p "assert(0);" else begin
                        e.p (sprintf "if");
                        Hashtbl.iter (fun id xs ->
                            e.p (sprintf ":: r == %s -> if" id);
                            List.iter (fun x ->
                                match x.t with
                                |Message id ->
                                    indent_fn e (fun e ->
                                        let targ_state = !(x.target) in
                                        tickfn e [] targ_state;
                                    );
                                 |_ -> failwith "message"
                            ) xs;
                            e.p "fi;"
                        ) msghash;
                        indent_fn e (fun e -> e.p (sprintf ":: else -> goto %s_bad_statecall" func_name));
                        e.p "fi";                    
                    end
                )
            ) block_list;
            e.p "fi";
            e.p (sprintf ":: gen_done -> goto %s_end_full" func_name);
            e.p "od;";
            e.p "assert(0);";
            comment e "All other states are bad";
            e.p (sprintf "%s_bad_statecall:" func_name);
            e.p "printf(\"MSC: Bad_statecall exception\\n\");";
            e.p "err = true;";
            e.p (sprintf "%s_end_full:" func_name);
            e.p "printf(\"MSC: End state reached\\n\");";
            e.p (sprintf "%s_done = true" func_name);
            e.p "} /* atomic end */";
            ()
        ); 
    ) env;

    comment e "Statecall Generator";
    e.p (sprintf "active proctype generator() {");
    indent_fn e (fun e ->
        e.p "progress:";
        e.p "do";
        Hashtbl.iter (fun sc _ ->
            let la = ref [] in
            Hashtbl.iter (fun fname (fenv, fdef) ->
                if fdef.export then begin
                    let h = Hashtbl.create 1 in
                    extract_statecalls h fenv;
                    if Hashtbl.mem h sc then la := (fname,sc) :: !la;
                end;
            ) env.funcs;
            let msc = msg_of_statecall sc in
            e.p (sprintf ":: atomic { !err; ");
            indent_fn e (fun e ->
                List.iter (fun (fname,sc) ->
                    e.p (sprintf "if :: %s!%s; p=%s ::%s_done || err -> goto end_gen fi;"
                    (chan_of_funcname fname) msc msc fname)) (List.rev !la);
                e.p "}");
        ) env.statecalls;
        e.p ":: err -> goto end_gen";
        e.p "od;";
        e.p "end_gen: gen_done = true";
    );
    e.p "}";
    e.nl ();
    ()
    
let generate proout genv =
    let penv = init_printer ~comment:("/*","*/") proout in
    let env = { statecalls = Hashtbl.create 1; funcs = genv.functions } in
    Hashtbl.iter (fun _ (fenv, _) -> extract_statecalls env.statecalls fenv) env.funcs;
    pp_env env penv
