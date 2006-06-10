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
 * $Id: spl_optimiser.ml,v 1.11 2006/02/08 19:20:11 avsm Exp $
 *)

open Printf
open Spl_syntaxtree
open Spl_cfg
open Spl_utils

let rec fold = function
	|And (a,b) as o -> begin
		match (fold a, fold b) with
		|False,_
		|_,False -> False
		|True,a
		|a,True -> a
		|_ -> o
	end
	|Or (a,b) as o -> begin
		match (fold a, fold b) with
		|True,_
		|_,True -> True
		|False,a
		|a,False -> a
		|_ -> o
	end
	|Identifier id as o -> o
	|Int_constant x as o -> o
	|Not a as o -> begin
		match fold a with
		|True -> False
		|False -> True
		|_ -> o
	end
	|True -> True
	|False -> False
	|Greater (a,b) as o -> begin
		match (fold a, fold b) with
		|(Int_constant a),(Int_constant b) ->
			if (a > b) then True else False
		|_ -> o
	end
	|Greater_or_equal (a,b) as o -> begin
		match (fold a, fold b) with
		|(Int_constant a),(Int_constant b) ->
			if (a >= b) then True else False
		|_ -> o
	end
	|Less (a,b) as o -> begin
		match (fold a, fold b) with
		|(Int_constant a),(Int_constant b) ->
			if (a < b) then True else False
		|_ -> o
	end
	|Less_or_equal (a,b) as o -> begin
		match (fold a, fold b) with
		|(Int_constant a),(Int_constant b) ->
			if (a <= b) then True else False
		|_ -> o
	end
	|Equals (a,b) as o -> begin
		match (fold a, fold b) with
		|(Int_constant a),(Int_constant b) ->
			if (a = b) then True else False
		|_ -> o
	end
	|Plus (a,b) as o -> begin
		match (fold a, fold b) with
		|(Int_constant a),(Int_constant b) ->
			Int_constant (a+b)
		|_ -> o
	end
	|Minus (a,b) as o -> begin
		match (fold a, fold b) with
		|(Int_constant a),(Int_constant b) ->
			Int_constant (a-b)
		|_ -> o
	end
	|Multiply (a,b) as o -> begin
		match (fold a, fold b) with
		|(Int_constant a),(Int_constant b) ->
			Int_constant (a*b)
		|_ -> o		
	end
	|Divide (a,b) as o -> begin
		match (fold a, fold b) with
		|(Int_constant a),(Int_constant b) ->
			Int_constant (a/b)
		|_ -> o		
	end

(* Run a function over all states and transitions *)
let trans_iter fn genv =
    Hashtbl.iter (fun fname (env, func) ->
        Hashtbl.iter (fun sname state ->
            fn state
        ) env.blocks
    ) genv.functions

(* Constant fold all expressions *)
let constant_fold genv =
	trans_iter (fun state ->
		state.edges <- List.map (fun edge ->
			{edge with t=(
			match edge.t with
			|Condition e -> Condition (fold e)
			|Assignment (id,e) -> Assignment (id, (fold e))
			|x -> x)}
		) state.edges
	) genv

(* If a block has only transitions with Condition true,
   then just get rid of it *)
let all_exits_true genv env =
    let elim = ref 0 in
    let initial_state = initial_state_of_env env in
    Hashtbl.iter (fun sname state ->
        if List.length state.edges > 0 &&
           (initial_state.label <> sname) then begin
            let real = List.filter (fun x -> match x.t with
            |Condition True -> false
            |_ -> true) state.edges in
            if List.length real = 0 then begin
                (* This block is redundant, so reroute its edges *)
                incr elim;
                (* Go through all blocks pointing to this one *)
                trans_iter (fun sr ->
                    (* Get the edges to keep, and the ones to replicate *)
                    let to_move, other = List.partition
                        (fun x -> !(x.target).label = state.label) sr.edges in
                    (* Create new edges pointing to the relevant targets *)
                    let new_edges = List.map (fun t_from ->
                        List.map (fun t_to ->
                            { t=t_from.t; target=t_to.target; cl=t_to.cl; loc=t_from.loc }
                        ) state.edges
                    ) to_move in
                    sr.edges <- other @ (List.flatten new_edges);
                ) genv;
                Hashtbl.remove env.blocks sname;
            end
        end
    ) env.blocks;
    !elim

let all_entries_true genv env =
    let elim = ref 0 in
    Hashtbl.iter (fun sname state ->
        (* Figure out how many states point into this state *)
        let pointing_edges = ref [] in
        trans_iter (fun s ->
            let l = List.filter (fun x -> !(x.target).label = state.label) s.edges in
            let r = List.map (fun x -> (s,x)) l in
            pointing_edges := r @ (!pointing_edges)
        ) genv;
        if List.length !pointing_edges > 0 then begin
            let all_true = ref true in
            List.iter (fun (s,e) -> match e.t with
            |Condition True -> ()
            |_ -> all_true := false) !pointing_edges;
            if !all_true then begin
                (* All edges pointing here are Condition True, so re-route.
                   For every edge in !pointing_edges, map to all outgoing transitions *)
                incr elim;
                List.iter (fun (s,e) ->
                    let to_move, other = List.partition
                        (fun x -> !(x.target).label = state.label) s.edges in
                    s.edges <- other @ state.edges;
                ) !pointing_edges;
                Hashtbl.remove env.blocks sname;
            end
        end
    ) env.blocks;
    !elim

(* If a function is only called once, remove redundant return register checks *)    
(* broken , needs to cross across environments 
let fcalls genv =
    let rmed = ref 0 in
    let remove_register reg fenv =
        Hashtbl.iter (fun sname state ->
            state.edges <- List.map (fun t -> match t.t with
                |Condition (Equals (Identifier r, _)) when r = reg->
                    incr rmed;
                    {t with t=(Condition True)}
                |Assignment (r, _) when r = reg ->
                    incr rmed;
                    {t with t=(Condition True)}
                |x -> t
            ) state.edges
        ) fenv.blocks
    in
    (* how many times is each non-exported function called? *)
    let fhash = Hashtbl.create 1 in
    Hashtbl.iter (fun fname (fenv, fdef) ->
        Hashtbl.iter (fun f () ->
            (* fname calls f *)
            hashtbl_add_list fhash f ()
        ) fenv.functions_called;
    ) genv.functions;
    Hashtbl.iter (fun f n ->
        (* can eliminate return registers with Condition True for single calls *)
        if List.length n = 1 then Hashtbl.iter (fun fname (fenv,fdef) ->
            remove_register (sprintf "%s_return" fname) fenv) genv.functions;
    ) fhash
*)

let optimise genv =
    constant_fold genv;
    let total = ref 0 in
    let elim = ref 1 in
    while !elim > 0 do
        elim := 0;
        Hashtbl.iter (fun fname (env, func) ->
            elim := !elim + (all_exits_true genv env);
            elim := !elim + (all_entries_true genv env);
        ) genv.functions;
        total := !elim + !total
    done;
    !total
