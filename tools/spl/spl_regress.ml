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
 * $Id: spl_regress.ml,v 1.6 2006/02/06 16:03:02 avsm Exp $
 *)

open Spl_utils.Printer
open Printf
open Str

exception Bad_file_format of string

let default_output_file = "regress_main.ml"

type id = string
type args = string list
type ocaml =
    |Module of (id * ocamls)
    |Open of id
    |Statement of string
    |Function of (id * args * ocamls)
    |Try_with of (ocamls * pattern_match)
    |List_iter of (string * string * ocamls)
and
pattern_match =
    (id * ocaml list) list
and ocamls = ocaml list

let rec output_ocaml e = function
    |Module (name, rest) -> 
        e.p (sprintf "module %s = struct" (String.capitalize name));
        indent_fn e (fun e -> output_ocamls e rest);
        e.p "end";
        e.nl ()
    |Open s ->
        e.p (sprintf "open %s" (String.capitalize s));
    |Function (n,a,rest) ->
        e.p (sprintf "let %s %s =" n (String.concat " " a));
        indent_fn e (fun e -> output_ocamls e rest);
        e.nl ()
    |Try_with (a,b) ->
        e.p "try";
        indent_fn e (fun e -> output_ocamls e a);
        e.p "with";
        output_pattern_match e b;
        e.nl ()
    |List_iter (a,b,rest) ->
        e.p (sprintf "List.iter (fun %s ->" a);
        indent_fn e (fun e -> output_ocamls e rest);
        e.p (sprintf ") %s;" b);
    |Statement s ->
        e.p s
and
output_ocamls e = List.iter (output_ocaml e)
and
output_pattern_match e x =
    List.iter (fun (id, rest) ->
        e.p (sprintf "|%s -> begin" id);
        indent_fn e (fun e ->
            output_ocamls e rest;
            e.p "end";
        )
    ) x


let gen_ocaml combos =
    let afn = String.concat ";" in
    [
        Function ("_",[],
            (List.map (fun (p,seq) ->
                Statement (sprintf "fn %s [%s] [%s];" (string_of_bool p)
                    (afn (List.map (fun x -> "`" ^ x) seq)) (afn (List.map (fun x -> sprintf "\"%s\"" x) seq)))) combos)
            @ [Statement "()"]
        )
    ]

(* Insert bad S0 states at every point as well as the correct states *)
let gen_combos pass seqs =
    let dummy = "S0" in
    let rec numlist n a = match n with |(-1) -> a |x -> numlist (n-1) (x::a) in
    let nums = numlist (List.length seqs) [] in
    let fails = List.map (fun n ->
        (false,
            (
                let c = ref (-1) in
                let l,r = List.partition (fun x -> incr c; !c < n) seqs in
                let r = dummy :: r in
                l @ r
            )
        )
    ) nums in
    let ok = (pass, seqs) in
    ok :: fails
    
let _ =
    let ifile = Sys.argv.(1) in
    let ofile = default_output_file in
    let fout = open_out ofile in    
    let env = init_printer fout in
    let args = Hashtbl.create 1 in
    let fin = open_in ifile in
    try while true do
        let l = input_line fin in
        let bits = split (regexp_string ":") l in
        if List.length bits != 2 then
            raise (Bad_file_format "Every line must be <arg>:<value>");
        Hashtbl.add args (List.hd bits) (List.nth bits 1);
    done with
        End_of_file -> begin close_in fin; end;
    let all_runs_fn k = List.map (fun x -> split (regexp_string " ") x) 
        (Hashtbl.find_all args k) in
    let good_runs = List.map (gen_combos true) (all_runs_fn "G") in
    let bad_runs = List.map (gen_combos false) (all_runs_fn "B") in
    let x = gen_ocaml (List.flatten (good_runs @ bad_runs)) in
    let prefix = [
        Open "Gcstat";
        Function ("gcs", [], [Statement "gcstat_init \"localhost\" 1234"]);
        Function ("out", [], [Statement "prerr_endline"]);
        Function ("conc", [], [Statement "String.concat \";\""]);
        Function ("pass", ["m"; "s"],
            [Statement "out (\"PASS:\" ^ (if m then \"G:\" else \"B:\") ^ (conc s));";
             Statement "gcstat_checkpoint gcs \"end\"";
            ]);
        Function ("fail", ["m"; "s"],
            [Statement "out (\"FAIL:\" ^ (if m then \"G:\" else \"B:\") ^ (conc s));";
             Statement "gcstat_checkpoint gcs \"end\";";
             Statement "exit 1"]);
        Function ("fn", ["want_pass"; "(seq:Regress_automaton.s list)"; "seqs"],
            [ Statement "let a = ref (Regress_automaton.init ()) in";
(*              Statement "prerr_endline (\"Ticking: \" ^ (String.concat \",\" seqs));"; *)
              Statement "gcstat_checkpoint gcs \"start\";";
              Try_with (
                [ List_iter ("x", "seq", [
(*                    Statement "prerr_endline (\"Tick: \" ^ (string_of_statecall x));"; *)
                    Statement "a := Regress_automaton.tick !a x;";
                    Statement "gcstat_send gcs;";
(*                    Statement "Regress_automaton.print !a;" *)
                  ]);
                  Statement "if want_pass then pass want_pass seqs else fail want_pass seqs"],
                [("Regress_automaton.Bad_statecall",
                    [Statement "if want_pass then fail want_pass seqs else pass want_pass seqs"
                    ]
                )]
              );
            ]
        )
    ] in
    let suffix = [] in
    List.iter (output_ocaml env) (prefix @ x @ suffix);
    close_out fout;
    ()
