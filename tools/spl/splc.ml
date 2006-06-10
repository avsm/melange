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
 * $Id: splc.ml,v 1.29 2006/02/09 17:44:52 avsm Exp $
 *)

open Printf
open Spl_utils

type output_mode =
    |Dot
    |OCaml
    |Promela
    |GUI
    |Debug
    
let default_output_mode = Promela
let default_optimisation = true
let default_log_level = Logger.Normal

let main =
    let mode = ref default_output_mode in
    let optimise = ref default_optimisation in
    let log_level = ref default_log_level in
    let files = ref [] in
    let ofile = ref None in
    let statecall_fname = ref None in
    let html_dir = ref "." in
    let debug = ref false in
    let parse = [
        "-O", Arg.Int (fun x -> if x = 0 || x = 1 then
            optimise := (x=1) else
            raise (Arg.Bad (sprintf "Bad optimisation level %d" x))),
        "Optimisation level [0|1]";
        "-q", Arg.Unit (fun () -> log_level := Logger.Quiet),
        "Quiet mode";
        "-v", Arg.Unit (fun () -> log_level := Logger.Verbose),
        "Verbose mode";
        "-hdir", Arg.String (fun x -> html_dir := x),
        "HTML Template Directory [.]";
        "-d", Arg.Bool (fun x -> debug := true),
        "Debug mode [false]";
        "-t", Arg.String (fun x -> mode := match x with
            |"ocaml"|"ml" -> OCaml
            |"dot" -> Dot
            |"promela"|"pro" -> Promela
            |"debug" -> Debug
            |"gui" -> GUI
            |x -> raise (Arg.Bad ("Unknown output type: " ^ x))),
        "Output mode [ocaml|dot|promela|gui|debug]";
        "-s", Arg.String (fun x -> statecall_fname := Some x),
        "Filename for statecalls output (backend specific)";
        "-o", Arg.String (fun x -> ofile := Some x),
        "Output file basename (without extension)" 
    ] in
    let usagestr = "Usage: splc <options> input-file" in
    Arg.parse parse (fun x -> files := x :: !files) usagestr;
    (* Need to have one input file *)
    if List.length !files < 1 then begin Arg.usage parse usagestr; exit 1; end;
    Logger.set_log_level !log_level;
    Logger.log (sprintf "Input files: %s" (String.concat " " !files));
    (* Open file and start compilation process *)
    let results = List.map (fun file ->
        Logger.log (sprintf "Processing %s" file);
        let fin = open_in file in
        let lexbuf = Lexing.from_channel fin in
        Spl_location.start_parse file;
        let result = try Spl_parser.main Spl_lexer.token lexbuf 
        with Spl_syntaxtree.Syntax_error l -> begin
            Logger.log_quiet (sprintf "Syntax error%s near token '%s'"
                (Spl_location.string_of_location l) (Lexing.lexeme lexbuf));
            exit 1;
        end in
        let _ = try Spl_typechecker.type_check result
        with Spl_typechecker.Type_error (e,l) -> begin
            Logger.log_quiet (sprintf "Type error%s %s"
                (Spl_location.string_of_location l) e);
            exit 1;
        end in
        (* normalise locations for the html output *)
        List.iter Spl_debug.pp_statements result;
        (* generate cfg *)
        let genv = Spl_cfg.generate_states file result in
        if !optimise then begin
            Logger.log "Optimising CFA... ";
            let removed = Spl_optimiser.optimise genv in
            Logger.log (sprintf "  %d state%s eliminated" removed
            (if removed > 1 then "s" else ""));
        end;
        if !debug then begin
            Spl_debug.pp_global !html_dir genv
        end;
        genv
    ) !files in
    match !mode with
    |Dot ->
        List.iter (fun genv ->
            let file = genv.Spl_cfg.filename in
            let dot_basename = Filename.chop_extension file in
            let dot_ochan subgraphname fn =
                let outfname = sprintf "%s-%s.dot" dot_basename subgraphname in
                let fout = open_out outfname in
                Logger.log (sprintf "DOT graph (%s): %s" subgraphname outfname);
                fn fout;
                close_out fout
            in
            Spl_dot.generate dot_ochan genv;
        ) results;
        exit 0
    |GUI ->
        List.iter (fun genv ->
            let file = genv.Spl_cfg.filename in
            let gui_basename = Filename.chop_extension file in
            let gui_ochan subgraphname fn =
                let outfname = sprintf "%s-%s.gui" gui_basename subgraphname in
                let fout = open_out outfname in
                Logger.log (sprintf "GUI graph (%s): %s" subgraphname outfname);
                fn fout;
                close_out fout
            in
            Spl_gui.generate gui_ochan genv;
        ) results;
        exit 0
    |OCaml  ->
        let sfile = match !statecall_fname with
        |None -> failwith "Must specify statecall filename with -s for OCaml"
        |Some x -> x in
        let fnames =
            if List.length !files = 1 then begin
                match !ofile with
                |None -> [safe_chop (List.hd !files)]
                |Some x -> [x]
            end else begin
                match !ofile with
                |None ->
                    List.map (fun x -> safe_chop x) !files
                |Some b ->
                    let basenm = safe_chop b in
                    List.map (fun x -> basenm ^ "_" ^
                        (safe_chop (Filename.basename x))) !files
            end;
        in
        Logger.log (sprintf "Input files: %s" (String.concat " " !files));
        Logger.log (sprintf "Output files: %s" (String.concat " " fnames));
        Spl_ocaml.generate sfile fnames !debug results;
        exit 0
    |Promela ->
        List.iter (fun genv ->
            let file = genv.Spl_cfg.filename in
            let basename = Filename.chop_extension file in
            let outfname = sprintf "%s.promela" basename in
            let fout = open_out outfname in
            Logger.log (sprintf "Promela: %s" outfname);
            Spl_promela.generate fout genv;
            close_out fout
        ) results;
        exit 0
    |Debug ->
        exit 0
