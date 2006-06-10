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
 * $Id: spl_debug.ml,v 1.15 2006/02/12 18:06:49 avsm Exp $
 *)

open Printf
open Spl_syntaxtree
open Spl_utils
open Printer

let string_of_arg = function
    |Integer x -> "int " ^ x
    |Boolean x -> "bool " ^ x
    |Unknown x -> "unknown" ^ x

let string_of_args i = 
    sprintf "(%s)" (String.concat "," (List.map string_of_arg i))

let string_of_ids i =
    String.concat "," i

let map_attrs attrs =
    String.concat " " (List.map (fun (n,x) -> sprintf "%s=\"%s\"" n x) attrs)

let span cl x =
    sprintf "<span class=\"%s\">%s</span>" cl x
let spanid = span "identifier"

let tag ?(attrs=[]) t l =
    sprintf "<%s %s>%s</%s>" t (map_attrs attrs) (String.concat "\n" l) t

let spaces num =
    let buf = Buffer.create 1 in
    for i = 1 to num do
        Buffer.add_string buf "&nbsp;";
    done;
    Buffer.contents buf

let line_num = ref 0

(* Convert expression to a string *)
let rec string_of_expr = function
    | And (a,b) -> sprintf "(%s &amp;&amp; %s)" (string_of_expr a) (string_of_expr b)
    | Or (a,b) -> sprintf "(%s || %s)" (string_of_expr a) (string_of_expr b)
    | Identifier i -> i
    | Not e -> sprintf "(!%s)" (string_of_expr e)
    | True -> "true"
    | False -> "false"
    | Greater (a,b) -> sprintf "(%s &gt; %s)" (string_of_expr a) (string_of_expr b)
    | Less (a,b) -> sprintf "(%s &lt; %s)"  (string_of_expr a) (string_of_expr b)
    | Greater_or_equal (a,b) -> sprintf "(%s >= %s)"  (string_of_expr a) (string_of_expr b)
    | Less_or_equal (a,b) -> sprintf "(%s <= %s)"  (string_of_expr a) (string_of_expr b)
    | Equals (a,b) -> sprintf "(%s == %s)"  (string_of_expr a) (string_of_expr b)
    | Plus (a,b) -> sprintf "(%s + %s)" (string_of_expr a) (string_of_expr b)
    | Minus (a,b) -> sprintf "(%s - %s)" (string_of_expr a) (string_of_expr b)
    | Multiply (a,b) -> sprintf "(%s * %s)" (string_of_expr a) (string_of_expr b)
    | Divide (a,b) -> sprintf "(%s / %s)" (string_of_expr a) (string_of_expr b)
    | Int_constant a -> sprintf "%d" a

let rec pp_statement buf i (_,xs) =
    let ind x =
        let r = sprintf "<tr id=\"line%d\"><td class=\"location\">%d</td><td>%s%s</td></tr>\n"
            !line_num !line_num (spaces (i*2)) x in
        incr line_num;
        Buffer.add_string buf r
    in
    let indid x = ind (sprintf "%s;" (spanid x)) in
    let curloc () = { Spl_location.file_name=None; line_num= !line_num; column_num=0 } in
    let ppxs = List.map (pp_statement buf (succ i)) in
    let loc = curloc () in
    match xs with
    | Abort -> indid "abort"; (loc, Abort)
    | Exit -> indid "exit"; (loc, Exit)
    | Sequence x as p -> ind (sprintf "%s;" (span "statecall" x)); (loc, p)
    | Assign (i,e) as p -> ind (sprintf "%s = %s;" i (string_of_expr e)); (loc, p)
    | Function_call (n,fs) as p ->
        ind (sprintf "%s%s;" n (string_of_args fs)); (loc, p)
    | Do_until (e, xs) ->
        ind (sprintf "%s {" (spanid "do"));
        let xs' = ppxs xs in
        ind (sprintf "%s %s" (spanid "until") (string_of_expr e));
        (loc, Do_until (e, xs'))
    | Multiple (l,h,xs) ->
        let l' = match l with None -> 0 |Some x -> x in
        let h' = match h with None -> "&#8734;" |Some x -> string_of_int x in
        ind (sprintf "%s (%d,%s) {" (spanid "multiple") l' h');
        let xs' = ppxs xs in
        ind "}";
        (loc, Multiple(l,h,xs'))
    | Always_allow (ids, xs) ->
        ind (spanid "allow");
        List.iter (fun x -> ind (sprintf "&nbsp;%s" (span "statecall" x))) ids;
        ind (sprintf "%s {" (spanid "in"));
        let xs' = ppxs xs in
        ind (sprintf "}");
        (loc, Always_allow(ids,xs'))
    | While (e, xs) ->
        ind (sprintf "%s (%s):" (spanid "while") (string_of_expr e));
        let xs' = ppxs xs in
        (loc, While (e,xs'))
    | During_handle (xs, exsl) ->
        ind (sprintf "%s {" (spanid "during"));
        let xs' = ppxs xs in
        let exsl' = List.map (fun x ->
            ind (sprintf "} %s {" (spanid "handle"));
            ppxs x
        ) exsl in
        ind "}";
        (loc, During_handle (xs', exsl'))
    | Either_or (xsl) ->
        let fst = ref true in
        let fn () = spanid (if !fst then (fst := false; "either") else "} or") in
        let xsl' = List.map (fun (gc,xs) ->
            let cond = match gc with
            |True -> ""
            |x -> sprintf "(%s)" (string_of_expr gc) in
            ind (sprintf "%s %s {" (fn ()) cond);
            (gc, (ppxs xs));
        ) xsl in
        ind "}";
        (loc, Either_or(xsl'))
    
let pp_statements f =
    let buf = Buffer.create 1 in
    f.body <- List.map (pp_statement buf 0) f.body;
    f.html <- Buffer.contents buf

let pp_func anm (env,f) =
    let a = sprintf "(<a href=\"javascript:toggle('%s_body')\">#</a>) %s &nbsp;" anm anm in
    let b = tag "div" ~attrs:["id",anm] [
        sprintf "<td valign=\"top\">&nbsp; <div class=\"starthidden\" id=\"%s_body\"><table><tr class=\"autoheader\"><td colspan=\"2\">
            (<a href=\"javascript:toggle('%s_body')\">#</a>) <b>%s</b></td></tr><tr><td>
            <table width=\"100%%\" class=\"autobody\"><tr><td>" anm anm anm;
        f.html;
        "</td></tr></table></td></tr></table></div></td>";
    ] in
    a,b

let pp_global html_dir genv =
    let h = Hashtbl.create 1 in
    Hashtbl.iter (fun fname (env,f) ->
        let anm = fname in
        if f.export then begin 
            Hashtbl.add h anm (pp_func anm (env,f));
        end;
    ) genv.Spl_cfg.functions;
    let tmpfn f = string_of_file (sprintf "%s/%s" html_dir f) in
    let html = tmpfn "template.html" in
    let buf = Buffer.create 1 in
    Buffer.add_substitute buf (function 
        |"STYLE" -> tmpfn "style.css"
        |"MOCHIKIT" -> tmpfn "mochikit.js"
        |"CANVASGRAPH" -> tmpfn "canvasgraph.js"
        |"JAVASCRIPT" -> tmpfn "script.js"
        |"TOTALLINES" -> string_of_int !line_num
        |"SPLURL" -> "http://localhost:1234/"
        |"AUTOMENU" -> String.concat "\n" (Hashtbl.fold (fun k v a -> fst v :: a) h [])
        |"AUTO" ->
            let graph = "<div><canvas width=\"200\" height=\"200\" id=\"piechart\"></canvas></div>" in
            let es = Hashtbl.fold (fun k v a -> snd v :: a) h [] in
            sprintf "<table><tr><td valign=\"top\">&nbsp;%s<br /><div id=\"allcalls\"></div></td>%s</tr></table>" graph (String.concat "" es)
        |x -> failwith x
    ) html;
    genv.Spl_cfg.webpage <- (Buffer.contents buf)
