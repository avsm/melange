(*pp cpp *)
(*
 * Copyright (c) 2006 Anil Madhavapeddy <anil@recoil.org>
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
 *)

open Printf
open Ssh_utils

type t = { raw: string option;   (* needed for KEXDH_REPLY signature hash *)
           protoversion: string;
           softwareversion: string }
           
let to_string p =
    match p.raw with
    |None -> sprintf "SSH-%s-%s" p.protoversion p.softwareversion
    |Some x -> x

exception Parse_failure

(* Read version character by character.  Cant use channels due to buffering *)
let read_version_line ofd =
	let buf = String.create 256 in
	let ver = ref "" in
	let off = ref 0 in
	let got_nl = ref false in
	while not (!got_nl) do
		ofd#read buf !off 1;
		match String.get buf !off with
		|'\n' ->
			(* newline is here, so stop reading *)
			got_nl := true;
			ver := String.sub buf 0 !off
		|'\r' ->
			(* just discard this, as it will be followed by a newline *)
			()
		|_ -> incr off
	done;
	!ver
	
let unmarshal ofd = 
    let str = read_version_line ofd in
	prerr_endline (sprintf "Remote version is: %s" str);
    (* It should be in form SSH-protoversion-softwareversioncomments    *)
    if String.sub str 0 4 <> "SSH-" 
    then raise Parse_failure;
    let str' = String.sub str 4 (String.length str - 4) in
    let other_dash =
        try String.index str' '-' 
        with Not_found -> raise Parse_failure in
    let proto = String.sub str' 0 (other_dash - 1) in
    let software = String.sub str' (other_dash + 1) (String.length str' - other_dash - 1) in
    { raw = Some str; protoversion = proto; softwareversion = software }
