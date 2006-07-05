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

let unmarshal ofd = 
    (* XXX - need to make sure input_line is safe wrt \r\n - avsm *)
    let str = input_line ofd#in_channel in
    (* there might be ASCII 13 characters in the buffer and probably    *)
    (* one stuck on the end.                                            *)
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
