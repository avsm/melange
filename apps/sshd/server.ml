(*
 * Copyright (c) 2004 David Scott <dave@recoil.org>
 * Copyright (c) 2004 Anil Madhavapeddy <anil@recoil.org>
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
 * $Id: server.ml,v 1.2 2006/01/18 18:45:00 avsm Exp $
 *)
 
(* NB Both Pervasives and Unix define 'stdout'. Opening this module  *)
(* overwrites the Pervasives version.                                *)

open Unix
open Ssh_utils

(* The signature for a 'server function' *)
type serverfn = Olog.base_log -> Unix.file_descr -> unit

(* A 'handler' (a style of dispatch) matches this signature *)
type handler = Olog.base_log -> serverfn -> Unix.file_descr -> Unix.sockaddr -> unit

(* handler which fork()s to serve every request *)
let handle_by_fork msg server_fun s caller = 
  (* The "double fork" trick, the process which calls server_fun will not
     leave a zombie process *)
  msg#debug "fork()";
  match fork() with
    0 -> if fork() <> 0 then exit 0; (* The son exits, the grandson works *)
      server_fun msg caller s;
      close s;
      exit 0
  | id -> close s; ignore(waitpid [] id) (* Reclaim the son *)

let handle_single msg server_fun s caller =
    server_fun msg caller s;
    close s;
    exit 0

(* Return a socket bound to the supplied address *)
let create_sock_and_bind sockaddr = 
  let domain = match sockaddr with 
  | ADDR_UNIX _ -> PF_UNIX 
  | ADDR_INET(_,_) -> PF_INET in
  let sock = socket domain SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  bind sock sockaddr;
  sock

let block_accept_dispatch msg handler sock = 
  msg#debug ("Socket bound to " ^ (string_of_sock_addr (getsockname sock)));
  listen sock 5;
  while true do
    let (s, caller) = accept sock in handler s caller
  done

(* Similar to Unix.establish_server. Give it a style of request handling *)
(* and a specific request handler and it does the rest.                  *)
let establish_server msg handler sockaddr =
  let sock = create_sock_and_bind sockaddr in
  block_accept_dispatch msg handler sock
