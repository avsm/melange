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

let max_packet_size = 35000

type pool = {
    size: int;                       (* bytes in pool pages *)
    mutable free_list: string list;  (* list of buffers *)
    mutable used_list: string list;  (* used buffers *)
}
let pool =
    let size = max_packet_size in
    let l = List.map (fun _ -> String.create size) [1;2;3;4;5;6;7;8;9;10;11;12] in
    { size=size; free_list=l; used_list=[] }

let get () =
    match pool.free_list with
    |hd::tl ->
        pool.free_list <- tl;
        pool.used_list <- hd :: pool.used_list;
        Mpl_stdlib.new_env hd
    |[] -> failwith "out of luck"

let reset () =
    List.iter (fun b ->
      (*  String.fill b 0 (String.length b) 'X';  *)
        pool.free_list <- b :: pool.free_list;
    ) pool.used_list;
    pool.used_list <- []

let get_fn fn =
    let buf = match pool.free_list with
    |hd::tl ->
        pool.free_list <- tl;
        hd
    |[] -> failwith "out of luck" in
    always (fun () -> pool.free_list <- buf :: pool.free_list)
        (fun () -> fn (Mpl_stdlib.new_env buf))
        
let get_string_fn fn =
    get_fn (fun env -> fn env; Mpl_stdlib.string_of_env env)
