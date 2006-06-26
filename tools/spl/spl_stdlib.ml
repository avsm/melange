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

(* Standard Helper functions for using SPL automata *)

let get_tcp_sock_factory port =
        let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Unix.setsockopt s Unix.SO_REUSEADDR true;
        Unix.bind s (Unix.ADDR_INET(Unix.inet_addr_any,port));
        Unix.listen s 5;
        let sock_factory () =
            let fd,_ = Unix.accept s in
            let ic = Unix.in_channel_of_descr fd in
            let oc = Unix.out_channel_of_descr fd in
            oc,ic
        in
        sock_factory