(*
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
 * $Id: ssh_pty.ml,v 1.2 2006/03/16 12:23:01 avsm Exp $
 *)

type pty = {
    masterfd: Unix.file_descr;
    slavefd: Unix.file_descr;
    name: string;
}

type pty_window = {
    row: int32;
    col: int32;
    xpixel: int32;
    ypixel: int32;
}

(* Exceptions raised by Pty functions *)
exception Pty_error of string
let _ = Callback.register_exception "pty_error" (Pty_error "")

(* External declarations of Pty bindings *)
external open_pty : unit -> pty = "pty_open_pty"
external switch_controlling_pty : pty -> unit = "pty_switch_controlling_tty"
external window_size : pty -> pty_window -> unit = "pty_window_size"

(* Convenience ML functions *)
let close_pty pty =
    try Unix.close pty.masterfd with _ -> ();
    try Unix.close pty.slavefd with _ -> ()

(* Internal declarations of Pty bindings *)
let string_of_pty p = Printf.sprintf "name=%s" p.name
