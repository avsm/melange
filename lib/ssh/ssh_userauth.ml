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
 * $Id: ssh_userauth.ml,v 1.2 2006/01/14 17:46:00 avsm Exp $
 *)

open Ssh_utils

exception Unknown_userauth

type t =
    |Password
    |Public_key
    |Keyboard_interactive
    |Host_based
        
let to_string = function
|Password -> "password"
|Public_key -> "publickey"
|Keyboard_interactive -> "keyboard-interactive"
|Host_based -> "hostbased"

let of_string = function
|"password" -> Password
|"publickey" -> Public_key
|"keyboard-interactive" -> Keyboard_interactive
|"hostbased" -> Host_based
|_ -> raise Unknown_userauth

let to_string x = String.concat "," (List.map to_string x)