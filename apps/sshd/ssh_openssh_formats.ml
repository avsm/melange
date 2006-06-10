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
 * $Id: ssh_openssh_formats.ml,v 1.3 2006/03/17 02:55:43 avsm Exp $
 *
 * Compatibility routines for OpenSSH file formats
 *)

open Ssh_utils

exception Parse_failure

(* Parse /etc/moduli format, documented in moduli(5) from OpenSSH,
   raises Parse_failure if anything messes up *)
let moduli file primes =
    Random.self_init ();
    try begin
        let fin = open_in file in
        try while true do
            let line = input_line fin in
            if String.length line > 0 && String.get line 0 != '#' then begin
                let spl = Str.split (Str.regexp_string " ") line in
                let nth = List.nth spl in
                let size = Int32.succ (Int32.of_string (nth 4)) in
                let generator = Mpl_stdlib.Mpl_mpint.of_string 
                    (binary_of_hex (Printf.sprintf "%02x" (int_of_string (nth 5)))) in
                let prime = Mpl_stdlib.Mpl_mpint.of_string (binary_of_hex (nth 6)) in
                hashtbl_add_to_list primes size (prime, generator)
                end
        done;
        with End_of_file -> close_in fin
    end with _ -> raise Parse_failure
