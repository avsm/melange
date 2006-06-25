(*
 * Copyright (c) 2004,2005 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2004 David Scott <dave@recoil.org>
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
 * $Id: ssh_utils.ml,v 1.5 2006/03/17 02:55:43 avsm Exp $
 *)

open Unix
open Cryptokit

(* --------------------------------------
 * Utility functions to dump out strings
 * -------------------------------------- *)

(* Take some action if the option parameter is present *)
let umay fn = function
  | Some x -> ignore(fn x)
  | None -> ()

let may fn = function
  | Some x -> Some (fn x)
  | None -> None

let always afterfn fn =
  try
    let ret = fn () in
    afterfn ();
    ret
  with x -> begin
    afterfn ();
    raise x
  end

let string_of_sock_addr = function
  | ADDR_UNIX (path) -> "ADDR_UNIX(" ^ path ^ ")"
  | ADDR_INET (server_addr, port) -> 
      "ADDR_INET(" ^ (string_of_inet_addr server_addr) ^","^
      (string_of_int port) ^ ")"

let string_of_socket_domain = function
  | PF_UNIX -> "PF_UNIX"
  | PF_INET -> "PF_INET"
  | PF_INET6 -> "PF_INET6"

let string_of_host_entry he =
    let sosl x = Printf.sprintf "[%s]" (String.concat "," (Array.to_list x)) in
    Printf.sprintf "name=%s, aliases=%s, domain=%s, addrs=%s"
        he.h_name (sosl he.h_aliases) (string_of_socket_domain he.h_addrtype)
        (sosl (Array.map string_of_inet_addr he.h_addr_list))

(* Bunch of string functions *)

(* string -> char list *)
let rec explode s = 
  if (String.length s = 0) then []
  else (String.get s 0)::(explode (String.sub s 1 (String.length s - 1)))

(* char list -> string *)
let rec implode chars = 
  let s = String.create (List.length chars) in
  let rec setchar n = function
    | [] -> ()
    | (c::cs) -> String.set s n c; setchar (n+1) cs

  in  setchar 0 chars; s

(* string -> (char -> bool) -> string list *)
let split s c =
  let rec search results acc tocome = match tocome with
  | [] -> results @ [acc]
  | (x::xs) -> if (c x) then search (results @ [acc]) [] xs
      else search results (acc @ [x]) xs
  in
  List.map implode (search [] [] (explode s))

(* Look for a key in a hashtbl and add to a list if its present *)
let hashtbl_add_to_list h k v =
    try
        let x = Hashtbl.find h k in
        Hashtbl.replace h k (v::x)
    with Not_found ->
        Hashtbl.add h k [v]

(* Simple cryptokit transforms to get binary strings *)
let binary_of_hex h =
    let h = Str.global_replace (Str.regexp_string " ") "" h in
	let padded = match (String.length h mod 2) with
	|0 -> ""
	|_ -> "0"
	in
	transform_string (Hexa.decode()) (padded ^ h)

(* round an integer up to the nearest alignment *)
let round_up x align =
    let r = x / align * align in (* round down *)
    if x - r > 0 then
        r + align (* round up *)
    else
        r

let binary_of_base64 h = transform_string (Base64.decode()) h
let hex_of_binary b = transform_string (Hexa.encode()) b
let base64_of_binary b = transform_string (Hexa.encode()) b

(* Dump in openssh's dump format to compare binary blobs easily *)
let openssh_dump_hex blob =
    let h = hex_of_binary blob in
    let oc = prerr_char in
    let sep = 8 in
    let num = 4 in
    let s = ref 0 in
    let n = ref 0 in
    String.iter (fun c ->
        oc c;
        incr n;
        if !n = num then begin
            n := 0; 
            oc ' ';
            incr s;
            if !s = sep then begin
                s := 0;
                oc '\n';
            end;
        end;
    ) h;
    oc '\n'

(* error -> string *)
let string_of_cryptokit_error = function
	|Wrong_key_size -> "Key is too long or too short for the given cipher"
	|Wrong_IV_size -> "Initialization vector does not have the same size as the block size"
	|Wrong_data_length -> "Total length of input data for a transform is not an integral multiple of input block size"
	|Bad_padding -> "Incorrect padding bytes were found after decryption"
	|Output_buffer_overflow -> "Output buffer for a transform exceeds the maximal length of a Caml string"
	|Incompatible_block_size -> "Combination of two block ciphers attempted where the ciphers have different block sizes"
	|Number_too_long -> "Denotes an internal error in RSA key generation or encryption"
	|Seed_too_short -> "Seed given to a pseudo random number generator is too short"
	|Message_too_long -> "Message passed to RSA encryption or decryption is greater than the modulus of the RSA key"
	|Bad_encoding -> "Illegal characters were found in an encoding of binary data such as base 64 or hexadecimal"
	|Compression_error (x,y) -> "Compression or decompression error: " ^ x ^ ", " ^ y
	|No_entropy_source -> "No entropy source (OS, /dev/random or EGD) was found for Random.secure_rng"
	|Entropy_source_closed -> "End of file on a device or EGD entropy source"
	|Compression_not_supported -> "Data compression functions are not available"

let string_of_rsa_host_key k =
    let pe s x = (s ^ ": " ^ x) ^ "\n" in
    (pe "size" (string_of_int k.RSA.size)) ^
    (pe "n" k.RSA.n) ^
    (pe "e" k.RSA.e) ^
    (pe "d" k.RSA.d) ^
    (pe "p" k.RSA.p) ^
    (pe "q" k.RSA.q) ^
    (pe "dp" k.RSA.dp) ^
    (pe "dq" k.RSA.dq) ^
    (pe "qinv" k.RSA.qinv) 
