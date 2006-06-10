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
 * $Id: ssh_algorithms.ml,v 1.5 2006/02/20 12:11:12 avsm Exp $
 *)

open Cryptokit

exception Not_implemented

module Cipher = struct

    type t =
    |TripleDES_CBC
    |AES_256_CBC
    |AES_192_CBC
    |AES_128_CBC
    |Arcfour
    |None

    type mode =
    | Encrypt
    | Decrypt

    type info = { name: t; key_len: int; block_size: int }

    type fn = Stream of Stream.stream_cipher | Block of Block.block_cipher

    exception Unknown of string

    let to_string = function
    | TripleDES_CBC -> "3des-cbc"
    | AES_256_CBC -> "aes256-cbc"
    | AES_192_CBC -> "aes192-cbc"
    | AES_128_CBC -> "aes128-cbc"
    | Arcfour -> "arcfour"
    | None -> "anil"

    let from_string = function
    | "3des-cbc" -> TripleDES_CBC
    | "aes256-cbc" -> AES_256_CBC
    | "aes192-cbc" -> AES_192_CBC
    | "aes128-cbc" -> AES_128_CBC
    | "arcfour" -> Arcfour
    | "none" -> None
    | "anil" -> None
    | x -> raise (Unknown x)

    let info = function
    | TripleDES_CBC -> {name=TripleDES_CBC; key_len=24; block_size=8}
    | AES_256_CBC -> {name=AES_256_CBC; key_len=32; block_size=16}
    | AES_192_CBC -> {name=AES_192_CBC; key_len=24; block_size=16}
    | AES_128_CBC -> {name=AES_128_CBC; key_len=16; block_size=16}
    | Arcfour -> {name=Arcfour; key_len=16; block_size=8}
    | None -> {name=None; key_len=0; block_size=0}

    (* Given an initialisation vector, key, algorithm and cipher direction,
       return a wrapped Cryptokit transformation of the appropriate type *)
    let fn iv key alg =
        let null_stream = object
            method transform = String.blit
            method wipe = ()
        end in
    function
    | Encrypt -> begin match alg with
        | TripleDES_CBC ->
            let t = new Block.triple_des_encrypt key in
            Block (new Block.cbc_encrypt ~iv:iv t)
        | AES_256_CBC | AES_192_CBC | AES_128_CBC ->
            let t = new Block.aes_encrypt key in
            Block (new Block.cbc_encrypt ~iv:iv t)
        | Arcfour -> Stream (new Stream.arcfour key)
        | None -> Stream null_stream
        end
    | Decrypt -> begin match alg with
        | TripleDES_CBC ->
            let t = new Block.triple_des_decrypt key in
            Block (new Block.cbc_decrypt ~iv:iv t)
        | AES_256_CBC | AES_192_CBC | AES_128_CBC ->
            let t = new Block.aes_decrypt key in
            Block (new Block.cbc_decrypt ~iv:iv t)
        | Arcfour -> Stream (new Stream.arcfour key)
        | None -> Stream null_stream
        end

    (* Accept a wrapper Cryptokit object and process the given data
       through it.  Data must meet blocksize requirements *)
    let data cipher srcbuf srcoff dstbuf dstoff len =
        match cipher with 
        | Block y ->
            let bs = y#blocksize in
            let num_blocks = (len / bs) - 1 in
            assert (num_blocks >= 0);
            for i = 0 to num_blocks do
                y#transform srcbuf (srcoff+(i*bs)) dstbuf (dstoff+(i*bs))
            done;
        | Stream y ->
            y#transform srcbuf srcoff dstbuf dstoff len
end

module MAC = struct
    type t =
    | SHA1
    | SHA1_96
    | MD5
    | MD5_96
    | None

    type info = { name: t; digest_len: int; key_len: int; fn: string->hash }
        
    exception Unknown of string
    
    let to_string = function
    | SHA1 -> "hmac-sha1"
    | SHA1_96 -> "hmac-sha1-96"
    | MD5 -> "hmac-md5"
    | MD5_96 -> "hmac-md5-96"
    | None -> "anilmac"
    
    let from_string = function
    | "hmac-sha1" -> SHA1
    | "hmac-sha-96" -> SHA1_96
    | "hmac-md5" -> MD5
    | "hmac-md5-96" -> MD5_96
    | "anilmac" -> None
    | "none" -> None
    | x -> raise (Unknown x)
   
    let null_mac (_:string) = object
        method add_byte (x:int) = ()
        method add_char (x:char) = ()
        method add_string (x:string) = ()
        method add_substring (x:string) (_:int) (_:int) = ()
        method hash_size = 0
        method result = ""
        method wipe = () end
    let info = function
    | SHA1 -> { name=SHA1; digest_len=20; key_len=20; fn=MAC.hmac_sha1}
    | SHA1_96 -> { name=SHA1_96; digest_len=12; key_len=20; fn=MAC.hmac_sha1}
    | MD5 -> { name=MD5; digest_len=16; key_len=16; fn=MAC.hmac_md5 }
    | MD5_96 -> { name=MD5_96; digest_len=12; key_len=16; fn=MAC.hmac_md5 }
    | None -> { name=None; digest_len=0; key_len=0; fn=null_mac }

    (* Given an algorithm, sequence number, initialisation vector and some data,
       generate a hash and return it *)
    let generate mac seqnum key src off len =
        let hash = mac.fn key in
        let i32 = !seqnum in
        let i32s = String.create 4 in
        let (&&) = Int32.logand in let (>>) = Int32.shift_right_logical in
        i32s.[3] <- Char.chr (Int32.to_int (i32 && 255l));
        i32s.[2] <- Char.chr (Int32.to_int ((i32 >> 8) && 255l));
        i32s.[1] <- Char.chr (Int32.to_int ((i32 >> 16) && 255l));
        i32s.[0] <- Char.chr (Int32.to_int ((i32 >> 24) && 255l));
        hash#add_string i32s;
        hash#add_substring src off len;
        let mk = hash#result in
        match mac.name with
        | SHA1 | MD5 -> mk
        | SHA1_96 | MD5_96 -> String.sub mk 0 mac.digest_len
        | None -> ""

end
