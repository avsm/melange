(*
 * Copyright (c) 2004,2005 Anil Madhavapeddy <anil@recoil.org>
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
 * $Id: ssh_kex.ml,v 1.10 2006/03/17 02:55:43 avsm Exp $
 *)

(* Key-Exchange stuff ------------------------------------------------ *)

open Ssh_utils

exception Not_implemented
exception Key_too_short

module Methods = struct

    module M  = Mpl_stdlib
    module MP = M.Mpl_mpint
    module MB = M.Mpl_byte
    module BS = M.Mpl_string32
    module I32 = M.Mpl_uint32
    module MR = M.Mpl_raw
    
    type t = 
       | DiffieHellmanGexSHA1
       | DiffieHellmanGroup1SHA1
       | DiffieHellmanGroup14SHA1

    module DHGroup = struct
        type kex_hash = {
            v_c: Ssh_version.t;           (* clients version *)
            v_s: Ssh_version.t;           (* servers version *)
            i_c: string;                            (* clients last kexinit *)
            i_s: string;                            (* servers last kexinit *)
            k_s: string;                            (* servers host key *)
            e: MP.t;                                (* exchange value sent by client *)
            f: MP.t;                                (* response value sent by server *)
            k: MP.t;                                (* the shared secret *)
        }
        
        let marshal args =
            Ssh_pool.get_string_fn (fun env ->
                let ms x = ignore(BS.marshal env (BS.of_string x)) in
                let mp x = ignore(MP.marshal env x) in
                ms (Ssh_version.to_string args.v_c);
                ms (Ssh_version.to_string args.v_s);
                ms args.i_c;
                ms args.i_s;
                ms args.k_s;
                mp args.e;
                mp args.f;
                mp args.k;
            )
    end
    
    module DHGex = struct
        type kex_hash = {
            v_c: Ssh_version.t;   (* clients version string (no \r\n) *)
            v_s: Ssh_version.t;   (* servers version string (no \r\n) *)
            i_c: string;   (* payload of clients last kexinit  *)
            i_s: string;   (* payload of servers last kexinit  *)
            k_s: string;   (* servers host key                 *)
            min: int32;    (* minimal number of bits in group  *)
            n: int32;      (* preferred number of bits         *)
            max: int32;    (* maximum number of bits in group  *)
            p: MP.t;       (* safe prime                       *)
            g: MP.t;       (* generator for subgroup           *)
            e: MP.t;       (* exchange value sent by client    *)
            f: MP.t;       (* response value sent by server    *)
            k: MP.t;       (* the shared secret                *)
        }    

        let marshal a =
            Ssh_pool.get_string_fn (fun env ->
                let bsm x = ignore(BS.marshal env (BS.of_string x)) in
                let mpm x = ignore(MP.marshal env x) in
                let um x = ignore(I32.marshal env (I32.of_int32 x)) in
                bsm (Ssh_version.to_string a.v_c);
                bsm (Ssh_version.to_string a.v_s);
                bsm a.i_c;
                bsm a.i_s;
                bsm a.k_s;
                um a.min;
                um a.n;
                um a.max;
                mpm a.p;
                mpm a.g;
                mpm a.e;
                mpm a.f;
                mpm a.k;
            )
        
        type moduli = (int32, (MP.t * MP.t) list) Hashtbl.t
        let choose ~min ~want ~max (primes:moduli) =
            let best_size = Hashtbl.fold (fun size _ best_size ->
                if size < min || size > max then
                    best_size
                else begin
                    if (size > want && size < best_size) ||
                       (size > best_size && best_size < want) then
                        size
                    else
                        best_size
                end
            ) primes 0l in
            try
                let options = Hashtbl.find primes best_size in
                let len = List.length options in
                let choice = Random.int len in
                Some (List.nth options choice)
            with Not_found -> None

    end

    exception Unknown of string

    let to_string = function
       | DiffieHellmanGexSHA1 -> "diffie-hellman-group-exchange-sha1"
       | DiffieHellmanGroup1SHA1 -> "diffie-hellman-group1-sha1"
       | DiffieHellmanGroup14SHA1 -> "diffie-hellman-group14-sha1"

    let from_string = function
       | "diffie-hellman-group-exchange-sha1" ->
            DiffieHellmanGexSHA1
       | "diffie-hellman-group1-sha1" ->
            DiffieHellmanGroup1SHA1
       | "diffie-hellman-group14-sha1" ->
            DiffieHellmanGroup14SHA1
       | x -> raise (Unknown x)

    let public_parameters = function
       | DiffieHellmanGroup1SHA1 ->
            (* RFC2409 - Oakley group 2, 1024-bit MODP group *)
            let p = binary_of_hex (
              "FFFFFFFF FFFFFFFF C90FDAA2 2168C234 C4C6628B 80DC1CD1" ^
              "29024E08 8A67CC74 020BBEA6 3B139B22 514A0879 8E3404DD" ^
              "EF9519B3 CD3A431B 302B0A6D F25F1437 4FE1356D 6D51C245" ^
              "E485B576 625E7EC6 F44C42E9 A637ED6B 0BFF5CB6 F406B7ED" ^
              "EE386BFB 5A899FA5 AE9F2411 7C4B1FE6 49286651 ECE65381" ^
              "FFFFFFFF FFFFFFFF") in
            let g = binary_of_hex "02" in
            (MP.of_string p, MP.of_string g)
       | DiffieHellmanGroup14SHA1 ->
            (* RFC3526 - Oakley group 14, 2048-bit MODP group *)
            let p = binary_of_hex (
                "FFFFFFFF FFFFFFFF C90FDAA2 2168C234 C4C6628B 80DC1CD1" ^
                "29024E08 8A67CC74 020BBEA6 3B139B22 514A0879 8E3404DD" ^
                "EF9519B3 CD3A431B 302B0A6D F25F1437 4FE1356D 6D51C245" ^
                "E485B576 625E7EC6 F44C42E9 A637ED6B 0BFF5CB6 F406B7ED" ^
                "EE386BFB 5A899FA5 AE9F2411 7C4B1FE6 49286651 ECE45B3D" ^
                "C2007CB8 A163BF05 98DA4836 1C55D39A 69163FA8 FD24CF5F" ^
                "83655D23 DCA3AD96 1C62F356 208552BB 9ED52907 7096966D" ^
                "670C354E 4ABC9804 F1746C08 CA18217C 32905E46 2E36CE3B" ^
                "E39E772C 180E8603 9B2783A2 EC07A28F B5C55DF0 6F4C52C9" ^
                "DE2BCBF6 95581718 3995497C EA956AE5 15D22618 98FA0510" ^
                "15728E5A 8AACAA68 FFFFFFFF FFFFFFFF") in
            let g = binary_of_hex "02" in
            (MP.of_string p, MP.of_string g)
       | DiffieHellmanGexSHA1 ->
            (* XXX not yet done, but will depend on the group negotiation *)
            raise Not_implemented

    (* Kex algorithm choice, from draft-ietf-secsh-transport-24 Section 7.1 *)
    let algorithm_choice ~kex ~enc_cs ~enc_sc ~mac_cs ~mac_sc exnfn =
        let match_supported ty (sl,cl) =
            let sl = Str.split (Str.regexp_string ",") sl in
            let cl = Str.split (Str.regexp_string ",") cl in
            match List.filter (fun k -> List.mem k sl) cl with
            |hd::tl -> hd
            |[] -> raise (exnfn ty)
        in
        (* Select a key exchange method *)
        let s_kex = from_string
            (match_supported "kex" kex) in
        (* Select a client->server cipher *)
        let s_enc_cs = Ssh_algorithms.Cipher.from_string
            (match_supported "encryption_client_server" enc_cs) in
        (* And a server->client cipher *)
        let s_enc_sc = Ssh_algorithms.Cipher.from_string
            (match_supported "encryption_server_client" enc_sc) in
        (* Select client->server MAC *)
        let s_mac_cs = Ssh_algorithms.MAC.from_string
            (match_supported "mac_client_server" mac_cs) in
        (* And a server->client MAC *)
        let s_mac_sc = Ssh_algorithms.MAC.from_string
            (match_supported "mac_server_client" mac_sc) in
        (* Return em all *)
        (s_kex, s_enc_cs, s_enc_sc, s_mac_cs, s_mac_sc)
    
    let cryptokit_params p g =
        let p = MP.to_string p in
        let g = MP.to_string g in
        {Cryptokit.DH.p=p; g=g; privlen=256}
        
    (* Initial value to transmit to initiate DH exchange *)
    let compute_init rng p g =
        let params = cryptokit_params p g in
        let priv = Cryptokit.DH.private_secret ~rng:rng params in
        let e = MP.of_string (Cryptokit.DH.message params priv) in
        (e, priv)

    (* Return the shared secret string given public params and the
       private secret, and the other sides response mpint *)
    let compute_shared_secret p g priv resp =
        let resp = MP.to_string resp in
        let params = cryptokit_params p g in
        let s = Cryptokit.DH.shared_secret params priv resp in
        MP.of_string s
        
    (* Calculate reply to a diffie-hellman key exchange init *)        
    let compute_reply rng p g e =
        let params = cryptokit_params p g in
        (* Generate a response number for us *)
        let server_private = Cryptokit.DH.private_secret ~rng:rng params in
        let f = MP.of_string (Cryptokit.DH.message params server_private) in
        (* Calculate secret from e and f *)
        let secret = compute_shared_secret p g server_private e in
        (f, secret)

    (* RFC3447 EMSA-PKCS1-v1_5, need to pad signature to size n of hostkey.
       Also hashes the input message using SHA1 *)
    let pad_rsa_signature hk m =
        let emlen = MP.bytes hk in
        let h = Cryptokit.hash_string (Cryptokit.Hash.sha1()) m in
        let der_sha1 = binary_of_hex "3021300906052b0e03021a05000414" in
        let t = der_sha1 ^ h in
        let tlen = String.length t in
        let padlen = emlen - tlen - 3 in
        if padlen <= 0 then raise Key_too_short;
        let ps = String.make padlen '\255' in
        Printf.sprintf "%c%c%s%c%s" '\000' '\001' ps '\000' t

    (* draft-ietf-secsh-transport-18.txt, section 7.2
     * K:shared secret; H:kexinit hash; X:charid; session_id:first kexinit hash
     *)
    let derive_key hashfn k h session_id size x =
        (* Be careful here ... the hashfn is only good for one use once evaluated, hence
             the need to pass a unit arg through *)
        let htrans () = Cryptokit.hash_string (hashfn ()) in
        let kh = Ssh_pool.get_string_fn (fun env ->
            ignore(MP.marshal env k); ignore(MR.marshal env h)) in
        let sidm = Ssh_pool.get_string_fn (fun env ->
            MR.marshal env kh; MB.marshal env (MB.of_char x); MR.marshal env session_id) in
        let k1 = htrans () sidm in
        (* We need this many iterations to have enough for the requested size *)
        let iterations = size / (String.length k1) in
        let kn = Array.create (iterations+1) "" in
        kn.(0) <- k1;
        if iterations > 0 then begin
            for i = 1 to iterations do
                let ksofar = Array.sub kn 0 i in
                let kcat = String.concat "" (Array.to_list ksofar) in
                kn.(i) <- htrans () (kh ^ kcat);
            done;
        end;
        let total = String.concat "" (Array.to_list kn) in
        String.sub total 0 size
    
    (* Given a public key, a hash and a signed version of it, make sure they
       the signed version is valid *)  
    let verify_rsa_signature (pubkey:Ssh_message.Key.RSA.o) hash signed =
        let n = pubkey#n in
        let key_bits = MP.bits n in
        let key = { Cryptokit.RSA.size=key_bits;
            e=MP.to_string pubkey#e; n=MP.to_string n;
            d=""; p=""; q=""; dp=""; dq=""; qinv=""} in
        let padded_hash = pad_rsa_signature n hash in
        let unsigned = Cryptokit.RSA.unwrap_signature key signed in
        unsigned = padded_hash
end
