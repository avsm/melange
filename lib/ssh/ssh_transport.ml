(*
 * Copyright (c) 2004 David Scott <dave@recoil.org>
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
 * $Id: ssh_transport.ml,v 1.15 2006/03/17 02:55:43 avsm Exp $
 *)

open Printf
open Ssh_utils

let max_payload = 32768
let max_packet_size = 35000

module Version = struct

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
end           

(* Interface to request network buffers safely without excessive allocation *)
module Pool = struct
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

end

module Packet = struct

    exception Invalid of string

    (* Compute the amount of padding required in a packet from the length   *)
    (* of the payload data and the cipher block size.                       *)
    let amount_padding_required payload_length cipher_blk_size = 
        let non_padding_length = 4 (* uint32 *) + 1 (* byte *) + payload_length in
        let m = max cipher_blk_size 8 in
        (* Padding must be at least 4 and <= 255 and a multiple of m *)
        if (non_padding_length mod m) == 0 then m
        else if m - (non_padding_length mod m) < 4
        then m - (non_padding_length mod m) + m
        else m - (non_padding_length mod m)

    let unmarshal ~block_size ~decryptfn ~macfn (fd:Ounix.tcp_odescr) =
        let module M = Mpl_stdlib in
        let fillfn buf off amt =
           (* amount we read is complicated ... *)
           let amt =
            (* If at start of packet, then read the minimum chunk to determine length *)
            if off = 0 then max block_size 8 else begin
                (* Elsewhere, we must consider the block size *)
                if block_size > 0 then 
                    (* If encrypted, read only in chunks of block_size by rounding up *)
                    Ssh_utils.round_up amt block_size
                else
                    (* Otherwise, we can just read amount *)
                    amt
            end in
            let obuf = fd#read_buf amt in
            let obuflen = String.length obuf in
            let () = decryptfn obuf 0 buf off obuflen in
            obuflen
        in
        let env = Pool.get () in
        M.set_fillfn env fillfn;
        let p = Ssh_message.Ssh.unmarshal env in
        (* calculate the MAC we are expecting *)
        let expecting = M.env_fn p#env macfn in 
        let mac_len = String.length expecting in
        let actual = fd#read_buf mac_len in
        if expecting <> actual then
            raise (Invalid (sprintf "MAC validation failed: %S <> %S" expecting actual));
        (* check padding length *)
        if p#padding_length < (amount_padding_required p#data_length block_size) then
            raise (Invalid "too little padding");
        (* XXX decompress not implemented yet *)
        (* return environment based at payload *)
        Mpl_stdlib.default_fillfn env;
        p#data_env

    (* Class type for transmitted packets *)
    class type xmit = object
        method env : Mpl_stdlib.env
        method prettyprint : unit
        method xmit_statecall : Ssh_statecalls.t
    end
    type xmit_t = Mpl_stdlib.env -> xmit

    let marshal ~block_size ~padfn ~cryptfn ~macfn ~splfn (fd:Ounix.tcp_odescr) (data:xmit_t) =
        let module M = Mpl_stdlib in
        let txenv = Pool.get () in
        (* XXX compression not supported yet *)
        let d env = splfn (data env) in
        (* calculate padding *)
        let padding env =
            let align = max block_size 8 in
            let abspos = M.curbase env + (M.curpos env) in
            let len = Ssh_utils.round_up (abspos + 4) align - abspos in
            let pad = padfn len in
            M.Mpl_raw.marshal env pad
        in
        (* marshal the payload and padding into place *)
        let p = Ssh_message.Ssh.t ~data:(`Sub d) ~padding:(`Sub padding) txenv in
        (* compute mac *)
        let mac = M.env_fn p#env macfn in
        (* encrypt packet *)
        M.env_fn p#env (fun buf off len ->
            let encbuf = String.create len in
            cryptfn buf off encbuf 0 len;
            fd#write_buf encbuf;
            fd#write_buf mac;
        )
end
