(*pp cpp *)
(*
 * Copyright (c) 2005,2006 Anil Madhavapeddy <anil@recoil.org>
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
 * $Id: ssh_env.ml,v 1.25 2006/03/17 02:55:43 avsm Exp $
 *)

open Printf
open Ssh_utils
open Ssh_env_t

exception Disconnect_connection of (Ssh_message.Transport.Disconnect.reason_code_t * string)
exception Unexpected_packet of string
exception Internal_error of string

let version = {
    Ssh_version.raw = None;
    softwareversion = "MLSSH_0.2";
    protoversion = "2.0"
}

(* Everything need to send/receive packets through the transport *)
type conn = {
    encrypt_block_size: int;
    decrypt_block_size: int;
    cryptfn : string -> int -> string -> int -> int -> unit;
    decryptfn : string -> int -> string -> int -> int -> unit;
    decrypt_macfn : string -> int -> int -> string;
    crypt_macfn : string -> int -> int -> string;
}

(* Inputs for kex negotiation, used by both client and server *)
type negotiation_state = {
    client_kexinit : string;
    server_kexinit : string;
    cipher_cs : Ssh_algorithms.Cipher.t;
    cipher_sc : Ssh_algorithms.Cipher.t;
    mac_cs : Ssh_algorithms.MAC.t;
    mac_sc : Ssh_algorithms.MAC.t;
}

type xmit = Ssh_transport.Packet.xmit
type xmit_t = Ssh_transport.Packet.xmit_t

class virtual env (conf:Ssh_env_t.t) =
    let _ = conf.fd#write_buf (sprintf "%s\n" (Ssh_version.to_string version)) in
    let other_version = Ssh_version.unmarshal conf.fd in
    object(self)

    (* Give access to RNG and logging to inherited objects *)
    val log = conf.log
    method log = log
    method rng = conf.rng
    
    (* Keep track of our current connection status *)
    val mutable connection_active = true
    method connection_active = connection_active

    (* Multiplexed channels *)
    val chans = new Ssh_channel.channel_env
    method chans = chans

    (* Return an odescr to drive this object with network traffic *)
    method get_ofd =
        new Ounix.stream_odescr ~rfn:(fun od -> self#read_ssh_packet)
            ~efn:(fun od -> connection_active <- false)
            conf.fd#fd

    (* State of the transport layer *)
    val mutable transport =
        let nullfn src srcoff dst off len = String.blit src srcoff dst off len in 
        let nullmac _ _ _ = "" in {
            decrypt_block_size = 0;
            encrypt_block_size = 0;
            cryptfn = nullfn;
            decryptfn = nullfn;
            decrypt_macfn = nullmac;
            crypt_macfn = nullmac;
        }

    (* Only one session id per session, cannot be changed once set *)
    val mutable session_id = None
    method set_session_id (x:string) =
        match session_id with
        |None -> session_id <- Some x
        |Some x -> raise (self#disconnect `Protocol_error)

    (* Sequence numbers for the SSH session.  Updated by xmit/recv_packet *)
    val rx_seq_num = ref (-1l)
    val tx_seq_num = ref (-1l)

    (* Virtual methods about versions provided by the concrete implementation *)
    method virtual server_version : Ssh_version.t
    method virtual client_version : Ssh_version.t
    method private other_version = other_version
    
    (* Accepts a decrypted environment and provides an OCaml structure for it *)
    method virtual decode_packet : int32 -> Mpl_stdlib.env -> Ssh_classify.t

    (* Constructs a closure for the KexInit packet depending on configuration *)
    method private kexinit =
        let bsl = String.concat "," in
        let kexlist = bsl (List.map Ssh_kex.Methods.to_string conf.kex_methods) in
        let maclist = bsl (List.map Ssh_algorithms.MAC.to_string conf.mac_methods) in
        let cipherlist = bsl (List.map Ssh_algorithms.Cipher.to_string conf.cipher_methods) in
        let hkeylist = bsl (List.map Ssh_keys.PublicKey.to_string conf.hostkey_algorithms) in
        let complist = bsl ["none"] in
        let langlist = bsl [] in
        let cookie = `Str (Cryptokit.Random.string conf.rng 16) in
        Ssh_message.Transport.KexInit.t ~cookie:cookie
            ~kex_algorithms:kexlist
            ~server_host_key_algorithms:hkeylist
            ~encryption_algorithms_client_to_server:cipherlist
            ~encryption_algorithms_server_to_client:cipherlist
            ~mac_algorithms_client_to_server:maclist
            ~mac_algorithms_server_to_client:maclist
            ~compression_algorithms_client_to_server:complist
            ~compression_algorithms_server_to_client:complist
            ~languages_client_to_server:langlist
            ~languages_server_to_client:langlist
            ~kex_packet_follows:false

    method virtual tick_automaton : Ssh_statecalls.t -> unit
    
    (* Receive and classify an SSH packet *)
    method private recv =
        let module TP = Ssh_transport.Packet in
        rx_seq_num := Int32.succ !rx_seq_num;
        let penv = TP.unmarshal
            ~block_size:transport.decrypt_block_size
            ~decryptfn:transport.decryptfn
            ~macfn:transport.decrypt_macfn
            conf.fd in
        let decoded = self#decode_packet !rx_seq_num penv in
        let _ = match Ssh_classify.recv_statecall decoded with
        |None -> ()
            (* route incoming statecall to correct channel *)
        |Some sc -> self#tick_automaton sc in 
        decoded
        
    (* Transmit an SSH packet *)
    method private xmit (pack:Mpl_stdlib.env -> xmit) =
        let module TP = Ssh_transport.Packet in
        tx_seq_num := Int32.succ !tx_seq_num;
        Ssh_transport.Packet.marshal ~block_size:transport.encrypt_block_size
            ~padfn:(Cryptokit.Random.string conf.rng)
            ~cryptfn:transport.cryptfn
            ~macfn:transport.crypt_macfn
            ~splfn:(fun o -> self#tick_automaton o#xmit_statecall)
            conf pack
    
    method private xmit_channel (chan:Ssh_channel.channel) (pack:Mpl_stdlib.env -> xmit) =
        tx_seq_num := Int32.succ !tx_seq_num;
        Ssh_transport.Packet.marshal ~block_size:transport.encrypt_block_size
            ~padfn:(Cryptokit.Random.string conf.rng)
            ~cryptfn:transport.cryptfn
            ~macfn:transport.crypt_macfn
            ~splfn:(fun o -> chan#tick_automaton o#xmit_statecall)
            conf pack

    method private read_ssh_packet =
        let module C = Ssh_classify in
        try
            match self#recv with
            |C.DHGexSHA1 x ->
                Ssh_message.Dhgexsha1.prettyprint x;
                self#dispatch_dhgexsha1_packet x
            |C.DHGroupSHA1 x ->
                Ssh_message.Dhgroupsha1.prettyprint x;
                self#dispatch_dhg1sha1_packet x
            |C.Transport x ->
                Ssh_message.Transport.prettyprint x;
                self#dispatch_transport_packet x
            |C.Auth x ->
                Ssh_message.Auth.prettyprint x;
                self#dispatch_auth_packet x
            |C.Channel x ->
                Ssh_message.Channel.prettyprint x;
                self#dispatch_channel_packet x
            |C.Unknown x ->
				log#debug "Unknown packet, sending unimplemented";
				self#dispatch_unimplemented_packet x
        with
            |Disconnect_connection (code,reason) ->
                self#xmit (Ssh_message.Transport.Disconnect.t
                    ~reason_code:code ~description:reason
                    ~language:"en_US" :> xmit_t);
                connection_active <- false
            |Unix.Unix_error _ ->
                connection_active <- false

    (* Virtual methods to handle the different packet types *)
    method virtual dispatch_dhgexsha1_packet : Ssh_message.Dhgexsha1.o -> unit
    method virtual dispatch_dhg1sha1_packet : Ssh_message.Dhgroupsha1.o -> unit
    method virtual dispatch_transport_packet : Ssh_message.Transport.o -> unit
    method virtual dispatch_auth_packet : Ssh_message.Auth.o -> unit
    method virtual dispatch_channel_packet : Ssh_message.Channel.o -> unit

    method dispatch_unimplemented_packet seq =
        self#xmit (Ssh_message.Transport.Unimplemented.t ~seq_num:seq :> xmit_t)

    (* Convenience functions for terminating a connection *)
    method disconnect ?(reason="") code =
        Disconnect_connection (code, reason)
        
    method protocol_error reason =
        self#disconnect ~reason `Protocol_error

    (* Derive session keys *)
    method private derive_session_keys neg_state sess_hash shared_secret is_server =
        (* First session hash is the session id for rest of session lifetime *)
        let session_id = match session_id with
            |None -> session_id <- Some sess_hash; sess_hash
            |Some x -> x in
        let derivefn bs x = Ssh_kex.Methods.derive_key (Cryptokit.Hash.sha1)
            shared_secret sess_hash session_id bs x in
        let cipher_cs = Ssh_algorithms.Cipher.info neg_state.cipher_cs in
        let cipher_sc = Ssh_algorithms.Cipher.info neg_state.cipher_sc in
        let mac_cs = Ssh_algorithms.MAC.info neg_state.mac_cs in
        let mac_sc = Ssh_algorithms.MAC.info neg_state.mac_sc in
    
        let key_size_cs = cipher_cs.Ssh_algorithms.Cipher.key_len in
        let key_size_sc = cipher_sc.Ssh_algorithms.Cipher.key_len in
        let block_size_cs = cipher_cs.Ssh_algorithms.Cipher.block_size in
        let block_size_sc = cipher_sc.Ssh_algorithms.Cipher.block_size in
        let mac_len_cs = mac_cs.Ssh_algorithms.MAC.key_len in
        let mac_len_sc = mac_sc.Ssh_algorithms.MAC.key_len in
        
        let client_to_server_iv = derivefn block_size_cs 'A' in
        let server_to_client_iv = derivefn block_size_sc 'B' in
        let client_to_server_key = derivefn key_size_cs 'C' in
        let server_to_client_key = derivefn key_size_sc 'D' in
        let integrity_client_to_server = derivefn mac_len_cs 'E' in
        let integrity_server_to_client = derivefn mac_len_sc 'F' in
        let hob = Ssh_utils.hex_of_binary in
        log#debug ("c->s iv   (A): " ^ (hob client_to_server_iv));
        log#debug ("s->c iv   (B): " ^ (hob server_to_client_iv));
        log#debug ("c->s key  (C): " ^ (hob client_to_server_key));
        log#debug ("s->c key  (D): " ^ (hob server_to_client_key));
        log#debug ("c->s hash (E): " ^ (hob integrity_client_to_server));
        log#debug ("s->c hash (F): " ^ (hob integrity_server_to_client));
        (* Layering violation here, but just putting this logic here saves
         * passing a load of variables between the derived client/server classes *)
        let module AC = Ssh_algorithms.Cipher in
        let module AM = Ssh_algorithms.MAC in
        if is_server then {
            decrypt_block_size = block_size_cs;
            encrypt_block_size = block_size_sc;
            decryptfn = AC.data (AC.fn client_to_server_iv client_to_server_key
                cipher_cs.AC.name AC.Decrypt);
            cryptfn = AC.data (AC.fn server_to_client_iv server_to_client_key
                cipher_sc.AC.name AC.Encrypt);
            decrypt_macfn = AM.generate mac_cs rx_seq_num integrity_client_to_server;
            crypt_macfn = AM.generate mac_sc tx_seq_num integrity_server_to_client;
           }
        else {
            decrypt_block_size = block_size_sc;
            encrypt_block_size = block_size_cs;
            decryptfn = AC.data (AC.fn server_to_client_iv server_to_client_key
            cipher_sc.AC.name AC.Decrypt);
            cryptfn = AC.data (AC.fn client_to_server_iv client_to_server_key
            cipher_cs.AC.name AC.Encrypt);
            decrypt_macfn = AM.generate mac_sc rx_seq_num integrity_server_to_client;
            crypt_macfn = AM.generate mac_cs tx_seq_num integrity_client_to_server;
           }
        
    method private set_transport conn =
        transport <- conn        

    method virtual kexinit_finished : bool
    (* Channel handling *)
    method virtual close_channel : Ssh_channel.channel -> unit

    method ofd_of_stdout (osel:Ounix.oselect) chan fd =
        let od = new Ounix.stream_odescr
            ~rfn:(fun ofd ->
                if self#kexinit_finished then
                    self#send_packet_from_fd (fun chan -> chan#set_stdout None)
                        (fun x -> self#xmit_channel chan (Ssh_message.Channel.Data.t
                        ~recipient_channel:chan#other_id ~data:x :> xmit_t))
                    chan ofd)
            ~efn:(fun ofd -> self#close_channel chan) fd in
        osel#add_ofd (od :> Ounix.odescr);
        od
        
    method ofd_of_stderr (osel:Ounix.oselect) chan fd =
        let od = new Ounix.stream_odescr
            ~rfn:(fun ofd ->
                if self#kexinit_finished then
                    self#send_packet_from_fd (fun chan -> chan#set_stdout None)
                        (fun x -> self#xmit_channel chan (Ssh_message.Channel.ExtendedData.t
                        ~recipient_channel:chan#other_id ~data_type:`Stderr ~data:x :> xmit_t))
                    chan ofd)
            ~efn:(fun ofd -> self#close_channel chan) fd in
        osel#add_ofd (od :> Ounix.odescr);
        od
        
    method send_packet_from_fd setfn tfn (chan:Ssh_channel.channel) (ofd:Ounix.stream_odescr) =
        let module M = Mpl_stdlib in
        let chan_err () =
            setfn chan;
            umay (fun _ -> self#close_channel chan) chan#pid
        in
        try
            let env = Ssh_pool.get () in
            M.fill env ofd#fd;
            let len = M.size env in
            if len > 0 then begin
                let _ = tfn (`Frag (M.Mpl_raw.total_frag env)) in ()
            end else chan_err ()
        with |Mpl_stdlib.IO_error |Unix.Unix_error _ -> chan_err ()

    method reset = Ssh_pool.reset ()
end
