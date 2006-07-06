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
 * $Id: ssh_server.ml,v 1.25 2006/03/17 02:55:43 avsm Exp $
 *)

open Printf
open Ssh_utils

type xmit_t = Ssh_env.xmit_t

type gex_state = {
    p : Mpl_stdlib.Mpl_mpint.t;
    g : Mpl_stdlib.Mpl_mpint.t;
    min : int32;
    n : int32;
    max : int32;
}

type transport_state =
    |Initial
    |KexInit of string (* The server KexInit payload *)
    |Negotiation_DHG1SHA1 of Ssh_env.negotiation_state
    |Negotiation_DHG14SHA1 of Ssh_env.negotiation_state
    |Negotiation_DHGexSHA1_Request of Ssh_env.negotiation_state
    |Negotiation_DHGexSHA1_Init of (Ssh_env.negotiation_state * gex_state)
    |NewKeys of Ssh_env.conn
    |Encrypted

class env conf server_conf = object(self)
    inherit Ssh_env.env conf as base
    
    (* Concrete versions to indicate we are the server *)
    method client_version = self#other_version
    method server_version = Ssh_env.version

    (* State of the transport layer, updated by dispatch functions *)
    val mutable transport_state = Initial

    (* List of successful authentications for this session *)
    val mutable auth_successes = ([]:Ssh_userauth.t list)
    (* A username we have authenticated with *)
    val mutable auth_username = (None:string option)

    (* SPL automaton *)
    val mutable automaton = Ssh_server_automaton.init ()
    method automaton = automaton
    
    method tick_automaton (x : Ssh_statecalls.t) =
        automaton <- Ssh_server_automaton.tick automaton x

    (* Transmit a kexinit, but also return the transmitted packet.
       This is done by wrapping the packet evaluator in a closure which
       records it into a reference when the packet is transmitted.
       Ugly, but effective since this is the only instance in the 
       protocol where the raw packet needs to retrieved in this way *)
    method xmit_kexinit =
        let r = ref None in
        let kx env =
            let x = self#kexinit env in
            let f = Mpl_stdlib.string_of_env env in
            r := Some f; x in
        self#xmit (kx :> xmit_t);
        match !r with |None -> raise Not_found |Some x -> x

    (* Indicate that a key exchange is not currently in progress *)
    method kexinit_finished =
        match transport_state with
        |Encrypted -> true
        |_ -> false

    initializer
        let kx = self#xmit_kexinit in
        transport_state <- KexInit kx;
        if conf.Ssh_env_t.debugger then begin
            let sock_factory = Spl_stdlib.get_tcp_sock_factory 1234 in
            let oc,_ = sock_factory () in
            Ssh_server_automaton.pagefn oc;
            Ssh_server_automaton.set_cfn automaton sock_factory;
        end
        
    (* Decode and classify an SSH packet *)
    method decode_packet env =
        let module M = Ssh_message in
        let module MC = Ssh_classify in
        match MC.peek env with
        | MC.TransportGeneric
        | MC.AlgorithmNegotiation ->
            MC.Transport (M.Transport.unmarshal env)
        | MC.KeyExchangeSpecific -> begin
            match transport_state with
            |Negotiation_DHG1SHA1 _
            |Negotiation_DHG14SHA1 _ -> MC.DHGroupSHA1 (M.Dhgroupsha1.unmarshal env)
            |Negotiation_DHGexSHA1_Request _
            |Negotiation_DHGexSHA1_Init _ -> MC.DHGexSHA1 (M.Dhgexsha1.unmarshal env)
            |_ -> raise (Ssh_env.Unexpected_packet "kex decode_packet")
        end
        | MC.UserAuthGeneric
        | MC.UserAuthSpecific ->
            (* XXX passwd_ns must be set correctly *)
            MC.Auth (M.Auth.unmarshal ~passwd_ns:false env) 
        | MC.ChannelGeneric
        | MC.ChannelSpecific ->
            (* XXX expecting_port must be set correctly *)
            MC.Channel (M.Channel.unmarshal ~expecting_port:false env)
        | MC.Reserved
        | MC.LocalExtensions -> raise MC.Unknown_packet

    method set_new_keys neg_state sess_hash shared_secret =
        let conn = self#derive_session_keys neg_state sess_hash shared_secret true in
        transport_state <- NewKeys conn

    method dispatch_transport_packet (p:Ssh_message.Transport.o) =
        match p with
        |`Ignore _ |`Debug _ -> ()
        |`KexInit client_ki -> begin
            let server_ki_frag = match transport_state with
            |KexInit x -> x
            |Encrypted -> self#xmit_kexinit (* rekey request *)
            |_ -> raise (Ssh_env.Internal_error "kexinit in unexpected state") in
            let bsl = String.concat "," in
            let kexlist = bsl (List.map Ssh_kex.Methods.to_string conf.Ssh_env_t.kex_methods) in
            let maclist = bsl (List.map Ssh_algorithms.MAC.to_string conf.Ssh_env_t.mac_methods) in
            let cipherlist = bsl (List.map Ssh_algorithms.Cipher.to_string conf.Ssh_env_t.cipher_methods) in
            let s_kex, s_enc_cs, s_enc_sc, s_mac_cs, s_mac_sc =
                Ssh_kex.Methods.algorithm_choice
                    ~kex: (kexlist, client_ki#kex_algorithms)
                    ~enc_cs: (cipherlist, client_ki#encryption_algorithms_client_to_server)
                    ~enc_sc: (cipherlist, client_ki#encryption_algorithms_server_to_client)
                    ~mac_cs: (maclist, client_ki#mac_algorithms_client_to_server)
                    ~mac_sc: (maclist, client_ki#mac_algorithms_server_to_client)
                (fun x -> raise (self#protocol_error (sprintf "No common algorithms for %s" x))) in
            let client_ki = Mpl_stdlib.string_of_env client_ki#env in
            let next_args = {
                Ssh_env.client_kexinit=client_ki;
                server_kexinit=server_ki_frag;
                cipher_cs=s_enc_cs; cipher_sc=s_enc_sc;
                mac_cs=s_mac_cs; mac_sc=s_mac_sc;
            } in
            match s_kex with
            |Ssh_kex.Methods.DiffieHellmanGroup1SHA1 ->
                self#tick_automaton `Expect_DHInit;
                transport_state <- Negotiation_DHG1SHA1 next_args
            |Ssh_kex.Methods.DiffieHellmanGroup14SHA1 ->
                self#tick_automaton `Expect_DHInit;
                transport_state <- Negotiation_DHG14SHA1 next_args
            |Ssh_kex.Methods.DiffieHellmanGexSHA1 ->
                self#tick_automaton `Expect_GexInit;
                transport_state <- Negotiation_DHGexSHA1_Request next_args
        end
        |`NewKeys _ ->
            (* received a newkeys, so transmit our own and switch to the new crypto *)
            let conn = match transport_state with
            |NewKeys x -> x |_ -> raise (self#protocol_error "newkeys") in
            self#xmit (Ssh_message.Transport.NewKeys.t :> xmit_t);
            self#set_transport conn;
            transport_state <- Encrypted
        |`ServiceReq (`UserAuth _) ->
            self#xmit (Ssh_message.Transport.ServiceAccept.UserAuth.t :> xmit_t);
            ()
        |`Disconnect _ |`ServiceAccept _
        |`ServiceReq _ |`Unimplemented _ ->
            raise (self#protocol_error "transport")
    
    method dispatch_dhg1sha1_packet (p:Ssh_message.Dhgroupsha1.o) =
        match p with
        |`Init x ->
            let proto, neg_state = match transport_state with
            |Negotiation_DHG1SHA1 a -> Ssh_kex.Methods.DiffieHellmanGroup1SHA1, a
            |Negotiation_DHG14SHA1 a -> Ssh_kex.Methods.DiffieHellmanGroup14SHA1, a
            |_ -> raise (self#protocol_error "dhg1sha1") in
            let p,g = Ssh_kex.Methods.public_parameters proto in
            let rsakey = server_conf#get_rsa_key in
            let mpint x = Mpl_stdlib.Mpl_mpint.of_string x in
            let xmitkey = `RSA (Ssh_message.Key.RSA.t ~e:(mpint rsakey.Cryptokit.RSA.e)
                ~n:(mpint rsakey.Cryptokit.RSA.n)) in
            let server_f, shared_secret = Ssh_kex.Methods.compute_reply self#rng p g x#e in
            let k_s = Ssh_pool.get_string_fn (Ssh_message.Key.m xmitkey) in
            let kex_hash_args = {
                Ssh_kex.Methods.DHGroup.v_c=self#client_version;
                v_s=self#server_version;
                i_c=neg_state.Ssh_env.client_kexinit;
                i_s=neg_state.Ssh_env.server_kexinit;
                k_s=k_s;
                e=x#e; f=server_f; k=shared_secret;
            } in
            let kex_hash_concat = Ssh_kex.Methods.DHGroup.marshal kex_hash_args in
            let hash = Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) kex_hash_concat in
            let signed_padded_hash = Cryptokit.RSA.sign rsakey
                (Ssh_kex.Methods.pad_rsa_signature (mpint rsakey.Cryptokit.RSA.n) hash) in
            let sig_h = Ssh_pool.get_string_fn (Ssh_message.Sig.RSA.t ~sig_blob:signed_padded_hash) in
            let kex_reply = Ssh_message.Dhgroupsha1.Reply.t ~k_s:k_s ~f:server_f ~sig_h:sig_h in
            self#xmit (kex_reply :> xmit_t);
            self#set_new_keys neg_state hash shared_secret
        |`Reply _ -> raise (self#protocol_error "dhgexsha1 reply")
        
    method dispatch_dhgexsha1_packet (p:Ssh_message.Dhgexsha1.o) =
        match p with
        |`Request x -> begin
            let args = match transport_state with
            |Negotiation_DHGexSHA1_Request a -> a
            |_ -> raise (self#protocol_error "dhgexsha1") in
            let minv = x#minv in
            let maxv = x#maxv in
            let n = x#n in
            let moduli = server_conf#moduli_init in
            match Ssh_kex.Methods.DHGex.choose ~min:minv ~want:n ~max:maxv moduli with
            |None -> raise (self#disconnect `Kex_failed)
            |Some (p,g) ->
                let reply = Ssh_message.Dhgexsha1.Group.t ~p:p ~g:g in
                let gex_state = {p=p; g=g; min=minv; max=maxv; n=n} in
                self#xmit (reply :> xmit_t);
                transport_state <- Negotiation_DHGexSHA1_Init (args, gex_state)
            end
        |`Init x ->
            let neg_state, gex_state = match transport_state with
            |Negotiation_DHGexSHA1_Init (x,y) -> x,y
            |_ -> raise (self#protocol_error "dhgexsha1") in
            let p,g = gex_state.p, gex_state.g in
            let rsakey = server_conf#get_rsa_key in
            let mpint x = Mpl_stdlib.Mpl_mpint.of_string x in
            let xmitkey = `RSA (Ssh_message.Key.RSA.t ~e:(mpint rsakey.Cryptokit.RSA.e)
                ~n:(mpint rsakey.Cryptokit.RSA.n)) in
            let server_f, shared_secret = Ssh_kex.Methods.compute_reply self#rng p g x#e in
            let k_s = Ssh_pool.get_string_fn (Ssh_message.Key.m xmitkey) in
            let kex_hash_args = {
                Ssh_kex.Methods.DHGex.v_c=self#client_version;
                v_s=self#server_version; p=p; g=g;
                i_c=neg_state.Ssh_env.client_kexinit;
                i_s=neg_state.Ssh_env.server_kexinit;
                k_s=k_s; e=x#e; f=server_f; k=shared_secret;
                min=gex_state.min; max=gex_state.max; n=gex_state.n;
            } in
            let kex_hash_concat = Ssh_kex.Methods.DHGex.marshal kex_hash_args in
            let hash = Cryptokit.hash_string (Cryptokit.Hash.sha1 ()) kex_hash_concat in
            let signed_padded_hash = Cryptokit.RSA.sign rsakey
                (Ssh_kex.Methods.pad_rsa_signature (mpint rsakey.Cryptokit.RSA.n) hash) in
            let sig_h = Ssh_pool.get_string_fn (Ssh_message.Sig.RSA.t ~sig_blob:signed_padded_hash) in
            let kex_reply = Ssh_message.Dhgexsha1.Reply.t ~k_s:k_s ~f:server_f ~sig_h:sig_h in
            self#xmit (kex_reply :> xmit_t);
            self#set_new_keys neg_state hash shared_secret
        |`Reply _ |`Group _ -> ()

    method dispatch_auth_packet (p:Ssh_message.Auth.o) =
      let module MA = Ssh_message.Auth in match p with
      |`Req (`None x) ->
        (* Display auth banner if required *)
        umay (fun msg -> self#xmit (MA.Banner.t ~banner:msg ~language:"en_US" :> xmit_t))
            (server_conf#auth_banner x#user_name);
        self#auth_response false
      |`Req (`Password (`Request x)) ->
        self#check_auth_method x#service x#user_name Ssh_userauth.Password;
        let attempt_auth, further_req =
            server_conf#auth_password x#user_name x#password in
        if attempt_auth then begin
            auth_successes <- Ssh_userauth.Password :: auth_successes;
            self#auth_response ~lib_req_auths:further_req true
        end else begin
            self#auth_response false
        end
      |`Req (`PublicKey (`Check x)) -> begin
        let alg = x#algorithm in
        match Ssh_keys.PublicKey.of_string alg with
        |Some Ssh_keys.PublicKey.RSAKey ->
            self#xmit (Ssh_message.Auth.PublicKey_OK.t
                ~algorithm:alg ~blob:x#blob :> xmit_t)
        |_ -> self#auth_response false
      end
      |`Req (`PublicKey (`Request x)) -> begin
        self#auth_response false
      end
      |`Req (`Password (`Change _))
      |`PublicKey_OK _ 
      |`Banner _
      |`Failure _
      |`ChangeReq _
      |`Success _ ->
        raise (self#protocol_error "auth")

    (* Test to see if we have completed all the authentications we need.
       req is a list of further authentication requested by the library, which
       may or may not have already succeeded previously.
       s is a bool which tells us if the most recent attempt succeeded or failed
       (if it succeeded but we need more auths, then we actually send a Failure
        with partial_success set to true) *)
    method private auth_response ?(lib_req_auths=[]) s =
        (* Get all supported authentication methods *)
        let all_auths = server_conf#auth_methods_supported in
        (* Partition the methods into ones which have succeeded, and ones which havent *)
        let succ_auths, notsucc_auths =
            List.partition (fun x -> List.mem x auth_successes) all_auths in
        (* Grant auth_succeeded if our most recent auth succeeded and the list of required
           auths from the library is satisfied by succ_auths.  Otherwise, return back the list
           of remaining auths from the global list so we dont leak which auths would be enough
           for this particular user. *)
        let auth_succeeded = s && (List.for_all 
            (fun x -> List.mem x succ_auths) lib_req_auths) in
        if auth_succeeded then begin
            self#xmit (Ssh_message.Auth.Success.t :> xmit_t)
        end else begin
            if List.length notsucc_auths = 0 then
                raise (self#disconnect `No_more_auth_methods_available);
            self#xmit (Ssh_message.Auth.Failure.t
                ~auth_continue:(Ssh_userauth.to_string notsucc_auths)
                ~partial_success:s :> xmit_t)
        end

    (* Make sure that the attempted authentication method is one we actually support,
       and has not already been authenticated previously.  Also, lock the username to
       the session and error out if it has changed since the first time it was mentioned.
       Maybe servers can theoretically support multiple usernames per session, but not us *)
    method private check_auth_method service user_name auth_method =
        let _ = match auth_username with
        |None -> auth_username <- Some user_name
        |Some env_user_name -> if user_name <> env_user_name then
            raise (self#disconnect ~reason:"username changed" `Illegal_user_name)
        in
        (* XXX is this a pre-auth information leak? do we care? *)
        if service <> "ssh-connection" then
            raise (self#disconnect `Service_not_available);
        let supported = List.mem auth_method (server_conf#auth_methods_supported) in
        let not_yet_done = not (List.mem auth_method auth_successes) in
        (* If either of these fail, bomb out, its a serious error *)
        if not (supported && not_yet_done) then raise (self#protocol_error "")

    (* Channel handling code *)
    (* --------------------- *)
    
    (* Attempt to close the channel.  If it still has a PID or stdout/stderr open, then
       do not actually send a close yet; this function is meant to be called multiple
       times from the various callbacks *)
    method close_channel (chan:Ssh_channel.channel) =
        (* First send a kill to encourage a sigchld to show up later *)
        umay (fun x -> try Unix.kill x Sys.sigterm with _ -> ()) chan#pid;
        match chan#pid, chan#stdout, chan#stderr, chan#close with
        |(None,None,None,false) -> 
            chan#set_close true;
            chan#close_pty;
            umay (fun s -> self#xmit (Ssh_message.Channel.Request.ExitStatus.t
                ~recipient_channel:chan#other_id
                ~exit_status:(Int32.of_int s) :> xmit_t)) chan#exit_status;
            self#xmit (Ssh_message.Channel.Close.t ~recipient_channel:chan#other_id :> xmit_t);
            server_conf#connection_del chan;
            chans#del_chan chan#our_id
        |_ -> ()

    method dispatch_channel_packet (p:Ssh_message.Channel.o) =
      let module MC = Ssh_message.Channel in
      let channel_check x fn =
        match chans#find_by_id x#recipient_channel with
        |None -> () (* err so what happens ehere i wonder *)
        |Some chan ->
            (* try to tick the channel *)
            let sc = (MC.recv_statecall p :> Ssh_statecalls.t) in
            chan#tick_automaton sc;
            fn (self#xmit_channel chan) chan;
        in
      let channel_check_reply x fn =
        match chans#find_by_id x#recipient_channel with
        |None -> () (* err so what happens ehere i wonder *)
        |Some chan ->
            (* try to tick the channel *)
            let sc = (MC.recv_statecall p :> Ssh_statecalls.t) in
            chan#tick_automaton sc;
            let res = fn (self#xmit_channel chan) chan in
            if x#want_reply then begin
                if res then
                    self#xmit_channel chan (MC.Success.t ~recipient_channel:chan#other_id :> xmit_t)
                else
                    self#xmit_channel chan (MC.Failure.t ~recipient_channel:chan#other_id :> xmit_t)
            end
        in      
      match p with
      |`Open (`Session x) -> begin
        match server_conf#connection_request x#window_size x#max_packet_size with
        |Ssh_config.Server.Con_deny r ->
            self#xmit (MC.OpenFailure.t ~recipient_channel:x#sender_channel
                ~reason_code:r ~language:"en_US" ~information:"" :> xmit_t);
        |Ssh_config.Server.Con_allow (our_window,our_packet_size) -> begin
            let chano = chans#new_chan ~initial_window:our_window ~packet_size:our_packet_size conf in
            match chano with
            |None ->
                self#xmit (MC.OpenFailure.t ~recipient_channel:x#sender_channel
                    ~reason_code:`Resource_shortage
                    ~information:"" ~language:"en_US" :> xmit_t);
            |Some chan ->
                chan#set_other_initial_window x#window_size;
                chan#set_other_packet_size x#max_packet_size;
                chan#set_other_id x#sender_channel;
                server_conf#connection_add chan;
                let resp = MC.OpenConfirmation.t ~sender_channel:chan#our_id
                    ~recipient_channel:chan#other_id
                    ~initial_window_size:chan#our_initial_window
                    ~maximum_packet_size:chan#our_packet_size
                in
                self#xmit (resp :> xmit_t)
        end
      end
      |`Request (`Pty x) ->
        channel_check_reply x (fun xmitfn chan ->
            let dims = (x#width_chars, x#height_rows, x#width_pixels, x#height_pixels) in  
            match server_conf#connection_add_pty chan x#term dims with
            |Some (pty, pwin) ->
                chan#tick_automaton `Expect_Pty_Success;
                chan#set_pty (pty,pwin);
                conf.Ssh_env_t.fd#set_nodelay true;
                true
            |None -> false
        )
      |`Request (`Exec x) ->
        channel_check_reply x (fun xmitfn chan ->
            match server_conf#connection_request_exec chan x#command with
            |Some (pid, stdin, stdout, stderr) ->
                chan#tick_automaton `Expect_Exec_Success;
                let ostdin = may (fun stdin -> new Ounix.stream_odescr stdin) stdin in
                let ostdout = may (self#ofd_of_stdout conf.Ssh_env_t.osel chan) stdout in
                let ostderr = may (self#ofd_of_stderr conf.Ssh_env_t.osel chan) stderr in
                chan#set_stdin ostdin;
                chan#set_stdout ostdout;
                chan#set_stderr ostderr;
                chan#set_pid pid;
                true
            |None -> false
        )
      |`Request (`Shell x) ->
        channel_check_reply x (fun xmitfn chan ->
            match server_conf#connection_request_shell chan chan#pty with
            |Some (pid, stdin, stdout, stderr) ->
                chan#tick_automaton `Expect_Shell_Success;
                let ostdin = may (fun stdin -> new Ounix.stream_odescr stdin) stdin in
                let ostdout = may (fun stdout -> self#ofd_of_stdout conf.Ssh_env_t.osel chan stdout) stdout in
                let ostderr = may (self#ofd_of_stderr conf.Ssh_env_t.osel chan) stderr in
                chan#set_stdin ostdin;
                chan#set_stdout ostdout;
                chan#set_stderr ostderr;
                chan#set_pid pid;
                true
            |None -> false
        )
      |`Data x ->
        channel_check x (fun xmitfn chan ->
            umay (fun fd ->
                let data = x#data in
                let datalen = Int32.of_int (String.length data) in
                umay (fun a -> xmitfn (MC.WindowAdjust.t ~recipient_channel:chan#other_id
                    ~bytes_to_add:a :> xmit_t)) (chan#consume_our_window datalen);
                fd#write_buf data;
            ) chan#stdin
        )
      |`EOF x ->
        (* The client no longer intends to send us data, so close the channel stdin *)
        channel_check x (fun xmitfn chan ->
            match chan#stdin with
            |Some fd ->
                fd#close;
                chan#set_stdin None
            |None ->
                raise (self#protocol_error "eof when no descr")
                (* should this really kill everything? *)
        )
      |`WindowAdjust x ->
        (* The other window size needs to be incremented *)
        channel_check x (fun xmitfn chan ->
            let new_size = Int32.add chan#other_window x#bytes_to_add in
            chan#set_other_window new_size
        )
      |`Close x ->
        channel_check x (fun xmitfn chan ->
            self#close_channel chan;
        )
      |`Request _ ->
        (* `Env, `ExitSignal, `ExitStatus, `LocalFlowControl, `Signal, `Subsystem,
            `WindowChange, `X11 *)
        raise (self#protocol_error "not yet");
      |`Open _ -> (* DirectTCP, `TCPForward, `X11 *)
        raise (self#protocol_error "not yet");
      |`ExtendedData _
      |`Failure _
      |`GlobalRequest _
      |`OpenConfirmation _
      |`OpenFailure _
      |`RequestFailure _
      |`RequestSuccess _
      |`RequestSuccessPort _
      |`Success _ -> raise (self#protocol_error "bad channel packet")

end