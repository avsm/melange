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
 * $Id: testserver.ml,v 1.21 2006/03/17 02:55:43 avsm Exp $
 *)

open Unix
open Ssh_utils
open Ssh_tty
open Printf

class mlsshd_config conf =
    let log = conf.Ssh_env.log in
    let rng = conf.Ssh_env.rng in
    let moduli_file = "./moduli" in
    let hostkey_file = "./server.rsa.key" in    
    let hostkey_size = 1024 in
    let rsa_hostkey =
            let fin = try open_in_bin hostkey_file
            with Sys_error x -> begin
                let keysize = hostkey_size in
                log#info (sprintf "Generating hostkey (%d bits)" keysize);
                let key = Cryptokit.RSA.new_key ~rng:rng keysize in
                let fout = open_out_bin hostkey_file in
                Marshal.to_channel fout key [];
                close_out fout;
                open_in_bin hostkey_file;
            end in
            let (key:Cryptokit.RSA.key) = Marshal.from_channel fin in
            close_in fin;
            key
    in
    object(self:#Ssh_config.server_config)

    val log = log
    
    (* Get server's private RSA key *)
    method get_rsa_key = rsa_hostkey

    (* Initialize a moduli file in OpenSSH format (normally /etc/moduli) *)
    method moduli_init =
        let primes = Hashtbl.create 1 in
        try
            Ssh_openssh_formats.moduli moduli_file primes;
            log#debug "Successfully initialised moduli";
            primes;
        with Ssh_openssh_formats.Parse_failure ->
            (* Clear primes to be safe *)
            log#debug "moduli parsing error";
            Hashtbl.clear primes;
            let g1p,g1g = Ssh_kex.Methods.public_parameters Ssh_kex.Methods.DiffieHellmanGroup1SHA1 in
            let g14p,g14g = Ssh_kex.Methods.public_parameters Ssh_kex.Methods.DiffieHellmanGroup14SHA1 in
            hashtbl_add_to_list primes 1024l (g1p,g1g);
            hashtbl_add_to_list primes 2048l (g14p,g14g);
            primes

    (* Check to see if a banner should be displayed to the user at
       the beginning of authentication *)
    method auth_banner (user_name:string) =
        Some "Nex rules!!\n"

    (* Methods which must all succeed for successful authentication *)
    method auth_methods_supported : Ssh_userauth.t list =
        [Ssh_userauth.Public_key; Ssh_userauth.Password]
    
    (* Callback for password auth, also requests a public key auth *)
    method auth_password username password : bool * Ssh_userauth.t list =
        let _ = [Ssh_userauth.Public_key] in
        let x = [] in
        let _ = (username = "avsm" && password = "wibble"), x in
        true, x
    
    (* Callback for public key auth, always succeeds for now
       but also requests password auth *)
    method auth_public_key (user_name:string) (pubkey:Ssh_message.Key.o) =
        let x = if true then [] else [Ssh_userauth.Password] in
        true, x

    (* We dont care what the client says, just reply with our own
       window size for the moment *)
    method connection_request ws ps =
        Ssh_config.Server.Con_allow (131072l, 32768l)
    
    (* Not really used at the moment *)
    val conns = Hashtbl.create 1
    
    method connection_add (id:Ssh_channel.channel) =
        log#debug "new conn";
        Hashtbl.add conns id ();
        ()
        
    method connection_del (id:Ssh_channel.channel) =
        log#debug "conn del";
        Hashtbl.remove conns id;
        ()
    
    method connection_add_pty id modes (row,col,xpixel,ypixel) =
        let pty = Ssh_pty.open_pty () in
        let tio = tcgetattr pty.Ssh_pty.masterfd in
        Tty.parse_modes tio modes;
        tcsetattr pty.Ssh_pty.masterfd TCSANOW tio;
        let pwin = {Ssh_pty.row=row; col=col; xpixel=xpixel; ypixel=ypixel} in
        Some (pty, pwin)

    method connection_request_exec chan cmd =
        let pin_r, pin_w = pipe () in
        let pout_r, pout_w = pipe () in
        let perr_r, perr_w = pipe () in 
        let pid = fork () in
        if pid = 0 then begin
            let args = Array.of_list (Str.split (Str.regexp_string " ") cmd) in
            let dup2_and_close f1 f2 =
                dup2 f1 f2;
                close f1 in
            close pin_w;
            dup2_and_close pin_r stdin;
            close pout_r;
            dup2_and_close pout_w stdout;
            close perr_r;
            dup2_and_close perr_w stderr;
            (* stderr is now redirected, dont send debug messages any more! *)
            try execvp args.(0) args
            with Unix_error (x,s,y) -> begin
                print_endline (sprintf "%s: %s" y (error_message x));
                Pervasives.exit 1
            end
        end;
        List.iter close [pin_r; pout_w; perr_w];
        let stdin = Some pin_w in
        let stdout = Some pout_r in
        let stderr = Some perr_r in
        Some (pid, stdin, stdout, stderr)
    
    method connection_request_shell id = function
      |Some (pty, pty_window) ->
        let pid = fork () in
        if pid = 0 then begin
             close pty.Ssh_pty.masterfd;
             Ssh_pty.switch_controlling_pty pty;
             Ssh_pty.window_size pty pty_window;
             dup2 pty.Ssh_pty.slavefd stdin;
             dup2 pty.Ssh_pty.slavefd stdout;
             dup2 pty.Ssh_pty.slavefd stderr;
             close pty.Ssh_pty.slavefd;
             let args = Array.make 1 "/bin/sh" in
             try execv "/bin/sh" args
             with Unix_error (x,s,y) -> begin
                print_endline (sprintf "%s: %s" y (error_message x));
                Pervasives.exit 1
            end
        end;
        (* XXX close pty.Ssh_pty.slavefd in parent? probably - avsm *)
        (* Need to dup so we have two distinct fds to retrieve the appropriate
           session.  I think this isn't strictly needed any more, but just to be
           safe until all the 'channel sanity checks' for unique keys go in *)
        let stdin = Some (Unix.dup pty.Ssh_pty.masterfd) in
        let stdout = Some (pty.Ssh_pty.masterfd) in
        let (stderr:Unix.file_descr option) = None in
        Some (pid, stdin, stdout, stderr)
      |None ->
        self#connection_request_exec id "/bin/sh"
end

let start_server port log caller fd =
    let kex = [
        Ssh_kex.Methods.DiffieHellmanGexSHA1;
        Ssh_kex.Methods.DiffieHellmanGroup14SHA1;
        Ssh_kex.Methods.DiffieHellmanGroup1SHA1;
    ] in
    let macs = [
        Ssh_algorithms.MAC.SHA1;
        Ssh_algorithms.MAC.SHA1_96;
        Ssh_algorithms.MAC.MD5;
        Ssh_algorithms.MAC.MD5_96;
    ] in
    let ciphers = [
        Ssh_algorithms.Cipher.AES_256_CBC;
        Ssh_algorithms.Cipher.AES_192_CBC;
        Ssh_algorithms.Cipher.AES_128_CBC;
        Ssh_algorithms.Cipher.Arcfour;
        Ssh_algorithms.Cipher.TripleDES_CBC;
    ] in
    let host_key_algorithms = [
        Ssh_keys.PublicKey.RSAKey
    ] in
    let osel = new Ounix.oselect in
    let osig = new Ounix.osignal in
    let conf = {
        Ssh_env.fd = new Ounix.tcp_odescr fd;
        log = log;
        rng = Cryptokit.Random.device_rng "/dev/urandom";
        kex_methods = kex;
        mac_methods = macs;
        cipher_methods = ciphers;
        hostkey_algorithms = host_key_algorithms;
        osel = osel;
    } in
    let server_conf = new mlsshd_config conf in
    let env = new Ssh_server.env conf (server_conf:>Ssh_config.server_config) in
    osel#add_ofd (env#get_ofd :> Ounix.odescr);

(*
    let debugger_on = ref false in
    let s = socket PF_INET SOCK_STREAM 0 in
    setsockopt s SO_REUSEADDR true;
    bind s (ADDR_INET(inet_addr_any,1234));
    listen s 5;
    let sock_factory = fun () ->
            log#debug (sprintf "sock_factory accept() --- %b" !debugger_on);
            let fd,_ = accept s in
            let ic = in_channel_of_descr fd in
            let oc = out_channel_of_descr fd in
            oc,ic
    in

    log#debug (sprintf "www_server_fn --- %b" !debugger_on);
    let oc,_ = sock_factory () in
    Ssh_server_automaton.pagefn oc;
    Ssh_server_automaton.set_cfn env#automaton sock_factory;
*)
    osig#add_sigchld_handler (fun () ->
        let retry = ref true in
        try while !retry do
            let pid, status = waitpid [WNOHANG] (-1) in
            retry := false;
            if pid <> 0 then begin
                umay (fun (chan:Ssh_channel.channel) ->
                    chan#clear_pid;
                    let _ = match status with
                    |WEXITED s -> chan#set_exit_status s
                    |_ -> () in
                    env#close_channel chan) (env#chans#find_by_pid pid);
            end
        done with Unix_error (err, _, _) as p -> begin
            match err with
            |EINTR -> ()
            |ECHILD -> ()
            |_ -> raise p
        end
    );
    try
    while env#connection_active do
        osel#read;
        osig#process;
        env#reset;
    done;
    with
    |Ssh_server_automaton.Bad_statecall -> log#debug "bad statecall!"
    |Ssh_server_channel.Bad_statecall -> log#debug "bad statecall channel!";
    log#debug "new_ssh_connection ended";
    exit 1
    
let _ =
    let port = 2222 in
    let sock = ADDR_INET (inet_addr_any, port) in
    let log = new Olog.stderr_log in
    log#init;
    log#set_critical;
    let sfun = Server.handle_single log (start_server port) in
    try
    Server.establish_server log sfun sock
    with
    |Cryptokit.Error x ->
        log#critical (string_of_cryptokit_error x);
        Pervasives.exit 1 
    |Unix.Unix_error (x,s,y) ->
        log#critical (sprintf "%s: %s: %s" s y (Unix.error_message x));
        Pervasives.exit 1
