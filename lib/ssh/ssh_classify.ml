(*
 * Copyright (c) 2006 Anil Madhavapeddy <anil@recoil.org>
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
 * $Id: ssh_classify.ml,v 1.5 2006/02/11 17:37:45 avsm Exp $
 *)

open Ssh_message

exception Unknown_packet of int32 (* sequence number of unknown packet *)

type t =
    | Transport of Transport.o
    | Auth of Auth.o
    | DHGroupSHA1 of Dhgroupsha1.o
    | DHGexSHA1 of Dhgexsha1.o
    | Channel of Channel.o
    | Unknown of int32 (* sequence number of unknown packet *)

type c =
    | TransportGeneric
    | AlgorithmNegotiation
    | KeyExchangeSpecific
    | UserAuthGeneric
    | UserAuthSpecific
    | ChannelGeneric
    | ChannelSpecific
    | Reserved
    | LocalExtensions

let global_recv (x:Channel.o) = match x with
    |`GlobalRequest x -> Some (Channel.GlobalRequest.recv_statecall x)
    |`RequestSuccess x -> Some Channel.RequestSuccess.recv_statecall
    |`RequestSuccessPort x -> Some Channel.RequestSuccessPort.recv_statecall
    |`RequestFailure x -> Some Channel.RequestFailure.recv_statecall
    |`Open x -> Some (Channel.Open.recv_statecall x)
    |_ -> None

let recv_statecall x = match x with
    | Transport x -> Some (Transport.recv_statecall x :> Ssh_statecalls.t)
    | Auth x -> Some (Auth.recv_statecall x :> Ssh_statecalls.t)
    | DHGroupSHA1 x -> Some (Dhgroupsha1.recv_statecall x :> Ssh_statecalls.t)
    | DHGexSHA1 x -> Some (Dhgexsha1.recv_statecall x :> Ssh_statecalls.t)
    | Channel x -> global_recv x
    | Unknown _ -> None

let prettyprint x = match x with
    | Transport x -> Transport.prettyprint x
    | Auth x ->  Auth.prettyprint x
    | DHGroupSHA1 x -> Dhgroupsha1.prettyprint x
    | DHGexSHA1 x -> Dhgexsha1.prettyprint x
    | Channel x -> Channel.prettyprint x
    | Unknown x -> Printf.printf "Unknown packet (#%lu)\n" x

let peek env =
    let module M = Mpl_stdlib in
    match M.Mpl_byte.to_int (M.Mpl_byte.at env 0) with
    | x when x >= 0 && x <= 19 -> TransportGeneric
    | x when x >= 20 && x <= 29 -> AlgorithmNegotiation
    | x when x >= 30 && x <= 49 -> KeyExchangeSpecific
    | x when x >= 50 && x <= 59 -> UserAuthGeneric
    | x when x >= 60 && x <= 79 -> UserAuthSpecific
    | x when x >= 80 && x <= 89 -> ChannelGeneric
    | x when x >= 90 && x <= 127 -> ChannelSpecific
    | x when x >= 128 && x <= 191 -> Reserved
    | x when x >= 192 && x <= 255 -> LocalExtensions
    |_ -> failwith "cannot happen"

