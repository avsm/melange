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
 * $Id: spl_cfg.mli,v 1.6 2006/02/09 17:44:52 avsm Exp $
 *)

type transition_method =
  | Condition of Spl_syntaxtree.expr
  | Message of Spl_syntaxtree.id
  | Assignment of (Spl_syntaxtree.id * Spl_syntaxtree.expr)
  | Terminate

type transition_class =
  | T_handle
  | T_normal

type state = {
  label : string;
  mutable edges : transition list;
}
and transition = {
  t : transition_method;
  target : state ref;
  cl : transition_class;
  loc : Spl_location.t option;
}

type env = {
  func_name : string;
  initial_state : state option ref;
  final_state : state option ref;
  blocks : (string, state) Hashtbl.t;
  registers : (Spl_syntaxtree.var_type, unit) Hashtbl.t;
  functions_called : (string, unit) Hashtbl.t;
}

type compiled_functions = (string, env * Spl_syntaxtree.func) Hashtbl.t

type global_env = {
  filename : string;
  functions : compiled_functions;
  counter : int ref;
  mutable webpage: string;
}

exception Block_not_unique of string
exception Unknown_variable of string
exception Unknown_function of string
exception Type_checking_invariant_failure
val string_of_transition_class : transition_class -> string
val initial_state_of_env : env -> state
val final_state_of_env : env -> state
val list_of_registers : env -> Spl_syntaxtree.var_type list
val blocks_of_function : env -> compiled_functions -> state list
val generate_states : string -> Spl_syntaxtree.func list -> global_env
