(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Stdlib

module Debug : sig
  type t

  val create : toplevel:bool -> bool -> t

  val find_loc : t -> ?after:bool -> int -> Parse_info.t option

  val is_empty : t -> bool

  val paths : t -> units:StringSet.t -> StringSet.t

  val hash_data_for_change_detection : data -> int
  (** Computes a hash of debug data for the sake of detecting changes
     to inputs that may affect compilation outputs. *)
end

type one =
  { code : Code.program
  ; cmis : StringSet.t
  ; debug : Debug.t
  }

val from_exe :
     ?includes:string list
  -> ?toplevel:bool
  -> ?exported_unit:string list
  -> ?dynlink:bool
  -> ?debug:[ `Full | `Names | `No ]
  -> in_channel
  -> one

val from_cmo :
     ?includes:string list
  -> ?toplevel:bool
  -> ?debug:[ `Full | `Names | `No ]
  -> Cmo_format.compilation_unit
  -> in_channel
  -> one

val from_cma :
     ?includes:string list
  -> ?toplevel:bool
  -> ?debug:[ `Full | `Names | `No ]
  -> Cmo_format.library
  -> in_channel
  -> one

val from_channel :
     in_channel
  -> [ `Cmo of Cmo_format.compilation_unit | `Cma of Cmo_format.library | `Exe ]

val from_string : string array -> string -> Code.program * Debug.t

val predefined_exceptions : unit -> Code.program

val normalize_module_name : string -> string

val denormalize_module_name : string -> string
