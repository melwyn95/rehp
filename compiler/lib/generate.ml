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

(*XXX
  Patterns:
  => loops should avoid absorbing the whole continuation...
     (detect when the continuation does not loop anymore and close
      the loop at this point)
  => should have special code for switches that include the preceding
     if statement when possible
  => if e1 then {if e2 then P else Q} else {if e3 then P else Q}
  => if e then return e1; return e2
  => if e then var x = e1; else var x = e2;
  => while (true) {.... if (e) continue; break; }

  - CLEAN UP!!!
*)

open! Stdlib

let debug = Debug.find "gen"

let times = Debug.find "times"

open Code
open Stdlib
module J = Rehp


(* For cases where location is very unreliable but at least we can get the
 * source file *)
let parse_source_file_from_debug_info debug ?after pc =
  match Parse_bytecode.Debug.find_loc debug ?after pc with
  | Some pi -> Some {pi with line = 0; col = 0}
  | None -> None

let ask_backend ?loc path =
  match Backend.Current.module_require () with
  | None ->
      raise (Errors.UserError(Errors.requireUnsupportedBackend path, loc))
  | Some loader -> (
    match loader path with
      | None ->
          raise(Errors.UserError(Errors.backendUnsupportedRequire path, loc))
      | Some expr -> expr)

(****)

let string_of_set s =
  String.concat ~sep:", " (List.map ~f:Addr.to_string (Addr.Set.elements s))

let rec list_group_rec f g l b m n =
  match l with
  | [] -> List.rev ((b, List.rev m) :: n)
  | a :: r ->
      let fa = f a in
      if Poly.(fa = b)
      then list_group_rec f g r b (g a :: m) n
      else list_group_rec f g r fa [ g a ] ((b, List.rev m) :: n)

let list_group f g l =
  match l with
  | [] -> []
  | a :: r -> list_group_rec f g r (f a) [ g a ] []

(* like [List.map] except that it calls the function with
   an additional argument to indicate whether we're mapping
   over the last element of the list *)
let rec map_last f l =
  match l with
  | [] -> assert false
  | [ x ] -> [ f true x ]
  | x :: xs -> f false x :: map_last f xs

(****)

module Share = struct
  type 'a aux =
    { strings : 'a StringMap.t
    ; applies : 'a IntMap.t
    ; prims : 'a StringMap.t
    ; language_requires : 'a StringMap.t
    }

  let empty_aux =
    { language_requires = StringMap.empty;
      prims = StringMap.empty;
      strings = StringMap.empty;
      applies = IntMap.empty }

  type t =
    { mutable count : int aux
    ; mutable vars : Id.t aux
    ; alias_prims : bool
    ; alias_strings : bool
    ; alias_apply : bool
    }

  let add_string s t =
    let n = try StringMap.find s t.strings with Not_found -> 0 in
    { t with strings = StringMap.add s (n + 1) t.strings }

  let add_prim s t =
    let n = try StringMap.find s t.prims with Not_found -> 0 in
    { t with prims = StringMap.add s (n + 1) t.prims }

  let add_require s t =
    let n = try StringMap.find s t.language_requires with Not_found -> 0 in
    { t with language_requires = StringMap.add s (n + 1) t.language_requires }


  (* Adds special primitives to the prims counter with count -1, which allows them
     to be hoisted/shared, even if they were never referenced in bytecode - but only
     if they "exist" (meaning only if the linker linked them in, or if they were
     passed to one of the register_prim functions in generate.ml *)
  let add_special_prim_if_exists s t =
    if Primitive.exists s then { t with prims = StringMap.add s (-1) t.prims } else t

  let add_special_prim_even_if_not_exists s t =
    { t with prims = StringMap.add s (-1) t.prims }

  let add_apply i t =
    let n = try IntMap.find i t.applies with Not_found -> 0 in
    { t with applies = IntMap.add i (n + 1) t.applies }

  let add_code_string s share =
    let share = add_string s share in
    if Config.Flag.use_js_string ()
    then share
    else add_prim "caml_string_of_jsbytes" share

  let add_code_istring s share = add_string s share

  let rec get_constant c t =
    match c with
    | String s -> add_code_string s t
    | IString s -> add_code_istring s t
    | Tuple (_, args, _) -> Array.fold_left args ~init:t ~f:(fun t c -> get_constant c t)
    | _ -> t

  let add_args args t =
    List.fold_left args ~init:t ~f:(fun t a ->
        match a with
        | Pc c -> get_constant c t
        | _ -> t)

  let get
      ~debug
      ?(alias_strings = false)
      ?(alias_prims = false)
      ?(alias_apply = true)
      { blocks; _ } : t =
    let count =
      Addr.Map.fold
        (fun addr block share ->
          List.fold_left block.body ~init:share ~f:(fun share i ->
              match i with
              | Let (_, Constant c) -> get_constant c share
              | Let (_, Apply (_, args, false)) -> add_apply (List.length args) share
              | Let (_, Prim (Extern "%closure", [ Pc (IString name | String name) ])) ->
                  let name = Primitive.resolve name in
                  let share =
                    if Primitive.exists name then add_prim name share else share
                  in
                  share
              (* caml_requires come from raw macros *)
              | Let (_, Prim (Extern "%caml_require", [Pc (IString path | String path)])) ->
                  let loc = parse_source_file_from_debug_info debug ~after:false addr in
                  (* Check once if the macro is even valid while we have some
                   * debug info *)
                  let _ = ask_backend ?loc path in
                  add_require path share
              | Let (_, Prim (Extern name, args)) ->
                  let name = Primitive.resolve name in
                  let share =
                    if Primitive.exists name then add_prim name share else share
                  in
                  add_args args share
              | Let (_, Prim (_, args)) -> add_args args share
              | _ -> share))
        blocks
        empty_aux
    in
    (* TODO: Probably need to make it so that we add special prims even if they
       don't "exist". Each backend can determine which primitives it needs *)
    let count =
      List.fold_left
        [ "caml_trampoline"
        ; "caml_trampoline_return"
        ; "caml_list_of_js_array"
          (* These need to be added to the "special prims" in order to be
             shared/hoisted(because they don't appear in bytecode I think?).
             TODO: Renamed these. *)
        ; "caml_arity_test"
          (* Allow the runtime to optionally predefine several caml_calls. *)
        ; "caml_call1"
        ; "caml_call2"
        ; "caml_call3"
        ; "caml_call4"
        ; "caml_call5"
        ; "caml_call6"
        ; "caml_call7"
        ; "caml_call8"
        ; "caml_call9"
        ; "caml_call10"
        ; "caml_call11"
        ; "caml_call12"
        ; "caml_call13"
        ; "is_int"
        ; "left_shift_32"
        ; "unsigned_right_shift_32"
        ; "right_shift_32"
        ; "caml_wrap_exception"
        ; "caml_exn_with_js_backtrace"
        ]
        ~init:count
        ~f:(fun acc x -> add_special_prim_if_exists x acc)
    in
    (*


        jsoo vs. rehp's exception wrapping differences:
        -----------------------------------------------
        Rehp ensures that that the improved_stacktrace flag will always provide
        an opportunity for thrown exceptions to be wrapped regardless of if
        traces are desired. Some language backends can only throw special
        exception classes and those languages would require the improved_stacktrace
        flag to wrap thrown exceptions, and the excwrap flag to unwrap them correctly
        when caught.

                       Operation | jsoo                         rehp
        ----------------------------------------------------------------------------------------
        Unwrap exceptions when   | caml_wrap_exception          caml_wrap_exception
                they're caught   |
        When flag excwrap        |
        -------------------------+--------------------------------------------------------------
        Wrap exceptions when     | nothing                      nothing
        they're thrown with      |
        !Flag.improved_stacktrace|
        -------------------------+--------------------------------------------------------------
        Wrap exceptions when     | nothing                      caml_wrap_thrown_exception_traceless
        they're thrown with      |
        Flag.improved_stacktrace |
        'NoTrace'                |
        -------------------------+--------------------------------------------------------------
        Wrap exceptions when     | caml_exn_with_js_backtrace   caml_wrap_thrown_exception
        they're thrown with      |
        Flag.improved_stacktrace |
        'Normal'                 |
        -------------------------+--------------------------------------------------------------
        Wrap exceptions when     | caml_exn_with_js_backtrace   caml_wrap_thrown_exception_reraise
        they're thrown with      |
        Flag.improved_stacktrace |
        'Reraise'                |

        - NoTrace, Normal, and ReRaise are determined by which primitive is
         called to raise the exception (in the bytecode)

        Rehp uses the `improved_stacktrace` flag to guarantee an opportunity to
        *wrap* the exception if necessary, it's not really using that to
        "improve the stacktrace". Arguably we could rename that jsoo argument
        to be more accurate.

     *)
    let count =
      List.fold_left
        [ "caml_wrap_thrown_exception_traceless"
        ; "caml_wrap_thrown_exception_reraise"
        ; "caml_wrap_thrown_exception"
        ; "caml_arity_test"
        ; "caml_call1"
        ; "caml_call2"
        ; "caml_call3"
        ; "caml_call4"
        ; "caml_call5"
        ; "caml_call6"
        ; "caml_call7"
        ; "caml_call8"
        ; "caml_call9"
        ; "caml_call10"
        ; "caml_call11"
        ; "caml_call12"
        ; "caml_call13"
        ; "is_int"
        ; "left_shift_32"
        ; "unsigned_right_shift_32"
        ; "right_shift_32"
        ]
        ~init:count
        ~f:(fun acc x -> add_special_prim_even_if_not_exists x acc)
    in
    { count; vars = empty_aux; alias_strings; alias_prims; alias_apply }

  let get_string gen s t =
    if not t.alias_strings
    then gen s
    else
      try
        let c = StringMap.find s t.count.strings in
        if c > 1
        then (
          try J.EVar (StringMap.find s t.vars.strings)
          with Not_found ->
            let x = Var.fresh_n (Printf.sprintf "cst_%s" s) in
            let v = Id.V x in
            t.vars <- { t.vars with strings = StringMap.add s v t.vars.strings };
            J.EVar v)
        else gen s
      with Not_found -> gen s

  let get_prim ?pretty_name gen s t =
    let varname =
      match pretty_name with
      | None -> s
      | Some nm -> nm
    in
    let s = Primitive.resolve s in
    if not t.alias_prims
    then gen s
    else
      try
        let c = StringMap.find s t.count.prims in
        (* If the usage count of the prim is > 1 then try to hoist the variable
           definition to the top. Also, if it is a "special" prim (marked with count
           -1), then always hoist it to the top as well. *)
        if c > 1 || c = -1
        then (
          try J.EVar (StringMap.find s t.vars.prims)
          with Not_found ->
            let x = Var.fresh_n varname in
            let v = Id.V x in
            t.vars <- { t.vars with prims = StringMap.add s v t.vars.prims };
            J.EVar v)
        else gen s
      with Not_found -> gen s

  let get_require gen s t =
    let path = s
    in
      try
        let c = StringMap.find s t.count.language_requires in
        (* If the usage count of the require is > 1 then try to hoist the variable *)
        if c >= 1
        then (
          try J.EVar (StringMap.find s t.vars.language_requires)
          with Not_found ->
            let bname = Filename.basename path in
            let var_name =
              match String.find_substring ".js" bname 0 with
              | exception Not_found -> bname
              | i ->
                  (match i == ((String.length bname) - 3) with
                   | true -> Filename.chop_extension bname
                   | false -> bname) in
            let x = Var.fresh_n var_name in
            let v = Id.V x in
            t.vars <- { t.vars with language_requires = StringMap.add s v t.vars.language_requires };
            J.EVar v)
        else gen s
      with Not_found -> gen s

  let get_apply gen n t =
    if not t.alias_apply
    then gen n
    else
      try J.EVar (IntMap.find n t.vars.applies)
      with Not_found ->
        let x = Var.fresh_n (Printf.sprintf "call%d" n) in
        let v = Id.V x in
        t.vars <- { t.vars with applies = IntMap.add n v t.vars.applies };
        J.EVar v
end

let if_prim_supplied s ~if_supplied ~fallback =
  let s = Primitive.resolve s in
  match Backend.Current.is_prim_supplied () s with
  | None -> fallback ()
  | Some pretty_name -> if_supplied ~pretty_name

module Ctx = struct
  type t =
    { mutable blocks : block Addr.Map.t
    ; live : int array
    ; mutable module_export_metadatas : Module_export_metadata.t list
    ; share : Share.t
    ; debug : Parse_bytecode.Debug.t
    ; exported_runtime : Code.Var.t option
    }

  let initial ~exported_runtime blocks live share debug =
    { module_export_metadatas = []; blocks; live; share; debug; exported_runtime }
end

let var x = J.EVar (Id.V x)

let int n = J.ENum n

let int32 n = J.ENum (Int32.to_int n)

let to_int cx = J.EBin (J.Bor, cx, int 0)

let unsigned' x = J.EBin (J.Lsr, x, int 0)

let unsigned x =
  let pos_int32 =
    match x with
    | J.ENum num -> ( try Int32.(J.Num.to_int32 num >= 0l) with _ -> false)
    | _ -> false
  in
  if pos_int32 then x else unsigned' x

let one = int 1

let zero = int 0

let plus_int x y =
  match x, y with
  | J.ENum y, x when J.Num.is_zero y -> x
  | x, J.ENum y when J.Num.is_zero y -> x
  | J.ENum x, J.ENum y -> J.ENum (J.Num.add x y)
  | x, y -> J.EBin (J.Plus, x, y)

let bool e = J.ECond (e, one, zero)

(*let boolnot e = J.ECond (e, zero, one)*)
let val_float f = f

(*J.EArr [Some (J.EFloat 253.); Some f]*)
let float_val e = e

(****)

let source_location ctx ?after pc =
  match Parse_bytecode.Debug.find_loc ctx.Ctx.debug ?after pc with
  | Some pi -> Loc.Pi pi
  | None -> Loc.N

let maybe_parse_info loc = match loc with
  | Loc.Pi pi -> Some pi
  | _ -> None


(****)

let float_const f = J.ENum (J.Num.of_float f)

let s_var name = J.EVar (Id.ident name)

let runtime_fun ctx name =
  match ctx.Ctx.exported_runtime with
  | Some runtime -> J.EDot (J.EVar (J.V runtime), name)
  | None -> s_var name

let str_js s = J.EStr (s, `Bytes)

let unsigned' ~ctx x =
  if_prim_supplied
    "unsigned_right_shift_32"
    ~if_supplied:(fun ~pretty_name ->
      let p =
        Share.get_prim
          ~pretty_name
          (runtime_fun ctx)
          "unsigned_right_shift_32"
          ctx.Ctx.share
      in
      J.ECall (p, [ x; int 0 ], Loc.N))
    ~fallback:(fun () -> J.EBin (J.Lsr, x, int 0))

let unsigned ~ctx x =
  let pos_int32 =
    match x with
    | J.EInt num -> ( try Int32.of_int num >= 0l with _ -> false)
    | _ -> false
  in
  if pos_int32 then x else unsigned' ~ctx x

let ecall f args loc = J.ECall (f, List.map args ~f:(fun x -> x, `Not_spread), loc)

let arity_test ~ctx x =
  if_prim_supplied
    "caml_arity_test"
    ~if_supplied:(fun ~pretty_name ->
      let p =
        Share.get_prim ~pretty_name (runtime_fun ctx) "caml_arity_test" ctx.Ctx.share
      in
      J.ECall (p, [ x ], Loc.N))
    ~fallback:(fun () -> J.EArityTest x)

let is_int ~ctx x =
  if_prim_supplied
    "is_int"
    ~if_supplied:(fun ~pretty_name ->
      let p = Share.get_prim ~pretty_name (runtime_fun ctx) "is_int" ctx.Ctx.share in
      J.ECall (p, [ x ], Loc.N))
    ~fallback:(fun () -> J.EUn (IsInt, x))

(****)

(*
Some variables are constant:   x = 1
Some may change after effectful operations : x = y[z]

There can be at most one effectful operations in the queue at once

let (e, expr_queue) = ... in
flush_queue expr_queue e
*)

let const_p = 0

let mutable_p = 1

let mutator_p = 2

let flush_p = 3

let or_p p q = max p q

let is_mutable p = p >= mutable_p

(*let is_mutator p = p >= mutator_p*)
let kind k =
  match k with
  | `Pure -> const_p
  | `Mutable -> mutable_p
  | `Mutator -> mutator_p
let ocaml_string ~ctx ~loc s =
  if Config.Flag.use_js_string ()
  then s
  else
    let p = Share.get_prim (runtime_fun ctx) "caml_string_of_jsbytes" ctx.Ctx.share in
    ecall p [ s ] loc

let rec constant_rec ~ctx x level instrs =
  match x with
  | Null -> s_var "null", instrs
  | String s ->
      let e = Share.get_string str_js s ctx.Ctx.share in
      let e = ocaml_string ~ctx ~loc:Loc.N e in
      let pretty_name = "string" in
      let p =
        Share.get_prim ~pretty_name (runtime_fun ctx) "caml_new_string" ctx.Ctx.share
      in
      J.ECall (p, [ e ], Loc.N), instrs
  | IString s -> Share.get_string str_js s ctx.Ctx.share, instrs
  | Float f -> float_const f, instrs
  | Float_array a ->
      ( J.EStruct
          (int Obj.double_array_tag
          :: Array.to_list (Array.map a ~f:(fun f -> float_const f)))
      , instrs )
  | Int64 i ->
      ( J.EStruct
          [ int 255
          ; int (Int64.to_int i land 0xffffff)
          ; int (Int64.to_int (Int64.shift_right i 24) land 0xffffff)
          ; int (Int64.to_int (Int64.shift_right i 48) land 0xffff)
          ]
      , instrs )
  | Tuple (tag, a, _) -> (
      let constant_max_depth = Config.Param.constant_max_depth () in
      let rec detect_list n acc = function
        | Tuple (0, [| x; l |], _) -> detect_list (succ n) (x :: acc) l
        | Int 0l -> if n > constant_max_depth then Some acc else None
        | _ -> None
      in
      match detect_list 0 [] x with
      | Some elts_rev ->
          let arr, instrs =
            List.fold_left elts_rev ~init:([], instrs) ~f:(fun (arr, instrs) elt ->
                let js, instrs = constant_rec ~ctx elt level instrs in
                Some js :: arr, instrs)
          in
          let p =
            Share.get_prim (runtime_fun ctx) "caml_list_of_js_array" ctx.Ctx.share
          in
          J.ECall (p, [ J.EArr arr ], Loc.N), instrs
      | None ->
          let split = level = constant_max_depth in
          let level = if split then 0 else level + 1 in
          let l, instrs =
            List.fold_left (Array.to_list a) ~init:([], instrs) ~f:(fun (l, instrs) cc ->
                let js, instrs = constant_rec ~ctx cc level instrs in
                js :: l, instrs)
          in
          let l, instrs =
            if split
            then
              List.fold_left l ~init:([], instrs) ~f:(fun (acc, instrs) js ->
                  match js with
                  | J.EStruct _ ->
                      let v = Code.Var.fresh_n "partial" in
                      let instrs =
                        (J.Variable_statement [ Id.V v, Some (js, Loc.N) ], Loc.N)
                        :: instrs
                      in
                      J.EVar (Id.V v) :: acc, instrs
                  | _ -> js :: acc, instrs)
            else List.rev l, instrs
          in
          J.EStruct (int tag :: l), instrs)
  | Int i -> int32 i, instrs

let constant ~ctx x level =
  let expr, instr = constant_rec ~ctx x level [] in
  expr, List.rev instr

type queue_elt =
  { prop : int
  ; cardinal : int
  ; ce : J.expression
  ; loc : Loc.t
  ; deps : Code.Var.Set.t
  }

let access_queue queue x =
  try
    let elt = List.assoc x queue in
    if elt.cardinal = 1
    then (elt.prop, elt.ce), List.remove_assoc x queue
    else
      ( (elt.prop, elt.ce)
      , List.map queue ~f:(function
            | x', elt when Var.equal x x' -> x', { elt with cardinal = pred elt.cardinal }
            | x -> x) )
  with Not_found -> (const_p, var x), queue

let access_queue' ~ctx queue x =
  match x with
  | Pc c ->
      let js, instrs = constant ~ctx c (Config.Param.constant_max_depth ()) in
      assert (instrs = []);
      (* We only have simple constants here *)
      (const_p, js), queue
  | Pv x -> access_queue queue x

let access_queue_may_flush queue v x =
  let tx, queue = access_queue queue x in
  let _, instrs, queue =
    List.fold_left
      queue
      ~init:(Code.Var.Set.singleton v, [], [])
      ~f:(fun (deps, instrs, queue) ((y, elt) as eq) ->
        if Code.Var.Set.exists (fun p -> Code.Var.Set.mem p deps) elt.deps
        then
          ( Code.Var.Set.add y deps
          , (J.Variable_statement [ Id.V y, Some (elt.ce, elt.loc) ], elt.loc) :: instrs
          , queue )
        else deps, instrs, eq :: queue)
  in
  instrs, (tx, List.rev queue)

let should_flush cond prop = cond <> const_p && cond + prop >= flush_p

let flush_queue expr_queue prop (l : J.statement_list) =
  let instrs, expr_queue =
    if prop >= flush_p
    then expr_queue, []
    else List.partition ~f:(fun (_, elt) -> should_flush prop elt.prop) expr_queue
  in
  let instrs =
    List.map instrs ~f:(fun (x, elt) ->
        J.Variable_statement [ Id.V x, Some (elt.ce, elt.loc) ], elt.loc)
  in
  List.rev_append instrs l, expr_queue

let flush_all expr_queue l = fst (flush_queue expr_queue flush_p l)

let enqueue expr_queue prop x ce loc cardinal acc =
  let instrs, expr_queue =
    if Config.Flag.compact ()
    then if is_mutable prop then flush_queue expr_queue prop [] else [], expr_queue
    else flush_queue expr_queue flush_p []
  in
  let deps = Js_simpl.get_variable Code.Var.Set.empty ce in
  let deps =
    List.fold_left expr_queue ~init:deps ~f:(fun deps (x', elt) ->
        if Code.Var.Set.mem x' deps then Code.Var.Set.union elt.deps deps else deps)
  in
  instrs @ acc, (x, { prop; ce; loc; cardinal; deps }) :: expr_queue

(****)

type state =
  { all_succs : (int, Addr.Set.t) Hashtbl.t
  ; (* not used *)
    succs : (int, int list) Hashtbl.t
  ; backs : (int, Addr.Set.t) Hashtbl.t
  ; preds : (int, int) Hashtbl.t
  ; mutable loops : Addr.Set.t
  ; mutable loop_stack : (Addr.t * (J.Label.t * bool ref)) list
  ; mutable visited_blocks : Addr.Set.t
  ; mutable interm_idx : int
  ; ctx : Ctx.t
  ; mutable blocks : Code.block Addr.Map.t
  ; at_toplevel : bool
  }

let get_preds st pc = try Hashtbl.find st.preds pc with Not_found -> 0

let incr_preds st pc = Hashtbl.replace st.preds pc (get_preds st pc + 1)

let decr_preds st pc = Hashtbl.replace st.preds pc (get_preds st pc - 1)

let protect_preds st pc = Hashtbl.replace st.preds pc (get_preds st pc + 1000000)

let unprotect_preds st pc = Hashtbl.replace st.preds pc (get_preds st pc - 1000000)

let unprotect_preds st pc = Hashtbl.replace st.preds pc (get_preds st pc - 1000000)

module DTree = struct
  (* This as to be kept in sync with the way we build conditionals
     and switches! *)

  type cond =
    | IsTrue
    | CEq of int32
    | CLt of int32
    | CLe of int32

  type 'a t =
    | If of cond * 'a t * 'a t
    | Switch of (int list * 'a t) array
    | Branch of 'a
    | Empty

  let normalize a =
    a
    |> Array.to_list
    |> List.stable_sort ~cmp:(fun (cont1, _) (cont2, _) -> Poly.compare cont1 cont2)
    |> list_group fst snd
    |> List.map ~f:(fun (cont1, l1) -> cont1, List.flatten l1)
    |> List.stable_sort ~cmp:(fun (_, l1) (_, l2) ->
           compare (List.length l1) (List.length l2))
    |> Array.of_list

  let build_if b1 b2 = If (IsTrue, Branch b1, Branch b2)

  let build_switch (a : cont array) : 'a t =
    let m = Config.Param.switch_max_case () in
    let ai = Array.mapi a ~f:(fun i x -> x, i) in
    (* group the contiguous cases with the same continuation *)
    let ai : (Code.cont * int list) array =
      Array.of_list (list_group fst snd (Array.to_list ai))
    in
    let rec loop low up =
      let array_norm : (Code.cont * int list) array =
        normalize (Array.sub ai ~pos:low ~len:(up - low + 1))
      in
      let array_len = Array.length array_norm in
      if array_len = 1 (* remaining cases all jump to the same branch *)
      then Branch (fst array_norm.(0))
      else
        try
          (* try to optimize when there are only 2 branch *)
          match array_norm with
          | [| (b1, [ i1 ]); (b2, _l2) |] ->
              If (CEq (Int32.of_int i1), Branch b1, Branch b2)
          | [| (b1, _l1); (b2, [ i2 ]) |] ->
              If (CEq (Int32.of_int i2), Branch b2, Branch b1)
          | [| (b1, l1); (b2, l2) |] ->
              let bound l1 =
                match l1, List.rev l1 with
                | min :: _, max :: _ -> min, max
                | _ -> assert false
              in
              let min1, max1 = bound l1 in
              let min2, max2 = bound l2 in
              if max1 < min2
              then If (CLt (Int32.of_int max1), Branch b2, Branch b1)
              else if max2 < min1
              then If (CLt (Int32.of_int max2), Branch b1, Branch b2)
              else raise Not_found
          | _ -> raise Not_found
        with Not_found -> (
          (* do we have to split again ? *)
          (* we count the number of cases, default/last case count for one *)
          let nbcases = ref 1 (* default case *) in
          for i = 0 to array_len - 2 do
            nbcases := !nbcases + List.length (snd array_norm.(i))
          done;
          if !nbcases <= m
          then Switch (Array.map array_norm ~f:(fun (x, l) -> l, Branch x))
          else
            let h = (up + low) / 2 in
            let b1 = loop low h and b2 = loop (succ h) up in
            let range1 = snd ai.(h) and range2 = snd ai.(succ h) in
            match range1, range2 with
            | [], _ | _, [] -> assert false
            | _, lower_bound2 :: _ -> If (CLe (Int32.of_int lower_bound2), b2, b1))
    in
    let len = Array.length ai in
    if len = 0 then Empty else loop 0 (len - 1)

  let rec fold_cont f b acc =
    match b with
    | If (_, b1, b2) ->
        let acc = fold_cont f b1 acc in
        let acc = fold_cont f b2 acc in
        acc
    | Switch a -> Array.fold_left a ~init:acc ~f:(fun acc (_, b) -> fold_cont f b acc)
    | Branch (pc, _) -> f pc acc
    | Empty -> acc

  let nbcomp a =
    let rec loop c = function
      | Empty -> c
      | Branch _ -> c
      | If (_, a, b) ->
          let c = succ c in
          let c = loop c a in
          let c = loop c b in
          c
      | Switch a ->
          let c = succ c in
          Array.fold_left a ~init:c ~f:(fun acc (_, b) -> loop acc b)
    in
    loop 0 a
end

let fold_children blocks pc f accu =
  let block = Addr.Map.find pc blocks in
  match block.branch with
  | Return _ | Raise _ | Stop -> accu
  | Branch (pc', _) | Poptrap ((pc', _), _) -> f pc' accu
  | Pushtrap ((pc1, _), _, (pc2, _), _) ->
      let accu = f pc1 accu in
      let accu = f pc2 accu in
      accu
  | Cond (_, cont1, cont2) -> DTree.fold_cont f (DTree.build_if cont1 cont2) accu
  | Switch (_, a1, a2) ->
      let a1 = DTree.build_switch a1 and a2 = DTree.build_switch a2 in
      let accu = DTree.fold_cont f a1 accu in
      let accu = DTree.fold_cont f a2 accu in
      accu

let rec build_graph st pc anc =
  if not (Addr.Set.mem pc st.visited_blocks)
  then (
    st.visited_blocks <- Addr.Set.add pc st.visited_blocks;
    let anc = Addr.Set.add pc anc in
    let s = Code.fold_children st.blocks pc Addr.Set.add Addr.Set.empty in
    let backs = Addr.Set.inter s anc in
    Hashtbl.add st.backs pc backs;
    let s = fold_children st.blocks pc (fun x l -> x :: l) [] in
    let succs = List.filter s ~f:(fun pc -> not (Addr.Set.mem pc anc)) in
    Hashtbl.add st.succs pc succs;
    Addr.Set.iter (fun pc' -> st.loops <- Addr.Set.add pc' st.loops) backs;
    List.iter succs ~f:(fun pc' -> build_graph st pc' anc);
    List.iter succs ~f:(fun pc' -> incr_preds st pc'))

let rec dominance_frontier_rec st pc visited grey =
  let n = get_preds st pc in
  let v = try Addr.Map.find pc visited with Not_found -> 0 in
  if v < n
  then
    let v = v + 1 in
    let visited = Addr.Map.add pc v visited in
    if v = n
    then
      let grey = Addr.Set.remove pc grey in
      let s = Hashtbl.find st.succs pc in
      List.fold_right s ~init:(visited, grey) ~f:(fun pc' (visited, grey) ->
          dominance_frontier_rec st pc' visited grey)
    else visited, if v = 1 then Addr.Set.add pc grey else grey
  else visited, grey

let dominance_frontier st pc =
  snd (dominance_frontier_rec st pc Addr.Map.empty Addr.Set.empty)

let rec resolve_node interm pc =
  try resolve_node interm (fst (Addr.Map.find pc interm)) with Not_found -> pc

let resolve_nodes interm s =
  Addr.Set.fold (fun pc s' -> Addr.Set.add (resolve_node interm pc) s') s Addr.Set.empty

(****)

let rec visit visited prev s m x l =
  if not (Var.Set.mem x visited)
  then
    let visited = Var.Set.add x visited in
    let y = Var.Map.find x m in
    if Code.Var.compare x y = 0
    then visited, None, l
    else if Var.Set.mem y prev
    then
      let t = Code.Var.fresh () in
      visited, Some (y, t), (x, t) :: l
    else if Var.Set.mem y s
    then
      let visited, aliases, l = visit visited (Var.Set.add x prev) s m y l in
      match aliases with
      | Some (a, b) when Code.Var.compare a x = 0 -> visited, None, (b, a) :: (x, y) :: l
      | _ -> visited, aliases, (x, y) :: l
    else visited, None, (x, y) :: l
  else visited, None, l

let visit_all params args =
  let m = Subst.build_mapping params args in
  let s = List.fold_left params ~init:Var.Set.empty ~f:(fun s x -> Var.Set.add x s) in
  let _, l =
    Var.Set.fold
      (fun x (visited, l) ->
        let visited, _, l = visit visited Var.Set.empty s m x l in
        visited, l)
      s
      (Var.Set.empty, [])
  in
  l

let parallel_renaming params args continuation queue =
  let l = List.rev (visit_all params args) in
  List.fold_left
    l
    ~f:(fun continuation (y, x) queue ->
      let instrs, ((px, cx), queue) = access_queue_may_flush queue y x in
      let st, queue =
        flush_queue
          queue
          px
          (instrs @ [ J.Variable_statement [ Id.V y, Some (cx, Loc.N) ], Loc.N ])
      in
      st @ continuation queue)
    ~init:continuation
    queue

(****)

(* let apply_fun_raw ctx f params = *)
(*   let n = List.length params in *)
(*   J.ECond (J.EBin (J.EqEq, J.EDot (f, "length"), *)
(*                    J.EFloat (float n)), *)
(*            J.ECall (f, params, Loc.N), *)
(*            J.ECall (runtime_fun ctx "caml_call_gen", *)
(*                     [f; J.EArr (List.map (fun x -> Some x) params)], Loc.N)) *)
let apply_fun_raw ctx f params =
  let n = List.length params in
  J.ECond
   (* FROM JSOO: 
   `( J.EBin (J.EqEq, J.EDot (f, "length"), int n)
    , ecall f params J.N
    , ecall
        (runtime_fun ctx "caml_call_gen")
        [ f; J.EArr (List.map params ~f:(fun x -> Some x)) ]
        J.N ) *)
    ( J.EBin (J.EqEqEq, arity_test ~ctx f, J.EInt n)
    , J.ECall (f, params, Loc.N)
    , J.ECall
        ( runtime_fun ctx "caml_call_gen"
        , [ f; J.EArr (List.map params ~f:(fun x -> Some x)) ]
        , Loc.N ) )
    Array.to_list
      (Array.init n ~f:(fun i ->
           let a = Var.fresh_n (Printf.sprintf "a%d" i) in
           Id.V a))
  in
  let f' = J.EVar f in
  let params' = List.map params ~f:(fun x -> J.EVar x) in
  J.EFun
    ( None
    , f :: params
    , [ J.Statement (J.Return_statement (Some (apply_fun_raw ctx f' params'))), Loc.N ]
    , Loc.N )

let apply_fun ctx f params loc =
  if Config.Flag.inline_callgen ()
  then apply_fun_raw ctx f params
  else
    let len = List.length params in
    let prim_name = Printf.sprintf "caml_call%d" len in
    if_prim_supplied
      prim_name
      (* If compiled with a runtime.js input that has defined caml_call_x
         then we can pull in references to those runtime functions. *)
      ~if_supplied:(fun ~pretty_name ->
        let p = Share.get_prim ~pretty_name (runtime_fun ctx) prim_name ctx.Ctx.share in
        J.ECall (p, f :: params, Loc.N))
        (* Otherwise, if it's not in a "primitive", we will define a new
           caml_call_x inside this module. *)
      ~fallback:(fun () ->
        let y = Share.get_apply (generate_apply_fun ctx) len ctx.Ctx.share in
        J.ECall (y, f :: params, loc))

(****)

let to_int cx = J.EUn (J.ToInt, cx)

(* 32 bit ints *)

let _ =
  List.iter
    ~f:(fun (nm, nm') -> Primitive.alias nm nm')
    [ "%int_mul", "caml_mul"
    ; "%int_div", "caml_div"
    ; "%int_mod", "caml_mod"
    ; "caml_int32_neg", "%int_neg"
    ; "caml_int32_add", "%int_add"
    ; "caml_int32_sub", "%int_sub"
    ; "caml_int32_mul", "%int_mul"
    ; "caml_int32_div", "%int_div"
    ; "caml_int32_mod", "%int_mod"
    ; "caml_int32_and", "%int_and"
    ; "caml_int32_or", "%int_or"
    ; "caml_int32_xor", "%int_xor"
    ; "caml_int32_shift_left", "%int_lsl"
    ; "caml_int32_shift_right", "%int_asr"
    ; "caml_int32_shift_right_unsigned", "%int_lsr"
    ; "caml_int32_of_int", "%identity"
    ; "caml_int32_to_int", "%identity"
    ; "caml_int32_of_float", "caml_int_of_float"
    ; "caml_int32_to_float", "%identity"
    ; "caml_int32_format", "caml_format_int"
    ; "caml_int32_of_string", "caml_int_of_string"
    ; "caml_int32_compare", "caml_int_compare"
    ; "caml_nativeint_neg", "%int_neg"
    ; "caml_nativeint_add", "%int_add"
    ; "caml_nativeint_sub", "%int_sub"
    ; "caml_nativeint_mul", "%int_mul"
    ; "caml_nativeint_div", "%int_div"
    ; "caml_nativeint_mod", "%int_mod"
    ; "caml_nativeint_and", "%int_and"
    ; "caml_nativeint_or", "%int_or"
    ; "caml_nativeint_xor", "%int_xor"
    ; "caml_nativeint_shift_left", "%int_lsl"
    ; "caml_nativeint_shift_right", "%int_asr"
    ; "caml_nativeint_shift_right_unsigned", "%int_lsr"
    ; "caml_nativeint_of_int", "%identity"
    ; "caml_nativeint_to_int", "%identity"
    ; "caml_nativeint_of_float", "caml_int_of_float"
    ; "caml_nativeint_to_float", "%identity"
    ; "caml_nativeint_of_int32", "%identity"
    ; "caml_nativeint_to_int32", "%identity"
    ; "caml_nativeint_format", "caml_format_int"
    ; "caml_nativeint_of_string", "caml_int_of_string"
    ; "caml_nativeint_compare", "caml_int_compare"
    ; "caml_nativeint_bswap", "caml_int32_bswap"
    ; "caml_int64_of_int", "caml_int64_of_int32"
    ; "caml_int64_to_int", "caml_int64_to_int32"
    ; "caml_int64_of_nativeint", "caml_int64_of_int32"
    ; "caml_int64_to_nativeint", "caml_int64_to_int32"
    ; "caml_float_of_int", "%identity"
    ; "caml_array_get_float", "caml_array_get"
    ; "caml_floatarray_get", "caml_array_get"
    ; "caml_array_get_addr", "caml_array_get"
    ; "caml_array_set_float", "caml_array_set"
    ; "caml_floatarray_set", "caml_array_set"
    ; "caml_array_set_addr", "caml_array_set"
    ; "caml_array_unsafe_get_float", "caml_array_unsafe_get"
    ; "caml_floatarray_unsafe_get", "caml_array_unsafe_get"
    ; "caml_array_unsafe_set_float", "caml_array_unsafe_set"
    ; "caml_floatarray_unsafe_set", "caml_array_unsafe_set"
    ; "caml_alloc_dummy_float", "caml_alloc_dummy"
    ; "caml_make_array", "%identity"
    ; "caml_ensure_stack_capacity", "%identity"
    ; "caml_js_from_float", "%identity"
    ; "caml_js_to_float", "%identity"
    ]

(* Internal primitives are ones created/used by the jsoo compiler, not
 * referenced in bytecode. *)
let internal_primitives = Hashtbl.create 31

let internal_prim name =
  try Hashtbl.find internal_primitives name with Not_found -> None

(* Registers the *internal* primitive [name] with [Jsoo_primitive] so that
 * [Driver] can see them, along with all the other primitives added when
 * observed by [Linker].  Registering them with [Jsoo_primitive] merely makes
 * their presence known.  In order for them to be useful, either the linker
 * must link in implementations of them, or they must be considered "internal"
 * primitives, which [Generate] registers whenever it registers with
 * [Jsoo_primitive].  "internal primitives" also have a Rehp implementation. *)
let register_prim name k f =
  Primitive.register name k None None;
  Hashtbl.add internal_primitives name (Some f)

let register_un_prim name k f =
  register_prim name k (fun l queue ctx loc ->
      match l with
      | [ x ] ->
          let (px, cx), queue = access_queue' ~ctx queue x in
          f cx loc, or_p (kind k) px, queue
      | _ -> failwith name)

let register_un_prim_ctx name k f =
  register_prim name k (fun l queue ctx loc ->
      match l with
      | [ x ] ->
          let (px, cx), queue = access_queue' ~ctx queue x in
          f ctx cx loc, or_p (kind k) px, queue
      | _ -> assert false)

let register_bin_prim_ctx name k f =
  register_prim name k (fun l queue ctx loc ->
      match l with
      | [ x; y ] ->
          let (px, cx), queue = access_queue' ~ctx queue x in
          let (py, cy), queue = access_queue' ~ctx queue y in
          f ctx cx cy loc, or_p (kind k) (or_p px py), queue
      | _ -> assert false)

let register_bin_prim name k f =
  register_prim name k (fun l queue ctx loc ->
      match l with
      | [ x; y ] ->
          let (px, cx), queue = access_queue' ~ctx queue x in
          let (py, cy), queue = access_queue' ~ctx queue y in
          f cx cy loc, or_p (kind k) (or_p px py), queue
      | _ -> assert false)

let register_tern_prim name f =
  register_prim name `Mutator (fun l queue ctx loc ->
      match l with
      | [ x; y; z ] ->
          let (px, cx), queue = access_queue' ~ctx queue x in
          let (py, cy), queue = access_queue' ~ctx queue y in
          let (pz, cz), queue = access_queue' ~ctx queue z in
          f cx cy cz loc, or_p mutator_p (or_p px (or_p py pz)), queue
      | _ -> assert false)

let rec take ?(so_far = []) n lst =
  if n < 0
  then raise (Invalid_argument "take passed negative")
  else if n == 0
  then List.rev so_far, lst
  else
    match lst with
    | Pc (IString hd) :: tl -> take ~so_far:(Some hd :: so_far) (n - 1) tl
    | Pc (Int int32_name) :: tl when Int32.to_int int32_name == -1 ->
        take ~so_far:(None :: so_far) (n - 1) tl
    | _ ->
        raise
          (Invalid_argument
             ("Malformed metadata in compiled module (problem with args) - "
             ^ "metadata must be of the form (string(export-name)|-1) arity \
                list(string-arg-names)"))

let rec module_metadata so_far ?(so_far_len = List.length so_far) lst =
  match lst with
  | [] -> List.rev so_far
  | hd :: Pc (Int int32_arity) :: rest ->
      let name =
        match hd with
        | Pc (Int int32_name) when Int32.to_int int32_name == -1 -> None
        | Pc (IString name) -> Some name
        | _ ->
            raise
              (Invalid_argument
                 ("Malformed metadata in compiled module - name field is not string and \
                   not -1 - "
                 ^ "metadata must be of the form (string(export-name)|-1) arity \
                    list(string-arg-names)"))
      in
      let arity_n = Int32.to_int int32_arity in
      let taken, untaken = take arity_n rest in
      let module_export_metadata =
        { Module_export_metadata.original_name = name
        ; export_index = so_far_len + 1
        ; arity = arity_n
        ; arg_names = taken
        }
      in
      module_metadata
        (module_export_metadata :: so_far)
        ~so_far_len:(so_far_len + 1)
        untaken
  | _ ->
      raise
        (Invalid_argument
           ("Malformed metadata in compiled module. "
           ^ "metadata must be of the form (string(export-name)|-1) arity \
              list(string-arg-names)"))

let register_module_exporter name f =
  register_prim name `Mutator (fun l queue ctx loc ->
      match l with
      | x :: y :: z :: rest ->
          let (px, cx), queue = access_queue' ~ctx queue x in
          let (py, cy), queue = access_queue' ~ctx queue y in
          let (pz, cz), queue = access_queue' ~ctx queue z in
          let module_md = module_metadata [] rest in
          ctx.Ctx.module_export_metadatas <- module_md;
          f ctx cx cy cz module_md loc, or_p mutator_p (or_p px (or_p py pz)), queue
      | _ -> assert false)

let register_un_math_prim name prim =
  register_un_prim name `Pure (fun cx loc ->
      J.ECall (J.EDot (s_var "Math", prim), [ cx ], loc))
(* let register_un_math_prim name prim =
  register_un_prim name `Pure (fun cx loc ->
      ecall (J.EDot (s_var "Math", prim)) [ cx ] loc) *)

let register_bin_math_prim name prim =
  register_bin_prim name `Pure (fun cx cy loc ->
      J.ECall (J.EDot (s_var "Math", prim), [ cx; cy ], loc))
(* let register_bin_math_prim name prim =
  register_bin_prim name `Pure (fun cx cy loc ->
      ecall (J.EDot (s_var "Math", prim)) [ cx; cy ] loc) *)

let _ =
  register_un_prim_ctx "%caml_format_int_special" `Pure (fun ctx cx loc ->
      let s = J.EBin (J.Plus, str_js "", cx) in
      ocaml_string ~ctx ~loc s);
  (* register_bin_prim "caml_array_unsafe_get" `Mutable (fun cx cy _ ->
      Mlvalue.Array.field cx cy) *)
  (* register_un_prim_ctx "%caml_format_int_special" `Pure (fun ctx cx loc ->
      let p = Share.get_prim (runtime_fun ctx) "caml_new_string" ctx.Ctx.share in
      J.ECall (p, [ J.EUn (J.IntToString, cx) ], loc)); *)
  register_module_exporter
    "%caml_register_global_module_metadata"
    (fun ctx cx cy cz md loc ->
      let runtime_var_getter () =
        Share.get_prim (runtime_fun ctx) "caml_register_global" ctx.Ctx.share
      in
      match Backend.Current.custom_module_registration () with
      | None -> J.ECall (runtime_var_getter (), [ cx; cy; cz ], loc)
      | Some reg -> (
          match reg runtime_var_getter cy md with
          | None -> J.ECall (runtime_var_getter (), [ cx ], loc)
          | Some expr -> expr));

  (* The version that has been optimized by specialize_js.ml *)
  register_module_exporter "%caml_register_global_module" (fun ctx cx cy cz md loc ->
      let runtime_var_getter () =
        Share.get_prim (runtime_fun ctx) "caml_register_global" ctx.Ctx.share
      in
      J.ECall (runtime_var_getter (), [ cx; cy; cz ], loc));

  register_un_prim_ctx "%caml_load_global_module" `Pure (fun ctx cx loc ->
      let runtime_var_getter () =
        Share.get_prim (runtime_fun ctx) "caml_load_global_module" ctx.Ctx.share
      in
      match cx with
      | J.EStr (normalized_module_name, encoding) -> (
          match Backend.Current.custom_module_loader () with
          | None -> J.ECall (runtime_var_getter (), [ cx ], loc)
          | Some loader -> (
              match loader runtime_var_getter normalized_module_name with
              | None -> J.ECall (runtime_var_getter (), [ cx ], loc)
              | Some expr -> expr))
      | _ -> J.ECall (runtime_var_getter (), [ cx ], loc));

  register_un_prim_ctx "%caml_require" `Pure (fun ctx cx loc ->
    match cx with
    | J.EStr (path, _) -> (
      (* We don't have any fallback for requires that were not "hoisted". They
       * must be hoisted. *)
      Share.get_require (fun path ->
       J.EStr
        ("Cannot require module " ^ path ^ " likely because backend does not support requires",
        `Utf8))
      path
      ctx.Ctx.share);
    | _ -> failwith(Errors.weirdArgumentsToCamlRequire));

  (* This codepath unlikely to be hit since raw macro provides a better error *)
  register_un_prim "%caml_js_to_string_from_raw" `Mutable (fun cx loc ->
    raise (
      Errors.UserError(
        Errors.invalidStringFromMacro "Unknown macro contents", maybe_parse_info loc)));

  register_un_prim "polymorphic_log" `Mutable (fun cx loc ->
      J.ECall (s_var "polymorphic_log", [ cx ], loc));
  register_bin_prim "caml_array_unsafe_get" `Mutable (fun cx cy _ ->
      J.EArrAccess (cx, plus_int cy one));
  register_bin_prim "%int_add" `Pure (fun cx cy _ -> to_int (plus_int cx cy));
  register_bin_prim "%int_sub" `Pure (fun cx cy _ -> to_int (J.EBin (J.Minus, cx, cy)));
  register_bin_prim "%direct_int_mul" `Pure (fun cx cy _ ->
      to_int (J.EBin (J.Mul, cx, cy)));
  register_bin_prim "%direct_int_div" `Pure (fun cx cy _ ->
      to_int (J.EBin (J.Div, cx, cy)));
  register_bin_prim "%direct_int_mod" `Pure (fun cx cy _ ->
      to_int (J.EBin (J.Mod, cx, cy)));
  register_bin_prim "%int_and" `Pure (fun cx cy _ -> J.EBin (J.Band, cx, cy));
  register_bin_prim "%int_or" `Pure (fun cx cy _ -> J.EBin (J.Bor, cx, cy));
  register_bin_prim "%int_xor" `Pure (fun cx cy _ -> J.EBin (J.Bxor, cx, cy));
  register_bin_prim_ctx "%int_lsl" `Pure (fun ctx cx cy _ ->
      if_prim_supplied
        "left_shift_32"
        ~if_supplied:(fun ~pretty_name ->
          let p =
            Share.get_prim ~pretty_name (runtime_fun ctx) "left_shift_32" ctx.Ctx.share
          in
          J.ECall (p, [ cx; cy ], Loc.N))
        ~fallback:(fun () -> J.EBin (J.Lsl, cx, cy)));
  register_bin_prim_ctx "%int_lsr" `Pure (fun ctx cx cy _ ->
      if_prim_supplied
        "unsigned_right_shift_32"
        ~if_supplied:(fun ~pretty_name ->
          let p =
            Share.get_prim
              ~pretty_name
              (runtime_fun ctx)
              "unsigned_right_shift_32"
              ctx.Ctx.share
          in
          to_int (J.ECall (p, [ cx; cy ], Loc.N)))
        ~fallback:(fun () -> to_int (J.EBin (J.Lsr, cx, cy))));
  register_bin_prim_ctx "%int_asr" `Pure (fun ctx cx cy _ ->
      if_prim_supplied
        "right_shift_32"
        ~if_supplied:(fun ~pretty_name ->
          let p =
            Share.get_prim ~pretty_name (runtime_fun ctx) "right_shift_32" ctx.Ctx.share
          in
          J.ECall (p, [ cx; cy ], Loc.N))
        ~fallback:(fun () -> J.EBin (J.Asr, cx, cy)));
  register_un_prim "%int_neg" `Pure (fun cx _ -> to_int (J.EUn (J.Neg, cx)));
  register_bin_prim "caml_eq_float" `Pure (fun cx cy _ ->
      bool (J.EBin (J.FloatEqEq, float_val cx, float_val cy)));
  register_bin_prim "caml_neq_float" `Pure (fun cx cy _ ->
      bool (J.EBin (J.FloatNotEq, float_val cx, float_val cy)));
  register_bin_prim "caml_ge_float" `Pure (fun cx cy _ ->
      bool (J.EBin (J.FloatLe, float_val cy, float_val cx)));
  register_bin_prim "caml_le_float" `Pure (fun cx cy _ ->
      bool (J.EBin (J.FloatLe, float_val cx, float_val cy)));
  register_bin_prim "caml_gt_float" `Pure (fun cx cy _ ->
      bool (J.EBin (J.FloatLt, float_val cy, float_val cx)));
  register_bin_prim "caml_lt_float" `Pure (fun cx cy _ ->
      bool (J.EBin (J.FloatLt, float_val cx, float_val cy)));
  register_bin_prim "caml_add_float" `Pure (fun cx cy _ ->
      val_float (J.EBin (J.FloatPlus, float_val cx, float_val cy)));
  register_bin_prim "caml_sub_float" `Pure (fun cx cy _ ->
      val_float (J.EBin (J.FloatMinus, float_val cx, float_val cy)));
  register_bin_prim "caml_mul_float" `Pure (fun cx cy _ ->
      val_float (J.EBin (J.FloatMul, float_val cx, float_val cy)));
  register_bin_prim "caml_div_float" `Pure (fun cx cy _ ->
      val_float (J.EBin (J.FloatDiv, float_val cx, float_val cy)));
  register_un_prim "caml_neg_float" `Pure (fun cx _ ->
      val_float (J.EUn (J.FloatNeg, float_val cx)));
  register_bin_prim "caml_fmod_float" `Pure (fun cx cy _ ->
      val_float (J.EBin (J.FloatMod, float_val cx, float_val cy)));
  register_tern_prim "caml_array_unsafe_set" (fun cx cy cz _ ->
      J.EBin (J.Eq, J.EArrAccess (cx, plus_int cy one), cz));
  register_un_prim "caml_alloc_dummy" `Pure (fun cx loc ->
    match cx with
    | J.EInt i ->
      (* This allocates a "dummy" cell which will later be updated in-place.
      This primitive is emitted by OCaml compiler during processing of
      recursive let-bindings. Requested size is `i`, but as block tags are
      stored as first elements of an array, we allocate `i+1` to have enough
      room for that. All values except for integers and float arrays/records
      are tagged in OCaml, so assuming tag will be present during consequent
      update is a sane thing to do. *)
      J.EStruct (Array.make (i + 1) (J.EInt 0) |> Array.to_list)
    | _ -> failwith(
      "Encountered a caml_alloc_dummy without an integer size supplied. " ^
      "This is a bug in the compiler. Please report it.")
  );
  register_un_prim "caml_obj_dup" `Mutable (fun cx loc -> J.ECopy (cx, loc));
  register_un_prim "caml_int_of_float" `Pure (fun cx _loc -> (J.EUn (J.FloatToInt, cx)));
  register_un_math_prim "caml_abs_float" "abs";
  register_un_math_prim "caml_acos_float" "acos";
  register_un_math_prim "caml_asin_float" "asin";
  register_un_math_prim "caml_atan_float" "atan";
  register_bin_math_prim "caml_atan2_float" "atan2";
  register_un_math_prim "caml_ceil_float" "ceil";
  register_un_math_prim "caml_cos_float" "cos";
  register_un_math_prim "caml_exp_float" "exp";
  register_un_math_prim "caml_floor_float" "floor";
  register_un_math_prim "caml_log_float" "log";
  register_bin_math_prim "caml_power_float" "pow";
  register_un_math_prim "caml_sin_float" "sin";
  register_un_math_prim "caml_sqrt_float" "sqrt";
  register_un_math_prim "caml_tan_float" "tan";
  register_un_prim "caml_js_from_bool" `Pure (fun cx _ ->
      J.EUn (J.Not, J.EUn (J.Not, cx)));
  register_un_prim "caml_js_to_bool" `Pure (fun cx _ -> (J.EUn (J.ToBool, cx)));
  register_un_prim "caml_js_from_string" `Mutable (fun cx loc ->
      J.ECall (J.EDot (cx, "toString"), [], loc));
  register_tern_prim "caml_js_set" (fun cx cy cz _ ->
      J.EBin (J.Eq, J.EAccess (cx, cy), cz));
  register_bin_prim "caml_js_get" `Mutable (fun cx cy _ -> J.EAccess (cx, cy));
  (* Like caml_js_get but will never optimized to EDot *)
  register_bin_prim "caml_js_dict_get" `Mutable (fun cx cy _ -> J.EAccess (cx, cy));
  (* Like caml_js_set but will never optimized to EDot *)
  register_bin_prim "caml_js_dict_set" `Mutable (fun cx cy _ -> J.EAccess (cx, cy));
  register_bin_prim "caml_js_delete" `Mutable (fun cx cy _ ->
      J.EUn (J.Delete, J.EAccess (cx, cy)));
  register_bin_prim "caml_js_equals" `Mutable (fun cx cy _ ->
      bool (J.EBin (J.EqEq, cx, cy)));
  register_bin_prim "caml_js_instanceof" `Pure (fun cx cy _ ->
      bool (J.EBin (J.InstanceOf, cx, cy)));
  register_un_prim "caml_js_typeof" `Pure (fun cx _ -> J.EUn (J.Typeof, cx))

(* This is not correct when switching the js-string flag *)
(* {[
    register_un_prim "caml_jsstring_of_string" `Mutable (fun cx loc ->
      J.ECall (J.EDot (cx, "toString"), [], loc));
    register_bin_prim "caml_string_notequal" `Pure (fun cx cy _ ->
      J.EBin (J.NotEqEq, cx, cy));
    register_bin_prim "caml_string_equal" `Pure (fun cx cy _ ->
      bool (J.EBin (J.EqEq, cx, cy)))
     ]}
   *)

(****)
(* when raising ocaml exception and [improved_stacktrace] is enabled,
   tag the ocaml exception with a Javascript error (that contain js stacktrace).
   {[ throw e ]}
   becomes
   {[ throw (thrown_exception(e,false)) ]}
*)
let throw_statement ctx cx k loc =
  match (k : [ `Normal | `Reraise | `Notrace ]) with
  | _ when not (Config.Flag.improved_stacktrace ()) -> [ J.Throw_statement cx, loc ]
  | `Notrace ->
      [ ( J.Throw_statement
            (J.ECall
               ( Share.get_prim
                   (runtime_fun ctx)
                   "caml_wrap_thrown_exception_traceless"
                   ctx.Ctx.share
               , [ cx ]
               , loc ))
        , loc )
      ]
  | `Normal ->
      [ ( J.Throw_statement
            (J.ECall
               ( Share.get_prim
                   (runtime_fun ctx)
                   "caml_wrap_thrown_exception"
                   ctx.Ctx.share
               , [ cx ]
               , loc ))
        , loc )
      ]
  | `Reraise ->
      [ ( J.Throw_statement
            (J.ECall
               ( Share.get_prim
                   (runtime_fun ctx)
                   "caml_wrap_thrown_exception_reraise"
                   ctx.Ctx.share
               , [ cx ]
               , loc ))
        , loc )
      ]

let rec translate_expr ctx queue loc _x e level : _ * J.statement_list =
  match e with
  | Apply (x, l, true) ->
      let (px, cx), queue = access_queue queue x in
      let args, prop, queue =
        List.fold_right
          ~f:(fun x (args, prop, queue) ->
            let (prop', cx), queue = access_queue queue x in
            cx :: args, or_p prop prop', queue)
          l
          ~init:([], or_p px mutator_p, queue)
      in
      (J.ECall (cx, args, loc), prop, queue), []
  | Apply (x, l, false) ->
      let args, prop, queue =
        List.fold_right
          ~f:(fun x (args, prop, queue) ->
            let (prop', cx), queue = access_queue queue x in
            cx :: args, or_p prop prop', queue)
          l
          ~init:([], mutator_p, queue)
      in
      let (prop', f), queue = access_queue queue x in
      let prop = or_p prop prop' in
      let e = apply_fun ctx f args loc in
      (e, prop, queue), []
  | Block (tag, a, _array_or_not) ->
      let contents, prop, queue =
        List.fold_right
          ~f:(fun x (args, prop, queue) ->
            let (prop', cx), queue = access_queue queue x in
            cx :: args, or_p prop prop', queue)
          (Array.to_list a)
          ~init:([], const_p, queue)
      in
      (* TODO: (rehp) - consider using Mlvalue construct
         let x =
           match array_or_not with
           | Array -> Mlvalue.Array.make ~tag ~args:contents
           | NotArray | Unknown -> Mlvalue.Block.make ~tag ~args:contents
         in
         (x, prop, queue), []
      *)
      (J.ETag (tag, contents), prop, queue), []
  | Field (x, n) ->
      let (px, cx), queue = access_queue queue x in
      (J.EStructAccess (cx, n + 1), or_p px mutable_p, queue), []
  | Closure (args, ((pc, _) as cont)) ->
      let loc = source_location ctx ~after:true pc in
      let clo = compile_closure ctx false cont in
      let clo =
        match clo with
        | (st, Loc.N) :: rem -> (st, Loc.U) :: rem
        | _ -> clo
      in
      let clo = J.EFun (None, List.map args ~f:(fun v -> Id.V v), clo, loc) in
      (clo, flush_p, queue), []
  | Constant c ->
      let js, instrs = constant ~ctx c level in
      (js, const_p, queue), instrs
  | Prim (Extern "debugger", _) ->
      let ins =
        if Config.Flag.debugger () then J.Debugger_statement else J.Empty_statement
      in
      (int 0, const_p, queue), [ ins, loc ]
  | Prim (p, l) ->
      let res =
        match p, l with
        | Vectlength, [ x ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            J.EVectlength cx, px, queue
        | Array_get, [ x; y ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            let (py, cy), queue = access_queue' ~ctx queue y in
            J.EArrAccess (cx, plus_int cy one), or_p mutable_p (or_p px py), queue
        | Extern "caml_js_var", [ Pc (String nm | IString nm) ] ->
            ERaw [ Rehp.RawText nm ], const_p, queue
        (* The fact that caml_js_raw_expr has "registered arity" is likely
         * going to be a problem here *)
        | Extern "%caml_js_expanded_raw_macro_done", Pc (String m | IString m) :: l
        | Extern "caml_js_raw_expr", Pc (String m | IString m) :: ([] as l) ->
            let args, prop, queue =
              List.fold_right
                ~f:(fun x (args, prop, queue) ->
                  let (prop', cx), queue = access_queue' ~ctx queue x in
                  cx :: args, or_p prop prop', queue)
                l
                ~init:([], mutator_p, queue)
            in
            let be = Backend.Current.compiler_backend_flag () in
            let macro_data = Raw_macro.extractExpanded ~forBackend:be m in
            let node_list = Raw_macro.parseNodeList macro_data in
            let flattened = Raw_macro.flattenFinal macro_data args node_list in
            Rehp.ERaw (flattened), prop, queue
        | Extern ("caml_js_expr" | "caml_pure_js_expr"), [ Pc (String nm | IString nm) ]
          -> (
            try
              let e = Rehp.ERaw [ Rehp.RawText nm ] in
              e, const_p, queue
            with Parse_js.Parsing_error pi ->
              failwith
                (Printf.sprintf
                   "Parsing error %S at l:%d col:%d"
                   nm
                   (pi.Parse_info.line + 1)
                   pi.Parse_info.col))
        | Extern "%js_array", l ->
            let args, prop, queue =
              List.fold_right
                ~f:(fun x (args, prop, queue) ->
                  let (prop', cx), queue = access_queue' ~ctx queue x in
                  cx :: args, or_p prop prop', queue)
                l
                ~init:([], const_p, queue)
            in
            J.EArr (List.map args ~f:(fun x -> Some x)), prop, queue
        | Extern "%closure", [ Pc (IString name | String name) ] ->
            let prim = Share.get_prim (runtime_fun ctx) name ctx.Ctx.share in
            prim, const_p, queue
        | Extern "%caml_js_opt_call", f :: o :: l ->
            let (pf, cf), queue = access_queue' ~ctx queue f in
            let (po, co), queue = access_queue' ~ctx queue o in
            let args, prop, queue =
              List.fold_right
                ~f:(fun x (args, prop, queue) ->
                  let (prop', cx), queue = access_queue' ~ctx queue x in
                  cx :: args, or_p prop prop', queue)
                l
                ~init:([], mutator_p, queue)
            in
            J.ECall (J.EDot (cf, "call"), co :: args, loc), or_p (or_p pf po) prop, queue
        | Extern "%caml_js_opt_fun_call", f :: l ->
            let (pf, cf), queue = access_queue' ~ctx queue f in
            let args, prop, queue =
              List.fold_right
                ~f:(fun x (args, prop, queue) ->
                  let (prop', cx), queue = access_queue' ~ctx queue x in
                  cx :: args, or_p prop prop', queue)
                l
                ~init:([], mutator_p, queue)
            in
            J.ECall (cf, args, loc), or_p pf prop, queue
        | Extern "%caml_js_opt_meth_call", o :: Pc (String m | IString m) :: l ->
            let (po, co), queue = access_queue' ~ctx queue o in
            let args, prop, queue =
              List.fold_right
                ~f:(fun x (args, prop, queue) ->
                  let (prop', cx), queue = access_queue' ~ctx queue x in
                  cx :: args, or_p prop prop', queue)
                l
                ~init:([], mutator_p, queue)
            in
            J.ECall (J.EDot (co, m), args, loc), or_p po prop, queue
        | Extern "%caml_js_opt_new", c :: l ->
            let (pc, cc), queue = access_queue' ~ctx queue c in
            let args, prop, queue =
              List.fold_right
                ~f:(fun x (args, prop, queue) ->
                  let (prop', cx), queue = access_queue' ~ctx queue x in
                  cx :: args, or_p prop prop', queue)
                l
                ~init:([], mutator_p, queue)
            in
            J.ENew (cc, if args = [] then None else Some args), or_p pc prop, queue
        (* caml_js_property_get is like caml_js_get but will _only_ be
           optimized to EDot and cannot use the runtime fallback *)
        | ( Extern ("caml_js_get" | "caml_js_property_get")
          , [ Pv o; Pc (String f | IString f) ] )
          when Backend.Current.is_ident () f ->
            let (po, co), queue = access_queue queue o in
            J.EDot (co, f), or_p po mutable_p, queue
        (* caml_js_property_set is like caml_js_set but will _only_ be
           optimized to EDot and cannot use the runtime fallback *)
        | ( Extern ("caml_js_set" | "caml_js_property_set")
          , [ Pv o; Pc (String f | IString f); v ] )
          when Backend.Current.is_ident () f ->
            let (po, co), queue = access_queue queue o in
            let (pv, cv), queue = access_queue' ~ctx queue v in
            J.EBin (J.Eq, J.EDot (co, f), cv), or_p (or_p po pv) mutator_p, queue
        | Extern "caml_js_delete", [ Pv o; Pc (String f | IString f) ]
          when Backend.Current.is_ident () f ->
            let (po, co), queue = access_queue queue o in
            J.EUn (J.Delete, J.EDot (co, f)), or_p po mutator_p, queue
        | Extern "%overrideMod", [ Pc (String m | IString m); Pc (String f | IString f) ]
          ->
            runtime_fun ctx (Printf.sprintf "caml_%s_%s" m f), const_p, queue
        | Extern "%overrideMod", _ -> assert false
        | Extern "%caml_js_opt_object", fields ->
            let rec build_fields queue l =
              match l with
              | [] -> const_p, [], queue
              | Pc (String nm | IString nm) :: x :: r ->
                  let (prop, cx), queue = access_queue' ~ctx queue x in
                  let prop', r', queue = build_fields queue r in
                  or_p prop prop', (Id.PNS nm, cx) :: r', queue
              | _ -> assert false
            in
            let prop, fields, queue = build_fields queue fields in
            J.EObj fields, prop, queue
        | Extern "caml_alloc_dummy_function", [ _; size ] ->
            let i, queue =
              let (_px, cx), queue = access_queue' ~ctx queue size in
              match cx with
              | J.ENum i -> Int32.to_int (J.Num.to_int32 i), queue
              | _ -> assert false
            in
            let args = Array.to_list (Array.init i ~f:(fun _ -> J.V (Var.fresh ()))) in
            let f = J.V (Var.fresh ()) in
            let call =
              ecall (J.EDot (J.EVar f, "fun")) (List.map args ~f:(fun v -> J.EVar v)) loc
            in
            let e =
              J.EFun
                (Some f, args, [ J.Statement (J.Return_statement (Some call)), J.N ], J.N)
            in
            e, const_p, queue
        | Extern "caml_alloc_dummy_function", _ -> assert false
        | Extern name, l -> (
            let name = Primitive.resolve name in
            match internal_prim name with
            | Some f -> f l queue ctx loc
            | None ->
                if String.is_prefix name ~prefix:"%"
                then failwith (Printf.sprintf "Unresolved internal primitive: %s" name);
                let prim = Share.get_prim (runtime_fun ctx) name ctx.Ctx.share in
                let prim_kind = kind (Primitive.kind name) in
                let args, prop, queue =
                  List.fold_right
                    ~f:(fun x (args, prop, queue) ->
                      let (prop', cx), queue = access_queue' ~ctx queue x in
                      cx :: args, or_p prop prop', queue)
                    l
                    ~init:([], prim_kind, queue)
                in
                ecall prim args loc, prop, queue)
        | Not, [ x ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            J.EBin (J.Minus, one, cx), px, queue
        | Lt, [ x; y ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            let (py, cy), queue = access_queue' ~ctx queue y in
            bool (J.EBin (J.Lt, cx, cy)), or_p px py, queue
        | Le, [ x; y ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            let (py, cy), queue = access_queue' ~ctx queue y in
            bool (J.EBin (J.Le, cx, cy)), or_p px py, queue
        | Eq, [ x; y ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            let (py, cy), queue = access_queue' ~ctx queue y in
            bool (J.EBin (J.EqEqEq, cx, cy)), or_p px py, queue
        | Neq, [ x; y ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            let (py, cy), queue = access_queue' ~ctx queue y in
            bool (J.EBin (J.NotEqEq, cx, cy)), or_p px py, queue
        | IsInt, [ x ] ->
            (* JavaScript engines recognize the pattern
               'typeof x==="number"'; if the string is shared,
               less efficient code is generated. *)
            let (px, cx), queue = access_queue' ~ctx queue x in
            is_int ~ctx cx, px, queue
        | Ult, [ x; y ] ->
            let (px, cx), queue = access_queue' ~ctx queue x in
            let (py, cy), queue = access_queue' ~ctx queue y in
            bool (J.EBin (J.Lt, unsigned ~ctx cx, unsigned ~ctx cy)), or_p px py, queue
        | (Vectlength | Array_get | Not | IsInt | Eq | Neq | Lt | Le | Ult), _ ->
            assert false
      in
      res, []

and translate_instr ctx expr_queue loc instr =
  match instr with
  | Let (x, e) -> (
      let (ce, prop, expr_queue), instrs = translate_expr ctx expr_queue loc x e 0 in
      (* This is why using Var.fresh_n to give readable names results in more bloated code.
       * Being named means we will "keep_name" which means variable bindings
       * won't get inlined into call sites. We need another kind of Var
       * "Var.fresh_convenient_name" which wouldln't prevent it from getting
       * inlined, but just give a more readable name in the event that it was
       * not. *)
      let keep_name x =
        match Code.Var.get_name x with
        | None -> false
        | Some s -> not (String.is_prefix s ~prefix:"jsoo_")
      in
      match ctx.Ctx.live.(Var.idx x), e with
      | 0, _ ->
          (* deadcode is off *)
          flush_queue expr_queue prop (instrs @ [ J.Expression_statement ce, loc ])
      | 1, _
        when Config.Flag.compact () && ((not (Config.Flag.pretty ())) || not (keep_name x))
        ->
          enqueue expr_queue prop x ce loc 1 instrs
      (* We could inline more.
         size_v : length of the variable after serialization
         size_c : length of the constant after serialization
         num : number of occurrence
         size_c * n < size_v * n + size_v + 1 + size_c
      *)
      | n, Constant (Int _ | Float _) -> enqueue expr_queue prop x ce loc n instrs
      | _ ->
          flush_queue
            expr_queue
            prop
            (instrs @ [ J.Variable_statement [ Id.V x, Some (ce, loc) ], loc ]))
  | Set_field (x, n, y) ->
      let (_px, cx), expr_queue = access_queue expr_queue x in
      let (_py, cy), expr_queue = access_queue expr_queue y in
      flush_queue
        expr_queue
        mutator_p
        [ ( J.Expression_statement (J.EBin (J.Eq, J.EStructAccess (cx, n + 1), cy))
          , loc )
        ]
  (* --/++ post/prefix is difficult to get working in all languages. Use +=
     instead. For example x[1]++ isn't supported in PHP. *)
  | Offset_ref (x, n) ->
      (* FIX: may overflow.. *)
      let (_px, cx), expr_queue = access_queue expr_queue x in
      flush_queue
        expr_queue
        mutator_p
        [ ( J.Expression_statement
              (J.EBin (J.PlusEq, J.EStructAccess (cx, 1), int n))
          , loc )
        ]
  | Array_set (x, y, z) ->
      let (_px, cx), expr_queue = access_queue expr_queue x in
      let (_py, cy), expr_queue = access_queue expr_queue y in
      let (_pz, cz), expr_queue = access_queue expr_queue z in
      flush_queue
        expr_queue
        mutator_p
        [ ( J.Expression_statement (J.EBin (J.Eq, J.EArrAccess (cx, plus_int cy one), cz))
          , loc )
        ]

and translate_instrs ctx expr_queue loc instr =
  match instr with
  | [] -> [], expr_queue
  | instr :: rem ->
      let st, expr_queue = translate_instr ctx expr_queue loc instr in
      let instrs, expr_queue = translate_instrs ctx expr_queue loc rem in
      st @ instrs, expr_queue

and compile_block st queue (pc : Addr.t) frontier interm =
  if queue <> [] && (Addr.Set.mem pc st.loops || not (Config.Flag.inline ()))
  then flush_all queue (compile_block st [] pc frontier interm)
  else (
    if pc >= 0
    then (
      if Addr.Set.mem pc st.visited_blocks
      then (
        Format.eprintf "Trying to compile a block twice !!!! %d@." pc;
        assert false);
      st.visited_blocks <- Addr.Set.add pc st.visited_blocks);
    if debug ()
    then (
      if Addr.Set.mem pc st.loops then Format.eprintf "@[<2>for(;;){@,";
      Format.eprintf "block %d;@ @?" pc);
    (if Addr.Set.mem pc st.loops
    then
      let lab =
        match st.loop_stack with
        | (_, (l, _)) :: _ -> Javascript.Label.succ l
        | [] -> Javascript.Label.zero
      in
      st.loop_stack <- (pc, (lab, ref false)) :: st.loop_stack);
    let succs = Hashtbl.find st.succs pc in
    let backs = Hashtbl.find st.backs pc in
    (* Remove limit *)
    if pc < 0 then List.iter succs ~f:(fun pc -> unprotect_preds st pc);
    let succs = List.map succs ~f:(fun pc -> pc, dominance_frontier st pc) in
    let grey =
      List.fold_right
        ~f:(fun (_, frontier) grey -> Addr.Set.union frontier grey)
        succs
        ~init:Addr.Set.empty
    in
    let new_frontier = resolve_nodes interm grey in
    let block = Addr.Map.find pc st.blocks in
    let seq, queue =
      translate_instrs st.ctx queue (source_location st.ctx pc) block.body
    in
    let body =
      seq
      @
      match block.branch with
      | Code.Pushtrap ((pc1, args1), x, (pc2, args2), pc3s) ->
          (* FIX: document this *)
          let pc2s = resolve_nodes interm (dominance_frontier st pc2) in
          let pc3s =
            Addr.Set.fold
              (fun pc3 acc ->
                (* We need to make sure that pc3 is live (indeed, the
                   continuation may have been optimized away by inlining) *)
                if Hashtbl.mem st.succs pc3
                then
                  (* no need to limit body for simple flow with no instruction.
                     eg return and branch *)
                  let rec limit pc =
                    if Addr.Set.mem pc pc2s
                    then false
                    else
                      let block = Addr.Map.find pc st.blocks in
                      block.body <> []
                      ||
                      match block.branch with
                      | Return _ -> false
                      | Poptrap ((pc', _), _) | Branch (pc', _) -> limit pc'
                      | _ -> true
                  in
                  if limit pc3 then Addr.Set.add pc3 acc else acc
                else acc)
              pc3s
              Addr.Set.empty
          in
          let grey = Addr.Set.union pc2s pc3s in
          Addr.Set.iter (incr_preds st) grey;
          let prefix, grey', new_interm = colapse_frontier st grey interm in
          assert (Addr.Set.cardinal grey' <= 1);
          let inner_frontier = Addr.Set.union new_frontier grey' in
          if debug () then Format.eprintf "@[<2>try {@,";
          let body =
            prefix
            @ compile_branch
                st
                []
                (pc1, args1)
                None
                Addr.Set.empty
                inner_frontier
                new_interm
          in
          if debug () then Format.eprintf "} catch {@,";
          let x =
            let block2 = Addr.Map.find pc2 st.blocks in
            let m = Subst.build_mapping args2 block2.params in
            try Var.Map.find x m with Not_found -> x
          in
          let handler = compile_block st [] pc2 inner_frontier new_interm in
          if debug () then Format.eprintf "}@]@ ";
          Addr.Set.iter (decr_preds st) grey;
          let after, exn_escape =
            if not (Addr.Set.is_empty grey')
            then
              let pc = Addr.Set.choose grey' in
              let exn_escape =
                let x' = Var.fork x in
                let found = ref false in
                let map_var y =
                  if Code.Var.equal x y
                  then (
                    found := true;
                    x')
                  else y
                in
                let subst_block pc blocks =
                  Addr.Map.add pc (Subst.block map_var (Addr.Map.find pc blocks)) blocks
                in
                let blocks =
                  Code.traverse
                    { fold = Code.fold_children }
                    subst_block
                    pc
                    st.blocks
                    st.blocks
                in
                if !found then st.blocks <- blocks;
                if !found then Some x' else None
              in
              if Addr.Set.mem pc frontier
              then [], exn_escape
              else compile_block st [] pc frontier interm, exn_escape
            else [], None
          in
          let handler =
            if st.ctx.Ctx.live.(Var.idx x) > 0 && Config.Flag.excwrap ()
            then
              ( J.Expression_statement
                  (J.EBin
                     ( J.Eq
                     , J.EVar (J.V x)
                     , ecall
                         (Share.get_prim
                            (runtime_fun st.ctx)
                            "caml_wrap_exception"
                            st.ctx.Ctx.share)
                         [ J.EVar (J.V x) ]
                         J.N ))
              , J.N )
              :: handler
            else handler
          in
          let handler =
            match exn_escape with
            | Some x' ->
                handler
                @ [ J.Variable_statement [ J.V x', Some (EVar (J.V x), J.N) ], J.N ]
            | None -> handler
          in
          flush_all
            queue
            (( J.Try_statement (body, Some (J.V x, handler), None)
             , source_location st.ctx pc )
             :: after)
      | _ ->
          let prefix, new_frontier, new_interm =
            colapse_frontier st new_frontier interm
          in
          assert (Addr.Set.cardinal new_frontier <= 1);
          (* Beware evaluation order! *)
          let cond =
            compile_conditional
              st
              queue
              pc
              block.branch
              block.handler
              backs
              new_frontier
              new_interm
              succs
          in
          prefix
          @ cond
          @
          if Addr.Set.cardinal new_frontier = 0
          then []
          else
            let pc = Addr.Set.choose new_frontier in
            if Addr.Set.mem pc frontier
            then []
            else compile_block st [] pc frontier interm
    in
    if Addr.Set.mem pc st.loops
    then
      let label =
        match st.loop_stack with
        | (_, (l, used)) :: r ->
            st.loop_stack <- r;
            if !used then Some l else None
        | [] -> assert false
      in
      let st =
        ( J.Loop_statement
            ( J.Block
                (if Addr.Set.cardinal frontier > 0
                then (
                  if debug ()
                  then Format.eprintf "@ break (%d); }@]" (Addr.Set.choose new_frontier);
                  body @ [ J.Break_statement None, Loc.N ])
                else (
                  if debug () then Format.eprintf "}@]";
                  body))
            , Loc.N )
        , source_location st.ctx pc )
      in
      match label with
      | None -> [ st ]
      | Some label -> [ J.Labelled_statement (label, st), Loc.N ]
    else body)

and colapse_frontier st new_frontier interm =
  if Addr.Set.cardinal new_frontier > 1
  then (
    if debug ()
    then
      Format.eprintf
        "colapse frontier into %d: %s@."
        st.interm_idx
        (string_of_set new_frontier);
    let x = Code.Var.fresh_n "switch" in
    let a =
      Addr.Set.elements new_frontier
      |> List.map ~f:(fun pc -> pc, get_preds st pc)
      |> List.sort ~cmp:(fun (_, (c1 : int)) (_, (c2 : int)) -> compare c2 c1)
      |> List.map ~f:fst
    in
    if debug () then Format.eprintf "@ var %a;" Code.Var.print x;
    let idx = st.interm_idx in
    st.interm_idx <- idx - 1;
    let cases = Array.map a ~f:(fun pc -> pc, []) in
    let switch =
      if Array.length cases > 2
      then Code.Switch (x, cases, [||])
      else Code.Cond (x, cases.(1), cases.(0))
    in
    st.blocks <-
      Addr.Map.add
        idx
        { params = []; handler = None; body = []; branch = switch }
        st.blocks;
    let pc_i = List.mapi ~f:(fun i pc -> pc, i) a in
    let default = 0 in
    (* There is a branch from this switch to the members
       of the frontier. *)
    Addr.Set.iter (fun pc -> incr_preds st pc) new_frontier;
    (* Put a limit: we are going to remove other branches
       to the members of the frontier (in compile_conditional),
       but they should remain in the frontier. *)
    Addr.Set.iter (fun pc -> protect_preds st pc) new_frontier;
    Hashtbl.add st.succs idx (Addr.Set.elements new_frontier);
    Hashtbl.add st.backs idx Addr.Set.empty;
    ( Addr.Set.singleton idx
    , Array.fold_right
        (Array.mapi ~f:(fun i pc -> pc, i) a)
        ~init:interm
        ~f:(fun (pc, i) interm -> Addr.Map.add pc (idx, (x, i)) interm) ))
  else new_frontier, interm

and compile_decision_tree st _queue handler backs frontier interm succs loc cx dtree =
  (* Some changes here may require corresponding changes
     in function [DTree.fold_cont] above. *)
  let rec loop cx = function
    | DTree.Empty -> assert false
    | DTree.Branch ((pc, _) as cont) ->
        (* Block of code that never continues (either returns, throws an exception
           or loops back) *)
        (* If not found in successors, this is a backward edge *)
        let never =
          let d = try List.assoc pc succs with Not_found -> Addr.Set.empty in
          (not (Addr.Set.mem pc frontier || Addr.Map.mem pc interm))
          && Addr.Set.is_empty d
        in
        never, compile_branch st [] cont handler backs frontier interm
    | DTree.If (cond, cont1, cont2) ->
        let never1, iftrue = loop cx cont1 in
        let never2, iffalse = loop cx cont2 in
        let e' =
          match cond with
          | IsTrue -> cx
          | CEq n -> J.EBin (J.EqEqEq, int32 n, cx)
          | CLt n -> J.EBin (J.Lt, int32 n, cx)
          | CLe n -> J.EBin (J.Le, int32 n, cx)
        in
        ( never1 && never2
        , Js_simpl.if_statement
            e'
            loc
            (J.Block iftrue, Loc.N)
            never1
            (J.Block iffalse, Loc.N)
            never2
            (Backend.Current.allow_simplify_ifdecl () && Config.Flag.simplify_ifdecl ()) )
    | DTree.Switch a ->
        let all_never = ref true in
        let len = Array.length a in
        let last_index = len - 1 in
        let arr =
          Array.mapi a ~f:(fun i (ints, cont) ->
              let never, cont = loop cx cont in
              if not never then all_never := false;
              let cont =
                if never || (* default case *) i = last_index
                then cont
                else cont @ [ J.Break_statement None, Loc.N ]
              in
              ints, cont)
        in
        let _, last = arr.(last_index) in
        let l = Array.to_list (Array.sub arr ~pos:0 ~len:(len - 1)) in
        let l =
          List.flatten
            (List.map l ~f:(fun (ints, br) ->
                 map_last (fun last i -> J.EInt i, if last then br else []) ints))
        in
        !all_never, [ J.Switch_statement (cx, l, last), loc ]
  in
  let cx, binds =
    match cx with
    | (J.EVar _ | _) when DTree.nbcomp dtree <= 1 -> cx, []
    | _ ->
        let v = Id.V (Code.Var.fresh ()) in
        J.EVar v, [ J.Variable_statement [ v, Some (cx, Loc.N) ], Loc.N ]
  in
  binds @ snd (loop cx dtree)

and compile_conditional st queue pc last handler backs frontier interm succs =
  List.iter succs ~f:(fun (pc, _) -> if Addr.Map.mem pc interm then decr_preds st pc);
  (if debug ()
  then
    match last with
    | Branch _ | Poptrap _ | Pushtrap _ -> ()
    | Return _ -> Format.eprintf "ret"
    | Raise _ -> Format.eprintf "raise"
    | Stop -> Format.eprintf "stop"
    | Cond _ -> Format.eprintf "@[<hv 2>cond{@,"
    | Switch _ -> Format.eprintf "@[<hv 2>switch{@,");
  let loc = source_location st.ctx pc in
  let res =
    match last with
    | Return x ->
        let (_px, cx), queue = access_queue queue x in
        flush_all queue [ J.Return_statement (Some cx), loc ]
    | Raise (x, k) ->
        let (_px, cx), queue = access_queue queue x in
        flush_all queue (throw_statement st.ctx cx k loc)
    | Stop -> flush_all queue [ J.Return_statement None, loc ]
    | Branch cont -> compile_branch st queue cont handler backs frontier interm
    | Pushtrap _ -> assert false
    | Poptrap (cont, _) ->
        flush_all queue (compile_branch st [] cont None backs frontier interm)
    | Cond (x, c1, c2) ->
        let (_px, cx), queue = access_queue queue x in
        let b =
          compile_decision_tree
            st
            queue
            handler
            backs
            frontier
            interm
            succs
            loc
            cx
            (DTree.build_if c1 c2)
        in
        flush_all queue b
    | Switch (x, [||], a2) ->
        let (_px, cx), queue = access_queue queue x in
        let code =
          compile_decision_tree
            st
            queue
            handler
            backs
            frontier
            interm
            succs
            loc
            (J.EStructAccess (cx, 0))
            (DTree.build_switch a2)
        in
        flush_all queue code
    | Switch (x, a1, [||]) ->
        let (_px, cx), queue = access_queue queue x in
        let code =
          compile_decision_tree
            st
            queue
            handler
            backs
            frontier
            interm
            succs
            loc
            cx
            (DTree.build_switch a1)
        in
        flush_all queue code
    | Switch (x, a1, a2) ->
        (* The variable x is accessed several times,
           so we can directly refer to it *)
        (* We do not want to share the string "number".
           See comment for IsInt *)
        let b1 =
          compile_decision_tree
            st
            queue
            handler
            backs
            frontier
            interm
            succs
            loc
            (var x)
            (DTree.build_switch a1)
        in
        let b2 =
          compile_decision_tree
            st
            queue
            handler
            backs
            frontier
            interm
            succs
            loc
            (J.EStructAccess (var x, 0))
            (DTree.build_switch a2)
        in
        let code =
          Js_simpl.if_statement
            (is_int ~ctx:st.ctx (var x))
            loc
            (Js_simpl.block b1)
            false
            (Js_simpl.block b2)
            false
            (Config.Flag.simplify_ifdecl ())
        in
        flush_all queue code
  in
  (if debug ()
  then
    match last with
    | Branch _ | Poptrap _ | Pushtrap _ | Return _ | Raise _ | Stop -> ()
    | Switch _ | Cond _ -> Format.eprintf "}@]@ ");
  res

and compile_argument_passing ctx queue (pc, args) _backs continuation =
  if args = []
  then continuation queue
  else
    let block = Addr.Map.find pc ctx.Ctx.blocks in
    parallel_renaming block.params args continuation queue

and compile_exn_handling ctx queue (pc, args) handler continuation =
  if pc < 0
  then continuation queue
  else
    let block = Addr.Map.find pc ctx.Ctx.blocks in
    match block.handler with
    | None -> continuation queue
    | Some (x0, (h_pc, h_args)) ->
        let old_args =
          match handler with
          | Some (y, (old_pc, old_args)) ->
              assert (
                Var.compare x0 y = 0
                && old_pc = h_pc
                && List.length old_args = List.length h_args);
              old_args
          | None -> []
        in
        (* When an extra block is inserted during code generation,
           args is [] *)
        let m = Subst.build_mapping (if args = [] then [] else block.params) args in
        let h_block = Addr.Map.find h_pc ctx.Ctx.blocks in
        let rec loop continuation old args params queue =
          match args, params with
          | [], [] -> continuation queue
          | x :: args, y :: params ->
              let z, old =
                match old with
                | [] -> None, []
                | z :: old -> Some z, old
              in
              let x' = try Some (Var.Map.find x m) with Not_found -> Some x in
              if Var.compare x x0 = 0 || x' = z
              then loop continuation old args params queue
              else
                let (px, cx), queue = access_queue queue x in
                let st, queue =
                  (*FIX: we should flush only the variables we need rather than doing this;
                     do the same for closure free variables *)
                  match 2 (*ctx.Ctx.live.(Var.idx y)*) with
                  | 0 -> assert false
                  | 1 -> enqueue queue px y cx (source_location ctx pc) 1 []
                  | _ ->
                      flush_queue
                        queue
                        px
                        [ (let loc = source_location ctx pc in
                           J.Variable_statement [ Id.V y, Some (cx, loc) ], loc)
                        ]
                in
                st @ loop continuation old args params queue
          | _ -> assert false
        in
        loop continuation old_args h_args h_block.params queue

and index_of test li =
  let i = ref (-1) in
  List.iteri ~f:(fun i' e -> if test e then i := i') li;
  if !i = -1 then raise Not_found;
  !i

and compile_branch st queue ((pc, _) as cont) handler backs frontier interm =
  compile_argument_passing st.ctx queue cont backs (fun queue ->
      compile_exn_handling st.ctx queue cont handler (fun queue ->
          if Addr.Set.mem pc backs
          then (
            let label, index =
              match st.loop_stack with
              | [] -> assert false
              | (pc', _) :: rem ->
                  if pc = pc'
                  then None, 0
                  else
                    let lab, used = List.assoc pc rem in
                    let index = index_of (fun e -> fst e = pc) rem in
                    used := true;
                    Some lab, index
            in
            if debug ()
            then
              if label = None
              then Format.eprintf "continue;@ "
              else Format.eprintf "continue (%d);@ " pc;
            flush_all queue [ J.Continue_statement (label, Some index), Loc.N ])
          else if Addr.Set.mem pc frontier || Addr.Map.mem pc interm
          then (
            if debug () then Format.eprintf "(br %d)@ " pc;
            flush_all queue (compile_branch_selection pc interm))
          else compile_block st queue pc frontier interm))

and compile_branch_selection pc interm =
  try
    let pc, (x, i, default) = Addr.Map.find pc interm in
    if debug () then Format.eprintf "@ %a=%d;" Code.Var.print x i;
    let branch = compile_branch_selection pc interm in
    if default
    then branch
    else (J.Expression_statement (EBin (Eq, EVar (Loc.V x), int i)), Loc.N) :: branch
  with Not_found -> []

and compile_closure ctx (pc, args) =
  let st =
    { visited_blocks = Addr.Set.empty
    ; loops = Addr.Set.empty
    ; loop_stack = []
    ; all_succs = Hashtbl.create 17
    ; succs = Hashtbl.create 17
    ; backs = Hashtbl.create 17
    ; preds = Hashtbl.create 17
    ; interm_idx = -1
    ; ctx
    ; blocks = ctx.Ctx.blocks
    ; at_toplevel
    }
  in
  build_graph st pc Addr.Set.empty;
  let current_blocks = st.visited_blocks in
  st.visited_blocks <- Addr.Set.empty;
  if debug () then Format.eprintf "@[<hov 2>closure{@,";
  let res =
    compile_branch st [] (pc, args) None Addr.Set.empty Addr.Set.empty Addr.Map.empty
  in
  if Addr.Set.cardinal st.visited_blocks <> Addr.Set.cardinal current_blocks
  then (
    let missing = Addr.Set.diff current_blocks st.visited_blocks in
    Format.eprintf "Some blocks not compiled %s!@." (string_of_set missing);
    assert false);
  if debug () then Format.eprintf "}@]@ ";
  List.map res ~f:(fun (st, loc) -> J.Statement st, loc)

let generate_shared_value ctx =
  (* Applies must be generated before the strings/shared values because the
     applies might record a use of a primitive for caml_arity_check *)
  let applies =
    if not (Config.Flag.inline_callgen ())
    then
      let applies =
        List.map
          (IntMap.bindings ctx.Ctx.share.Share.vars.Share.applies)
          ~f:(fun (n, v) ->
            match generate_apply_fun ctx n with
            | J.EFun (_, param, body, nid) ->
                J.Function_declaration (v, param, body, nid), Loc.U
            | _ -> assert false)
      in
      applies
    else []
  in

  let strings =
    ( J.Statement
        (J.Variable_statement
          (List.map
              (StringMap.bindings ctx.Ctx.share.Share.vars.Share.language_requires)
              ~f:(fun (s, v) -> v, Some (ask_backend s, Loc.N))
           @ List.map
              (StringMap.bindings ctx.Ctx.share.Share.vars.Share.strings)
              ~f:(fun (s, v) -> v, Some (str_js s, Loc.N))
           @ List.map
               (StringMap.bindings ctx.Ctx.share.Share.vars.Share.prims)
               ~f:(fun (s, v) -> v, Some (runtime_fun ctx s, Loc.N))
           ))
    , Loc.U )
  in
  strings :: applies

let compile_program ctx pc =
  let res = compile_closure ctx (pc, []) in
  let res = generate_shared_value ctx @ res in
  if debug () then Format.eprintf "@.@.";
  res

let f (p : Code.program) ~exported_runtime ~live_vars debug =
  let t' = Timer.make () in
  let share = Share.get ~debug ~alias_prims:(exported_runtime != None) p in
  let exported_runtime =
    if exported_runtime then Some (Code.Var.fresh_n "runtime") else None
  in
  let ctx = Ctx.initial ~exported_runtime p.blocks live_vars share debug in
  let p = compile_program ctx p.start in
  if times () then Format.eprintf "  code gen.: %a@." Timer.print t';
  p, ctx.module_export_metadatas
