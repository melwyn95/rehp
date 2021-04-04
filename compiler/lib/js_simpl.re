/* Js_of_ocaml compiler
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
 */
open! Stdlib;
module J = Javascript;
// TODO: sync Rehp and Javascript variants
/* module J = Rehp; */

let rec enot_rec = e => {
  let (_, cost) as res =
    switch (e) {
    | [@implicit_arity] J.ESeq(e1, e2) =>
      let (e2', cost) = enot_rec(e2);
      ([@implicit_arity] J.ESeq(e1, e2'), cost);
    | [@implicit_arity] J.ECond(e1, e2, e3) =>
      let (e2', cost2) = enot_rec(e2);
      let (e3', cost3) = enot_rec(e3);
      ([@implicit_arity] J.ECond(e1, e2', e3'), cost2 + cost3);
    | [@implicit_arity] J.EBin(op, e1, e2) =>
      switch (op) {
      | J.Or =>
        let (e1', cost1) = enot_rec(e1);
        let (e2', cost2) = enot_rec(e2);
        ([@implicit_arity] J.EBin(J.And, e1', e2'), cost1 + cost2);
      | J.And =>
        let (e1', cost1) = enot_rec(e1);
        let (e2', cost2) = enot_rec(e2);
        ([@implicit_arity] J.EBin(J.Or, e1', e2'), cost1 + cost2);
      | J.EqEq => ([@implicit_arity] J.EBin(J.NotEq, e1, e2), 0)
      | J.NotEq => ([@implicit_arity] J.EBin(J.EqEq, e1, e2), 0)
      | J.EqEqEq => ([@implicit_arity] J.EBin(J.NotEqEq, e1, e2), 0)
      | J.NotEqEq => ([@implicit_arity] J.EBin(J.EqEqEq, e1, e2), 0)
      /* Disabled: this is not correct!
            {[ !(x < 0) ]} and {[ x >= 0 ]} give different result when x = nan
            {[
              | J.Lt ->
                  (J.EBin (J.Le, e2, e1), 0)
              | J.Le ->
                  (J.EBin (J.Lt, e2, e1), 0)
            ]}
         */
      | _ => ([@implicit_arity] J.EUn(J.Not, e), 1)
      }
    | [@implicit_arity] J.EUn(J.Not, e) => (e, 0)
    | [@implicit_arity]
      J.EUn(J.Neg | J.Pl | J.Typeof | J.Void | J.Delete | J.Bnot, _) => (
        [@implicit_arity] J.EUn(J.Not, e),
        0,
      )
    | J.EBool(b) => (J.EBool(!b), 0)
    | J.ECall(_)
    | J.EAccess(_)
    | J.EDot(_)
    | J.ENew(_)
    | J.EVar(_)
    | J.EFun(_)
    | J.EStr(_)
    | J.EArr(_)
    | J.ENum(_)
    | J.EObj(_)
    | J.EQuote(_)
    | J.ERegexp(_)
    | [@implicit_arity] J.EUn(J.IncrA | J.IncrB | J.DecrA | J.DecrB, _) => (
        [@implicit_arity] J.EUn(J.Not, e),
        1,
      )
    };

  if (cost <= 1) {
    res;
  } else {
    ([@implicit_arity] J.EUn(J.Not, e), 1);
  };
};

let enot = e => fst(enot_rec(e));

let unblock = st =>
  switch (st) {
  | (J.Block(l), _) => l
  | _ => [st]
  };

let block = l =>
  switch (l) {
  | [x] => x
  | l => (J.Block(l), J.N)
  };

exception Not_expression;

let rec expression_of_statement_list = l =>
  switch (l) {
  | [(J.Return_statement(Some(e)), _), ..._] => e
  | [(J.Expression_statement(e), _), ...rem] =>
    [@implicit_arity] J.ESeq(e, expression_of_statement_list(rem))
  | _ => raise(Not_expression)
  };

let expression_of_statement = st =>
  switch (fst(st)) {
  | J.Return_statement(Some(e)) => e
  | J.Block(l) => expression_of_statement_list(l)
  | _ => raise(Not_expression)
  };

exception Not_assignment;

let rec assignment_of_statement_list = l =>
  switch (l) {
  | [(J.Variable_statement([(x, Some(e))]), _)] => (x, e)
  | [(J.Expression_statement(e), _), ...rem] =>
    let (x, (e', nid)) = assignment_of_statement_list(rem);
    (x, ([@implicit_arity] J.ESeq(e, e'), nid));
  | _ => raise(Not_assignment)
  };

let assignment_of_statement = st =>
  switch (fst(st)) {
  | J.Variable_statement([(x, Some(e))]) => (x, e)
  | J.Block(l) => assignment_of_statement_list(l)
  | _ => raise(Not_assignment)
  };

let simplify_condition =
  fun
  | [@implicit_arity] J.ECond(e, J.ENum(one), J.ENum(zero))
      when J.Num.is_one(one) && J.Num.is_zero(zero) => e
  | [@implicit_arity] J.ECond(e, J.ENum(zero), J.ENum(one))
      when J.Num.is_one(one) && J.Num.is_zero(zero) =>
    [@implicit_arity] J.EUn(J.Not, e)
  | [@implicit_arity]
    J.ECond(
      [@implicit_arity] J.EBin(J.NotEqEq | J.NotEq, J.ENum(n), y),
      e1,
      e2,
    )
  | [@implicit_arity]
    J.ECond(
      [@implicit_arity] J.EBin(J.NotEqEq | J.NotEq, y, J.ENum(n)),
      e1,
      e2,
    ) =>
    [@implicit_arity]
    J.ECond([@implicit_arity] J.EBin(J.Band, y, J.ENum(n)), e1, e2)
  | cond => cond;

let rec if_statement_2 = (e, loc, iftrue, truestop, iffalse, falsestop) => {
  let e = simplify_condition(e);
  switch (fst(iftrue), fst(iffalse)) {
  /* Empty blocks */
  | (J.Block([]), J.Block([])) =>
    switch (e) {
    | J.EVar(_) => []
    | _ => [(J.Expression_statement(e), loc)]
    }
  | (J.Block([]), _) =>
    if_statement_2(enot(e), loc, iffalse, falsestop, iftrue, truestop)
  | (_, J.Block([])) => [
      ([@implicit_arity] J.If_statement(e, iftrue, None), loc),
    ]
  | _ =>
    /* Generates conditional */
    try (
      {
        let (x1, (e1, _)) = assignment_of_statement(iftrue);
        let (x2, (e2, _)) = assignment_of_statement(iffalse);
        if (Poly.(x1 != x2)) {
          raise(Not_assignment);
        };
        let exp =
          if (Poly.(e1 == e)) {
            [@implicit_arity] J.EBin(J.Or, e, e2);
          } else {
            [@implicit_arity] J.ECond(e, e1, e2);
          };
        [(J.Variable_statement([(x1, Some((exp, loc)))]), loc)];
      }
    ) {
    | Not_assignment =>
      try (
        {
          let e1 = expression_of_statement(iftrue);
          let e2 = expression_of_statement(iffalse);
          [
            (
              J.Return_statement(Some([@implicit_arity] J.ECond(e, e1, e2))),
              loc,
            ),
          ];
        }
      ) {
      | Not_expression =>
        if (truestop) {
          [
            ([@implicit_arity] J.If_statement(e, iftrue, None), loc),
            ...unblock(iffalse),
          ];
        } else if (falsestop) {
          [
            ([@implicit_arity] J.If_statement(enot(e), iffalse, None), loc),
            ...unblock(iftrue),
          ];
        } else {
          [
            (
              [@implicit_arity] J.If_statement(e, iftrue, Some(iffalse)),
              loc,
            ),
          ];
        }
      }
    }
  };
};

let unopt = b =>
  switch (b) {
  | Some(b) => b
  | None => (J.Block([]), J.N)
  };

let if_statement = (e, loc, iftrue, truestop, iffalse, falsestop) => {
  /*FIX: should be done at an earlier stage*/
  let e = simplify_condition(e);
  switch (iftrue, iffalse) {
  /* Shared statements */
  | (([@implicit_arity] J.If_statement(e', iftrue', iffalse'), loc), _)
      when Poly.(iffalse == unopt(iffalse')) =>
    if_statement_2(
      [@implicit_arity] J.EBin(J.And, e, e'),
      loc,
      iftrue',
      truestop,
      iffalse,
      falsestop,
    )
  | (([@implicit_arity] J.If_statement(e', iftrue', iffalse'), loc), _)
      when Poly.(iffalse == iftrue') =>
    if_statement_2(
      [@implicit_arity] J.EBin(J.And, e, [@implicit_arity] J.EUn(J.Not, e')),
      loc,
      unopt(iffalse'),
      truestop,
      iffalse,
      falsestop,
    )
  | (_, ([@implicit_arity] J.If_statement(e', iftrue', iffalse'), loc))
      when Poly.(iftrue == iftrue') =>
    if_statement_2(
      [@implicit_arity] J.EBin(J.Or, e, e'),
      loc,
      iftrue,
      truestop,
      unopt(iffalse'),
      falsestop,
    )
  | (_, ([@implicit_arity] J.If_statement(e', iftrue', iffalse'), loc))
      when Poly.(iftrue == unopt(iffalse')) =>
    if_statement_2(
      [@implicit_arity] J.EBin(J.Or, e, [@implicit_arity] J.EUn(J.Not, e')),
      loc,
      iftrue,
      truestop,
      iftrue',
      falsestop,
    )
  | _ => if_statement_2(e, loc, iftrue, truestop, iffalse, falsestop)
  };
};

let rec get_variable = acc =>
  fun
  | [@implicit_arity] J.ESeq(e1, e2)
  | [@implicit_arity] J.EBin(_, e1, e2)
  | [@implicit_arity] J.EAccess(e1, e2) =>
    get_variable(get_variable(acc, e1), e2)
  | [@implicit_arity] J.ECond(e1, e2, e3) =>
    get_variable(get_variable(get_variable(acc, e1), e2), e3)
  | [@implicit_arity] J.EUn(_, e1)
  | [@implicit_arity] J.EDot(e1, _)
  | [@implicit_arity] J.ENew(e1, None) => get_variable(acc, e1)
  | [@implicit_arity] J.ECall(e1, el, _)
  | [@implicit_arity] J.ENew(e1, Some(el)) =>
    [(e1, `Not_spread), ...el]
    |> List.map(~f=((a, _)) => a)
    |> List.fold_left(~init=acc, ~f=get_variable)
  | J.EVar(J.V(v)) => Code.Var.Set.add(v, acc)
  | J.EVar(J.S(_)) => acc
  | J.EFun(_)
  | J.EStr(_)
  | J.EBool(_)
  | J.ENum(_)
  | J.EQuote(_)
  | J.ERegexp(_) => acc
  | J.EArr(a) =>
    List.fold_left(
      ~f=
        (acc, i) =>
          switch (i) {
          | None => acc
          | Some(e1) => get_variable(acc, e1)
          },
      ~init=acc,
      a,
    )
  | J.EObj(l) =>
    List.fold_left(
      ~f=(acc, (_, e1)) => get_variable(acc, e1),
      ~init=acc,
      l,
    );
