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

/* Types for identifiers. */

open Stdlib;
type identifier = string;

type ident_string = {
  name: identifier,
  var: option(Code.Var.t),
  loc: Loc.t,
};

type t =
  | S(ident_string)
  | V(Code.Var.t);

let ident: (~loc: Loc.t=?, ~var: Code.Var.t=?, identifier) => t =
  (~loc=N, ~var=?, name) => S({name, var, loc});

type property_name =
  | PNI(identifier)
  | PNS(string)
  | PNN(float);

let property_name_to_JS = pn => switch (pn) {
  | PNI(i) => Javascript.PNI(i)
  | PNS(s) => Javascript.PNS(s)
  | PNN(n) => Javascript.PNN(Javascript.Num.of_float(n))
  }

let compare_ident = (t1, t2) =>
  switch (t1, t2) {
  | (V(v1), V(v2)) => Code.Var.compare(v1, v2)
  | (S({name: s1, var: v1, _}), S({name: s2, var: v2, _})) =>
    switch (String.compare(s1, s2)) {
    | 0 =>
      switch (v1, v2) {
      | (None, None) => 0
      | (None, _) => (-1)
      | (_, None) => 1
      | (Some(v1), Some(v2)) => Code.Var.compare(v1, v2)
      }
    | n => n
    }
  | (S(_), V(_)) => (-1)
  | (V(_), S(_)) => 1
  };

module IdentSet =
  Set.Make({
    type nonrec t = t;
    let compare = compare_ident;
  });
module IdentMap =
  Map.Make({
    type nonrec t = t;
    let compare = compare_ident;
  });
