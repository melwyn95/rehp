open Stdlib;

/**
 * All the code that "expands" high level Rehp into concrete Php.
 */
module Expand = {
  let seq = (e1, e2) => Php.ECond(Php.EBin(Or, e1, EBool(true)), e2, e2);
  /*
   * There is one instance of a hardcoded JS expression value in the jsoo
   * library, which needs to be special cased when converting to PHP.
   */
  let rawText = s =>
    String.compare(s, "(function (exn) { throw exn })") === 0
      ? "(function($exn) {throw $exn;})" : s;
};

let indent = {contents: 0};

type vars = {
  names: StringMap.t(int),
  vars: Code.Var.Map.t(int),
};

type continueKind =
  | NoContinue
  | ContinueWithLabel(list(string))
  | UnlabelledContinueInCase
  | ContinueWithLabelAndInCase(list(string));

type enclosedBy =
  | NoLoopOrSwitch
  | UnlabelledLoop
  | LabelledForLoop(string)
  | Switch;

type input = {
  vars,
  /*
   * "enclosedBy" represents whether the current AST is immediately enclosed
   * within a labelled loop, a regular loop, or a switch. For example:
   *
   *   function f() {
   *     x = 3; // "enclosedBy" is NoLoopOrSwitch when processing this statement
   *     a: for () {
   *       x = 3; // "enclosedBy" is LabelledForLoop("a")
   *       for () {
   *         x = 3; // "enclosedBy" is UnlabelledLoop
   *       }
   *       switch () {
   *         case 0:
   *           x = 3; // "enclosedBy" is Switch
   *         case 1:
   *           for () {
   *             x = 3; // "enclosedBy" is UnlabelledLoop
   *           }
   *       }
   *     }
   *   }
   */
  enclosedBy,
};

type output = {
  dec: vars,
  use: vars,
  /*
   * "freeLabels" tracks the labels used in continue statements that haven't
   * been enclosed with the corresponding loop yet.
   *
   * For example, "freeLabels" is ["a"] for this AST:
   *
   *   for () {
   *     continue a;
   *   }
   *
   * But it is [] (empty) here:
   *
   *   a: for () {
   *     continue a;
   *   }
   *
   * Continues without labels add "" (empty string) to the list. For example,
   * this AST has [""] for "freeLabels":
   *
   *   if () {
   *     continue;
   *   }
   *
   * And so does this one:
   *
   *   switch () {
   *     case 0:
   *       continue;
   *   }
   *
   * Functions and loops "shield" unlabelled continues. So this AST has [] for
   * "freeLabels":
   *
   *   switch () {
   *     case 0:
   *       break;
   *     case 1:
   *       return 3;
   *     case 2:
   *       for () {
   *         continue;
   *       }
   *     case 3:
   *       $f = function () {
   *         continue;
   *       };
   *   }
   */
  freeLabels: list(string),
};

let escapeIdent = s => Stdlib.escape(s, '$', "____");
let identStr = (~ref=false, name: string): string =>
  ref ? "&$" ++ (name: string) : "$" ++ (escapeIdent(name): string);
let ident = (~ref=false, _input, i): Id.t =>
  switch (i) {
  | Id.S({name: id, _}) =>
    Id.S({name: identStr(~ref, id), var: None, loc: N})
  | Id.V(_) => assert(false)
  };

let nullExprLoc =
  Some((Php.EVar(Id.S({name: "null", var: None, loc: Loc.N})), Loc.N));

exception Unsupported_statement;

let binopFromRehp = binop =>
  switch (binop) {
  | Rehp.Eq => Php.Eq
  | StarEq => StarEq
  | SlashEq => SlashEq
  | ModEq => ModEq
  | PlusEq => PlusEq
  | MinusEq => MinusEq
  | BandEq => BandEq
  | BxorEq => BandEq
  | BorEq => BorEq
  | Or => Or
  | And => And
  | Bor => Bor
  | Bxor => Bxor
  | Band => Band
  | EqEq => EqEq
  | NotEq => NotEq
  | FloatEqEq => EqEq
  | FloatNotEq => NotEq
  | EqEqEq => EqEqEq
  | NotEqEq => NotEqEq
  | Lt => Lt
  | Le => Le
  | Gt => Gt
  | Ge => Ge
  | FloatLt => Lt
  | FloatLe => Le
  | FloatGt => Gt
  | FloatGe => Ge
  | InstanceOf => InstanceOf
  | Lsl =>
    raise(
      Invalid_argument(
        "Should not be producing << (Lsl) when targeting PHP."
        ++ " The runtime file you supplied likely omitted left_shift_32",
      ),
    )
  /* Php doesn't have an equivalent - this should have been turned into calls
   * to unsigned_right_shift_32 by this point. */
  | Lsr =>
    raise(
      Invalid_argument(
        "Should not be producing >>> (Lsr) when targeting PHP."
        ++ " The runtime file you supplied likely omitted unsigned_right_shift_32",
      ),
    )
  | Asr =>
    raise(
      Invalid_argument(
        "Should not be producing >>> (Asr) when targeting PHP."
        ++ " The runtime file you supplied likely omitted right_shift_32",
      ),
    )
  | Plus => Plus
  | FloatPlus => FloatPlus
  | IntPlus => IntPlus
  | Minus => Minus
  | FloatMinus => Minus
  | Mul => Mul
  | FloatMul => Mul
  | Div => Div
  | FloatDiv => Div
  | Mod => Mod
  | FloatMod => Mod
  };

let emptyVars = {names: StringMap.empty, vars: Code.Var.Map.empty};
let emptyOutput = {dec: emptyVars, use: emptyVars, freeLabels: []};

let exists = (vars, id) =>
  switch (id) {
  | Id.S({name: s, _}) => StringMap.mem(s, vars.names)
  | V(v) => Code.Var.Map.mem(v, vars.vars)
  };

/*
 * It's become clear we're really using these as a set, not a map
 */
let addVar = (varsAndNames, v) =>
  if (exists(varsAndNames, v)) {
    varsAndNames;
  } else {
    switch (v) {
    | Id.S({name, _}) => {
        vars: varsAndNames.vars,
        names: StringMap.add(name, 1, varsAndNames.names),
      }
    | V(v) => {
        names: varsAndNames.names,
        vars: Code.Var.Map.add(v, 1, varsAndNames.vars),
      }
    };
  };

let mergeSum = (_k, count1, count2) =>
  switch (count1, count2) {
  | (None, None) => None /* Okay, what situation is this? */
  | (Some(_), None) => Some(1)
  | (None, Some(_)) => Some(1)
  | (Some(_), Some(_)) => Some(1)
  };

let isEmpty = (vars: vars) =>
  Code.Var.Map.is_empty(vars.vars) && StringMap.is_empty(vars.names);

let mergeVars = (vars: vars, next: vars): vars => {
  vars: Code.Var.Map.merge(mergeSum, vars.vars, next.vars),
  names: StringMap.merge(mergeSum, vars.names, next.names),
};

/*
 * TODO: Always remove zerod out values.
 */
let remove = (vars: vars, remove: vars): vars => {
  vars:
    Code.Var.Map.filter(
      (k, _) => !Code.Var.Map.mem(k, remove.vars),
      vars.vars,
    ),
  names:
    StringMap.filter((k, _) => !StringMap.mem(k, remove.names), vars.names),
};

let intersect = (vars: vars, intersectWith: vars): vars => {
  vars:
    Code.Var.Map.filter(
      (k, _) => Code.Var.Map.mem(k, intersectWith.vars),
      vars.vars,
    ),
  names:
    StringMap.filter(
      (k, _) => StringMap.mem(k, intersectWith.names),
      vars.names,
    ),
};

let exists = (vars: vars, id) =>
  switch (id) {
  | Id.S({name: s, _}) => StringMap.mem(s, vars.names)
  | V(v) => Code.Var.Map.mem(v, vars.vars)
  };

let mergeOutputs = (a, b) => {
  dec: mergeVars(a.dec, b.dec),
  use: mergeVars(a.use, b.use),
  freeLabels: List.concat([a.freeLabels, b.freeLabels]),
};

let mergeOutputList = lst =>
  List.fold_left(~f=mergeOutputs, ~init=emptyOutput, lst);

let createRef = ((name, _)) => {
  let newRef = Php.ENew(EVar(Id.S({name: "Ref", var: None, loc: N})), None);
  (
    Php.EVar(Id.S({name: identStr(name), var: None, loc: N})),
    Some((newRef, Loc.N)),
  );
};

let topLevelIdentifiersSt = (newVarsSoFar, st) =>
  switch (st) {
  /*
   * This doesn't handle the case where a variable is used *before* it is
   * defined somewhere. That's okay, we'll consider that to be invalid Rehp IR.
   */
  | Rehp.Variable_statement(l) =>
    let augmentEnv = (env, (id, _eopt)) => addVar(env, id);
    List.fold_left(~f=augmentEnv, ~init=newVarsSoFar, l);
  /*
   * TODO: Probably need to go one level deeper on the try body.
   */
  | Try_statement(_, _) => newVarsSoFar
  | _ => newVarsSoFar
  };

let topLevelIdentifiers = (newVarsSoFar: vars, (src, _)) =>
  switch (src) {
  | Rehp.Function_declaration((id, _, _, _)) => addVar(newVarsSoFar, id)
  | Statement(stmt) => topLevelIdentifiersSt(newVarsSoFar, stmt)
  };

/* No inputs to each stage, and no other output but the computed AST */
let optOutput = (f, x) =>
  switch (x) {
  | None => (emptyOutput, None)
  | Some(data) =>
    let (output, mapped) = f(data);
    (output, Some(mapped));
  };

/*
 * This is a special label that indicates a continue occurred inside a switch.
 * We need this label because "continue" in switches in Hack/PHP mean the
 * same as break. To have the same behavior as JS, we use the continue_label
 * and check it after the switch.
 *
 * For example, this JS:
 *
 *   for () {
 *     switch() {
 *       case 0:
 *         continue
 *     }
 *   }
 *
 * Becomes this Hack/PHP:
 *
 *   for () {
 *     switch() {
 *       case 0:
 *         continue_label = "#";
 *         break;
 *     }
 *     if (continue_label === "#") {
 *       continue;
 *     }
 *   }
 */
let switchLabel = "#";
let continueLabel = Php.EVar(Id.ident("$continue_label"));

let breakIfNonnullContinueLabel = {
  let compareContinueLabelToNull = Php.EBin(NotEqEq, continueLabel, ENULL);
  (
    Php.If_statement(
      compareContinueLabelToNull,
      (Php.Block([(Break_statement, Loc.N)]), Loc.N),
      None,
      false,
    ),
    Loc.N,
  );
};

let continueIfContinueLabelEquals = (~alternate=?, label) => {
  let compareContinueToParentLoop =
    Php.EBin(EqEqEq, continueLabel, EStr(label, `Utf8));
  (
    Php.If_statement(
      compareContinueToParentLoop,
      (Php.Block([(Continue_statement, Loc.N)]), Loc.N),
      alternate,
      true,
    ),
    Loc.N,
  );
};

let setContinueLabel = label => (
  Php.Variable_statement([
    (
      continueLabel,
      Some((
        switch (label) {
        | None => ENULL
        | Some(label) => Php.EStr(label, `Utf8)
        },
        Loc.N,
      )),
    ),
  ]),
  Loc.N,
);

let wrapInStruct = lst => Php.EStruct(lst);

let rec expression = (input, x) =>
  switch (x) {
  | Rehp.ESeq(e1, e2) =>
    let (e1Out, e1Mapped) = expression(input, e1);
    let (e2Out, e2Mapped) = expression(input, e2);
    let joined = mergeOutputs(e1Out, e2Out);
    (joined, Expand.seq(e1Mapped, e2Mapped));
  | Rehp.ERaw(segments) =>
    let (output, mapped) =
      List.fold_left(
        segments, ~init=(emptyOutput, []), ~f=((output, curMapped), segment) =>
        switch (segment) {
        | Rehp.RawText(s) => (
            output,
            [Php.RawText(Expand.rawText(s)), ...curMapped],
          )
        | Rehp.RawSubstitution(e) =>
          let (nextOutput, nextMapped) = expression(input, e);
          (
            mergeOutputs(nextOutput, output),
            [Php.RawSubstitution(nextMapped), ...curMapped],
          );
        }
      );
    (output, Php.ERaw(List.rev(mapped)));
  | Rehp.ECond(e1, e2, e3) =>
    let (e1Out, e1Mapped) = expression(input, e1);
    let (e2Out, e2Mapped) = expression(input, e2);
    let (e3Out, e3Mapped) = expression(input, e3);
    let joined = mergeOutputList([e1Out, e2Out, e3Out]);
    (joined, Php.ECond(e1Mapped, e2Mapped, e3Mapped));
  | Rehp.EBin(b, e1, e2) =>
    let (e1Out, e1Mapped) = expression(input, e1);
    let (e2Out, e2Mapped) = expression(input, e2);
    let joined = mergeOutputs(e1Out, e2Out);
    (joined, Php.EBin(binopFromRehp(b), e1Mapped, e2Mapped));
  | Rehp.EUn(b, e1) => unopFromRehp(input, b, e1)
  | Rehp.ECall(e1, e2, loc) =>
    let (e1Out, e1Mapped) = expression(input, e1);
    let (e2Outs, e2_mappeds) =
      List.split(List.map(~f=expression(input), e2));
    (
      mergeOutputList([e1Out, ...e2Outs]),
      Php.ECall(e1Mapped, e2_mappeds, loc),
    );
  | Rehp.ECopy(e1, loc) =>
    let (e1Out, e1Mapped) = expression(input, e1);
    (e1Out, Php.ECall(Php.EDot(e1Mapped, "toVector"), [], loc));
  | Rehp.EAccess(e1, e2) =>
    let (e1Out, e1Mapped) = expression(input, e1);
    let (e2Out, e2Mapped) = expression(input, e2);
    (mergeOutputs(e1Out, e2Out), Php.EAccess(e1Mapped, e2Mapped));
  | Rehp.EStructAccess(e, index) =>
    let (eOutput, eMapped) = expression(input, e);
    (eOutput, Php.EStructAccess(eMapped, Php.EInt(index)));
  | Rehp.EArrAccess(e1, e2) =>
    let (e1Out, e1Mapped) = expression(input, e1);
    let (e2Out, e2Mapped) = expression(input, e2);
    let joined = mergeOutputs(e1Out, e2Out);
    (joined, Php.EArrAccess(e1Mapped, e2Mapped));
  | Rehp.EDot(e1, id) =>
    let (e1Out, e1Mapped) = expression(input, e1);
    (e1Out, EDot(e1Mapped, id));
  | Rehp.ENew(e1, Some(args)) =>
    let (e1Out, e1Mapped) = expression(input, e1);
    let (argsOuts, argsMappeds) =
      List.split(List.map(~f=expression(input), args));
    (
      mergeOutputList([e1Out, ...argsOuts]),
      ENew(e1Mapped, Some(argsMappeds)),
    );
  | Rehp.ENew(e1, None) =>
    let (e1Out, e1Mapped) = expression(input, e1);
    (e1Out, Php.ENew(e1Mapped, None));
  | Rehp.EVar(Id.S({name: "null", _})) => (emptyOutput, Php.ENULL)
  /* Undefined is NULL */
  /* TODO: Come up with suitable alternative for PHP */
  | Rehp.EVar(Id.S({name: "undefined", _})) => (emptyOutput, Php.ENULL)
  | Rehp.EVar(v) =>
    let output = {...emptyOutput, use: addVar(emptyVars, v)};
    (
      output,
      if (exists(input.vars, v)) {
        EVar(ident(input, v));
      } else {
        EDot(EVar(ident(input, v)), "contents");
      },
    );
  | Rehp.EFun((idopt, params, body, nid)) =>
    /* New vars scoped to the body of function */
    let paramVars = List.fold_left(~f=addVar, ~init=emptyVars, params);
    /*
     * Rehp intermediate representation assumes that EFun's identifier (which
     * is almost always omitted in practice) may only be available to the
     * function body's scope - not containing scope. There isn't a
     * straightforward way to represent that in Php. It should be delegated to
     * stubs, and we can remove the ability for Efun to have identifiiers.
     */
    let augmentedEnv = mergeVars(input.vars, paramVars);
    let output = {...emptyOutput, dec: paramVars};
    let (bodyOutput, bodyMap) =
      sources(
        output,
        {vars: augmentedEnv, enclosedBy: NoLoopOrSwitch},
        body,
      );
    /* Rehp models an IR with "function scope" for variables. */
    /* Declarations reset at function boundaries. */
    let bodyUsesFromOutside = remove(bodyOutput.use, paramVars);
    let bodyUsesFromOutside = remove(bodyUsesFromOutside, bodyOutput.dec);
    let output = {...emptyOutput, use: bodyUsesFromOutside};
    let paramIdentList = List.map(~f=ident(input), params);

    let bodyUsesFromOutsideIdents =
      List.map(
        ~f=((k, _)) => Id.ident(identStr(~ref=false, k)),
        StringMap.bindings(bodyUsesFromOutside.names),
      );

    (
      output,
      ELam((
        idopt,
        paramIdentList,
        bodyMap,
        [],
        bodyUsesFromOutsideIdents,
        nid,
      )),
    );

  | Rehp.EVectlength(e) =>
    let (eOutput, eMapped) = expression(input, e);
    let eMapped = Php.(EBin(Minus, EDot(eMapped, "count()"), EInt(1)));
    (eOutput, eMapped);
  | Rehp.EArrLen(e) =>
    let (eOutput, eMapped) = expression(input, e);
    (eOutput, EArrLen(eMapped));
  | Rehp.EStruct(l) => foldExpressions(emptyOutput, input, [], l)
  | Rehp.ETag(i, l) =>
    let (outs, mappeds) = List.split(List.map(~f=expression(input), l));
    (mergeOutputList(outs), Php.ETag(Php.EInt(i), mappeds));
  /* Should have already converted EArr to functions. */
  | Rehp.EArr(l) => (
      emptyOutput,
      Php.EArr(
        List.map(
          ~f=
            elem =>
              switch (elem) {
              | Some(elem) => Some(snd(expression(input, elem)))
              | None => None
              },
          l,
        ),
      ),
    )
  | Rehp.EArityTest(_) => assert(false)
  | Rehp.EObj(l) =>
    let (outs, mappeds) =
      List.split(
        List.map(
          ~f=
            ((i, e)) => {
              let (output, mapped) = expression(input, e);
              (output, (i, mapped));
            },
          l,
        ),
      );
    (mergeOutputList(outs), Php.EObj(mappeds));
  | Rehp.EStr(x, y) => (emptyOutput, Php.EStr(x, y))
  | Rehp.EBool(b) => (emptyOutput, Php.EBool(b))
  | Rehp.EFloat(n) => (emptyOutput, Php.ENum(n))
  | Rehp.EInt(n) => (emptyOutput, Php.EInt(n))
  | Rehp.EQuote(s) => (emptyOutput, Php.EQuote(s))
  | Rehp.ERegexp(x, y) => (emptyOutput, Php.ERegexp(x, y))
  | Rehp.ECustomRequire(_) => failwith("ECustomRequire not supported for PHP")
  | Rehp.ECustomRegister(_) =>
    failwith("ECustonRegister not supported for PHP")
  | Rehp.ERequire(_) => failwith("ERequire not supported for PHP")
  | Rehp.ERuntime => failwith("ERuntime not supported for PHP")
  }

and foldExpressions = (output, input, revMapped, remain) =>
  switch (remain) {
  | [] => (output, wrapInStruct(List.rev(revMapped)))
  | [hd, ...tl] =>
    let (thisOutput, mapped) = expression(input, hd);
    foldExpressions(
      mergeOutputs(output, thisOutput),
      input,
      [mapped, ...revMapped],
      tl,
    );
  }

/*
 * For a given Rehp unary operator, and an expression that has already been
 * converted into Php, turn the unary Rehp operation into Php.
 */
and unopFromRehp = (input, unop, rehpExpr) =>
  switch (unop) {
  | Rehp.Not =>
    let (outMapped, exprMapped) = expression(input, rehpExpr);
    (outMapped, Php.EUn(Php.Not, exprMapped));
  | ToBool
  | FloatToInt
  | ToInt =>
    let (outMapped, exprMapped) = expression(input, rehpExpr);
    (outMapped, EUn(ToInt, exprMapped));
  | IntToString =>
    let (outMapped, exprMapped) = expression(input, rehpExpr);
    (outMapped, EUn(IntToString, exprMapped));
  | Neg
  | FloatNeg =>
    let (outMapped, exprMapped) = expression(input, rehpExpr);
    (outMapped, EUn(Neg, exprMapped));
  | Typeof =>
    let (outMapped, exprMapped) = expression(input, rehpExpr);
    (outMapped, EUn(Typeof, exprMapped));
  /* Should have already been converted inIdentifier_renaming_php. */
  | IsInt =>
    raise(
      Invalid_argument(
        "Should not be producing IsInt when targeting PHP."
        ++ " The runtime file you supplied likely omitted is_int",
      ),
    )
  /* Only for stubs */
  | Void =>
    let (outMapped, exprMapped) = expression(input, rehpExpr);
    (outMapped, EUn(Void, exprMapped));
  | Delete =>
    let (outMapped, exprMapped) = expression(input, rehpExpr);
    (outMapped, EUn(Delete, exprMapped));
  | Bnot =>
    let (outMapped, exprMapped) = expression(input, rehpExpr);
    (outMapped, EBin(Minus, EInt(1), exprMapped));
  | IncrA =>
    let (outMapped, exprMapped) = expression(input, rehpExpr);
    (outMapped, EUn(IncrA, exprMapped));
  | DecrA =>
    let (outMapped, exprMapped) = expression(input, rehpExpr);
    (outMapped, EUn(DecrA, exprMapped));
  | IncrB =>
    let (outMapped, exprMapped) = expression(input, rehpExpr);
    (outMapped, EUn(IncrB, exprMapped));
  | DecrB =>
    let (outMapped, exprMapped) = expression(input, rehpExpr);
    (outMapped, EUn(DecrB, exprMapped));
  }

and forStatement = (output, input, (s, loc), label) => {
  let nextInput = {
    vars: input.vars,
    enclosedBy:
      switch (label) {
      | None => UnlabelledLoop
      | Some(label) => LabelledForLoop(label)
      },
  };
  let (nextOutput, mapped) = statement(output, nextInput, s);

  /* Always wrapped for loop contents in a block */
  /* Reset the continueLabel whenever entering a labelled loop */
  let (mapped, myLabel) =
    switch (mapped, label) {
    | (Php.Block(s), None) => (mapped, "")
    | (Php.Block(s), Some(myLabel)) => (
        Php.Block([setContinueLabel(None), ...s]),
        myLabel,
      )
    | (_, None) => (Php.Block([(mapped, loc)]), "")
    | (_, Some(myLabel)) => (
        Php.Block([setContinueLabel(None), (mapped, loc)]),
        myLabel,
      )
    };
  let forStatementNode = (
    Php.For_statement(Left(None), None, None, (mapped, loc)),
    loc,
  );

  /* shield any continues that have this loop's label */
  let freeLabels =
    List.filter(label => label != myLabel, nextOutput.freeLabels);

  let li =
    switch (input.enclosedBy, List.length(freeLabels) > 0) {
    /* if the loop does not contain a labelled continue,
       or isn't inside any labelled loop,
       then just output the loop */
    | (_, false)
    | (NoLoopOrSwitch, true) => [forStatementNode]

    /* if the loop contains a labelled continue,
       and is inside an unlabelled loop,
       then check the continueLabel to break to the outter loop */
    | (UnlabelledLoop, true) => [
        forStatementNode,
        breakIfNonnullContinueLabel,
      ]

    /* if the loop contains a labelled continue,
       and is inside an switch statement
       then check the continueLabel to break out of the switch */
    | (Switch, true) => [forStatementNode, breakIfNonnullContinueLabel]

    /* if the loop contains a labelled continue,
       and is inside a labelled loop,
       then check if the labels match to continue,
       and also check the continueLabel to break to the outter loop */
    | (LabelledForLoop(parentLabel), true) =>
      let onlyContinueToParentLabel =
        List.for_all(label => label == parentLabel, freeLabels);

      /* if the unshielded continues inside the loop only continue
         to the enclosing loop, then we don't need to break */
      let checkStatement =
        onlyContinueToParentLabel
          ? continueIfContinueLabelEquals(parentLabel)
          : continueIfContinueLabelEquals(
              ~alternate=breakIfNonnullContinueLabel,
              parentLabel,
            );
      [forStatementNode, checkStatement];
    };

  ({...nextOutput, freeLabels}, Php.Statement_list(li));
}

and switchStatement = (output, input, e, l, def) => {
  let nextInput = {vars: input.vars, enclosedBy: Switch};
  let (eOutput, eMapped) = expression(nextInput, e);
  /* Backward-compatible behavior, TODO: cleanup PHP Ast and printing */
  let defo =
    switch (def) {
    | [] => None
    | _ => Some(def)
    };
  let (dOutput, dMapped) = optOutput(statements(output, nextInput), defo);
  let forEach = ((e, s)) => {
    let (eOutput, eMapped) = expression(nextInput, e);
    let (stmOutput, stmMapped) = statements(output, nextInput, s);
    let outs = mergeOutputs(eOutput, stmOutput);
    (outs, (eMapped, stmMapped));
  };
  let (lOutput, lMapped) = List.split(List.map(~f=forEach, l));
  let outs = mergeOutputList([eOutput, dOutput, ...lOutput]);
  let switchNode = (
    Php.Switch_statement(eMapped, lMapped, dMapped, []),
    Loc.N,
  );

  let labels = List.filter(label => label != "", outs.freeLabels);
  let continueKind =
    if (List.length(outs.freeLabels) === 0) {
      NoContinue;
    } else if (List.length(outs.freeLabels) === List.length(labels)) {
      ContinueWithLabel(labels);
    } else if (List.length(labels) === 0) {
      UnlabelledContinueInCase;
    } else {
      ContinueWithLabelAndInCase(labels);
    };

  let li =
    switch (input.enclosedBy, continueKind) {
    /* if the switch does not contain a continue,
       or isn't inside any labelled structure,
       then just output the switch */
    | (_, NoContinue)
    | (NoLoopOrSwitch, _) => [switchNode]

    /* check for a non-null label, to break further out */
    | (UnlabelledLoop, ContinueWithLabel(_))
    | (Switch, UnlabelledContinueInCase)
    | (Switch, ContinueWithLabel(_))
    | (Switch, ContinueWithLabelAndInCase(_)) => [
        switchNode,
        breakIfNonnullContinueLabel,
      ]

    /* 1. reset the continue label before entering the switch, and
       2. check for the special "switch" label, to continue inside the loop */
    | (UnlabelledLoop, UnlabelledContinueInCase)
    | (LabelledForLoop(_), UnlabelledContinueInCase) => [
        setContinueLabel(None),
        switchNode,
        continueIfContinueLabelEquals(switchLabel),
      ]

    /* 1. reset the continue label before entering the switch, and
       2. check for the special "switch" label, to continue inside the loop, and
       3. check for a non-null label, to break further out */
    | (UnlabelledLoop, ContinueWithLabelAndInCase(_)) => [
        setContinueLabel(None),
        switchNode,
        continueIfContinueLabelEquals(
          ~alternate=breakIfNonnullContinueLabel,
          switchLabel,
        ),
      ]

    /* 1. reset the continue label before entering the switch, and
       2. check the special "switch" label, to continue inside the loop, and
       3. check the current loop's label, to continue inside the loop, and
       4. check for a non-null label, to break further out */
    | (
        LabelledForLoop(parentLabel),
        ContinueWithLabel(labels) | ContinueWithLabelAndInCase(labels),
      ) =>
      let onlyContinueToParentLabel =
        List.for_all(label => label == parentLabel, labels);

      /* if the unshielded continues inside the switch only continue
         to the enclosing loop, then we don't need to break */
      let checkStatement =
        onlyContinueToParentLabel
          ? continueIfContinueLabelEquals(parentLabel)
          : continueIfContinueLabelEquals(
              ~alternate=breakIfNonnullContinueLabel,
              parentLabel,
            );
      [
        setContinueLabel(None),
        switchNode,
        continueIfContinueLabelEquals(~alternate=checkStatement, switchLabel),
      ];
    };
  (outs, Php.Statement_list(li));
}

and foldVars =
    (
      output,
      input,
      revMapped: list((Php.expression, option((Php.expression, Loc.t)))),
      remain,
    )
    : (output, list((Php.expression, option((Php.expression, Loc.t))))) =>
  switch (remain) {
  | [] => (output, List.rev(revMapped))
  | [(id, rhs), ...tl] =>
    let identMapped = ident(input, id);
    /* print_string( */
    /*   String.make(indent.contents, ' ') ++ "var declare " ++ str(id), */
    /* ); */
    /* print_newline(); */
    let (nextOutput, input, mapped) =
      switch (rhs) {
      | None =>
        if (exists(output.use, id)) {
          let mapped = (
            Php.EVar(identMapped),
            Some((Php.EVar(identMapped), Loc.N)),
          );
          (output, input, mapped);
        } else {
          let nextOutput = {...output, dec: addVar(output.dec, id)};
          let input = {...input, vars: addVar(input.vars, id)};
          let mapped = (Php.EVar(identMapped), None);
          (nextOutput, input, mapped);
        }
      | Some((rhs, loc)) =>
        let (rhsOutput, rhsMapped) = expression(input, rhs);
        /* TODO: Add a !exists(output.dec) to fix the validFloatLexem case */
        if (!exists(output.dec, id)
            && (exists(output.use, id) || exists(rhsOutput.use, id))) {
          let nextOutput = mergeOutputs(output, rhsOutput);
          let dummy = (
            Php.EDot(EVar(identMapped), "contents"),
            Some((rhsMapped, loc)),
          );
          (nextOutput, input, dummy);
        } else {
          let nextOutput = mergeOutputs(output, rhsOutput);
          let nextOutput = {...nextOutput, dec: addVar(nextOutput.dec, id)};
          let input = {...input, vars: addVar(input.vars, id)};
          let mapped = (Php.EVar(identMapped), Some((rhsMapped, loc)));
          (nextOutput, input, mapped);
        };
      };
    foldVars(nextOutput, input, [mapped, ...revMapped], tl);
  }

and nullifyInitializers = (revSoFar, lst) =>
  switch (lst) {
  | [] => Some(List.rev_map(~f=((e, none)) => (e, nullExprLoc), revSoFar))
  | [(_, None) as hd, ...tl] => nullifyInitializers([hd, ...revSoFar], tl)
  | [(_, Some(_)), ...tl] => None
  }

and statement = (output, input, x) => {
  let (nextOutput, mapped) =
    switch (x) {
    | Rehp.Block(b) =>
      let (nextOutput, mappedStatements) = statements(output, input, b);
      (nextOutput, Php.Block(mappedStatements));
    | Rehp.Variable_statement(l) =>
      /* print_string(String.make(indent.contents, ' ') ++ "<vars>"); */
      indent.contents = indent.contents + 2;
      /* print_newline(); */
      let (nextOuput, mappedResults) = foldVars(output, input, [], l);
      let mappedResults =
        switch (nullifyInitializers([], mappedResults)) {
        | None => mappedResults
        | Some(nulled) => nulled
        };
      let ret = (nextOuput, Php.Variable_statement(mappedResults));
      indent.contents = indent.contents - 2;
      /* print_string(String.make(indent.contents, ' ') ++ "</vars>"); */
      /* let returningUsed = */
      /*   List.map(((k, v)) => k, Util.StringMap.bindings(out.use.names)); */
      /* print_newline(); */
      /* print_string( */
      /*   String.make(indent.contents, ' ') */
      /*   ++ " -- " */
      /*   ++ " returning used:!" */
      /*   ++ String.concat(",", returningUsed), */
      /* ); */
      /* print_newline(); */
      ret;
    | Rehp.Empty_statement => (emptyOutput, Php.Empty_statement)
    | Rehp.Debugger_statement => (emptyOutput, Php.Debugger_statement)
    | Rehp.Expression_statement(e) =>
      let (eOutput, mapped) = expression(input, e);
      (eOutput, Php.Expression_statement(mapped));
    | Rehp.If_statement(e, s, sopt) =>
      let statementLocation = ((stmt: Rehp.statement, loc: Loc.t)) => {
        let (nextOutput, mapped) = statement(output, input, stmt);
        (nextOutput, (mapped, loc));
      };
      let (exprOutput, exprMapped) = expression(input, e);
      let (ifOutput, ifMapped) = statementLocation(s);
      let (soptOutput, soptMapped) = optOutput(statementLocation, sopt);
      let nextOutput = mergeOutputList([exprOutput, ifOutput, soptOutput]);
      (nextOutput, If_statement(exprMapped, ifMapped, soptMapped, false));
    | Rehp.Loop_statement(s, loc) =>
      forStatement(output, input, (s, loc), None)
    | Rehp.Continue_statement(s, _) =>
      let (label, li) =
        switch (s, input.enclosedBy) {
        /* if the continue isn't labeled, and occurs inside a switch,
           switch/cases can't have continues, so convert it to a break
           and set continueLabel to a special "switch" label */
        | (None, Switch) => (
            "",
            [
              setContinueLabel(Some(switchLabel)),
              (Php.Break_statement, Loc.N),
            ],
          )
        /* if the continue isn't labeled, and isn't in a switch,
           keep it as a continue*/
        | (None, _) => ("", [(Php.Continue_statement, Loc.N)])
        /* if the continue has a label, then convert it to a break and set
           continueLabel to its label */
        | (Some(lbl), _) =>
          let label = Javascript.Label.to_string(lbl);
          (
            label,
            [setContinueLabel(Some(label)), (Php.Break_statement, Loc.N)],
          );
        };
      ({...emptyOutput, freeLabels: [label]}, Php.Statement_list(li));
    /* TODO: remove labels from Rehp break statements */
    | Rehp.Break_statement(_s) => (emptyOutput, Break_statement)
    | Rehp.Return_statement(e) =>
      let (eOutput, eMapped) = optOutput(expression(input), e);
      (eOutput, Return_statement(eMapped));
    | Rehp.Labelled_statement(lbl, (Rehp.Loop_statement(s, loc), _loc2)) =>
      forStatement(
        output,
        input,
        (s, loc),
        Some(Javascript.Label.to_string(lbl)),
      )
    | Rehp.Labelled_statement(_) =>
      /* Only For_statements can be labelled */
      /* TODO: remove labelled statements from Rehp, replace with a flag in For_statements */
      raise(Unsupported_statement)
    | Rehp.Throw_statement(e) =>
      let (eOutput, eMapped) = expression(input, e);
      (eOutput, Throw_statement(eMapped));
    | Rehp.Switch_statement(e, l, def) =>
      switchStatement(output, input, e, l, def)
    /*
     * TODO: For Php, the exception is not actually block scoped and so we don't
     * need to do any special handling here.
     */
    | Rehp.Try_statement(b, catch) =>
      /*
       * Customization that augments the scope with catch identifier.
       */
      let identAndStatements = ((idnt, st)) => {
        let identMapped = ident(input, idnt);
        let addedVars = addVar(emptyVars, idnt);
        let augmentedInput = {
          vars: mergeVars(input.vars, addedVars),
          enclosedBy: input.enclosedBy,
        };
        let (stOutput, stMapped) = statements(output, augmentedInput, st);
        let stUses = remove(stOutput.use, addedVars);
        let nextOutput = {...stOutput, use: stUses};
        (nextOutput, (identMapped, stMapped));
      };
      let (bOutput, bMapped) = statements(output, input, b);
      let (catchOutput, catchMapped) = identAndStatements(catch);
      (
        mergeOutputs(bOutput, catchOutput),
        Try_statement(bMapped, Some(catchMapped), None),
      );
    };
  (mergeOutputs(output, nextOutput), mapped);
}

and foldStatements = (origOutput, output, input, revMapped, remain) =>
  switch (remain) {
  | [] => (output, List.rev(revMapped))
  | [(s, loc), ...tl] =>
    let (thisOutput, thisMapped) =
      statement({...output, freeLabels: origOutput.freeLabels}, input, s);
    let nextOutput = mergeOutputs(output, thisOutput);
    let nextInput = {
      vars: mergeVars(input.vars, nextOutput.dec),
      enclosedBy: input.enclosedBy,
    };

    foldStatements(
      origOutput,
      nextOutput,
      nextInput,
      [(thisMapped, loc), ...revMapped],
      tl,
    );
  }

and statements = (output, input, l) => {
  /* print_string(String.make(indent.contents, ' ') ++ "<statements>"); */
  /* print_newline(); */
  indent.contents = indent.contents + 2;
  let ret = foldStatements(output, output, input, [], l);
  indent.contents = indent.contents - 2;
  /* print_string(String.make(indent.contents, ' ') ++ "</statements>"); */
  /* print_newline(); */
  ret;
}

/* TODO: The free vars should also be mapped over. But if you wait to add
   them until the end, that isn't required. */
and source = (output, input, x) =>
  switch (x) {
  /*
   * TODO: For now, this should be converted to a Rehp.EFun, since that is what
   * a Rehp.Function_declaration actually models. A Php.Function_declaration is
   * a top level global namespaced function with very different semantics, and
   * no ability to "use".  For now Php.Function_declaration retains the ability
   * to use as it was a fork of Rehp.
   */
  | Rehp.Function_declaration((id, params, body, nid)) =>
    let lam = Rehp.EFun((None, params, body, nid));
    let asVar = Rehp.Variable_statement([(id, Some((lam, nid)))]);
    /* print_string("Function_declaration " ++ str(id)); */
    /* print_newline(); */
    source(output, input, Rehp.Statement(asVar));

  | Statement(s) =>
    let (nextOutput, mapped) = statement(output, input, s);
    (nextOutput, Php.Statement(mapped));
  }

and foldSources = (output, input, revMapped, remain) =>
  switch (remain) {
  | [] => (output, List.rev(revMapped))
  | [(s, loc), ...tl] =>
    let (thisOutput, thisMapped) = source(output, input, s);
    let nextOutput = mergeOutputs(output, thisOutput);
    let nextInput = {
      vars: mergeVars(input.vars, nextOutput.dec),
      enclosedBy: NoLoopOrSwitch,
    };
    foldSources(
      nextOutput,
      nextInput,
      [(thisMapped, loc), ...revMapped],
      tl,
    );
  }

/*
 * Ensures that all the declared variables in a function body contributes to
 * the `input` lexical scope of the next binding. This satisfies very linearly
 * declared environments, but is not sufficient for mutually recursive
 * bindings.
 *
 * To support mutually recursive bindings, we will first do a single shallow
 * analysis of all the variable bindings and functions declared.  For each one,
 * we bump their declaration-scope counts. We need to be careful not to bump
 * them twice though (actually that might not matter for input).
 */
and sources = (output, input, x) => {
  /* print_string ("SOURCES"); */
  /* print_newline (); */
  let topLevelIdents =
    List.fold_left(~f=topLevelIdentifiers, ~init=input.vars, x);
  let (nextOutput, mappeds) = foldSources(output, input, [], x);
  let toHoist =
    remove(
      remove(intersect(nextOutput.use, topLevelIdents), nextOutput.dec),
      input.vars,
    );
  if (isEmpty(toHoist)) {
    (
      /* print_string ("/SOURCES"); */
      /* print_newline (); */
      nextOutput,
      mappeds,
    );
  } else {
    let identsAndInits = StringMap.bindings(toHoist.names);
    let refs = List.map(~f=createRef, identsAndInits);
    let refDecls = Php.Statement(Variable_statement(refs));
    /*
     * EFun will remove the used variables that are in dec.
     */
    let nextOutput = {
      ...nextOutput,
      dec: mergeVars(nextOutput.dec, toHoist),
    };
    /* print_string ("/SOURCES"); */
    /* print_newline (); */
    (nextOutput, [(refDecls, Loc.N), ...mappeds]);
  };
}

and program = (input, x) => {
  /* print_string("PROGRAM"); */
  /* print_newline(); */
  let ret = sources(emptyOutput, input, x);
  /* print_string("/PROGRAM"); */
  /* print_newline(); */
  ret;
};
