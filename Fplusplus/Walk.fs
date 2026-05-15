module Fw.Walk

open FSharp.Compiler.Symbols

type Ty = FSharpType
type Mfv = FSharpMemberOrFunctionOrValue
module P = FSharpExprPatterns

// type E = E of E<E>

type E =
  | AddressOf of lvalue: E
  | AddressSet of lvalue: E * rvalue: E
  | Application of f: E * tys: Ty list * args: E list
  | Call of
    o: E option *
    mfv: Mfv *
    tyArgs1: Ty list *
    tyArgs2: Ty list *
    args: E list
  | Coerce of target: Ty * e: E
  | FastIntegerForLoop of
    start: E *
    limit: E *
    consume: E *
    isUp: bool
  | ILAsm of asm: string * typeArgs: Ty list * args: E list
  | ILFieldGet of o: E option * Ty * fieldName: string
  | ILFieldSet of o: E option * Ty * fieldName: string * value: E
  | IfThenElse of guard: E * wt: E * wf: E
  | Lambda of var: Mfv * body: E
  | Let of binding: Mfv * value: E * body: E
  | LetRec of recursiveBindings: (Mfv * E) list * body: E
  | NewArray of ty: Ty * values: E list
  | NewDelegate of ty: Ty * body: E
  | NewObject of ty: Mfv * tyArgs: Ty list * args: E list
  | NewRecord of ty: Ty * args: E list
  | NewTuple of ty: Ty * args: E list
  | Quote of q: E
  | FieldGet of o: E option * ty: Ty * field: FSharpField
  | AnonRecordGet of o: E * ty: Ty * fieldIndex: int
  | FieldSet of o: E option * ty: Ty * field: FSharpField * value: E
  | Sequential of E * E
  | TryFinally of body: E * finalize: E
  | TryWith of exprA: E * mfvA: Mfv * exprB: E * mfvB: Mfv * exprC: E
  | TupleGet of ty: Ty * index: int * from: E
  | DecisionTree of decision: E * targets: (Mfv list * E) list
  | DecisionTreeSuccess of
    decisionTargetIdx: int *
    decisionTargetExprs: E list
  | TypeLambda of genParam: FSharpGenericParameter list * body: E
  | TypeTest of Ty * E
  | UnionCaseSet of E * Ty * FSharpUnionCase * FSharpField * E
  | UnionCaseGet of E * Ty * FSharpUnionCase * FSharpField
  | UnionCaseTest of E * Ty * FSharpUnionCase
  | UnionCaseTag of E * Ty
  | ObjectExpr of
    objType: Ty *
    baseCall: E *
    overrides: FSharpObjectExprOverride list *
    implementations: (Ty * FSharpObjectExprOverride list) list
  | TraitCall of
    sourceTypes: Ty list *
    traitName: string *
    typeArgs: FSharp.Compiler.Syntax.SynMemberFlags *
    typeInstantiation: Ty list *
    argTypes: Ty list *
    argExprs: E list
  | ValueSet of toSet: Mfv * value: E
  | WhileLoop of guard: E * body: E
  | BaseValue of Ty
  | DefaultValue of Ty
  | ThisValue of Ty
  | Const of obj * Ty
  | Value of Mfv

  static let metadata =
    System.Runtime.CompilerServices.ConditionalWeakTable()

  member internal this.Associate (e: FSharpExpr) =
    metadata.AddOrUpdate(this, e)
    this

  member this.Type =
    // match typesTable.TryGetValue this with
    // | true, t -> t
    // | false, _ -> failwith $"No type information for {this}"
    match metadata.TryGetValue this with
    | true, e -> e.Type
    | false, _ -> failwith $"No type information for {this}"

let rec convert (e: FSharpExpr) : E =
  match e with
  | P.AddressOf lvalue -> AddressOf(convert lvalue)
  | P.AddressSet(lvalue, rvalue) ->
    AddressSet(convert lvalue, convert rvalue)
  | P.Application(f, tys, args) ->
    Application(convert f, tys, List.map convert args)
  | P.Call(o, mfv, tyArgs1, tyArgs2, args) ->
    Call(
      Option.map convert o,
      mfv,
      tyArgs1,
      tyArgs2,
      List.map convert args
    )
  | P.Coerce(target, e) -> Coerce(target, convert e)
  | P.FastIntegerForLoop(start, limit, consume, isUp, dbgFor, dbgTo) ->
    FastIntegerForLoop(
      convert start,
      convert limit,
      convert consume,
      isUp
    )
  | P.ILAsm(asm, typeArgs, args) ->
    ILAsm(asm, typeArgs, List.map convert args)
  | P.ILFieldGet(o, ty, fieldName) ->
    ILFieldGet(Option.map convert o, ty, fieldName)
  | P.ILFieldSet(o, ty, fieldName, value) ->
    ILFieldSet(Option.map convert o, ty, fieldName, convert value)
  | P.IfThenElse(guard, wt, wf) ->
    IfThenElse(convert guard, convert wt, convert wf)
  | P.Lambda(var, body) -> Lambda(var, convert body)
  | P.Let((binding, value, dbg), body) ->
    Let(binding, convert value, convert body)
  | P.LetRec(recursiveBindings, body) ->
    let rbs =
      List.map
        (fun (mfv, b, dbg) -> (mfv, convert b))
        recursiveBindings

    LetRec(rbs, convert body)
  | P.NewArray(ty, values) -> NewArray(ty, List.map convert values)
  | P.NewDelegate(ty, body) -> NewDelegate(ty, convert body)
  | P.NewObject(ty, tyArgs, args) ->
    NewObject(ty, tyArgs, List.map convert args)
  | P.NewRecord(ty, args) -> NewRecord(ty, List.map convert args)
  | P.NewTuple(ty, args) -> NewTuple(ty, List.map convert args)
  | P.Quote q -> Quote(convert q)
  | P.FSharpFieldGet(o, ty, field) ->
    FieldGet(Option.map convert o, ty, field)
  | P.AnonRecordGet(o, ty, fieldIndex) ->
    AnonRecordGet(convert o, ty, fieldIndex)
  | P.FSharpFieldSet(o, ty, field, value) ->
    FieldSet(Option.map convert o, ty, field, convert value)
  | P.Sequential(t, t1) -> Sequential(convert t, convert t1)
  | P.TryFinally(body, finalize, tryDbgPt, finallyDbgPt) ->
    TryFinally(convert body, convert finalize)
  | P.TryWith(exprA, mfvA, exprB, mfvB, exprC, dbgA, dbgB) ->
    TryWith(convert exprA, mfvA, convert exprB, mfvB, convert exprC)
  | P.TupleGet(ty, index, from) -> TupleGet(ty, index, convert from)
  | P.DecisionTree(decision, targets) ->
    let newTargets = [
      for mfvs, expr in targets do
        mfvs, convert expr
    ]

    DecisionTree(convert decision, newTargets)
  | P.DecisionTreeSuccess(decisionTargetIdx, decisionTargetExprs) ->
    DecisionTreeSuccess(
      decisionTargetIdx,
      List.map convert decisionTargetExprs
    )
  | P.TypeLambda(genParam, body) -> TypeLambda(genParam, convert body)
  | P.TypeTest(ty, t) -> TypeTest(ty, convert t)
  | P.UnionCaseSet(a, ty, unionCase, field, b) ->
    UnionCaseSet(convert a, ty, unionCase, field, convert b)
  | P.UnionCaseGet(t, ty, unionCase, field) ->
    UnionCaseGet(convert t, ty, unionCase, field)
  | P.UnionCaseTest(t, ty, unionCase) ->
    UnionCaseTest(convert t, ty, unionCase)
  | P.UnionCaseTag(t, ty) -> UnionCaseTag(convert t, ty)
  | P.ObjectExpr(objType, baseCall, overrides, implementations) ->
    ObjectExpr(objType, convert baseCall, overrides, implementations)
  | P.TraitCall(sourceTypes,
                traitName,
                typeArgs,
                typeInstantiation,
                argTypes,
                argExprs) ->
    TraitCall(
      sourceTypes,
      traitName,
      typeArgs,
      typeInstantiation,
      argTypes,
      List.map convert argExprs
    )
  | P.ValueSet(set, value) -> ValueSet(set, convert value)
  | P.WhileLoop(guard, body, dbgPt) ->
    WhileLoop(convert guard, convert body)
  | P.BaseValue ty -> BaseValue ty
  | P.DefaultValue ty -> DefaultValue ty
  | P.ThisValue ty -> ThisValue ty
  | P.Const(foo, ty) -> Const(foo, ty)
  | P.Value mfv -> Value mfv
  | _ -> failwith "Unrecognized"
  |> _.Associate(e)
// |> E

let rec visitExpr f (n: int) (e: FSharpExpr) =
  f n e

  match e with
  | FSharpExprPatterns.AddressOf(lvalueExpr) ->
    visitExpr f (n + 1) lvalueExpr
  | FSharpExprPatterns.AddressSet(lvalueExpr, rvalueExpr) ->
    visitExpr f (n + 1) lvalueExpr
    visitExpr f (n + 1) rvalueExpr
  | FSharpExprPatterns.Application(funcExpr, typeArgs, argExprs) ->
    visitExpr f (n + 1) funcExpr
    visitExprs f (n + 1) argExprs
  | FSharpExprPatterns.Call(objExprOpt,
                            memberOrFunc,
                            typeArgs1,
                            typeArgs2,
                            argExprs) ->
    visitObjArg f (n + 1) objExprOpt
    visitExprs f (n + 1) argExprs
  | FSharpExprPatterns.Coerce(targetType, inpExpr) ->
    visitExpr f (n + 1) inpExpr
  | FSharpExprPatterns.FastIntegerForLoop(startExpr,
                                          limitExpr,
                                          consumeExpr,
                                          isUp,
                                          _,
                                          _) ->
    visitExpr f (n + 1) startExpr
    visitExpr f (n + 1) limitExpr
    visitExpr f (n + 1) consumeExpr
  | FSharpExprPatterns.ILAsm(asmCode, typeArgs, argExprs) ->
    visitExprs f (n + 1) argExprs
  | FSharpExprPatterns.ILFieldGet(objExprOpt, fieldType, fieldName) ->
    visitObjArg f (n + 1) objExprOpt
  | FSharpExprPatterns.ILFieldSet(objExprOpt,
                                  fieldType,
                                  fieldName,
                                  valueExpr) ->
    visitObjArg f (n + 1) objExprOpt
  | FSharpExprPatterns.IfThenElse(guardExpr, thenExpr, elseExpr) ->
    visitExpr f (n + 1) guardExpr
    visitExpr f (n + 1) thenExpr
    visitExpr f (n + 1) elseExpr
  | FSharpExprPatterns.Lambda(lambdaVar, bodyExpr) ->
    visitExpr f (n + 1) bodyExpr
  | FSharpExprPatterns.Let((bindingVar, bindingExpr, dbg), bodyExpr) ->
    visitExpr f (n + 1) bindingExpr
    visitExpr f (n + 1) bodyExpr
  | FSharpExprPatterns.LetRec(recursiveBindings, bodyExpr) ->
    for _, bindingExpr, _ in recursiveBindings do
      visitExpr f (n + 1) bindingExpr

    visitExpr f (n + 1) bodyExpr
  | FSharpExprPatterns.NewArray(arrayType, argExprs) ->
    visitExprs f (n + 1) argExprs
  | FSharpExprPatterns.NewDelegate(delegateType, delegateBodyExpr) ->
    visitExpr f (n + 1) delegateBodyExpr
  | FSharpExprPatterns.NewObject(objType, typeArgs, argExprs) ->
    visitExprs f (n + 1) argExprs
  | FSharpExprPatterns.NewRecord(recordType, argExprs) ->
    visitExprs f (n + 1) argExprs
  | FSharpExprPatterns.NewAnonRecord(recordType, argExprs) ->
    visitExprs f (n + 1) argExprs
  | FSharpExprPatterns.NewTuple(tupleType, argExprs) ->
    visitExprs f (n + 1) argExprs
  | FSharpExprPatterns.NewUnionCase(unionType, unionCase, argExprs) ->
    visitExprs f (n + 1) argExprs
  | FSharpExprPatterns.Quote(quotedExpr) ->
    visitExpr f (n + 1) quotedExpr
  | FSharpExprPatterns.FSharpFieldGet(objExprOpt,
                                      recordOrClassType,
                                      fieldInfo) ->
    visitObjArg f (n + 1) objExprOpt
  | FSharpExprPatterns.AnonRecordGet(objExpr,
                                     recordOrClassType,
                                     fieldInfo) ->
    visitExpr f (n + 1) objExpr
  | FSharpExprPatterns.FSharpFieldSet(objExprOpt,
                                      recordOrClassType,
                                      fieldInfo,
                                      argExpr) ->
    visitObjArg f (n + 1) objExprOpt
    visitExpr f (n + 1) argExpr
  | FSharpExprPatterns.Sequential(firstExpr, secondExpr) ->
    visitExpr f (n + 1) firstExpr
    visitExpr f (n + 1) secondExpr
  | FSharpExprPatterns.TryFinally(bodyExpr,
                                  finalizeExpr,
                                  dbgTry,
                                  dbgFinally) ->
    visitExpr f (n + 1) bodyExpr
    visitExpr f (n + 1) finalizeExpr
  | FSharpExprPatterns.TryWith(bodyExpr,
                               _,
                               _,
                               catchVar,
                               catchExpr,
                               dbgTry,
                               dbgWith) ->
    visitExpr f (n + 1) bodyExpr
    visitExpr f (n + 1) catchExpr
  | FSharpExprPatterns.TupleGet(tupleType, tupleElemIndex, tupleExpr) ->
    visitExpr f (n + 1) tupleExpr
  | FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) ->
    visitExpr f (n + 1) decisionExpr
    List.iter (snd >> visitExpr f (n + 1)) decisionTargets
  | FSharpExprPatterns.DecisionTreeSuccess(decisionTargetIdx,
                                           decisionTargetExprs) ->
    visitExprs f (n + 1) decisionTargetExprs
  | FSharpExprPatterns.TypeLambda(genericParam, bodyExpr) ->
    visitExpr f (n + 1) bodyExpr
  | FSharpExprPatterns.TypeTest(ty, inpExpr) ->
    visitExpr f (n + 1) inpExpr
  | FSharpExprPatterns.UnionCaseSet(unionExpr,
                                    unionType,
                                    unionCase,
                                    unionCaseField,
                                    valueExpr) ->
    visitExpr f (n + 1) unionExpr
    visitExpr f (n + 1) valueExpr
  | FSharpExprPatterns.UnionCaseGet(unionExpr,
                                    unionType,
                                    unionCase,
                                    unionCaseField) ->
    visitExpr f (n + 1) unionExpr
  | FSharpExprPatterns.UnionCaseTest(unionExpr, unionType, unionCase) ->
    visitExpr f (n + 1) unionExpr
  | FSharpExprPatterns.UnionCaseTag(unionExpr, unionType) ->
    visitExpr f (n + 1) unionExpr
  | FSharpExprPatterns.ObjectExpr(objType,
                                  baseCallExpr,
                                  overrides,
                                  interfaceImplementations) ->
    visitExpr f (n + 1) baseCallExpr
    List.iter (visitObjMember f n) overrides

    List.iter
      (snd >> List.iter (visitObjMember f n))
      interfaceImplementations
  | FSharpExprPatterns.TraitCall(sourceTypes,
                                 traitName,
                                 typeArgs,
                                 typeInstantiation,
                                 argTypes,
                                 argExprs) ->
    // todo
    visitExprs f (n + 1) argExprs
  | FSharpExprPatterns.ValueSet(valToSet, valueExpr) ->
    visitExpr f (n + 1) valueExpr
  | FSharpExprPatterns.WhileLoop(guardExpr, bodyExpr, dbg) ->
    visitExpr f (n + 1) guardExpr
    visitExpr f (n + 1) bodyExpr
  | FSharpExprPatterns.BaseValue baseType -> ()
  | FSharpExprPatterns.DefaultValue defaultType -> ()
  | FSharpExprPatterns.ThisValue thisType -> ()
  | FSharpExprPatterns.Const(constValueObj, constType) -> ()
  | FSharpExprPatterns.Value(valueToGet) -> ()
  | _ -> failwith (sprintf "unrecognized %+A" e)

and visitExprs f n exprs = List.iter (visitExpr f n) exprs

and visitObjArg f n objOpt = Option.iter (visitExpr f n) objOpt

and visitObjMember f n memb = visitExpr f n memb.Body

open Microsoft.FSharp.Reflection
open System

/// Pretty-prints any F# discriminated union value as an ASCII tree.
let prettyPrintDU (value: obj) : string =
  let sb = System.Text.StringBuilder()

  let rec render (value: obj) (prefix: string) (isLast: bool) =
    let connector = if isLast then "└── " else "├── "
    let childPrefix = prefix + (if isLast then "    " else "│   ")

    match value with
    | null -> sb.AppendLine(prefix + connector + "<null>") |> ignore

    | v when FSharpType.IsUnion(v.GetType()) ->
      let t = v.GetType()
      let case, fields = FSharpValue.GetUnionFields(v, t)

      if fields.Length = 0 then
        sb.AppendLine(prefix + connector + case.Name) |> ignore
      else
        sb.AppendLine(prefix + connector + case.Name) |> ignore
        let fieldInfos = case.GetFields()

        fields
        |> Array.iteri (fun i field ->
          let isLastField = (i = fields.Length - 1)
          let fieldName = fieldInfos.[i].Name
          let fieldConnector = if isLastField then "└── " else "├── "

          let fieldChildPrefix =
            childPrefix + (if isLastField then "    " else "│   ")

          match field with
          | f when FSharpType.IsUnion(f.GetType()) ->
            sb.AppendLine(
              childPrefix + fieldConnector + fieldName + ":"
            )
            |> ignore

            render f fieldChildPrefix true
          | f when FSharpType.IsTuple(f.GetType()) ->
            sb.AppendLine(
              childPrefix + fieldConnector + fieldName + ": (tuple)"
            )
            |> ignore

            let elements = FSharpValue.GetTupleFields(f)

            elements
            |> Array.iteri (fun j el ->
              render el fieldChildPrefix (j = elements.Length - 1))
          | :? System.Collections.IEnumerable as f when
            not (f :? string)
            ->
            let items = f |> Seq.cast<obj> |> Seq.toArray

            sb.AppendLine(
              childPrefix
              + fieldConnector
              + $"{fieldName}: [{items.Length} items]"
            )
            |> ignore

            items
            |> Array.iteri (fun j item ->
              render item fieldChildPrefix (j = items.Length - 1))
          | f ->
            sb.AppendLine(
              childPrefix + fieldConnector + $"{fieldName}: {f}"
            )
            |> ignore)

    | v -> sb.AppendLine(prefix + connector + string v) |> ignore

  // Handle the root specially — no connector prefix
  let t = value.GetType()

  if FSharpType.IsUnion(t) then
    let case, fields = FSharpValue.GetUnionFields(value, t)

    if fields.Length = 0 then
      sb.AppendLine(case.Name) |> ignore
    else
      sb.AppendLine(case.Name) |> ignore
      let fieldInfos = case.GetFields()

      fields
      |> Array.iteri (fun i field ->
        render field "" (i = fields.Length - 1))
  else
    sb.AppendLine(string value) |> ignore

  sb.ToString().TrimEnd()
