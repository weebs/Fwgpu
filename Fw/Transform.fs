module rec Fw.Transform

open Ast
open FSharp.Compiler.Symbols

module P = FSharpExprPatterns

let qualifiedPath (mfv: FSharpMemberOrFunctionOrValue) =
  match mfv.DeclaringEntity with
  | Some ent ->
    let cpp = entTypeName ent
    let path = cpp + "::" + mfv.CompiledName
    path
  | None -> failwith "Empty declaring entity for module variable"

let isUnit (t: FSharpType) =
  try
    if t.IsGenericParameter then
      false
    elif
      t.ErasedType.BasicQualifiedName = "Microsoft.FSharp.Core.Unit"
    then
      true
    elif t.BasicQualifiedName = "Microsoft.FSharp.Core.unit" then
      true
    else
      false
  with ex ->
    printfn $"{ex}"
    false

let fieldName (field: FSharpField) = field.Name

let toCppPath (s: string) = s.Replace("+", "::").Replace(".", "::")

let typeName (t: FSharpType) =
  if t.BasicQualifiedName = "Microsoft.FSharp.Core.obj" then
    "System::Object"
  else
    t.TypeDefinition.BasicQualifiedName |> toCppPath

let entTypeName (ent: FSharpEntity) =
  ent.BasicQualifiedName |> toCppPath

let rec translate (e: FSharpExpr) : CppExpr =
  match e with
  | P.AddressOf expr -> Var $"&{translate expr |> print}"
  | P.TypeTest(ty, expr) ->
    let tyTarget =
      if ty.TypeDefinition.IsValueType then
        tyConvert ty
      else
        let (Gen("Gc", [ tyTarget ])) = tyConvert ty
        tyTarget

    CallGen(
      Var "System::IsType",
      [ Var(printType tyTarget) ],
      [ translate expr ]
    )
  // Call(
  //   DerefGetField(translate expr, "IsType"),
  //   [ Const(typeName ty, ty) ]
  // )
  | P.DecisionTreeSuccess(idx, exprs) ->
    Call(Var $"_{idx}", List.map translate exprs)
  | P.Coerce(ty, value) when ty.TypeDefinition.IsValueType = true ->
    ExprComment "Todo: Unwrap obj to value type"
  | P.Coerce(ty, value) when ty.TypeDefinition.IsValueType = false ->
    let (Gen("Gc", [ tyTarget ])) = tyConvert ty

    CallGen(
      Var "std::dynamic_pointer_cast",
      [ Var(printType tyTarget) ],
      [ translate value ]
    )
  | P.Const(o, t) ->
    if isUnit t then ExprComment "Unit ()" else Const(o, t)
  | P.Call(None, mfv, [], [], []) -> Var(qualifiedPath mfv)
  | P.Call(None, mfv, xs, [], args) ->
    if xs.Length > 0 then
      ()

    let path = qualifiedPath mfv
    Call(Var path, List.map translate args)
  | P.Call(None, mfv, xs, ys, args) ->
    let path = qualifiedPath mfv
    let genArgs = ys |> List.map (tyConvert >> printType >> Var)
    CallGen(Var path, genArgs, List.map translate args)
  | P.Call(Some o, mfv, xs, ys, args) ->
    let case =
      if o.Type.TypeDefinition.IsValueType then
        GetField
      else
        DerefGetField

    Call(
      case (translate o, mfv.CompiledName),
      args |> List.map translate
    )
  | P.Lambda(mfv, body) ->
    let stmts =
      if isUnit body.Type then
        translateS body
      else
        translateS body |> addReturn

    if
      mfv.IsCompilerGenerated && mfv.DisplayName.StartsWith "unitVar"
    then
      Lambda([], stmts, [])
    else
      Lambda([ mfv.CompiledName ], stmts, [])
  | P.Value mfv -> Var mfv.CompiledName
  | P.ThisValue ty -> Var "this"
  | P.NewRecord(ty, values) -> Var "todo"
  | P.FSharpFieldGet(Some expr, ty, field) ->
    // TODO : check if e.Type.TypeDefinition is a reference type
    match expr with
    | expr when expr.Type.TypeDefinition.IsValueType = false ->
      DerefGetField(translate expr, fieldName field)
    | P.Value mfv when
      mfv.IsMemberThisValue
      || mfv.CompiledName = "this" && mfv.IsCompilerGenerated
      ->
      DerefGetField(Var "this", fieldName field)
    | _ -> GetField(translate expr, fieldName field)
  | P.NewObject(mfv, tys, args) ->
    let ctorPath = entTypeName mfv.DeclaringEntity.Value

    if mfv.DeclaringEntity.Value.IsValueType then
      Call(Var ctorPath, List.map translate args)
    else
      CallGen(
        Var "std::make_shared",
        [ Var ctorPath ],
        [ Call(Var ctorPath, List.map translate args) ]
      )
  | P.Let((mfv, value, dbg), body) ->
    let var = Let(mfv.CompiledName, translate value)
    let cppBody = var :: translateS body
    let withReturn = addReturn cppBody
    Call(Lambda([], withReturn, []), [])
  | _ -> ExprComment $"%A{e}"
// let tree = Walk.prettyPrintDU e
// ExprComment $"%A{e}"
// printfn $"{tree}"
// ExprComment tree
// | _ -> failwith $"Unrecognized %A{e}"

and translateS (e: FSharpExpr) : CppStmt list =
  match e with
  | P.Let((mfv, exp, dbg), body) ->
    let value = translate exp
    let name = mfv.CompiledName
    Let(name, value) :: translateS body
  | P.ValueSet(mfv, value) -> [
      Assign(Var mfv.CompiledName, translate value)
    ]
  | P.Sequential(a, b) -> translateS a @ translateS b
  | P.FSharpFieldSet(Some dest, ty, field, value) -> [
      // todo : check if e.Type.TypeDefinition is a reference type?
      let case =
        match dest with
        | P.ThisValue _ -> DerefGetField
        | P.Value mfv when mfv.IsMemberThisValue -> DerefGetField
        | _ -> GetField

      Assign(case (translate dest, fieldName field), translate value)
    ]
  | P.IfThenElse(cond, wt, wf) -> [
      IfThenElse(translate cond, translateS wt, translateS wf)
    ]
  | P.DecisionTree(decision, targets) ->
    let desc = translateS decision

    let tgts = [
      for i in 0 .. targets.Length - 1 do
        let args = fst targets[i]
        let body = snd targets[i]

        SVariable(
          $"_{i}",
          Auto,
          Some(
            Lambda(
              args |> List.map _.FullName,
              translateS body |> addReturn,
              [ "&" ]
            )
          )
        )
    ]

    tgts @ desc
  | P.TryFinally(tryExpr, finallyExpr, dbgTry, dbgFinally) ->
    let tryBody = translateS tryExpr
    let finallyBody = translateS finallyExpr
    let tryLambda = Lambda([], tryBody |> addReturn, [ "&" ])
    let finallyLambda = Lambda([], finallyBody, [ "&" ])

    [
      SVariable("tryBody", Auto, Some tryLambda)
      SVariable("finally", Auto, Some finallyLambda)
      SVariable("result", tyConvert tryExpr.Type, None)
      TryCatch(
        [ Assign(Var "result", Call(Var "tryBody", [])) ],
        "...",
        []
      )
      Exp(Call(Var "finally", []))
      Exp(Var "result")
    ]
  | P.TryWith(a, mfvA, b, mfvB, c, dbgTry, dbgWith) -> [
      SComment "Try"
      yield! translateS a
      SComment "With"
      yield! translateS b
      SComment "???? trywith"
      yield! translateS c
    ]
  | _ -> [ Exp(translate e) ]

let funTyConvert (t: FSharpType) =
  let a = t.GenericArguments[0]
  let b = t.GenericArguments[1]
  let args = if isUnit a then "" else $"{printType (tyConvert a)}"
  let rt = printType (tyConvert b)
  Gen("std::function", [ Named $"{rt}({args})" ])

let tyConvert (t: FSharpType) =
  if t.IsGenericParameter then
    Named t.GenericParameter.Name
  elif Transform.isUnit t then
    Void
  else
    // todo: it literally said t.TypeDefinition.IsValueType is false for
    // System.Boolean lmao
    match t.TypeDefinition.AccessPath with
    | "Microsoft.FSharp.Core" ->
      match t.TypeDefinition.CompiledName with
      | "int" -> Int
      | "int32" -> Int
      | "bool" -> Bool
      | "byref`1" ->
        Named $"&{tyConvert t.GenericArguments[0] |> printType}"
      | "obj" -> Gen("Gc", [ Named "System::Object" ])
      | _ -> Auto
    | _ ->
      if t.IsFunctionType then
        funTyConvert t
      elif isUnit t then
        Void
      elif t.TypeDefinition.IsValueType = false then
        Gen("Gc", [ Named(typeName t) ])
      elif t.TypeDefinition.IsValueType then
        Named(typeName t)
      else
        Auto
