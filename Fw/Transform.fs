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

let fieldName (field: FSharpField) = field.Name

let toCppPath (s: string) = s.Replace("+", "::").Replace(".", "::")

let refTypeName (t: FSharpType) =
  t.TypeDefinition.BasicQualifiedName |> toCppPath

let entTypeName (ent: FSharpEntity) =
  ent.BasicQualifiedName |> toCppPath

let rec translate (e: FSharpExpr) : CppExpr =
  match e with
  | P.Const(o, t) -> Const(o, t)
  | P.Call(None, mfv, [], [], []) -> Var(qualifiedPath mfv)
  | P.Call(None, mfv, xs, ys, args) ->
    let path = qualifiedPath mfv
    Call(Var path, List.map translate args)
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
      Lambda([], stmts)
    else
      Lambda([ mfv.CompiledName ], stmts)
  | P.Value mfv -> Var mfv.CompiledName
  | P.ThisValue ty -> Var "this"
  | P.NewRecord(ty, values) -> Var "todo"
  | P.FSharpFieldGet(Some e, ty, field) ->
    // todo : check if e.Type.TypeDefinition is a reference type?
    match e with
    | P.Value mfv when mfv.IsMemberThisValue ->
      DerefGetField(Var "this", fieldName field)
    | _ -> GetField(translate e, fieldName field)
  | P.NewObject(mfv, tys, args) ->
    let ctorPath = entTypeName mfv.DeclaringEntity.Value

    CallGen(
      Var "std::make_shared",
      [ Var ctorPath ],
      [ Call(Var ctorPath, List.map translate args) ]
    )
  | P.Let((mfv, value, dbg), body) ->
    let var = Let(mfv.CompiledName, translate value)
    let cppBody = var :: translateS body
    let withReturn = addReturn cppBody
    Call(Lambda([], withReturn), [])
  | _ -> failwith $"Unrecognized %A{e}"

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
    match t.ErasedType.BasicQualifiedName with
    | "System.Int32" -> Int
    | qn ->
      if t.IsFunctionType then
        funTyConvert t
      elif t.TypeDefinition.IsValueType = false then
        Gen("std::shared_ptr", [ Named(refTypeName t) ])
      elif isUnit t then
        Ast.Void
      else
        Auto
