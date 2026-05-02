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
  | None ->
    failwith "Empty declaring entity for module variable"

let isUnit (t: FSharpType) =
  try
    if t.IsGenericParameter then
      false
    elif
      t.ErasedType.BasicQualifiedName = "Microsoft.FSharp.Core.Unit"
    then
      true
    elif
      t.BasicQualifiedName = "Microsoft.FSharp.Core.unit"
    then
      true
    else
      false
  with ex ->
    printfn $"{ex}"
    false

let fieldName (field: FSharpField) = field.Name

let toCppPath (s: string) =
  s.Replace("+", "::").Replace(".", "::").Replace("`", "_")

let typeName (t: FSharpType) =
  let baseTy =
    if
      t.BasicQualifiedName = "Microsoft.FSharp.Core.obj"
    then
      "System::Object"
    else
      // todo
      // t.TypeDefinition.BasicQualifiedName |> toCppPath
      t.BasicQualifiedName |> toCppPath

  if t.GenericArguments.Count = 0 then
    baseTy
  else
    let args =
      t.GenericArguments
      |> Seq.map tyConvert
      |> Seq.map printType
      |> String.concat ", "

    $"{baseTy}<{args}>"

let entTypeName (ent: FSharpEntity) =
  ent.BasicQualifiedName |> toCppPath

let rec translate (e: FSharpExpr) : CppExpr =
  match e with
  | P.AddressOf expr -> Var $"&{translate expr |> print}"
  | P.TypeTest(ty, expr) ->
    let tyTarget = tyConvert ty
    // if ty.TypeDefinition.IsValueType then
    //   tyConvert ty
    // if not (requiresGc ty) then
    //   tyConvert ty
    // else
    //   let tyTarget = tyConvert ty
    //   // let (Gen("Gc", [ tyTarget ])) = cppTy
    //   tyTarget

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
  | P.Coerce(ty, value) when
    // ty.TypeDefinition.IsValueType = true
    not (requiresGc ty)
    ->
    let cppTy = tyConvert ty

    // Var
    //   $"*({printType cppTy}*){translate value |> print}->__data /* TODO Proper obj type test and read */"
    let dataField = DerefGetField(translate value, "__data")

    CallGen(
      Var "std::any_cast",
      [ Var(printType cppTy) ],
      [ dataField ]
    )
  | P.Coerce(ty, value) when
    // ty.TypeDefinition.IsValueType = false
    requiresGc ty
    ->
    // let (Gen("Gc", [ tyTarget ])) = tyConvert ty
    let rt = tyConvert value.Type |> printType

    CallGen(
      // Var "std::dynamic_pointer_cast",
      Var "dynamic_cast",
      // [ Var(printType tyTarget) ],
      [ Var(tyConvert ty |> printType) ],
      [
        CallGen(
          Var "static_cast",
          [ Var rt ],
          [ translate value ]
        )
      ]
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

    let genArgs =
      ys |> List.map (tyConvert >> printType >> Var)

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
      mfv.IsCompilerGenerated
      && mfv.DisplayName.StartsWith "unitVar"
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
      || mfv.CompiledName = "this"
         && mfv.IsCompilerGenerated
      ->
      DerefGetField(Var "this", fieldName field)
    | _ -> GetField(translate expr, fieldName field)
  | P.NewObject(mfv, tys, args) ->
    let basePath = entTypeName mfv.DeclaringEntity.Value

    let ctor =
      if tys.Length = 0 then
        basePath
      else
        let args =
          tys
          |> List.map (tyConvert >> printType)
          |> String.concat ", " in

        $"{basePath}<{args}>"

    if
      not (requiresGc (mfv.DeclaringEntity.Value.AsType()))
    then
      Call(Var ctor, List.map translate args)
    else
      CallGen(
        Var "GcRoot",
        [ Var(ctor + "*") ],
        [
          Call(
            Var $"new (UseGC) {ctor}",
            List.map translate args
          )
        ]
      )
  | P.Let((mfv, value, dbg), body) ->
    let var = Let(mfv.CompiledName, translate value)
    let cppBody = var :: translateS body
    let withReturn = addReturn cppBody
    Call(Lambda([], withReturn, []), [])
  | P.ILAsm(asm, types, values) ->
    // let getGenComparerAsm =
    //   "[I_call\n   (Normalcall,\n    Microsoft.FSharp.Core.LanguagePrimitives::get_GenericComparer(...)(...),\n    None)]"

    // let getGenEqComparer =
    //   "[I_call\n   (Normalcall,\n    Microsoft.FSharp.Core.LanguagePrimitives::get_GenericComparer(...)(...),\n    None)]"

    // if asm = getGenComparerAsm then
    //   Var
    //     "Microsoft::FSharp::Core::LanguagePrimitives::GenericComparer"
    match asm with
    | "[I_call\n   (Normalcall,\n    Microsoft.FSharp.Core.LanguagePrimitives::get_GenericComparer(...)(...),\n    None)]" ->
      Var
        "Microsoft::FSharp::Core::LanguagePrimitives::GenericComparer"
    | "[I_call\n   (Normalcall,\n    Microsoft.FSharp.Core.LanguagePrimitives::get_GenericEqualityComparer(...)(...),\n    None)]" ->
      Var
        "Microsoft::FSharp::Core::LanguagePrimitives::GenericEqualityComparer"
    | _ -> ExprComment asm
  | _ -> ExprComment $"%A{e}"
// let tree = Walk.prettyPrintDU e
// ExprComment $"%A{e}"
// printfn $"{tree}"
// ExprComment tree
// | _ -> failwith $"Unrecognized %A{e}"

and translateS (e: FSharpExpr) : CppStmt list =
  match e with
  | P.Let((mfv,
           (P.AddressOf(P.Value valueMfv) as value),
           dbg),
          body) when
    mfv.CompiledName = valueMfv.CompiledName
    ->
    let cppTy = tyConvert mfv.FullType

    [
      SVariable("__temp", cppTy, None)
      Assign(Var "__temp", translate value)
      SVariable(mfv.CompiledName, cppTy, Some(Var "__temp"))
      yield! translateS body
    ]
  | P.Let((mfv, exp, dbg), body) ->
    // let value = translate exp
    // let name = mfv.CompiledName
    let name, ty, value = translateVar mfv exp
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
        | P.Value mfv when mfv.IsMemberThisValue ->
          DerefGetField
        | _ -> GetField

      Assign(
        case (translate dest, fieldName field),
        translate value
      )
    ]
  | P.IfThenElse(cond, wt, wf) -> [
      IfThenElse(
        translate cond,
        translateS wt,
        translateS wf
      )
    ]
  | P.WhileLoop(cond, body, dbg) -> [
      WhileLoop(translate cond, translateS body)
    ]
  | P.FastIntegerForLoop(from, until, body, isUp, dbgA, dbgB) ->
    match body with
    | P.Lambda(var, expr) ->
      let frm = translate from
      let untl = translate until
      let bdy = translateS expr
      // todo fix this hack
      let cond =
        if isUp then
          $"{var.CompiledName} <= {print untl}"
        else
          $"{var.CompiledName} >= {print untl}"

      let post =
        if isUp then
          $"{var.CompiledName}++"
        else
          $"{var.CompiledName}--"

      [
        ForLoop(
          SVariable(
            var.CompiledName,
            tyConvert var.FullType,
            Some frm
          ),
          Var cond,
          Exp(Var post),
          bdy
        )
      ]
    | _ ->
      failwith
        $"not sure how to convert integer for loop %A{e}"
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

    [
      if isUnit tryExpr.Type then
        TryCatch(tryBody, "...", [])
        yield! finallyBody
      else
        let tryLambda =
          Lambda([], tryBody |> addReturn, [ "&" ])

        let finallyLambda = Lambda([], finallyBody, [ "&" ])
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

  let args =
    if isUnit a then "" else $"{printType (tyConvert a)}"

  let rt = printType (tyConvert b)
  Gen("std::function", [ Named $"{rt}({args})" ])

let requiresGc (t: FSharpType) =
  if t.TypeDefinition.IsValueType then
    false
  else
    match t.TypeDefinition.AccessPath with
    | "Microsoft.FSharp.Core" ->
      match t.TypeDefinition.CompiledName with
      | "int" -> false
      | "int32" -> false
      | "bool" -> false
      | "byref`1" -> false
      // $"{tyConvert t.GenericArguments[0] |> printType}*"
      | "obj" -> true
      | _ ->
        failwith
          "Not sure of Microsoft.FSharp.Core type in requiresGc"
    | _ -> true

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
        // Named $"&{tyConvert t.GenericArguments[0] |> printType}"
        Named
          $"{tyConvert t.GenericArguments[0] |> printType}*"
      // | "obj" -> Gen("Gc", [ Named "System::Object" ])
      | "obj" -> Named "System::Object*"
      | _ -> Auto
    | _ ->
      if t.IsFunctionType then funTyConvert t
      elif isUnit t then Void
      else if requiresGc t then Named(typeName t + "*")
      else Named(typeName t)
// elif t.TypeDefinition.IsValueType = false then
//   Gen("Gc", [ Named(typeName t) ])
// elif t.TypeDefinition.IsValueType then
//   Named(typeName t)
// else
//   Auto

let translateVar (mfv: FSharpMemberOrFunctionOrValue) body =
  let ty =
    let baseTy = Transform.tyConvert mfv.FullType

    if Transform.requiresGc mfv.FullType then
      Ast.Gen("GcRoot", [ baseTy ])
    else
      baseTy

  let value = Transform.translate body
  mfv.CompiledName, ty, value
