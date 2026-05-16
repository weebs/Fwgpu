module rec Fw.Transform

open Fw.Type
open Ast
open FSharp.Compiler.Symbols

module P = FSharpExprPatterns

let replacements =
    dict [
        "[I_call\n   (Normalcall,\n    Microsoft.FSharp.Core.LanguagePrimitives::get_GenericComparer(...)(...),\n    None)]",
        "Microsoft::FSharp::Core::LanguagePrimitives::GenericComparer"
        "[I_call\n   (Normalcall,\n    Microsoft.FSharp.Core.LanguagePrimitives::get_GenericEqualityComparer(...)(...),\n    None)]",
        "Microsoft::FSharp::Core::LanguagePrimitives::GenericEqualityComparer"
    ]

let fieldName (field: FSharpField) = field.Name


let qualifiedPath (mfv: FSharpMemberOrFunctionOrValue) =
    let path =
        match mfv.DeclaringEntity with
        | Some ent ->
            let cpp = entTypeName ent
            let path = cpp + "::" + mfv.CompiledName
            path
        | None -> failwith "Empty declaring entity for module variable"

    replaceIncludedBCLNamespaces path


let entTypeName (ent: FSharpEntity) = ent.BasicQualifiedName |> toCppPath

let rec requiresTempRef (e: FSharpExpr) =
    match e with
    | _ when not (requiresGc e.Type) -> false
    | P.Const _ -> false
    | P.Call(None, mfv, [], [], []) -> mfv.IsMutable
    | P.Value mfv -> mfv.IsMutable
    | P.DefaultValue _ when requiresGc e.Type -> false
    | P.Coerce(ty, expr) -> requiresGc ty && requiresTempRef expr
    | P.ILAsm(asm, _, _) when replacements.ContainsKey asm -> false
    | _ -> true

let translateCallArg (e: FSharpExpr) =
    if requiresTempRef e then
        // Call(GetField(Call(Var "GcRoot", [ translate e ]), "get"), [])
        translate e
    else
        translate e

let translateVar (mfv: FSharpMemberOrFunctionOrValue) body =
    let value = translate body

    if requiresGc mfv.FullType then
        // Call(Var "GcRoot", [ value ])
        value
    else
        value


let rec translate (e: FSharpExpr) : CppExpr =
    match e with
    | P.AddressOf expr -> Ref(translate expr)
    | P.ThisValue _ty -> Var "this"
    | P.Value mfv when mfv.IsMemberThisValue -> Var "this"
    | P.Call(None, mfv, [], [], []) -> Var(qualifiedPath mfv)
    // todo : hack
    | P.Value mfv when mfv.CompiledName = "bind@" -> Var "bind"
    | P.Value mfv ->
        if mfv.IsMutable && requiresGc mfv.FullType then
            GetField(Var mfv.CompiledName, "Value")
        else
            Var(mfv.CompiledName |> replaceIncludedBCLNamespaces)
    | P.TypeTest(ty, expr) ->
        let tyTarget = convert ty

        CallGen(Var "::IsType", [ Var(printType tyTarget) ], [ translate expr ])
    | P.DecisionTreeSuccess(idx, exprs) ->
        Call(Var $"_{idx}", List.map translate exprs)
    | P.Coerce(ty, value) when not (requiresGc ty) ->
        let cppTy = convert ty
        let dataField = DerefGetField(translate value, "__data")
        CallGen(Var "std::any_cast", [ Var(printType cppTy) ], [ dataField ])
    | P.Coerce(ty, value) when requiresGc ty ->
        let rt = convert value.Type |> printType

        if requiresGc value.Type then
            CallGen(
                Var "::coerce",
                [ Var rt; (convert ty |> printType |> Var) ],
                [ translate value ]
            )
        else
            CallGen(
                Var "dynamic_cast",
                [ Var(convert ty |> printType) ],
                [
                    Call(
                        DerefGetField(
                            Call(
                                Var "new System::Box",
                                [
                                    CallGen(
                                        Var "static_cast",
                                        [ Var rt ],
                                        [ translate value ]
                                    )
                                ]
                            ),
                            "get"
                        ),
                        []
                    )
                ]
            )
    | P.Const(o, t) ->
        if isUnit t then
            ExprComment "Unit ()"
        elif (convert t) = Named "System::String" then
            Call(Var "System::String", [ Const(o, t) ])
        else
            Const(o, t)
    | P.Application(callee, [], args) ->
        Call(
            DerefGetField(translate callee, "invoke"),
            List.map translateCallArg args
        )
    | P.Application(callee, typeArgs, args) ->
        let genericArgs = typeArgs |> List.map (convert >> printType >> Var)
        CallGen(translate callee, genericArgs, List.map translateCallArg args)
    | P.Call(None, mfv, classTypeArgs, [], args) ->
        let path = qualifiedPath mfv
        Call(Var path, List.map translateCallArg args)
    | P.Call(None, mfv, classTypeArgs, ys, args) ->
        let path = qualifiedPath mfv
        let genArgs = ys |> List.map (convert >> printType >> Var)
        CallGen(Var path, genArgs, List.map translateCallArg args)
    | P.Call(Some o, mfv, xs, ys, args) ->
        let isRefVar =
            function
            | P.Value mfv -> mfv.IsMutable
            | P.Call(None, mfv, [], [], []) -> mfv.IsMutable
            | _ -> false

        let case =
            if requiresGc o.Type || isRefVar o || isByRef o.Type then
                DerefGetField
            else
                GetField

        Call(case (translate o, mfv.CompiledName), args |> List.map translate)
    | P.Lambda(mfv, body) ->
        let stmts =
            if isUnit body.Type then
                translateS body
            else
                translateS body |> addReturn

        if mfv.IsCompilerGenerated && mfv.DisplayName.StartsWith "unitVar" then
            Lambda([], true, stmts, [ "=" ])
        else
            Lambda([ mfv.CompiledName ], true, stmts, [ "=" ])
        |> fun lambda ->
            let argTy = mfv.FullType |> convert |> printType
            let rt = body.Type |> convert |> printType
            New($"::FSharpFunc<{argTy}, {rt}>", [ lambda ])
    | P.FSharpFieldGet(Some expr, _ty, field) ->
        match expr with
        | expr when expr.Type.TypeDefinition.IsValueType = false ->
            DerefGetField(translate expr, fieldName field)
        | P.Value mfv when
            mfv.IsMemberThisValue
            || mfv.CompiledName = "this" && mfv.IsCompilerGenerated
            ->
            DerefGetField(Var "this", fieldName field)
        | P.Value mfv when mfv.IsMutable ->
            DerefGetField(translate expr, fieldName field)
        | _ -> GetField(translate expr, fieldName field)
    | P.NewObject(mfv, tys, args) ->
        let basePath = entTypeName mfv.DeclaringEntity.Value

        let ctor =
            if tys.Length = 0 then
                basePath
            else
                let args =
                    tys
                    |> List.map (convert >> printType)
                    |> String.concat ", " in

                $"{basePath}<{args}>"

        if not (requiresGc (mfv.DeclaringEntity.Value.AsType())) then
            Call(Var ctor, List.map translateCallArg args)
        else
            New(ctor, List.map translateCallArg args)
    | P.Sequential _ -> BlockExpr(translateS e)
    | P.ILAsm(asm, _types, _values) ->
        match replacements.TryGetValue asm with
        | true, replacement -> Var(replaceIncludedBCLNamespaces replacement)
        | _ -> ExprComment asm
    | P.DefaultValue t when requiresGc t -> Var "nullptr"
    | P.NewRecord(ty, values) when requiresGc ty ->
        // todo : hack
        let t = convert ty |> printType |> _.Replace("*", "")
        New(t, List.map translate values)
    | P.Let _
    | P.FastIntegerForLoop _
    | P.IfThenElse _
    | P.TryFinally _
    | P.TryWith _
    | P.LetRec _
    | P.WhileLoop _ -> BlockExpr(translateS e)
    | _ -> ExprComment $"%A{e}"

and translateS (e: FSharpExpr) : CppStmt list =
    match e with
    | P.Let((mfv, (P.AddressOf(P.Value valueMfv) as value), _dbg), body) when
        mfv.CompiledName = valueMfv.CompiledName
        ->
        let cppTy = convert mfv.FullType

        [
            SVariable("__temp", cppTy, None)
            Assign(Var "__temp", translate value)
            SVariable(mfv.CompiledName, cppTy, Some(Var "__temp"))
            yield! translateS body
        ]
    | P.Let((mfv, exp, _dbg), body) ->
        let varName =
            if mfv.CompiledName = "bind@" then
                "bind"
            else
                mfv.CompiledName

        let var =
            if mfv.IsMutable then
                SVariable(
                    varName,
                    Gen("Ref", [ convert exp.Type ]),
                    Some(translateVar mfv exp)
                )
            else
                SVariable(
                    varName,
                    convert exp.Type,
                    Some(translateVar mfv exp)
                )

        var :: translateS body
    | P.ValueSet(mfv, value) -> [
        Assign(Var mfv.CompiledName, translate value)
      ]
    | P.Sequential(a, b) -> translateS a @ translateS b
    | P.FSharpFieldSet(Some dest, _ty, field, value) -> [
        let case =
            match dest with
            | P.ThisValue _ -> DerefGetField
            | P.Value mfv when mfv.IsMemberThisValue || mfv.IsMutable ->
                DerefGetField
            | _ -> GetField

        Assign(case (translate dest, fieldName field), translate value)
      ]
    | P.IfThenElse(cond, wt, wf) -> [
        IfThenElse(translate cond, translateS wt, translateS wf)
      ]
    | P.WhileLoop(cond, body, _dbg) -> [
        WhileLoop(translate cond, translateS body)
      ]
    | P.FastIntegerForLoop(from, until, body, isUp, _dbgA, _dbgB) ->
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
                        convert var.FullType,
                        Some frm
                    ),
                    Var cond,
                    Exp(Var post),
                    bdy
                )
            ]
        | _ -> failwith $"not sure how to convert integer for loop %A{e}"
    | P.DecisionTree(decision, targets) ->
        let desc = translateS decision

        let cppTargets = [
            for i in 0 .. targets.Length - 1 do
                let args = fst targets[i] |> List.map _.FullName
                let body = snd targets[i] |> translateS |> addReturn

                SVariable(
                    $"_{i}",
                    Auto,
                    Some(Lambda(args, true, body, [ "&" ]))
                )
        ]

        cppTargets @ desc
    | P.TryFinally(tryExpr, finallyExpr, _dbgTry, _dbgFinally) ->
        let tryBody = translateS tryExpr
        let finallyBody = translateS finallyExpr

        [
            if isUnit tryExpr.Type then
                TryCatch(tryBody, "...", [])
                yield! finallyBody
            else
                let tryLambda = Lambda([], true, tryBody |> addReturn, [ "&" ])
                let finallyLambda = Lambda([], true, finallyBody, [ "&" ])
                SVariable("tryBody", Auto, Some tryLambda)
                SVariable("finally", Auto, Some finallyLambda)
                SVariable("result", convert tryExpr.Type, None)

                TryCatch(
                    [ Assign(Var "result", Call(Var "tryBody", [])) ],
                    "...",
                    []
                )

                Exp(Call(Var "finally", []))
                Exp(Var "result")
        ]
    | P.TryWith(tryExpr,
                filterVar,
                filterExpr,
                withVar,
                withExpr,
                dbgTry,
                dbgWith) -> [
        TryCatch(
            translateS tryExpr,
            $"const System::Exception& {withVar.CompiledName}",
            translateS withExpr
        )
      ]
    | P.LetRec(bindings, body) -> [
        for mfv, mfvBody, _dbg in bindings do
            SVariable(
                mfv.CompiledName,
                Gen("Ref", [ convert mfv.FullType ]),
                None
            )

            Assign(Var mfv.CompiledName, translate mfvBody)
        yield! translateS body
      ]
    | P.LetRec _ -> [ SComment $"%A{e}" ]
    | _ -> [ Exp(translate e) ]

let isByRef (t: FSharpType) =
    try
        match t.TypeDefinition.AccessPath with
        | "Microsoft.FSharp.Core" ->
            match t.TypeDefinition.CompiledName with
            | "byref`1" -> true
            | _ -> false
        | _ -> false
    with ex ->
        false

