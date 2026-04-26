module rec Fw.Transform

open Ast
open FSharp.Compiler.Symbols
open Fw.Ast
open System
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open System.Collections.Generic

// open FSharpSymbolPatterns
module P = FSharpExprPatterns

let qualifiedPath (mfv: FSharpMemberOrFunctionOrValue) =
    match mfv.DeclaringEntity with
    | Some ent ->
        let ns = ent.BasicQualifiedName
        let cpp = ns.Replace(".", "::").Replace("+", "::")
        let path = cpp + "::" + mfv.CompiledName
        path
    | None ->
        failwith "Empty declaring entity for module variable"

let isUnit (t: FSharpType) =
    t.ErasedType.BasicQualifiedName = "Microsoft.FSharp.Core.Unit"
    
let fieldName (field: FSharpField) =
    // field.FullName
    field.Name

let rec translate (e: FSharpExpr) : CppExpr =
    match e with
    | P.Const (o, t) ->
        Const (o, t)
    | P.Call (None, mfv, [], [], []) ->
        Var (qualifiedPath mfv)
    | P.Call (None, mfv, xs, ys, args) ->
        let path = qualifiedPath mfv
        Call (Var path, List.map translate args)
    | P.Call (Some o, mfv, xs, ys, args) ->
        Call(GetField(translate o, mfv.CompiledName), args |> List.map translate)
    // | P.Lambda (mfv, body) when ->
    //     if isUnit body.Type then
    //         Lambda ([], translateS body)
    //     else
    //         Lambda ([], translateS body |> addReturn)
    | P.Lambda (mfv, body) ->
        let stmts =
            if isUnit body.Type then translateS body
            else translateS body |> addReturn
        if mfv.IsCompilerGenerated && mfv.DisplayName.StartsWith "unitVar" then
            Lambda ([], stmts)
        else
            Lambda ([mfv.CompiledName], stmts)
    | P.Value mfv ->
        Var mfv.CompiledName
    | P.ThisValue ty ->
        Var "this"
    | P.NewRecord (ty, values) ->
        Var "todo"
    | P.FSharpFieldGet (Some e, ty, field) ->
        GetField (translate e, fieldName field)
    | P.NewObject (mfv, tys, args) ->
        Var "todo"
    // | P.Let ((mfv, value, dbg), body) when mfv.IsCompilerGenerated && mfv.CompiledName = "copyOfStruct" ->
    //     Call(Lambda([], Let(mfv.CompiledName) :: translateS body))
    //     translate body |> _.ReplaceVar("copyOfStruct", translate value)
    | P.Let ((mfv, value, dbg), body) ->
        let var = Let(mfv.CompiledName, translate value) 
        let cppBody = var :: translateS body
        let withReturn = addReturn cppBody
        Call(Lambda([],  withReturn), [])
    | _ -> failwith $"Unrecognized %A{e}"
    
and translateS (e: FSharpExpr) : CppStmt list =
    match e with
    | P.Let ((mfv, exp, dbg), body) ->
        let value = translate exp
        let name = mfv.CompiledName
        [
            Let (name, value)
            yield! translateS body
        ]
    | P.ValueSet (mfv, value) ->
        [ Assign (Var mfv.CompiledName, translate value) ]
    | P.Sequential (a, b) ->
        translateS a @ translateS b
    | P.FSharpFieldSet (Some dest, ty, field, value) ->
        [ Assign (GetField (translate dest, fieldName field), translate value) ]
    | _ ->
        [ Exp (translate e) ]
let funTyConvert (t: FSharpType) =
    let a = t.GenericArguments[0]
    let b = t.GenericArguments[1]
    let args =
        if isUnit a then ""
        else $"{printType (tyConvert a)}"
    let rt = printType (tyConvert b)
    Gen ("std::function", [ Named $"{rt}({args})" ])
let tyConvert (t: FSharpType) =
    match t.ErasedType.BasicQualifiedName with
    | "System.Int32" -> Int
    | qn ->
        if t.IsFunctionType then funTyConvert t
        elif isUnit t then Ast.Void
        else Auto
