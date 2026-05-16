module rec Fw.Type

open FSharp.Compiler.Symbols
open Fw.Ast

let isUnit (t: FSharpType) =
    try
        if not t.HasTypeDefinition then
            false
        elif t.TypeDefinition.IsByRef then
            false
        elif t.IsFunctionType then
            false
        elif t.IsGenericParameter then
            false
        elif t.ErasedType.BasicQualifiedName = "Microsoft.FSharp.Core.Unit" then
            true
        elif t.BasicQualifiedName = "Microsoft.FSharp.Core.unit" then
            true
        else
            false
    with ex ->
        printfn $"{ex}"
        false

let replaceIncludedBCLNamespaces (path: string) =
    path
        .Replace("Microsoft::FSharp::Core::Operators", "")
        .Replace(
            "Microsoft::FSharp::Core::LanguagePrimitives::IntrinsicFunctions",
            ""
        )
        .Replace("Microsoft::FSharp::Core::LanguagePrimitives", "")
        .Replace("Microsoft::FSharp::Collections", "")

let cppPath (s: string) =
    s.Replace("+", "::").Replace(".", "::").Replace("`", "_")
    |> replaceIncludedBCLNamespaces

let genericName (t: FSharpType) =
    let mutable types = t.GenericArguments |> Seq.toList
    let s = t.BasicQualifiedName
    let parts = s.Replace("+", ".").Split(".") |> Array.map ref

    for part in parts do
        if part.Value.Contains "`" then
            let ab = part.Value.Split("`")
            let name = ab[0]
            let arity = ab[1] |> System.Int32.Parse

            let txt =
                List.take arity types |> List.map (convert >> printType) |> String.concat ", "

            types <- List.skip arity types
            let app = $"{name}_{arity}<{txt}>"
            part.Value <- app

    let result = parts |> Array.map _.Value |> String.concat "::"
    result

let name (t: FSharpType) =
    let baseTy =
        if t.BasicQualifiedName = "Microsoft.FSharp.Core.obj" then
            "System::Object"
        else
            t.BasicQualifiedName |> cppPath

    if t.GenericArguments.Count = 0 then
        baseTy
    else
        genericName t
    |> replaceIncludedBCLNamespaces

let convert (t: FSharpType) =
    if t.IsGenericParameter then
        Named t.GenericParameter.Name
    elif isUnit t then
        Void
    elif t.IsFunctionType then
        let a = t.GenericArguments[0]
        let b = t.GenericArguments[1]
        Ptr(Gen("::FSharpFunc", [ convert a; convert b ]))
    else
        match t.TypeDefinition.AccessPath with
        | "Microsoft.FSharp.Core" ->
            match t.TypeDefinition.CompiledName with
            | "int" -> Int
            | "int32" -> Int
            | "bool" -> Bool
            | "byref`1" ->
                Named $"{convert t.GenericArguments[0] |> printType}*"
            | "obj" -> Named "System::Object*"
            | "string" -> Named "System::String"
            | "exn" -> Named "System::Exception"
            | _ -> Named $"auto /* {t.TypeDefinition.CompiledName} */"
        | _ ->
            if isUnit t then Void
            else if requiresGc t then Named(name t + "*")
            else Named(name t)
            
let requiresGc (t: FSharpType) =
    try
        if t.IsFunctionType then
            true
        elif t.TypeDefinition.IsValueType then
            false
        elif isUnit t then
            false
        else
            match t.TypeDefinition.AccessPath with
            | "Microsoft.FSharp.Core" ->
                match t.TypeDefinition.CompiledName with
                | "int" -> false
                | "int32" -> false
                | "bool" -> false
                | "byref`1" -> false
                | "obj" -> true
                // todo : printf format
                | "PrintfFormat`5" -> false
                | "string" -> true
                | _ ->
                    printfn
                        $"Warning: Not sure of Microsoft.FSharp.Core type {t.TypeDefinition.CompiledName} in requiresGc"

                    t.TypeDefinition.IsValueType
            | _ -> true
    with ex ->
        printfn $"{ex}"
        reraise ()
