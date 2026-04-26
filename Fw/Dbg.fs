module Fw.Dbg

open System
open FSharp.Compiler.Symbols

module P = FSharpExprPatterns

let rec shortPrint depth (e: FSharpExpr) =
    for i in 1..depth * 2 do Console.Write "."
    match e with
    | P.Call (None, mfv, [], [], []) ->
        match mfv.ApparentEnclosingEntity with
        | Some ent ->
            $"(Module Var?) {ent.BasicQualifiedName}.{mfv}"
        | None ->
            $"(Module Var?) {mfv}"
    | P.Call (callee, mfv, xs, ys, args) ->
        match callee with
        | Some e -> $"Call {mfv}"
        | None -> $"Call {mfv}"
    | P.Const (o, t) when t.BasicQualifiedName = "Microsoft.FSharp.Core.unit" ->
        $"Const ()"
    | P.Const (o, t) ->
        $"Const {o}"
    | P.Value mfv ->
        $"Value {mfv}"
    | P.Sequential (a, b) ->
        $"(Sequential)"
    | P.NewObject (mfv, tys, args) ->
        $"NewObject {mfv}"
    | P.FSharpFieldSet (e, t, field, v) ->
        $"FieldSet {field.Name}"
    | P.ThisValue t ->
        $"ThisValue {t.BasicQualifiedName}"
    | P.Let ((mfv, e, dbgPt), body) ->
        if not mfv.IsMutable then
            $"Let {mfv}"
        else
            $"Let mut {mfv}"
    | P.FSharpFieldGet (e, t, field) ->
        $"FieldGet {field.Name}"
    | P.Lambda (mfv, body) ->
        $"Lambda {mfv}"
    | P.ValueSet (mfv, e) ->
        $"ValueSet {mfv}"
    | P.NewRecord (t, fields) ->
        $"New Record: {t.BasicQualifiedName}"
    | P.DefaultValue t ->
        $"Default Value {t.BasicQualifiedName}"
    | _ ->
        failwith $"Unrecognized expression %A{e}"
    |> printfn "%s"
let rec translate depth (e: FSharpExpr) =
    for i in 1..(depth * 2) do Console.Write "."
    match e with
    | P.Call (None, mfv, [], [], []) ->
        match mfv.ApparentEnclosingEntity with
        | Some ent ->
            printfn $"Call (Var?) {ent.BasicQualifiedName}.{mfv}"
        | None ->
            printfn $"Call (Var?) {mfv}"
    | P.Const (o, t) when t.BasicQualifiedName = "Microsoft.FSharp.Core.unit" ->
        printfn $"Const ()"
    | P.Const (o, t) ->
        printfn $"Const {o}"
    | P.Call (callee, mfv, xs, ys, args) ->
        printfn $"Call {mfv}"
        match callee with
        | Some e -> translate (depth + 1) e
        | None -> ()
        for arg in args do
            translate (depth + 1) arg
    | P.Value mfv ->
        printfn $"Value {mfv}"
    | P.Sequential (a, b) ->
        printfn $"(Sequential)"
        translate depth a
        translate depth b
    | P.NewObject (mfv, tys, args) ->
        printfn $"NewObject {mfv}"
        for arg in args do
            translate (depth + 1) arg
    | P.FSharpFieldSet (e, t, field, v) ->
        printfn $"FieldSet {field.Name}"
        match e with
        | Some e -> translate (depth + 1) e
        | None -> failwith "Static field?"
        translate (depth + 1) v
    | P.ThisValue t ->
        printfn $"ThisValue {t.BasicQualifiedName}"
    | P.Let ((mfv, e, dbgPt), body) ->
        if not mfv.IsMutable then
            printfn $"Let {mfv}"
        else
            printfn $"Let mut {mfv}"
        translate (depth + 1) e
        translate depth body
    | P.FSharpFieldGet (e, t, field) ->
        match e with
        | Some e -> printfn $"FieldGet {field.Name}"; translate (depth + 1) e
        | None -> failwith "Static field?"
    | P.Lambda (mfv, body) ->
        printfn $"Lambda {mfv}"
        translate (depth + 1) body
    | P.ValueSet (mfv, e) ->
        printfn $"ValueSet {mfv}"
        translate (depth + 1) e
    | P.NewRecord (t, fields) ->
        printfn $"New Record: {t.BasicQualifiedName}"
        for field in fields do
            translate (depth + 1) field
    | P.DefaultValue t ->
        printfn $"Default Value {t.BasicQualifiedName}"
    | _ ->
        failwith $"Unrecognized expression %A{e}"
