module Fw.Program

open System
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open System.Collections.Generic

// open FSharpSymbolPatterns
module P = FSharpExprPatterns

let sourceCode = "
module Bar =
    let bar = 42
System.Console.WriteLine(string Bar.bar)
module Counter =
    let create () =
        let mutable n = 0
        fun () ->
            n <- n + 1
            n
// type Generic<'t>(x: 't) as doot =
type Generic<'t>(x: 't) =
    member this.Print() = System.Console.WriteLine(x.ToString())
    member this.CallsPrint() = this.Print()
module A =
    let f = Generic(1f)
let add x y = x + y
let result = add 40 2
System.Console.WriteLine(result)
let doot = Generic(1234)
doot.Print()
System.Console.WriteLine(doot.ToString())
A.f.Print()
A.f.Print()
"

// let parseAndTypeCheckSingleFile (file, input) =
type FSharpChecker with
    member this.parseAndTypeCheckSingleFile (file, input) = async {
        // Get context representing a stand-alone (script) file
        let! projOptions, errors = 
            this.GetProjectOptionsFromScript(file, input, assumeDotNetFramework=false)

        let! parseFileResults, checkFileResults = 
            this.ParseAndCheckFileInProject(file, 0, input, projOptions) 

        // Wait until type checking succeeds (or 100 attempts)
        match checkFileResults with
        | FSharpCheckFileAnswer.Succeeded(res) -> return parseFileResults, res
        | res -> return failwithf "Parsing did not finish... (%A)" res
    }

let checker = FSharpChecker.Create(keepAssemblyContents=true)
let parsed, file = checker.parseAndTypeCheckSingleFile("foo.fs", SourceText.ofString sourceCode) |> Async.RunSynchronously
        
type Ent() =
    member this.AddEnt () = ()
    member this.AddMember () = ()
    member this.AddInit () = ()

let rec stepDecl (depth: int) (decl: FSharpImplementationFileDeclaration) : Ast.CppDecl list =
    for i in 1..depth * 2 do Console.Write "."
    match decl with
    | FSharpImplementationFileDeclaration.Entity(entity, declarations) ->
        if entity.IsFSharpModule then
            printfn $"Module {entity.CompiledName}"
            [
                Ast.Namespace (entity.CompiledName, [
                    for decl in declarations do
                        yield! stepDecl (depth + 1) decl
                ])
            ]
        else
            let fields = entity.FSharpFields
            let members = entity.MembersFunctionsAndValues
            printfn $"Class {entity.CompiledName}"
            [
                for decl in declarations do
                    yield! stepDecl (depth + 1) decl
            ]
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(mfv, curriedArgs, body) ->
        if mfv.IsValue then
            printfn $"Mfv Value {mfv}"
        elif mfv.IsConstructor then
            printfn $"Mfv Constructor {mfv}"
        elif mfv.IsMember && mfv.IsFunction then
            printfn $"Mfv Member Function? {mfv}"
        elif mfv.IsFunction then
            printfn $"Mfv Function {mfv}"
        elif mfv.IsTypeFunction then
            printfn $"Mfv Type Function {mfv}"
        else
            printfn $"MemberOrFunctionOrValue {mfv}: %A{curriedArgs} -> {mfv.ReturnParameter.Type}"
        Walk.visitExpr Dbg.shortPrint (depth + 1) body
        let stmts =
            if body.Type.ErasedType.BasicQualifiedName = "Microsoft.FSharp.Core.Unit" then
                Transform.translateS body
            elif not body.Type.IsFunctionType && body.Type.BasicQualifiedName = "Microsoft.FSharp.Core.unit" then
                Transform.translateS body
            else
                Transform.translateS body |> Ast.addReturn
        printfn $"%A{stmts}"
        // let output = (result |> List.map Ast.printStmt |> String.concat ";") + ";"
        printfn $"{Ast.printBody stmts}"
        let rt = Transform.tyConvert body.Type
        // translate (depth + 1) body
        [
            Ast.Function (mfv.CompiledName, [], rt, stmts)
        ]
    | FSharpImplementationFileDeclaration.InitAction action ->
        printfn $"Init action"
        // translate (depth + 1) action
        Walk.visitExpr Dbg.shortPrint (depth + 1) action
        let result = Transform.translateS action
        printfn $"%A{result}"
        printfn $"{Ast.printBody result}"
        let r = action.Range
        let tag = $"__init_action_{r.StartLine}_{r.StartColumn}"
        [
            Ast.Struct {
                name = tag; inherits = []; decls = []; fields = []
                constructors = [
                    ([], result)
                ]
            }
            Ast.Variable (tag, Ast.Named tag, None)
        ]

printfn $"{file.ImplementationFile.Value.QualifiedName} ({file.ImplementationFile.Value.FileName})"
let items = ResizeArray()
for decl in file.ImplementationFile.Value.Declarations do
    for item in stepDecl 0 decl do
        items.Add item

module Format =
    open CliWrap
    open CliWrap.Buffered
    let source (src: string) : string =
        let cmd =
            Cli.Wrap("clang-format")
                .WithStandardInputPipe(PipeSource.FromString src)
        let result = cmd.ExecuteBufferedAsync().Task.Result
        result.StandardOutput
let cppFile = Seq.toList items |> List.map Ast.printDecl |> String.concat "\n"
let formatted = Format.source cppFile
printfn $"{formatted}"
printfn "Done"
