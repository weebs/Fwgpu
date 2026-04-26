module Fw.Compiler

open FSharp.Compiler.CodeAnalysis
open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open System.Collections.Generic

module Format =
    open CliWrap
    open CliWrap.Buffered
    let source (src: string) : string =
        let cmd =
            Cli.Wrap("clang-format")
                .WithStandardInputPipe(PipeSource.FromString src)
        let result = cmd.ExecuteBufferedAsync().Task.Result
        result.StandardOutput

let parseAndTypeCheckSingleFile (checker: FSharpChecker) (file) (input) = async {
    // Get context representing a stand-alone (script) file
    let! projOptions, errors = 
        checker.GetProjectOptionsFromScript(file, input, assumeDotNetFramework=false)

    let! parseFileResults, checkFileResults = 
        checker.ParseAndCheckFileInProject(file, 0, input, projOptions) 

    // Wait until type checking succeeds (or 100 attempts)
    match checkFileResults with
    | FSharpCheckFileAnswer.Succeeded(res) -> return parseFileResults, res
    | res -> return failwithf "Parsing did not finish... (%A)" res
}

type Logger() =
    let log = ResizeArray()
    member this.Info (info: string) = fun data -> log.Add data
    member this.Info (data: obj list) = log.Add data
    member this.Info<'t>(data: 't list) = data |> List.map box |> this.Info

type CppCompiler() =
    let log = Logger()
    let checker = FSharpChecker.Create(keepAssemblyContents=true)

    member this.Compile (code: string) =
        let parsed, file = 
            parseAndTypeCheckSingleFile checker "foo.fs" (SourceText.ofString code)
            |> Async.RunSynchronously
        let decls = file.ImplementationFile.Value.Declarations
        let items = ResizeArray()
        for decl in decls do
            let cppDecls = this.ProcessDecl decl
            for cppDecl in cppDecls do
                items.Add cppDecl
        let output = items |> Seq.toList |> List.map Ast.printDecl |> String.concat "\n"
        let formatted = Format.source output
        formatted
    member private this.ProcessDecl decl =
        match decl with
        | FSharpImplementationFileDeclaration.Entity(entity, declarations) ->
            this.ProcessEntity entity declarations
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (mfv, curriedArgs, body) ->
            this.ProcessMfv mfv curriedArgs body
        | FSharpImplementationFileDeclaration.InitAction action ->
            this.ProcessInitAction action
    member private this.ProcessEntity entity declarations =
        if entity.IsFSharpModule 
        then this.Module entity declarations
        else this.Class entity declarations
    member private this.Module entity declarations =
        [
            Ast.Namespace (entity.CompiledName, [
                for decl in declarations do
                    yield! this.ProcessDecl decl
            ])
        ]
    member private this.Class entity declarations =
        let fields = entity.FSharpFields
        let members = entity.MembersFunctionsAndValues
        printfn $"Class {entity.CompiledName}"
        [
            for decl in declarations do
                yield! this.ProcessDecl decl
        ]

    member private this.ProcessInitAction action =
        let body = Transform.translateS action
        let r = action.Range
        let tag = $"__init_action_{r.StartLine}_{r.StartColumn}"
        // We represent init actions with static objects with a constructor and 
        // then create a default variable afterwards
        [
            Ast.Struct {
                name = tag; inherits = []; decls = []; fields = []
                constructors = [
                    ([], body)
                ]
            }
            Ast.Variable (tag, Ast.Named tag, None)
        ]
    member private this.ProcessMfv mfv curriedArgs body =
        let stmts =
            if body.Type.ErasedType.BasicQualifiedName = "Microsoft.FSharp.Core.Unit" then
                Transform.translateS body
            elif not body.Type.IsFunctionType && body.Type.BasicQualifiedName = "Microsoft.FSharp.Core.unit" then
                Transform.translateS body
            else
                Transform.translateS body |> Ast.addReturn
        log.Info stmts
        let rt = Transform.tyConvert body.Type
        let funcName =
            if mfv.IsMember && mfv.IsFunction then
                Transform.qualifiedPath mfv
            else
                mfv.CompiledName

        if mfv.IsValue then
            this.Value mfv curriedArgs body
        else
            [
                if mfv.IsConstructor then
                    Ast.Comment $"Mfv Constructor {mfv}"
                elif mfv.IsMember && mfv.IsFunction then
                    Ast.Comment $"Mfv Member Function? {mfv}"
                elif mfv.IsFunction then
                    Ast.Comment $"Mfv Function {mfv}"
                elif mfv.IsTypeFunction then
                    Ast.Comment $"Mfv Type Function {mfv}"
                else
                    Ast.Comment $"MemberOrFunctionOrValue {mfv}: %A{curriedArgs} -> {mfv.ReturnParameter.Type}"
                // Ast.Function (mfv.CompiledName, [], rt, stmts)
                Ast.Function (funcName, [], rt, stmts)
            ]
    member private this.Value mfv args body =
        let ty = Transform.tyConvert mfv.FullType
        let value = Transform.translate body
        [
            Ast.Comment $"Mfv Value {mfv}"
            Ast.Variable (mfv.CompiledName, ty, Some value)
        ]
