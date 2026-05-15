module Fw.Compiler

open FSharp.Compiler.CodeAnalysis
open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open System.Collections.Generic

module P = FSharpExprPatterns
type FsImplFileDecl = FSharpImplementationFileDeclaration

module Deps =
    open FSharp.Data.LiteralProviders

    let corePath = TextFile.``standard_library.cpp``.Path

    let core =
        if File.Exists corePath then
            File.ReadAllText corePath
        else
            TextFile.``standard_library.cpp``.Text


module Format =
    open CliWrap
    open CliWrap.Buffered

    let source (src: string) : string =
        let cmd =
            Cli
                .Wrap("clang-format")
                .WithStandardInputPipe(PipeSource.FromString src)

        let result = cmd.ExecuteBufferedAsync().Task.Result
        result.StandardOutput

let parseAndTypeCheckSingleFile (checker: FSharpChecker) file input =
    async {
        // Get context representing a stand-alone (script) file
        let! projOptions, errors =
            checker.GetProjectOptionsFromScript(
                file,
                input,
                assumeDotNetFramework = false
            )

        let! parseFileResults, checkFileResults =
            checker.ParseAndCheckFileInProject(file, 0, input, projOptions)

        // Wait until type checking succeeds (or 100 attempts)
        match checkFileResults with
        | FSharpCheckFileAnswer.Succeeded(res) -> return parseFileResults, res
        | res -> return failwithf "Parsing did not finish... (%A)" res
    }

type FsMfv = FSharpMemberOrFunctionOrValue

type CppCompiler() =
    let types = Dictionary()


    let checker = FSharpChecker.Create(keepAssemblyContents = true)

    let argFromField (field: FSharpField) =
        field.Name, Transform.tyConvert field.FieldType

    let argFromParam (p: FSharpParameter) =
        p.FullName, Transform.tyConvert p.Type

    let argFromMfv (mfv: FsMfv) =
        mfv.CompiledName, Transform.tyConvert mfv.FullType

    // TODO : Properly figure out the this argument
    let isThisArgument (mfv: FSharpMemberOrFunctionOrValue) =
        mfv.CompiledName = "this"

    let toArgs (args: FSharpMemberOrFunctionOrValue list list) : Ast.ArgSig =
        args
        |> List.collect id
        |> List.filter (not << isThisArgument)
        |> List.filter (not << Transform.isUnit << _.FullType)
        |> List.map (fun mfv -> mfv.FullName, Transform.tyConvert mfv.FullType)

    let toMethodArgs (args: FSharpParameter IList IList) =
        args
        |> Seq.map Seq.toList
        |> Seq.collect id
        |> Seq.toList
        |> List.filter (not << Transform.isUnit << _.Type)
        |> List.map (fun p -> (p.FullName, Transform.tyConvert p.Type))

    let argsFromFunction (mfv: FsMfv) =
        // todo : Properly uncurry
        mfv.CurriedParameterGroups
        |> Seq.collect id
        |> Seq.map argFromParam
        |> Seq.toList
        // If the first argument is an empty unit arg just remove it
        |> function
            | [ ("", Ast.Void) ] -> []
            | args -> args

    member this.Compile code = this.Compile("test.fs", code)

    member this.Compile (filename: string, code: string) =
        let parsed, file =
            parseAndTypeCheckSingleFile
                checker
                filename
                (SourceText.ofString code)
            |> Async.RunSynchronously

        let rec loop (decl: FSharpImplementationFileDeclaration) =
            match decl with
            | FsImplFileDecl.Entity(ent, decls) ->
                if not (types.ContainsKey(ent.FullName)) then
                    types[ent.FullName] <- ResizeArray()

                for d in decls do
                    loop d
            | FsImplFileDecl.MemberOrFunctionOrValue(mfv, args, body) ->
                match mfv.DeclaringEntity with
                | Some ent ->
                    if not (types.ContainsKey(ent.FullName)) then
                        types[ent.FullName] <- ResizeArray()

                    types[ent.FullName].Add(mfv, args, body)
                | None -> printfn "???"
            | _ -> ()

        List.iter loop file.ImplementationFile.Value.Declarations

        file.ImplementationFile.Value.Declarations
        |> List.map this.ProcessDecl
        |> List.map Ast.printDecl
        |> String.concat "\n"
    // |> Format.source

    member private this.ProcessDecl decl =
        match decl with
        | FsImplFileDecl.Entity(entity, declarations) ->
            this.ProcessEntity entity declarations
        | FsImplFileDecl.MemberOrFunctionOrValue(mfv, curriedArgs, body) ->
            this.ProcessMfv mfv curriedArgs body
        | FsImplFileDecl.InitAction action -> this.ProcessInitAction action

    member private this.ProcessEntity entity declarations =
        if entity.IsFSharpModule then
            this.Module entity declarations
        elif entity.IsInterface then
            this.Interface entity declarations
        else
            this.Class entity declarations

    member private this.Module entity declarations =
        Ast.Namespace(
            entity.CompiledName,
            [
                for decl in declarations do
                    this.ProcessDecl decl
            ]
        )

    member private this.Interface entity declarations =
        Ast.Class {
            name = entity.CompiledName
            decls = [
                for mfv in entity.MembersFunctionsAndValues do
                    let args = toMethodArgs mfv.CurriedParameterGroups
                    let rt = Transform.tyConvert mfv.ReturnParameter.Type
                    let virtualName = mfv.FullName.Replace(".", "_")
                    let argNames = args |> List.map fst
                    Ast.DeletedVirtual(virtualName, { rt = rt; args = args })

                    Ast.Function(
                        mfv.CompiledName,
                        { rt = rt; args = args },
                        Some [
                            Ast.Exp(
                                Ast.Call(
                                    Ast.Var virtualName,
                                    argNames |> List.map Ast.Var
                                )
                            )
                        ]
                    )
            ]
            inherits = [ "virtual System::Object" ]
        }

    member private this.Class entity declarations =
        let fields =
            entity.FSharpFields
            |> Seq.map argFromField
            |> Seq.map (fun (name, ty) -> Ast.Variable(name, ty, None))
            |> Seq.toList

        let ctors =
            entity.MembersFunctionsAndValues
            |> Seq.filter _.IsConstructor
            |> Seq.map argsFromFunction
            |> Seq.map (fun args ->
                Ast.Constructor(entity.CompiledName, args, None))
            |> Seq.toList

        let members =
            entity.MembersFunctionsAndValues
            |> Seq.filter (not << _.IsConstructor)
            |> Seq.toList
            |> List.map this.MethodSig

        let otherMembers = types[entity.FullName]

        let generated =
            otherMembers
            |> Seq.map (fun (mfv, _, _) -> mfv)
            |> Seq.filter _.IsCompilerGenerated
            |> Seq.toList
            |> List.map this.MethodSig

        let inherits =
            match entity.BaseType with
            | Some bt when bt.BasicQualifiedName = "Microsoft.FSharp.Core.obj" -> [
                "virtual System::Object"
              ]
            | Some bt -> [ Transform.typeName bt ]
            | _ -> []

        let recordCtor (entity: FSharpEntity) =
            let args =
                entity.FSharpFields
                |> Seq.toList
                |> List.map (fun f -> (f.Name, Transform.tyConvert f.FieldType))

            Ast.Constructor(
                entity.CompiledName,
                args,
                Some [
                    for name, _ in args do
                        Ast.Assign(
                            Ast.DerefGetField(Ast.Var "this", name),
                            Ast.Var name
                        )
                ]
            )

        let decls = [
            yield! fields
            if entity.IsFSharpRecord then
                recordCtor entity
            yield! ctors
            yield! members
            yield! generated
            for decl in declarations do
                this.ProcessDecl decl
        ]

        // todo : AllInterfaces vs DeclaredInterfaces?
        let interfaces =
            entity.DeclaredInterfaces
            |> Seq.toList
            |> List.map Transform.typeName

        Ast.Class {
            name = entity.CompiledName
            inherits = inherits @ interfaces
            decls = decls
        }

    member private this.MethodSig mfv =
        let fnName =
            if not mfv.IsExplicitInterfaceImplementation then
                mfv.CompiledName
            else
                mfv.CompiledName.Replace(".", "_")

        let args = toMethodArgs mfv.CurriedParameterGroups
        let rt = Transform.tyConvert mfv.ReturnParameter.Type
        Ast.Function($"{fnName}", { rt = rt; args = args }, None)

    member private this.ProcessInitAction action =
        let body = Transform.translateS action
        // Init actions are represented with static IIFE lambdas
        Ast.INIT body

    member private this.ProcessMfv mfv curriedArgs body =
        let stmts =
            if Transform.isUnit body.Type then
                Transform.translateS body
            elif not body.Type.IsFunctionType && Transform.isUnit body.Type then
                Transform.translateS body
            else
                Transform.translateS body |> Ast.addReturn

        let rt = Transform.tyConvert body.Type

        let funcName =
            if mfv.IsMember && mfv.IsFunction then
                Transform.qualifiedPath mfv
            else
                mfv.CompiledName

        if mfv.IsValue then
            let value = Transform.translateVar mfv body
            Ast.Variable(mfv.CompiledName, Ast.Auto, Some value)
        elif mfv.IsConstructor then
            this.Constructor mfv curriedArgs body
        elif mfv.IsMember && mfv.IsFunction then
            // todo : Member function?
            this.Function mfv curriedArgs body
        elif mfv.IsFunction && curriedArgs = [] then
            // Lambda values
            let value = Transform.translateVar mfv body
            let lambda = Ast.Lambda([], true, [ Ast.Return value ], [])
            Ast.Variable(mfv.CompiledName, Ast.Auto, Some(Ast.Call(lambda, [])))
        elif mfv.IsFunction then
            this.Function mfv curriedArgs body
        else
            Ast.Sequence [
                if mfv.IsConstructor then
                    Ast.Comment $"Mfv Constructor {mfv}"
                elif mfv.IsTypeFunction then
                    Ast.Comment $"Mfv Type Function {mfv}"
                else
                    Ast.Comment
                        $"?? MemberOrFunctionOrValue {mfv}: %A{curriedArgs} -> {mfv.ReturnParameter.Type}"
                // Ast.Function (mfv.CompiledName, [], rt, stmts)
                Ast.Function(funcName, { args = []; rt = rt }, Some stmts)
            ]

    member private this.Constructor mfv curriedArgs body =
        let fix =
            match body with
            | P.Sequential(P.NewObject _, rest) -> rest
            | _ -> body

        let args = argsFromFunction mfv
        let stmts = Transform.translateS fix

        let fixStmts = stmts |> List.rev |> List.tail |> List.rev

        let className = $"{mfv.DeclaringEntity.Value.CompiledName}"

        let name = $"{className}::{className}"
        Ast.Constructor(name, args, Some fixStmts)

    member private this.Function mfv curriedArgs body =
        let rt = Transform.tyConvert body.Type
        // let args = argsFromFunction mfv
        let args = toArgs curriedArgs

        let converted = Walk.convert body
        printfn $"%A{converted}"
        // printfn $"%A{Walk.prettyPrintDU converted}"

        let stmts =
            Transform.translateS body
            |> fun stmts ->
                if Transform.isUnit body.Type then
                    stmts
                else
                    Ast.addReturn stmts

        let name =
            if mfv.IsMember then
                // Transform.qualifiedPath mfv
                let className = mfv.DeclaringEntity.Value.CompiledName
                // Replace interface methods "." with "_"
                let funcName = mfv.CompiledName.Replace(".", "_")
                $"{className}::{funcName}"
            else
                mfv.CompiledName

        let fn = Ast.Function(name, { args = args; rt = rt }, Some stmts)

        if mfv.GenericParameters.Count > 0 then
            let templateArgs =
                mfv.GenericParameters
                |> Seq.map (fun p -> $"typename {p.Name}")
                |> Seq.toList

            Ast.Template(templateArgs, fn)
        else
            fn
