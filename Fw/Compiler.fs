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

  let core =
    let path = TextFile.``standard_library.cpp``.Path

    if File.Exists path then
      File.ReadAllText path
    else
      TextFile.``standard_library.cpp``.Text

  let corePath = TextFile.``standard_library.cpp``.Path

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
    | FSharpCheckFileAnswer.Succeeded(res) ->
      return parseFileResults, res
    | res -> return failwithf "Parsing did not finish... (%A)" res
  }

type Logger() =
  let log = ResizeArray()
  member this.Info (info: string) = fun data -> log.Add data
  member this.Info (data: obj list) = log.Add data

  member this.Info<'t> (data: 't list) =
    data |> List.map box |> this.Info

type FsMfv = FSharpMemberOrFunctionOrValue

type CppCompiler() =
  let log = Logger()

  let checker = FSharpChecker.Create(keepAssemblyContents = true)

  let argFromField (field: FSharpField) =
    field.Name, Transform.tyConvert field.FieldType

  let argFromParam (p: FSharpParameter) =
    p.FullName, Transform.tyConvert p.Type

  let argFromMfv (mfv: FsMfv) =
    mfv.CompiledName, Transform.tyConvert mfv.FullType

  let argsFromFunction (mfv: FsMfv) =
    // todo : Properly uncurry
    mfv.CurriedParameterGroups
    |> Seq.collect id
    |> Seq.map argFromParam
    |> Seq.toList
    // If the first argument is an empty unit arg just remove it
    |> function
      | [ ("", Ast.Void) ] -> []
      // | ("", Ast.Void) :: rest -> rest
      | args -> args

  member this.Compile code = this.Compile("test.fs", code)

  member this.Compile (filename: string, code: string) =
    let parsed, file =
      parseAndTypeCheckSingleFile
        checker
        filename
        (SourceText.ofString code)
      |> Async.RunSynchronously

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
    | FsImplFileDecl.InitAction action ->
      this.ProcessInitAction action

  member private this.ProcessEntity entity declarations =
    if entity.IsFSharpModule then
      this.Module entity declarations
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

    let decls = [
      yield! fields
      yield! ctors
      yield! members
      for decl in declarations do
        this.ProcessDecl decl
    ]

    Ast.Class {
      name = entity.CompiledName
      inherits = []
      decls = decls
    }

  member private this.MethodSig mfv =
    let fname = mfv.CompiledName

    let args =
      mfv.CurriedParameterGroups
      |> Seq.map Seq.toList
      |> Seq.collect id
      |> Seq.toList
      |> List.map (fun p -> (p.FullName, Transform.tyConvert p.Type))

    let rt = Transform.tyConvert mfv.ReturnParameter.Type
    Ast.Function($"{fname}", { rt = rt; args = args }, None)

  member private this.ProcessInitAction action =
    let body = Transform.translateS action
    let r = action.Range
    let tag = $"__init_action_{r.StartLine}_{r.StartColumn}"
    // We represent init actions with static objects with a constructor and
    // then create a default variable afterwards
    Ast.Sequence [
      Ast.Struct {
        name = tag
        inherits = []
        decls = [ Ast.Constructor(tag, [], Some body) ]
      }
      Ast.Variable(tag, Ast.Named tag, None)
    ]

  member private this.ProcessMfv mfv curriedArgs body =
    let stmts =
      if Transform.isUnit body.Type then
        Transform.translateS body
      elif
        not body.Type.IsFunctionType && Transform.isUnit body.Type
      then
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
      this.Value mfv body
    elif mfv.IsConstructor then
      this.Constructor mfv curriedArgs body
    elif mfv.IsMember && mfv.IsFunction then
      // todo : Member function?
      this.Function mfv curriedArgs body
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
    let args = argsFromFunction mfv

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
        $"{className}::{mfv.CompiledName}"
      else
        mfv.CompiledName

    let fn = Ast.Function(name, { args = args; rt = rt }, Some stmts)

    if mfv.GenericParameters.Count > 0 then
      let templateArgs =
        mfv.GenericParameters
        |> Seq.map (fun p -> $"typename {p.Name}")
        |> String.concat ", "

      Ast.Template(templateArgs, fn)
    else
      fn

  member private this.Value mfv body =
    let ty = Transform.tyConvert mfv.FullType
    let value = Transform.translate body
    Ast.Variable(mfv.CompiledName, ty, Some value)
