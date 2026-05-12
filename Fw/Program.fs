module Fw.Program

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open System.Collections.Generic
open Compiler
open FSharp.Data.LiteralProviders


let sourceCode = File.ReadAllText TextFile.``foo.fsx``.Path

let cc = CppCompiler()
let result =
    Tests.Compiler.compileAndRunCode "foo" sourceCode
printfn $"{result.output}"
// let output = Path.Join(__SOURCE_DIRECTORY__, "cpp/foo.cpp")
// File.WriteAllText(output, "#include \"../standard_library.cpp\"\n" + code)