module Fw.Program

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open System.Collections.Generic
open Compiler


let sourceCode = "
let a = 40
let b = 2
let c = a + b
"

let cc = CppCompiler()
let code = cc.Compile(sourceCode)
let output = Path.Join(__SOURCE_DIRECTORY__, "cpp/foo.cpp")
File.WriteAllText(output, code)