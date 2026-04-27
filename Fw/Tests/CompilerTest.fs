module Fw.Tests.Compiler

open System.IO
open Fw.Compiler

open Xunit

[<Fact>]
let ``hello world`` () =
  let src = "System.Console.WriteLine(\"Hello, world!\")"
  let cc = CppCompiler ()
  let code = cc.Compile src
  printfn $"{src}\n{code}\n=========="

[<Fact>]
let ``basic module let values`` () =
  let sourceCode =
    "
let a = 40
let b = 2
"

  let cc = CppCompiler ()
  let code = cc.Compile sourceCode
  printfn $"{sourceCode}\n{code}\n=========="

[<Fact>]
let ``basic arithmetic inside a module let value`` () =
  let sourceCode =
    "
let a = 40
let b = 2
let c = a + b
"

  let cc = CppCompiler ()
  let code = cc.Compile sourceCode
  printfn $"{sourceCode}\n{code}\n=========="


[<Fact>]
let ``basic function`` () =
  let sourceCode =
    "
let add x y = x + y
let add3 x y z = x + y + z
"

  let cc = CppCompiler ()
  let code = cc.Compile sourceCode
  printfn $"{sourceCode}\n{code}\n=========="

[<Fact>]
let ``basic instance method`` () =
  let sourceCode =
    "
type Adder() =
    do System.Console.WriteLine(string 1)
    member this.Add x y = x + y
    member this.Add2 (x, y) = x + y
type AdderWithN(n: int) =
    do System.Console.WriteLine(string 1)
    member this.Add x y = x + y + n
    member this.Add2 (x, y) = x + y + n
"

  let cc = CppCompiler ()
  let code = cc.Compile sourceCode
  printfn $"{sourceCode}\n{code}\n=========="
  let fullCode = Deps.core + "\n" + code |> Format.source

  File.WriteAllText (
    "/Users/sbeew/repos/Fwgpu/Fw/cpp/simpleclass.cpp",
    fullCode
  )

[<Fact>]
let ``nested module`` () =
  let sourceCode =
    "
module Bar =
    module Baz =
        let add x y = x + y
let n = Bar.Baz.add 40 2
System.Console.WriteLine(string n)
"

  let cc = CppCompiler ()
  let code = cc.Compile sourceCode
  printfn $"{sourceCode}\n{code}\n=========="
