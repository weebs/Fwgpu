module Fw.Tests.Compiler

open Fw.Compiler

open Xunit

[<Fact>]
let ``basic module let values`` () =
    let sourceCode = "
let a = 40
let b = 2
"
    let cc = CppCompiler()
    let code = cc.Compile sourceCode
    printfn $"{sourceCode}\n{code}\n=========="

[<Fact>]
let ``basic arithmetic inside a module let value`` () =
    let sourceCode = "
let a = 40
let b = 2
let c = a + b
"
    let cc = CppCompiler()
    let code = cc.Compile sourceCode
    printfn $"{sourceCode}\n{code}\n=========="


[<Fact>]
let ``basic function`` () =
    let sourceCode = "
let add x y = x + y
"
    let cc = CppCompiler()
    let code = cc.Compile sourceCode
    printfn $"{sourceCode}\n{code}\n=========="