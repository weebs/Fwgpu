module Fw.Tests.Compiler

open System.IO
open Fw.Compiler

open Xunit
open Xunit.Abstractions

let compileAndRunCode (testName: string) (src: string) =
  let cc = CppCompiler()
  let code = cc.Compile src

  let fullCode =
    // Deps.core + "\n" + code + "int main() { return 0; }"
    "#include \"../standard_library.cpp\"\n"
    + code
    + "int main() { return 0; }"
    |> Format.source

  let dir =
    Path.GetDirectoryName(
      FSharp.Data.LiteralProviders.TextFile.``standard_library.cpp``.Path
    )

  // File.WriteAllText(
  //   Path.Join(dir, "/standard_library.cpp"),
  //   Deps.core
  // )

  let o = Path.Join(dir, $"/cpp/{testName}")
  let outPath = o + ".cpp"
  File.WriteAllText(outPath, fullCode)

  CliWrap.Cli
    // .Wrap("clang++")
    // .WithArguments([ outPath; "-o"; o ])
    .Wrap("zig")
    .WithArguments(
      [ "c++"; "-std=c++17"; "-g"; outPath; "-o"; o ]
    )
    .ExecuteAsync()
    .Task.Result
  |> ignore

  let sb = System.Text.StringBuilder()

  CliWrap.Cli
    .Wrap(o)
    .WithStandardOutputPipe(
      CliWrap.PipeTarget.ToStringBuilder sb
    )
    .ExecuteAsync()
    .Task.Result
  |> ignore

  {|
    code = Format.source code
    output = sb.ToString()
  |}

type TestClass(xunit: ITestOutputHelper) =
  [<Fact>]
  let ``hello world`` () =
    let src = "System.Console.WriteLine(\"Hello, world!\")"

    let result = compileAndRunCode "hello_world" src
    xunit.WriteLine result.code
    Assert.Equal("Hello, world!\n", result.output)

  [<Fact>]
  let ``basic module let values`` () =
    let sourceCode =
      "
let a = 40
let b = 2
"

    let cc = CppCompiler()
    let code = cc.Compile sourceCode
    xunit.WriteLine $"{sourceCode}\n{code}\n=========="

  [<Fact>]
  let ``basic arithmetic inside a module let value`` () =
    let sourceCode =
      "
let a = 40
let b = 2
let c = a + b
  "

    let cc = CppCompiler()
    let code = cc.Compile sourceCode
    xunit.WriteLine $"{sourceCode}\n{code}\n=========="


  [<Fact>]
  let ``basic function`` () =
    let sourceCode =
      "
let add x y = x + y
let add3 x y z = x + y + z
  "

    let cc = CppCompiler()
    let code = cc.Compile sourceCode
    xunit.WriteLine $"{sourceCode}\n{code}\n=========="

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

    let cc = CppCompiler()
    let code = cc.Compile sourceCode
    xunit.WriteLine $"{sourceCode}\n{code}\n=========="

    let fullCode = Deps.core + "\n" + code |> Format.source

    File.WriteAllText(
      "/Users/sbeew/repos/Fwgpu/Fw/cpp/simpleclass.cpp",
      fullCode
    )

  [<Fact>]
  let ``basic class`` () =
    let sourceCode =
      "
type Adder(n: int) =
  member this.Add (x, y) = x + y + n
  // member this.Add x y = x + y + n
let adder = Adder(40)
let result = adder.Add(1, 1)
System.Console.WriteLine result
  "

    let result = compileAndRunCode "basic_class" sourceCode

    xunit.WriteLine result.code
    Assert.Equal("42\n", result.output)

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

    let cc = CppCompiler()
    let code = cc.Compile sourceCode
    xunit.WriteLine $"{sourceCode}\n{code}\n=========="

  [<Fact>]
  let ``idisposable smoke test`` () =
    let src =
      "
type Foo() =
  interface System.IDisposable with
    member this.Dispose() =
      System.Console.WriteLine(\"Disposing...\")
//type [<Struct>] FooStruct() =
  //interface System.IDisposable with
    //member this.Dispose() = ()
let main () =
  use f = new Foo()
  //use fs = new FooStruct()
  System.Console.WriteLine(string f)
  //System.Console.WriteLine(string fs)
  0
main ()
  "

    let result = compileAndRunCode "idisposable" src
    xunit.WriteLine result.code

  [<Fact>]
  let ``list collection`` () =
    let src =
      "
let xs = ResizeArray()
let mutable sum = 0
for i in 1..10 do 
    xs.Add (i * 2)
for n in xs do
    sum <- sum + n

let ope () =
  for n in xs do
    System.Console.WriteLine n

System.Console.WriteLine(sum)
"

    let result = compileAndRunCode "list collection" src
    xunit.WriteLine result.code

  [<Fact>]
  let ``basic struct`` () =
    let src =
      "
type [<Struct>] Foo(n: int) =
  member this.Add x = x + n
let f = Foo(40)
System.Console.WriteLine(f.Add(2))"

    let result = compileAndRunCode "basic_struct" src
    xunit.WriteLine result.code

  [<Fact>]
  let ``id works`` () =
    let result = compileAndRunCode "id_works" "let id x = x"

    xunit.WriteLine result.code
