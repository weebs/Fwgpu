module Fw.Tests.FormattingTests

open CliWrap.Buffered
open NUnit.Framework
open CliWrap

[<Test>]
let ``cli`` () =
    let cmd =
        Cli.Wrap("clang-format")
            .WithStandardInputPipe(PipeSource.FromString "int add(int x, int y) {return x + y} int main() { return 0; }")
    let result = cmd.ExecuteBufferedAsync().Task.Result
    let out = result.StandardOutput
    printfn $"{out}"