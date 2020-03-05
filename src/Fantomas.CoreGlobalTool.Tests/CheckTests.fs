module Fantomas.CoreGlobalTool.Tests.CheckTests

open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers

[<Literal>]
let NeedsFormatting = """module A

let a =       5
let b= a +      123
"""

[<Literal>]
let WithErrors = """le a 2"""

[<Literal>]
let CorrectlyFormated = """module A

"""

[<Test>]
let ``formatted files should report exit code 0``() =
    use fileFixture = new TemporaryFileCodeSample(CorrectlyFormated)
    let exitCode = checkCode fileFixture.Filename
    exitCode |> should equal 0

[<Test>]
let ``invalid files should report exit code 1``() =
    use fileFixture = new TemporaryFileCodeSample(WithErrors)
    let exitCode = checkCode fileFixture.Filename
    exitCode |> should equal 1

[<Test>]
let ``files that need formatting should report exit code 99``() =
    use fileFixture = new TemporaryFileCodeSample(NeedsFormatting)
    let exitCode = checkCode fileFixture.Filename
    exitCode |> should equal 99
