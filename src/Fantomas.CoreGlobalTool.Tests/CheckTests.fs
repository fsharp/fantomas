module Fantomas.CoreGlobalTool.Tests.CheckTests

open System.IO
open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers

[<Literal>]
let NeedsFormatting =
    """module A

let a =       5
let b= a +      123
"""

[<Literal>]
let WithErrors = """let a ="""

[<Literal>]
let CorrectlyFormatted =
    """module A

"""

[<Test>]
let ``formatted files should report exit code 0`` () =
    use fileFixture =
        new TemporaryFileCodeSample(CorrectlyFormatted)

    let { ExitCode = exitCode } = checkCode [ fileFixture.Filename ]
    exitCode |> should equal 0

[<Test>]
let ``invalid files should report exit code 1`` () =
    use fileFixture = new TemporaryFileCodeSample(WithErrors)
    let { ExitCode = exitCode } = checkCode [ fileFixture.Filename ]
    exitCode |> should equal 1

[<Test>]
let ``files that need formatting should report exit code 99`` () =
    use fileFixture =
        new TemporaryFileCodeSample(NeedsFormatting)

    let { ExitCode = exitCode } = checkCode [ fileFixture.Filename ]
    exitCode |> should equal 99

[<Test>]
let ``check with Program.fs file`` () =
    let codeSnippet =
        """[<EntryPoint>]
let main _ = 0
"""

    use fileFixture =
        new TemporaryFileCodeSample(codeSnippet, fileName = "Program")

    let { ExitCode = exitCode } = checkCode [ fileFixture.Filename ]
    exitCode |> should equal 0

[<Test>]
let ``check with different line endings`` () =
    let codeSnippet =
        """let a =
    // some comment
    42
"""

    let snippetWithOtherLineEndings =
        if codeSnippet.Contains("\r\n") then
            codeSnippet.Replace("\r\n", "\n")
        else
            codeSnippet.Replace("\n", "\r\n")

    use fileFixture =
        new TemporaryFileCodeSample(snippetWithOtherLineEndings)

    let { ExitCode = exitCode } = checkCode [ fileFixture.Filename ]
    exitCode |> should equal 0

[<Test>]
let ``check with multiple files`` () =
    use fileFixtureOne =
        new TemporaryFileCodeSample("let a =  0")

    use fileFixtureTwo = new TemporaryFileCodeSample("let b = 1")

    let { ExitCode = exitCode; Output = output } =
        checkCode [ fileFixtureOne.Filename
                    fileFixtureTwo.Filename ]

    exitCode |> should equal 99

    let needsFormatting =
        sprintf "%s needs formatting" (Path.GetFileName(fileFixtureOne.Filename))

    output |> should contain needsFormatting

[<Test>]
let ``check with file and folder`` () =
    use fileFixtureOne =
        new TemporaryFileCodeSample("let a =  0", subFolders = Array.singleton "sub")

    use fileFixtureTwo = new TemporaryFileCodeSample("let b = 1")

    let { ExitCode = exitCode; Output = output } =
        checkCode [ fileFixtureOne.Filename
                    fileFixtureTwo.Filename ]

    exitCode |> should equal 99

    let needsFormatting =
        sprintf "sub%c%s needs formatting" Path.DirectorySeparatorChar (Path.GetFileName(fileFixtureOne.Filename))

    output |> should contain needsFormatting

[<Test>]
let ``honor ignore file when processing a folder`` () =
    let fileName = "A"

    let subFolders =
        System.Guid.NewGuid().ToString("N")
        |> Array.singleton

    use ignoreFixture =
        new TemporaryFileCodeSample("let a =  0", fileName = fileName, subFolders = subFolders)

    use inputFixture = new FantomasIgnoreFile("*.fsx")

    let { Output = output } =
        runFantomasTool (sprintf "--check .%c%s" Path.DirectorySeparatorChar subFolders.[0])

    output |> should not' (contain "ignored")
