module Fantomas.Tests.SpaceBeforeLowercaseInvocationTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let noSpaceBefore =
    { config with
          SpaceBeforeLowercaseInvocation = false }

/// Space before () in lowercase function call

[<Test>]
let ``default config should add space before unit in lowercase function call`` () =
    formatSourceString false "let value = myFunction()" config
    |> should
        equal
        """let value = myFunction ()
"""

[<Test>]
let ``spaceBeforeLowercaseInvocation = false, should not add space before unit in lowercase function call`` () =
    formatSourceString false "let value = myFunction()" noSpaceBefore
    |> should
        equal
        """let value = myFunction()
"""

// Space before parentheses (a+b) in lowercase function call

[<Test>]
let ``default config should add space before parentheses in lowercase function call`` () =
    formatSourceString false "let value = myFunction(a+b)" config
    |> should
        equal
        """let value = myFunction (a + b)
"""

[<Test>]
let ``spaceBeforeLowercaseInvocation = false, should not add space before parentheses in lowercase function call`` () =
    formatSourceString false "let value = myFunction(a+b)" noSpaceBefore
    |> should
        equal
        """let value = myFunction(a + b)
"""

[<Test>]
let ``spaceBeforeLowercaseInvocation should not have impact when member is called after unit`` () =
    formatSourceString false "let v1 = myFunction().Member" noSpaceBefore
    |> prepend newline
    |> should
        equal
        """
let v1 = myFunction().Member
"""

[<Test>]
let ``space before lower constructor without new`` () =
    formatSourceString
        false
        """
let tree1 =
    binaryNode(binaryNode(binaryValue 1, binaryValue 2), binaryNode(binaryValue 3, binaryValue 4))
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let tree1 =
    binaryNode (
        binaryNode (binaryValue 1, binaryValue 2),
        binaryNode (binaryValue 3, binaryValue 4)
    )
"""

[<Test>]
let ``space before lower case constructor invocation with new keyword`` () =
    formatSourceString
        false
        """
let person = new person("Jim", 33)

let otherThing =
    new foobar(longname1, longname2, longname3, longname4, longname5, longname6, longname7)
"""
        { config with MaxLineLength = 90 }
    |> prepend newline
    |> should
        equal
        """
let person = new person ("Jim", 33)

let otherThing =
    new foobar (
        longname1,
        longname2,
        longname3,
        longname4,
        longname5,
        longname6,
        longname7
    )
"""

[<Test>]
let ``space before lower member call`` () =
    formatSourceString
        false
        """
let myRegexMatch = Regex.matches(input, regex)

let myRegexMatchLong =
    Regex.matches("my longer input string with some interesting content in it","myRegexPattern")

let untypedRes = checker.parseFile(file, source, opts)

let untypedResLong =
    checker.parseFile(fileName, sourceText, parsingOptionsWithDefines, somethingElseWithARatherLongVariableName)
"""
        { config with MaxLineLength = 90 }
    |> prepend newline
    |> should
        equal
        """
let myRegexMatch = Regex.matches (input, regex)

let myRegexMatchLong =
    Regex.matches (
        "my longer input string with some interesting content in it",
        "myRegexPattern"
    )

let untypedRes = checker.parseFile (file, source, opts)

let untypedResLong =
    checker.parseFile (
        fileName,
        sourceText,
        parsingOptionsWithDefines,
        somethingElseWithARatherLongVariableName
    )
"""

[<Test>]
let ``no space before lowercase member calls and constructors`` () =
    formatSourceString
        false
        """
let tree1 =
    binaryNode(binaryNode(binaryValue 1, binaryValue 2), binaryNode(binaryValue 3, binaryValue 4))

let person = new person("Jim", 33)
let otherThing =
    new foobar(longname1, longname2, longname3, longname4, longname5, longname6, longname7)

let myRegexMatch = Regex.matches(input, regex)

let myRegexMatchLong =
    Regex.matches("my longer input string with some interesting content in it","myRegexPattern")

let untypedRes = checker.parseFile(file, source, opts)

let untypedResLong =
    checker.parseFile(fileName, sourceText, parsingOptionsWithDefines, somethingElseWithARatherLongVariableName)
"""
        { noSpaceBefore with
              MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
let tree1 =
    binaryNode(
        binaryNode(binaryValue 1, binaryValue 2),
        binaryNode(binaryValue 3, binaryValue 4)
    )

let person = new person("Jim", 33)

let otherThing =
    new foobar(
        longname1,
        longname2,
        longname3,
        longname4,
        longname5,
        longname6,
        longname7
    )

let myRegexMatch = Regex.matches(input, regex)

let myRegexMatchLong =
    Regex.matches(
        "my longer input string with some interesting content in it",
        "myRegexPattern"
    )

let untypedRes = checker.parseFile(file, source, opts)

let untypedResLong =
    checker.parseFile(
        fileName,
        sourceText,
        parsingOptionsWithDefines,
        somethingElseWithARatherLongVariableName
    )
"""

[<Test>]
let ``ignore setting when function call is the argument of prefix application`` () =
    formatSourceString
        false
        """
!-String.Empty.padLeft(braceSize + spaceAround)
(!-System.String.Empty.padRight(delta)) ({ ctx with RecordBraceStart = rest })
!- meh()
"""
        config
    |> prepend newline
    |> should
        equal
        """
!- String.Empty.padLeft(braceSize + spaceAround)
(!- System.String.Empty.padRight(delta)) ({ ctx with RecordBraceStart = rest })
!- meh()
"""
