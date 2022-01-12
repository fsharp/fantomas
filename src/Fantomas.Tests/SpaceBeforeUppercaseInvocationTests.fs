module Fantomas.Tests.SpaceBeforeUppercaseInvocationTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let spaceBeforeConfig =
    { config with
          SpaceBeforeUppercaseInvocation = true }

/// Space before () in Uppercase function call

[<Test>]
let ``default config should not add space before unit in uppercase function call`` () =
    formatSourceString false "let value = MyFunction()" config
    |> should
        equal
        """let value = MyFunction()
"""

[<Test>]
let ``spaceBeforeUppercaseInvocation should add space before unit in uppercase function call`` () =
    formatSourceString false "let value = MyFunction()" spaceBeforeConfig
    |> should
        equal
        """let value = MyFunction ()
"""

[<Test>]
let ``spaceBeforeUppercaseInvocation should add space before unit in chained uppercase function call`` () =
    formatSourceString false "let value = person.ToString()" spaceBeforeConfig
    |> should
        equal
        """let value = person.ToString ()
"""

// Exception to the rule

[<Test>]
let ``spaceBeforeUppercaseInvocation should not have impact when member is called after unit`` () =
    formatSourceString false "let v2 = OtherFunction().Member" spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
let v2 = OtherFunction().Member
"""

[<Test>]
let ``spaceBeforeUppercaseInvocation should not have impact when member is called after construction invocation, 1401``
    ()
    =
    formatSourceString
        false
        """
let x = DateTimeOffset(2017,6,1,10,3,14,TimeSpan(1,30,0)).LocalDateTime
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
let x =
    DateTimeOffset(
        2017,
        6,
        1,
        10,
        3,
        14,
        TimeSpan (1, 30, 0)
    )
        .LocalDateTime
"""

// Space before parentheses (a+b) in Uppercase function call

[<Test>]
let ``default config should not add space before parentheses in uppercase function call`` () =
    formatSourceString false "let value = MyFunction(a+b)" config
    |> should
        equal
        """let value = MyFunction(a + b)
"""

[<Test>]
let ``spaceBeforeUppercaseInvocation should add space before parentheses in uppercase function call`` () =
    formatSourceString false "let value = MyFunction(a+b)" spaceBeforeConfig
    |> should
        equal
        """let value = MyFunction (a + b)
"""

[<Test>]
let ``space before uppercase function application cannot apply with dot-chaining, 943`` () =
    formatSourceString
        false
        """foo.Bar().[5]
"""
        { config with
              SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
foo.Bar().[5]
"""

[<Test>]
let ``space before uppercase DotIndexedSet`` () =
    formatSourceString
        false
        """foo.Bar().[5] <- 5
"""
        { config with
              SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
foo.Bar().[5] <- 5
"""

[<Test>]
let ``setting SpaceBeforeUppercaseInvocation is not applied in the middle of a invocation chain, 853`` () =
    formatSourceString
        false
        """
module SomeModule =
    let DoSomething (a:SomeType) =
        let someValue = a.Some.Thing("aaa").[0]
        someValue
"""
        { config with
              SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
module SomeModule =
    let DoSomething (a: SomeType) =
        let someValue = a.Some.Thing("aaa").[0]
        someValue
"""

[<Test>]
let ``space before uppercase constructor without new`` () =
    formatSourceString
        false
        """
let tree1 =
    BinaryNode(BinaryNode(BinaryValue 1, BinaryValue 2), BinaryNode(BinaryValue 3, BinaryValue 4))
"""
        { spaceBeforeConfig with
              MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let tree1 =
    BinaryNode (
        BinaryNode (BinaryValue 1, BinaryValue 2),
        BinaryNode (BinaryValue 3, BinaryValue 4)
    )
"""

[<Test>]
let ``space before upper case constructor invocation with new keyword`` () =
    formatSourceString
        false
        """
let person = new Person("Jim", 33)

let otherThing =
    new Foobar(longname1, longname2, longname3, longname4, longname5, longname6, longname7)
"""
        { spaceBeforeConfig with
              MaxLineLength = 90 }
    |> prepend newline
    |> should
        equal
        """
let person = new Person ("Jim", 33)

let otherThing =
    new Foobar (
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
let ``space before uppercase member call`` () =
    formatSourceString
        false
        """
let myRegexMatch = Regex.Match(input, regex)

let myRegexMatchLong =
    Regex.Match("my longer input string with some interesting content in it","myRegexPattern")

let untypedRes = checker.ParseFile(file, source, opts)

let untypedResLong =
    checker.ParseFile(fileName, sourceText, parsingOptionsWithDefines, somethingElseWithARatherLongVariableName)
"""
        { spaceBeforeConfig with
              MaxLineLength = 90 }
    |> prepend newline
    |> should
        equal
        """
let myRegexMatch = Regex.Match (input, regex)

let myRegexMatchLong =
    Regex.Match (
        "my longer input string with some interesting content in it",
        "myRegexPattern"
    )

let untypedRes = checker.ParseFile (file, source, opts)

let untypedResLong =
    checker.ParseFile (
        fileName,
        sourceText,
        parsingOptionsWithDefines,
        somethingElseWithARatherLongVariableName
    )
"""

[<Test>]
let ``function application inside parenthesis followed by .DotIndexedGet, 1226`` () =
    formatSourceString
        false
        """
module Foo =
    let Bar () =
        (doc.DocumentNode.SelectNodes "//table").[0]
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let Bar () =
        (doc.DocumentNode.SelectNodes "//table").[0]
"""

[<Test>]
let ``ignore setting when function call is the argument of prefix application, 1488`` () =
    formatSourceString
        false
        """
!-String.Empty.PadLeft(braceSize + spaceAround)
(!-System.String.Empty.PadRight(delta)) ({ ctx with RecordBraceStart = rest })
!- Meh()
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
!- String.Empty.PadLeft(braceSize + spaceAround)
(!- System.String.Empty.PadRight(delta)) ({ ctx with RecordBraceStart = rest })
!- Meh()
"""
