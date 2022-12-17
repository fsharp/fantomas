﻿module Fantomas.Core.Tests.MultilinePatterns

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

let tap s =
    printfn "%s" s
    s

[<Test>]
let ``long ident tuple pattern`` () =
    formatSourceString
        false
        """
match a with
| X ( // comment
        y,
        // other comment
        z
  ) ->
  do ()
  1
"""
        config
    |> prepend newline
    |> should
        equal
        """
match a with
| X( // comment
    y,
    // other comment
    z
  ) ->
    do ()
    1
"""

[<Test>]
let ``multiple clauses with long ident app`` () =
    formatSourceString
        false
        """
match a with
| X( // comment
    y,
    // other comment
    z) ->
    do ()
    1
| Z(
    [|  1 ; 2 // comment
        3 |]
    ) ->
    do()
    2
"""
        config
    |> prepend newline
    |> should
        equal
        """
match a with
| X( // comment
    y,
    // other comment
    z
  ) ->
    do ()
    1
| Z(
    [| 1
       2 // comment
       3 |]
  ) ->
    do ()
    2
"""

[<Test>]
let ``multiple clauses with long ident app in or pattern`` () =
    formatSourceString
        false
        """
match a with
| X( // comment
    y,
    // other comment
    z)
| Z(
    [|  1 ; 2 // comment
        3 |]
    ) ->
    do()
    2
"""
        config
    |> prepend newline
    |> should
        equal
        """
match a with
| X( // comment
    y,
    // other comment
    z
  )
| Z(
    [| 1
       2 // comment
       3 |]
  ) ->
    do ()
    2
"""

[<Test>]
let ``single named pair pattern`` () =
    formatSourceString
        false
        """
match a with
| Xray ( y =
    Zulu (
        a,
        // comment
        b, c
        )
            ) ->
        do ()
        "meh"
"""
        config
    |> prepend newline
    |> should
        equal
        """
match a with
| Xray(y =
    Zulu(
        a,
        // comment
        b,
        c
    )
  ) ->
    do ()
    "meh"
"""

[<Test>]
let ``multiple short name pairs`` () =
    formatSourceString
        false
        """
match x with
| Alfa (
    bravo =
        Bravo(x, y, z)
    charlie =
        Charlie(x, y, z)
    delta =
        Delta(x, y, z)
  ) ->
  do ()
  "meh"
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| Alfa(bravo = Bravo(x, y, z); charlie = Charlie(x, y, z); delta = Delta(x, y, z)) ->
    do ()
    "meh"
"""

[<Test>]
let ``multiple short name pairs that don't fit on a single line`` () =
    formatSourceString
        false
        """
match x with
| Alfa (
    bravo =
        Bravo(x, y, z)
    charlie =
        Charlie(x, y, z)
    delta =
        Delta(x, y, z)
    echo =
        Echo (x, y, z)
    foxtrot =
        Foxtrot(x, y, z)
    golf =
        Golf(x,  y, z)
  ) ->
  do ()
  "meh"
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| Alfa(
    bravo = Bravo(x, y, z)
    charlie = Charlie(x, y, z)
    delta = Delta(x, y, z)
    echo = Echo(x, y, z)
    foxtrot = Foxtrot(x, y, z)
    golf = Golf(x, y, z)
  ) ->
    do ()
    "meh"
"""

[<Test>]
let ``a mix of multiple name pairs that don't fit on a single line`` () =
    formatSourceString
        false
        "
match x with
| Kilo (
    lima = Lima(a, b ,c ) ; mike = mickey;
    november = \"\"\"
Nothin' lasts forever
And we both know hearts can change
And it's hard to hold a candle
In the cold November rain
    \"\"\"
            ) ->
  do ()
  \"meh\"
"
        config
    |> prepend newline
    |> should
        equal
        "
match x with
| Kilo(
    lima = Lima(a, b, c)
    mike = mickey
    november =
        \"\"\"
Nothin' lasts forever
And we both know hearts can change
And it's hard to hold a candle
In the cold November rain
    \"\"\"
  ) ->
    do ()
    \"meh\"
"

[<Test>]
let ``long ident app with constants`` () =
    formatSourceString
        false
        """
match foo with
| SomeVeryLongMatchCase (1234567890,
                         1234567890,
                         1234567890,
                         1234567890,
                         1234567890,
                         1234567890,
                         1234567890,
                         1234567890,
                         1234567890) -> bar ()
| _ -> ()
"""
        { config with
            MaxLineLength = 80
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
match foo with
| SomeVeryLongMatchCase (
    1234567890,
    1234567890,
    1234567890,
    1234567890,
    1234567890,
    1234567890,
    1234567890,
    1234567890,
    1234567890
  ) -> bar ()
| _ -> ()
"""

[<Test>]
let ``syntax tree example one`` () =
    formatSourceString
        false
        """
match parseResults with
| ParsedInput.ImplFile (ParsedImplFileInput (contents = [ SynModuleOrNamespace.SynModuleOrNamespace(decls = [
    SynModuleDecl.Types(typeDefns = [SynTypeDefn(typeRepr = SynTypeDefnRepr.ObjectModel(members = [ _; SynMemberDefn.Member(memberDefn = SynBinding(trivia={ EqualsRange = Some mEquals }))]))])
]) ])) ->
    assertRange (3, 18) (3, 19) mEquals
| _ -> Assert.Fail "Could not get valid AST"
"""
        config
    |> prepend newline
    |> fun output ->
        printfn $"%s{output}"
        output
    |> should
        equal
        """
match parseResults with
| ParsedInput.ImplFile(
    ParsedImplFileInput(contents =
        [ SynModuleOrNamespace.SynModuleOrNamespace(decls =
            [ SynModuleDecl.Types(typeDefns =
                [ SynTypeDefn(typeRepr =
                    SynTypeDefnRepr.ObjectModel(members =
                        [ _; SynMemberDefn.Member(memberDefn = SynBinding(trivia = { EqualsRange = Some mEquals })) ]
                    )
                  ) ]
              ) ]
          ) ]
    )
  ) -> assertRange (3, 18) (3, 19) mEquals
| _ -> Assert.Fail "Could not get valid AST"
"""

[<Test>]
let ``syntax tree example two `` () =
    formatSourceString
        false
        """
match parseResults with
| ParsedInput.ImplFile(ParsedImplFileInput(
    modules = [ SynModuleOrNamespace.SynModuleOrNamespace(
                    decls = [ SynModuleDecl.Types(
                                  typeDefns = [ SynTypeDefn(
                                                    typeRepr = SynTypeDefnRepr.ObjectModel(
                                                        members = [ SynMemberDefn.ImplicitCtor _
                                                                    SynMemberDefn.GetSetMember(Some(SynBinding(
                                                                                                   headPat = SynPat.LongIdent(
                                                                                                       extraId = Some getIdent))),
                                                                                               Some(SynBinding(
                                                                                                   headPat = SynPat.LongIdent(
                                                                                                       extraId = Some setIdent))),
                                                                                               m,
                                                                                               { WithKeyword = mWith
                                                                                                 GetKeyword = Some mGet
                                                                                                 AndKeyword = Some mAnd
                                                                                                 SetKeyword = Some mSet }) ])) ]) ]) ])) ->
    ()
"""
        config
    |> prepend newline
    |> fun output ->
        printfn $"%s{output}"
        output
    |> should
        equal
        """
match parseResults with
| ParsedInput.ImplFile(
    ParsedImplFileInput(modules =
        [ SynModuleOrNamespace.SynModuleOrNamespace(decls =
            [ SynModuleDecl.Types(typeDefns =
                [ SynTypeDefn(typeRepr =
                    SynTypeDefnRepr.ObjectModel(members =
                        [ SynMemberDefn.ImplicitCtor _
                          SynMemberDefn.GetSetMember(
                              Some(SynBinding(headPat = SynPat.LongIdent(extraId = Some getIdent))),
                              Some(SynBinding(headPat = SynPat.LongIdent(extraId = Some setIdent))),
                              m,
                              { WithKeyword = mWith
                                GetKeyword = Some mGet
                                AndKeyword = Some mAnd
                                SetKeyword = Some mSet }
                          ) ]
                    )
                  ) ]
              ) ]
          ) ]
    )
  ) -> ()
"""

[<Test>]
let ``as pattern`` () =
    formatSourceString
        false
        """
match x with
| ABC(Y(
    itemOne = OhSomeActivePatternThing(a,
                                       b) as foo)) ->
    ()
"""
        { config with MaxLineLength = 30 }
    |> prepend newline
    |> should
        equal
        """
match x with
| ABC(
    Y(itemOne =
        OhSomeActivePatternThing(
            a, b
        ) as foo
    )
  ) -> ()
"""

[<Test>]
let ``or pattern`` () =
    formatSourceString
        false
        """
match x with
| ABC( Y(itemOne = OhSomeActivePatternThing(a,b) | S(one, two, three, four)))  -> ()
"""
        { config with MaxLineLength = 30 }
    |> prepend newline
    |> should
        equal
        """
match x with
| ABC(
    Y(itemOne =
        OhSomeActivePatternThing(
            a, b
        ) | S(
            one,
            two,
            three,
            four
        )
    )
  ) -> ()
"""

[<Test>]
let ``cons pattern`` () =
    formatSourceString
        false
        """
match x with
| ABC( Y(itemOne = OhSomeActivePatternThing(a,b) :: S(one, two, three, four)))  -> ()
"""
        { config with MaxLineLength = 30 }
    |> prepend newline
    |> should
        equal
        """
match x with
| ABC(
    Y(itemOne =
        OhSomeActivePatternThing(
            a, b
        ) :: S(
            one,
            two,
            three,
            four
        )
    )
  ) -> ()
"""

[<Test>]
let ``array pattern`` () =
    formatSourceString
        false
        """
match x with
| [| Y(
         itemOne = OhSomeActivePatternThing(a,
                                            b))
     S(one,
       two,
       three,
       four,
       five) |] -> ()
"""
        { config with MaxLineLength = 30 }
    |> prepend newline
    |> should
        equal
        """
match x with
| [| Y(itemOne =
         OhSomeActivePatternThing(
             a, b
         )
     )
     S(
         one,
         two,
         three,
         four,
         five
     ) |] -> ()
"""

[<Test>]
let ``array pattern, alt`` () =
    formatSourceString
        false
        """
match x with
| [| Y(
         itemOne = OhSomeActivePatternThing(a,
                                            b))
     S(one,
       two,
       three,
       four,
       five) |] -> ()
"""
        { config with
            MaxLineLength = 30
            MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should
        equal
        """
match x with
| [|
    Y(itemOne =
        OhSomeActivePatternThing(
            a, b
        )
    )
    S(
        one,
        two,
        three,
        four,
        five
    )
  |] -> ()
"""

[<Test>]
let ``record pattern`` () =
    formatSourceString
        false
        """
match x with
| { Y = Y(
        itemOne = OhSomeActivePatternThing(a,
                                           b))
    V = S(one,
          two,
          three,
          four,
          five) } -> ()
"""
        { config with MaxLineLength = 30 }
    |> prepend newline
    |> should
        equal
        """
match x with
| { Y = Y(itemOne =
        OhSomeActivePatternThing(
            a, b
        )
    )
    V = S(
        one,
        two,
        three,
        four,
        five
    ) } -> ()
"""

[<Test>]
let ``record pattern, alt`` () =
    formatSourceString
        false
        """
match x with
| { Y = Y(
        itemOne = OhSomeActivePatternThing(a,
                                           b))
    V = S(one,
          two,
          three,
          four,
          five) } -> ()
"""
        { config with
            MaxLineLength = 30
            MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should
        equal
        """
match x with
| {
      Y = Y(itemOne =
          OhSomeActivePatternThing(
              a, b
          )
      )
      V = S(
          one,
          two,
          three,
          four,
          five
      )
  } -> ()
"""

[<Test>]
let ``keep array on one line`` () =
    formatSourceString
        false
        """
match ast with
| ParsedInput.ImplFile(ParsedImplFileInput(
    contents = [ SynModuleOrNamespace.SynModuleOrNamespace(
                     decls = [ SynModuleDecl.Types([ SynTypeDefn.SynTypeDefn(
                                                         typeRepr = SynTypeDefnRepr.Simple(
                                                             simpleRepr = SynTypeDefnSimpleRepr.Enum(
                                                                 cases = [ SynEnumCase.SynEnumCase(
                                                                               trivia = { BarRange = None
                                                                                          EqualsRange = mEquals }) ]))) ],
                                                   _) ]) ])) -> assertRange (2, 15) (2, 16) mEquals
| _ -> Assert.Fail "Could not get valid AST"
"""
        config
    |> tap
    |> prepend newline
    |> should
        equal
        """
match ast with
| ParsedInput.ImplFile(
    ParsedImplFileInput(contents =
        [ SynModuleOrNamespace.SynModuleOrNamespace(decls =
            [ SynModuleDecl.Types(
                [ SynTypeDefn.SynTypeDefn(typeRepr =
                    SynTypeDefnRepr.Simple(simpleRepr =
                        SynTypeDefnSimpleRepr.Enum(cases =
                            [ SynEnumCase.SynEnumCase(trivia = { BarRange = None; EqualsRange = mEquals }) ]
                        )
                    )
                  ) ],
                _
              ) ]
          ) ]
    )
  ) -> assertRange (2, 15) (2, 16) mEquals
| _ -> Assert.Fail "Could not get valid AST"
"""
