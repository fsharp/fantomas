module Fantomas.Tests.LibraryOnlySynExprTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``SynExpr.LibraryOnlyStaticOptimization`` () =
    formatSourceString
        false
        """
     let FromZero () : 'T =
                (get32 0 :?> 'T) when 'T : BigInteger = BigInteger.Zero
"""
        config
    |> prepend newline
    |> should
        equal
        """
let FromZero () : 'T =
    (get32 0 :?> 'T) when 'T: BigInteger = BigInteger.Zero
"""
