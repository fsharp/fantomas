module Fantomas.Tests.ValidationTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``naked ranges are invalid outside for..in.do`` () =
    isValidFSharpCode
        false
        """
let factors number = 2L..number / 2L
                     |> Seq.filter (fun x -> number % x = 0L)"""
    |> should equal false

[<Test>]
let ``misplaced comments should give parser errors`` () =
    isValidFSharpCode
        false
        """
module ServiceSupportMethods =
    let toDisposable (xs : seq<'t // Sleep to give time for printf to succeed
                                  when 't :> IDisposable>) =
        { new IDisposable with
              member x.Dispose() = xs |> Seq.iter (fun x -> x.Dispose()) }"""
    |> should equal false

[<Test>]
let ``should fail on uncompilable extern functions`` () =
    isValidFSharpCode
        false
        """
[<System.Runtime.InteropServices.DllImport("user32.dll")>]
let GetWindowLong hwnd : System.IntPtr, index : int : int = failwith )"""
    |> should equal false
