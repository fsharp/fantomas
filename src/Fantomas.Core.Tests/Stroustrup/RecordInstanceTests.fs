﻿module Fantomas.Core.Tests.Stroustrup.RecordInstanceTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Stroustrup }

[<Test>]
let ``multiline field body expression where indent_size = 2`` () =
    formatSourceString
        """
let handlerFormattedRangeDoc (lines: NamedText, formatted: string, range: FormatSelectionRange) =
    let range =
      { Start =
          { Line = range.StartLine - 1
            Character = range.StartColumn }
        End =
          { Line = range.EndLine - 1
            Character = range.EndColumn } }

    [| { Range = range; NewText = formatted } |]
"""
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
let handlerFormattedRangeDoc (lines: NamedText, formatted: string, range: FormatSelectionRange) =
  let range = {
    Start = {
      Line = range.StartLine - 1
      Character = range.StartColumn
    }
    End = {
      Line = range.EndLine - 1
      Character = range.EndColumn
    }
  }

  [| { Range = range; NewText = formatted } |]
"""
