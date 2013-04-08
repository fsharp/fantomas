﻿module Fantomas.CodeFormatter

open System
open System.IO
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fantomas.FormatConfig
open Fantomas.CodePrinter

let internal parseWith fileName content = 
    // Create an interactive checker instance (ignore notifications)
    let checker = InteractiveChecker.Create(NotifyFileTypeCheckStateIsDirty ignore)
    // Get compiler options for a single script file
    let checkOptions = checker.GetCheckOptionsFromScriptRoot(fileName, content, DateTime.Now, [||])
    // Run the first phase (untyped parsing) of the compiler
    let untypedRes = checker.UntypedParse(fileName, content, checkOptions)
    match untypedRes.ParseTree with
    | Some tree -> tree
    | None -> failwith "parseWith: Unexpected input"

/// Parse a source code string
let parse fsi s = 
    let fileName = if fsi then "/tmp.fsi" else "/tmp.fs"
    parseWith fileName s

/// Format a source string using given config
let formatSourceString fsi s config =
    let tree = parse fsi s
    Context.createContext config s |> genParsedInput tree |> dump

/// Format a source string using given config; return None if failed
let tryFormatSourceString fsi s config =
    try
        Some (formatSourceString fsi s config)
    with 
    _ -> None

/// Format a source file using given config
let formatSourceFile f config = 
    let s = File.ReadAllText(f)
    let ext = Path.GetExtension(s)
    let fsi = ext = ".fsi" || ext = ".mli"
    formatSourceString fsi s config

/// Format inFile and write to outFile
let processSourceFile inFile outFile config = 
    let s = formatSourceFile inFile config
    File.WriteAllText(outFile, s)

/// Format inFile and write to outFile; return None if failed
let tryProcessSourceFile inFile outFile config = 
    try
        Some (processSourceFile inFile outFile config)
    with 
    _ -> None


