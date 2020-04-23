#r "paket:
nuget Fantomas 3.3.0
nuget FSharp.Compiler.Service 34.1.0
nuget Fake.Core.Target //"
#load "./.fake/script.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators
open Fantomas
open Fantomas.FormatConfig

let fantomasConfig =
    // look for fantomas-config.json in the current directory
    match CodeFormatter.ReadConfiguration(Shell.pwd()) with
    | Success c -> c
    | _ ->
        printfn "Cannot parse fantomas-config.json, using default"
        FormatConfig.Default

Target.create "CheckCodeFormat" (fun _ ->
    let result =
        !!"*.fs"
        |> FakeHelpers.checkCode fantomasConfig
        |> Async.RunSynchronously

    if result.IsValid then
        Trace.log "No files need formatting"
    elif result.NeedsFormatting then
        Trace.log "The following files need formatting:"
        List.iter Trace.log result.Formatted
        failwith "Some files need formatting, check output for more info"
    else
        Trace.logf "Errors while formatting: %A" result.Errors)

Target.create "Format" (fun _ ->
    !!"*.fs"
    |> FakeHelpers.formatCode fantomasConfig
    |> Async.RunSynchronously
    |> printfn "Formatted files: %A")

Target.runOrList()
