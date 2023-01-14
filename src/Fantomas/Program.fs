open System
open System.IO
open Fantomas.Core
open Fantomas
open Fantomas.Daemon
open Fantomas.Logging
open Argu
open System.Text
open Fantomas.Format
open System.Threading.Tasks
open System.Runtime.ExceptionServices

let extensions = set [| ".fs"; ".fsx"; ".fsi"; ".ml"; ".mli" |]

type Arguments =
    | [<Unique; AltCommandLine("-r")>] Recurse
    | [<Unique>] Force
    | [<Unique>] Profile
    | [<Unique>] Out of string
    | [<Unique>] Check
    | [<Unique>] Daemon
    | [<Unique>] Parallel
    | [<Unique; AltCommandLine("-v")>] Version
    | [<Unique>] Verbosity of string
    | [<MainCommand>] Input of string list

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Recurse -> "Process the input folder recursively."
            | Force -> "Print the output even if it is not valid F# code. For debugging purposes only."
            | Out _ ->
                "Give a valid path for files/folders. Files should have .fs, .fsx, .fsi, .ml or .mli extension only. Multiple files/folders are not supported."
            | Profile -> "Print performance profiling information."
            | Check ->
                "Don't format files, just check if they have changed. Exits with 0 if it's formatted correctly, with 1 if some files need formatting and 99 if there was an internal error"
            | Daemon -> "Daemon mode, launches an LSP-like server to can be used by editor tooling."
            | Version -> "Displays the version of Fantomas"
            | Input _ ->
                sprintf
                    "Input paths: can be multiple folders or files with %s extension."
                    (Seq.map (fun s -> "*" + s) extensions |> String.concat ",")

            | Parallel -> "Process files in parallel."
            | Verbosity _ -> "Set the verbosity level. Allowed values are n[ormal] and d[etailed]."

let timeAsync f =
    async {
        let sw = Diagnostics.Stopwatch.StartNew()
        let! res = f ()
        sw.Stop()
        printfn "Time taken: %O s" sw.Elapsed
        return res
    }

[<RequireQualifiedAccess>]
type InputPath =
    | File of string
    | Folder of string
    | Multiple of files: string list * folder: string list
    | NoFSharpFile of string
    | NotFound of string
    | Unspecified

[<RequireQualifiedAccess>]
type OutputPath =
    | IO of string
    | NotKnown

let isInExcludedDir (fullPath: string) =
    set [| "obj"; ".fable"; "fable_modules"; "node_modules" |]
    |> Set.map (fun dir -> sprintf "%c%s%c" Path.DirectorySeparatorChar dir Path.DirectorySeparatorChar)
    |> Set.exists fullPath.Contains

let isFSharpFile (s: string) =
    Set.contains (Path.GetExtension s) extensions

/// Get all appropriate files, either recursively or non-recursively
let rec allFiles isRec path =
    let searchOption =
        (if isRec then
             SearchOption.AllDirectories
         else
             SearchOption.TopDirectoryOnly)

    Directory.GetFiles(path, "*.*", searchOption)
    |> Seq.filter (fun f ->
        isFSharpFile f
        && not (isInExcludedDir f)
        && not (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) f))

/// Fantomas assumes the input files are UTF-8
/// As is stated in F# language spec: https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf#page=25
let private hasByteOrderMark file =
    async {
        if File.Exists(file) then
            let preamble = Encoding.UTF8.GetPreamble()

            use file = new FileStream(file, FileMode.Open, FileAccess.Read)

            let mutable bom = Array.zeroCreate 3
            do! file.ReadAsync(bom, 0, 3) |> Async.AwaitTask |> Async.Ignore<int>
            return bom = preamble
        else
            return false
    }

/// Format a source string using given config and write to a text writer
let processSourceString verbosity (force: bool) s (fileName: string) config =
    let writeResult (formatted: string) =
        async {
            let! hasBom = hasByteOrderMark fileName

            if hasBom then
                do! File.WriteAllTextAsync(fileName, formatted, Encoding.UTF8) |> Async.AwaitTask
            else
                do! File.WriteAllTextAsync(fileName, formatted) |> Async.AwaitTask

            logGrEqDetailed verbosity $"%s{fileName} has been written."
        }

    async {
        let! formatted = s |> Format.formatContentAsync config fileName

        match formatted with
        | Format.FormatResult.Formatted(_, formattedContent) -> do! formattedContent |> writeResult
        | Format.InvalidCode(file, formattedContent) when force ->
            stdlog $"%s{file} was not valid after formatting."
            do! formattedContent |> writeResult
        | Format.FormatResult.Unchanged file ->  logGrEqDetailed verbosity $"'%s{file}' was unchanged"
        | Format.IgnoredFile file -> logGrEqDetailed verbosity $"'%s{file}' was ignored"
        | Format.FormatResult.Error(_, ex) -> raise ex
        | Format.InvalidCode(file, _) -> raise (exn $"Formatting {file} lead to invalid F# code")
    }

/// Format inFile and write to text writer
let processSourceFile verbosity (force: bool) inFile (tw: TextWriter) =
    async {
        let! formatted = Format.formatFileAsync inFile

        match formatted with
        | Format.FormatResult.Formatted(_, formattedContent) -> do! tw.WriteAsync(formattedContent) |> Async.AwaitTask
        | Format.InvalidCode(file, formattedContent) when force ->
            stdlog $"%s{file} was not valid after formatting."
            do! tw.WriteAsync(formattedContent) |> Async.AwaitTask
        | Format.FormatResult.Unchanged _ ->
            let! input = inFile |> File.ReadAllTextAsync |> Async.AwaitTask
            do! input |> tw.WriteAsync |> Async.AwaitTask
        | Format.IgnoredFile file -> logGrEqDetailed verbosity $"'%s{file}' was ignored"
        | Format.FormatResult.Error(_, ex) -> raise ex
        | Format.InvalidCode(file, _) -> raise (exn $"Formatting {file} lead to invalid F# code")
    }

let private reportCheckResults (checkResult: Format.CheckResult) =
    checkResult.Errors
    |> List.map (fun (filename, exn) -> $"error: Failed to format %s{filename}: %s{exn.ToString()}")
    |> Seq.iter elog

    checkResult.Formatted
    |> List.map (fun filename -> $"%s{filename} needs formatting")
    |> Seq.iter stdlog

let runCheckCommand (verbosity: VerbosityLevel) (recurse: bool) (inputPath: InputPath) : int =
    let check files =
        Async.RunSynchronously(Format.checkCode files)

    let processCheckResult (checkResult: Format.CheckResult) =
        if checkResult.IsValid then
            logGrEqDetailed verbosity "No changes required."
            0
        else
            reportCheckResults checkResult
            if checkResult.HasErrors then 1 else 99

    match inputPath with
    | InputPath.NoFSharpFile s ->
        elog $"Input path '%s{s}' is unsupported file type"
        1
    | InputPath.NotFound s ->
        elog $"Input path '%s{s}' not found"
        1
    | InputPath.Unspecified _ ->
        elog "No input path provided. Call with --help for usage information."
        1
    | InputPath.File f when (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) f) ->
        logGrEqDetailed verbosity $"'%s{f}' was ignored"
        0
    | InputPath.File path -> path |> Seq.singleton |> check |> processCheckResult
    | InputPath.Folder path -> path |> allFiles recurse |> check |> processCheckResult
    | InputPath.Multiple(files, folders) ->
        let allFilesToCheck =
            seq {
                yield! files
                yield! (Seq.collect (allFiles recurse) folders)
            }

        allFilesToCheck |> check |> processCheckResult

[<EntryPoint>]
let main argv =
    let errorHandler =
        ProcessExiter(
            colorizer =
                function
                | ErrorCode.HelpText -> None
                | _ -> Some ConsoleColor.Red
        )

    let parser =
        ArgumentParser.Create<Arguments>(programName = "dotnet fantomas", errorHandler = errorHandler)

    let results = parser.ParseCommandLine argv

    let outputPath =
        match results.TryGetResult <@ Arguments.Out @> with
        | Some output -> OutputPath.IO output
        | None -> OutputPath.NotKnown

    let inputPath =
        let maybeInput = results.TryGetResult <@ Arguments.Input @>

        match maybeInput with
        | Some [ input ] ->
            if Directory.Exists(input) then
                InputPath.Folder input
            elif File.Exists input && isFSharpFile input then
                InputPath.File input
            elif File.Exists input then
                InputPath.NoFSharpFile input
            else
                InputPath.NotFound input
        | Some inputs ->
            let isFolder (path: string) = Path.GetExtension(path) = ""

            let rec loop
                (files: string list)
                (finalContinuation: string list * string list -> string list * string list)
                =
                match files with
                | [] -> finalContinuation ([], [])
                | h :: rest ->
                    loop rest (fun (files, folders) ->
                        if isFolder h then
                            files, (h :: folders)
                        else
                            (h :: files), folders
                        |> finalContinuation)

            let filesAndFolders = loop inputs id
            InputPath.Multiple filesAndFolders
        | None -> InputPath.Unspecified

    let force = results.Contains <@ Arguments.Force @>
    let profile = results.Contains <@ Arguments.Profile @>
    let recurse = results.Contains <@ Arguments.Recurse @>

    let version = results.TryGetResult <@ Arguments.Version @>

    let verbosity =
        let maybeVerbosity =
            results.TryGetResult <@ Arguments.Verbosity @>
            |> Option.map (fun v -> v.ToLowerInvariant())

        match maybeVerbosity with
        | None
        | Some "n"
        | Some "normal" -> VerbosityLevel.Normal
        | Some "d"
        | Some "detailed" -> VerbosityLevel.Detailed
        | Some _ ->
            elog "Invalid verbosity level"
            exit 1

    AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> closeAndFlushLog ())

    let inline reraiseAsync (e: exn) =
        let edi = ExceptionDispatchInfo.Capture e
        edi.Throw()

    let fileToFile (force: bool) (inFile: string) (outFile: string) =
        async {
            try
                logGrEqDetailed verbosity $"Processing %s{inFile}"
                let! hasByteOrderMark = hasByteOrderMark inFile

                use buffer =
                    if hasByteOrderMark then
                        new StreamWriter(
                            new FileStream(outFile, FileMode.OpenOrCreate, FileAccess.ReadWrite),
                            Encoding.UTF8
                        )
                    else
                        new StreamWriter(outFile)

                if profile then
                    let! length = File.ReadAllLinesAsync(inFile) |> Async.AwaitTask
                    length |> Seq.length |>  (fun l -> stdlog $"Line count: %i{l}")

                    do! timeAsync (fun () -> processSourceFile verbosity force inFile buffer)
                else
                    do! processSourceFile verbosity force inFile buffer


                buffer.Flush()
                logGrEqDetailed verbosity $"%s{outFile} has been written."
            with exn ->
                reraiseAsync exn
        }

    let stringToFile (force: bool) (s: string) (outFile: string) config =
        async {
            try
                if profile then
                    stdlog $"""Line count: %i{s.Length - s.Replace(Environment.NewLine, "").Length}"""

                    do! timeAsync (fun () -> processSourceString verbosity force s outFile config)
                else
                    do! processSourceString verbosity force s outFile config
            with exn ->
                reraiseAsync exn
        }

    let processFile force inputFile outputFile =
        async {
            if inputFile <> outputFile then
                return! fileToFile force inputFile outputFile
            else
                logGrEqDetailed verbosity $"Processing %s{inputFile}"
                let! content = File.ReadAllTextAsync inputFile |> Async.AwaitTask
                let config = EditorConfig.readConfiguration inputFile
                return! stringToFile force content inputFile config
        }

    let processFolder force inputFolder outputFolder =
        if not <| Directory.Exists(outputFolder) then
            Directory.CreateDirectory(outputFolder) |> ignore

        allFiles recurse inputFolder
        |> Seq.toList
        |> List.map (fun i ->
            // s supposes to have form s1/suffix
            let suffix = i.Substring(inputFolder.Length + 1)

            let o =
                if inputFolder <> outputFolder then
                    Path.Combine(outputFolder, suffix)
                else
                    i

            processFile force i o)


    let filesAndFolders force (files: string list) (folders: string list) =
        let fileTasks =
            files
            |> List.map (fun file ->
                if (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) file) then
                    logGrEqDetailed verbosity $"'%s{file}' was ignored"
                    async.Return()
                else
                    processFile force file file)

        let folderTasks =
            folders |> List.collect (fun folder -> processFolder force folder folder)

        (fileTasks @ folderTasks)

    let check = results.Contains <@ Arguments.Check @>
    let isDaemon = results.Contains <@ Arguments.Daemon @>

    let asyncRunner =
        if results.Contains <@ Arguments.Parallel @> then
            Async.Parallel
        else
            Async.Sequential
        >> Async.Ignore<unit array>
        >> Async.RunSynchronously

    if Option.isSome version then
        let version = CodeFormatter.GetVersion()
        stdlog $"Fantomas v%s{version}"
    elif isDaemon then
        let daemon =
            new FantomasDaemon(Console.OpenStandardOutput(), Console.OpenStandardInput())

        AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> (daemon :> IDisposable).Dispose())

        daemon.WaitForClose.GetAwaiter().GetResult()
        exit 0
    elif check then
        inputPath |> runCheckCommand verbosity recurse |> exit
    else
        try
            match inputPath, outputPath with
            | InputPath.NoFSharpFile s, _ ->
                elog $"Input path '%s{s}' is unsupported file type."
                exit 1
            | InputPath.NotFound s, _ ->
                elog $"Input path '%s{s}' not found."
                exit 1
            | InputPath.Unspecified, _ ->
                elog "Input path is missing. Call with --help for usage information."
                exit 1
            | InputPath.File f, _ when (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) f) ->
                logGrEqDetailed verbosity $"'%s{f}' was ignored"
            | InputPath.Folder p1, OutputPath.NotKnown -> processFolder force p1 p1 |> asyncRunner
            | InputPath.File p1, OutputPath.NotKnown -> processFile force p1 p1 |> List.singleton |> asyncRunner
            | InputPath.File p1, OutputPath.IO p2 -> processFile force p1 p2 |> List.singleton |> asyncRunner
            | InputPath.Folder p1, OutputPath.IO p2 -> processFolder force p1 p2 |> asyncRunner
            | InputPath.Multiple(files, folders), OutputPath.NotKnown ->
                filesAndFolders force files folders |> asyncRunner
            | InputPath.Multiple _, OutputPath.IO _ ->
                elog "Multiple input files are not supported with the --out flag."
                exit 1
        with exn ->
            elog $"%s{exn.Message}"
            exit 1

    0
