module Fantomas.Client.FantomasToolLocator

open System
open System.ComponentModel
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open Fantomas.Client.LSPFantomasServiceTypes
open StreamJsonRpc

let private alphaLowerThanFour version =
    version = "4.6.0-alpha-001"
    || version = "4.6.0-alpha-002"
    || version = "4.6.0-alpha-003"

let private (|CompatibleVersion|_|) (version: string) =
    let stripAlphaBeta = version.Split('-').[0]

    match Version.TryParse stripAlphaBeta with
    | true, parsedVersion ->
        if parsedVersion.Major = 4
           && parsedVersion.Minor = 6
           && alphaLowerThanFour version then
            // Only 4.6.0-alpha-004 has daemon capabilities
            None
        elif parsedVersion.Major >= 4
             && parsedVersion.Minor >= 6 then
            Some version
        else
            None
    | _ -> None

// In the future, fantomas-tool will be renamed to fantomas.
let private (|CompatibleToolName|_|) toolName =
    if toolName = "fantomas-tool"
       || toolName = "fantomas" then
        Some toolName
    else
        None

let private readOutputStreamAsLines (outputStream: StreamReader) : string list =
    let rec readLines (outputStream: StreamReader) (continuation: string list -> string list) =
        let nextLine = outputStream.ReadLine()

        if isNull nextLine then
            continuation []
        else
            readLines outputStream (fun lines -> nextLine :: lines |> continuation)

    readLines outputStream id

let private startProcess (ps: ProcessStartInfo) : Result<Process, ProcessStartError> =
    try
        Ok(Process.Start ps)
    with
    | :? Win32Exception as win32ex ->
        let pathEnv =
            Environment.GetEnvironmentVariable "PATH"

        Error(ProcessStartError.ExecutableFileNotFound(ps.FileName, ps.Arguments, pathEnv, win32ex.Message))
    | ex -> Error(ProcessStartError.UnExpectedException(ps.FileName, ps.Arguments, ex.Message))

let private runToolListCmd (Folder workingDir: Folder) (globalFlag: bool) : Result<string list, DotNetToolListError> =
    let ps = ProcessStartInfo("dotnet")
    ps.WorkingDirectory <- workingDir
    ps.EnvironmentVariables.Add("DOTNET_CLI_UI_LANGUAGE", "en-us")
    ps.CreateNoWindow <- true

    ps.Arguments <-
        if globalFlag then
            "tool list -g"
        else
            "tool list"

    ps.RedirectStandardOutput <- true
    ps.RedirectStandardError <- true
    ps.UseShellExecute <- false

    match startProcess ps with
    | Ok p ->
        p.WaitForExit()
        let exitCode = p.ExitCode

        if exitCode = 0 then
            let output = readOutputStreamAsLines p.StandardOutput
            Ok output
        else
            let error = p.StandardError.ReadToEnd()
            Error(DotNetToolListError.ExitCodeNonZero(ps.FileName, ps.Arguments, exitCode, error))
    | Error err -> Error(DotNetToolListError.ProcessStartError err)

let private (|CompatibleTool|_|) lines =
    let (|HeaderLine|_|) line =
        if Regex.IsMatch(line, @"^Package\sId\s+Version.+$") then
            Some()
        else
            None

    let (|Dashes|_|) line =
        if String.forall ((=) '-') line then
            Some()
        else
            None

    let (|Tools|_|) lines =
        let tools =
            lines
            |> List.choose
                (fun (line: string) ->
                    let parts =
                        line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

                    if parts.Length > 2 then
                        Some(parts.[0], parts.[1])
                    else
                        None)

        if List.isEmpty tools then
            None
        else
            Some tools

    match lines with
    | HeaderLine :: Dashes :: Tools tools ->
        let tool =
            List.tryFind
                (fun (packageId, version) ->
                    match packageId, version with
                    | CompatibleToolName _, CompatibleVersion _ -> true
                    | _ -> false)
                tools

        Option.map (snd >> FantomasVersion) tool
    | _ -> None

let findFantomasTool (workingDir: Folder) : FantomasToolResult =
    let localTools = runToolListCmd workingDir false

    match localTools with
    | Ok (CompatibleTool version) -> FoundLocalTool(workingDir, version)
    | Error err -> DotNetListError err
    | Ok _nonCompatibleLocalVersion ->
        let globalTools = runToolListCmd workingDir true

        match globalTools with
        | Ok (CompatibleTool version) -> FoundGlobalTool(workingDir, version)
        | Error err -> DotNetListError err
        | Ok _nonCompatibleGlobalVersion -> NoCompatibleVersionFound

let createForWorkingDirectory
    (Folder workingDirectory)
    (isGlobal: bool)
    : Result<RunningFantomasTool, ProcessStartError> =
    let processStart =
        if isGlobal then
            ProcessStartInfo("fantomas")
        else
            ProcessStartInfo("dotnet")

    processStart.UseShellExecute <- false
    processStart.Arguments <- sprintf "fantomas --daemon"
    processStart.WorkingDirectory <- workingDirectory
    processStart.RedirectStandardInput <- true
    processStart.RedirectStandardOutput <- true
    processStart.RedirectStandardError <- true
    processStart.CreateNoWindow <- true

    match startProcess processStart with
    | Ok daemonProcess ->
        let client =
            new JsonRpc(daemonProcess.StandardInput.BaseStream, daemonProcess.StandardOutput.BaseStream)

        do client.StartListening()

        try
            // Get the version first as a sanity check that connection is possible
            let _version =
                client.InvokeAsync<string>(Fantomas.Client.Contracts.Methods.Version)
                |> Async.AwaitTask
                |> Async.RunSynchronously

            Ok
                { RpcClient = client
                  Process = daemonProcess
                  IsGlobal = isGlobal }
        with
        | ex ->
            let error =
                if daemonProcess.HasExited then
                    let stdErr = daemonProcess.StandardError.ReadToEnd()
                    $"Daemon std error: {stdErr}.\nJsonRpc exception:{ex.Message}"
                else
                    ex.Message

            Error(ProcessStartError.UnExpectedException(processStart.FileName, processStart.Arguments, error))
    | Error err -> Error err
