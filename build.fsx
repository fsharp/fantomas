#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.Core.TargetOperators
open Fake.IO.Globbing.Operators
open System
open System.IO
open Fake.DotNet

let configuration = DotNet.BuildConfiguration.Release

// (<solutionFile>.sln is built during the building process)
let solutionFile = "fantomas.sln"

// Types and helper functions for building external projects (see the TestExternalProjects target below)
type ProcessStartInfo =
    { ProcessName: string
      Arguments: string list }

[<NoComparison>]
[<NoEquality>]
type ExternalProjectInfo =
    { GitUrl: string
      DirectoryName: string
      Tag: string
      SourceSubDirectory: string
      BuildConfigurationFn: string -> ProcessStartInfo }

// Construct the commands/arguments for running an external project build script for both windows and linux
// For linux we run this by invoking sh explicitly and passing the build.sh script as an argument as some
// projects generated on windows don't have the executable permission set for .sh scripts. On windows we
// treat .cmd files as executable
let configureBuildCommandFromFakeBuildScripts scriptPrefix argument pathToProject =
    if Environment.isWindows then
        { ProcessName = Path.combine pathToProject (sprintf "%s.cmd" scriptPrefix)
          Arguments = [ argument ] }
    else
        { ProcessName = "sh"
          Arguments =
            [ sprintf "%s/%s.sh" pathToProject scriptPrefix
              argument ] }

let configureBuildCommandFromDefaultFakeBuildScripts pathToProject =
    configureBuildCommandFromFakeBuildScripts "build" "Build" pathToProject

let configureBuildCommandDotnetBuild pathToProject =
    { ProcessName = "dotnet"
      Arguments = [ "build"; pathToProject ] }

// Construct the path of the fantomas executable to use for external project tests
let fantomasExecutableForExternalTests projectdir =
    let configuration =
        match configuration with
        | DotNet.BuildConfiguration.Debug -> "Debug"
        | DotNet.BuildConfiguration.Release -> "Release"
        | DotNet.BuildConfiguration.Custom s -> s

    { ProcessName = "dotnet"
      Arguments = [ $"%s{projectdir}/src/Fantomas/bin/%s{configuration}/net6.0/fantomas.dll" ] }

let externalProjectsToTest =
    [
      //    { GitUrl = @"https://github.com/fsprojects/Argu"
      //      DirectoryName = "Argu"
      //      Tag = "5.1.0"
      //      SourceSubDirectory = "src"
      //      BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
      { GitUrl = @"https://github.com/jack-pappas/ExtCore"
        DirectoryName = "ExtCore"
        Tag = "master"
        SourceSubDirectory = "."
        BuildConfigurationFn = configureBuildCommandDotnetBuild } ]

let externalProjectsToTestFailing =
    [ { GitUrl = @"https://github.com/fsprojects/Chessie"
        DirectoryName = "Chessie"
        Tag = "master"
        SourceSubDirectory = "src"
        BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
      { GitUrl = @"https://github.com/fscheck/FsCheck"
        DirectoryName = "FsCheck"
        Tag = "master"
        SourceSubDirectory = "src"
        BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
      { GitUrl = @"https://github.com/fsprojects/fantomas"
        DirectoryName = "Fantomas"
        Tag = "v2.9.0"
        SourceSubDirectory = "src"
        BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
      { GitUrl = @"https://github.com/SAFE-Stack/SAFE-BookStore"
        DirectoryName = "SAFE-BookStore"
        Tag = "master"
        SourceSubDirectory = "src"
        BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
      { GitUrl = @"https://github.com/fsprojects/Paket"
        DirectoryName = "Paket"
        Tag = "5.181.1"
        SourceSubDirectory = "src"
        BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
      { GitUrl = @"https://github.com/fsprojects/FSharpPlus"
        DirectoryName = "FSharpPlus"
        Tag = "v1.0.0"
        SourceSubDirectory = "src"
        BuildConfigurationFn = configureBuildCommandFromDefaultFakeBuildScripts }
      { GitUrl = @"https://github.com/MangelMaxime/fulma-demo"
        DirectoryName = "fulma-demo"
        Tag = "master"
        SourceSubDirectory = "src"
        BuildConfigurationFn = configureBuildCommandFromFakeBuildScripts "fake" "build" } ]

// --------------------------------------------------------------------------------------
// Clean build results & restore NuGet packages

// --------------------------------------------------------------------------------------
// Build library & test project
Target.create "Build" (fun _ -> DotNet.build (fun p -> { p with Configuration = configuration }) solutionFile)

// This takes the list of external projects defined above, does a git checkout of the specified repo and tag,
// tries to build the project, then reformats with fantomas and tries to build the project again. If this fails
// then there was a regression in fantomas that mangles the source code
let testExternalProjects externalProjectsToTest =
    let externalBuildErrors =
        let project = Environment.environVar "project"

        externalProjectsToTest
        |> if String.IsNullOrWhiteSpace(project) then
               id
           else
               List.filter (fun p -> p.DirectoryName = project)
        |> List.map (fun project ->
            let relativeProjectDir = sprintf "external-project-tests/%s" project.DirectoryName

            Shell.cleanDir relativeProjectDir
            // Use "shallow" clone by setting depth to 1 to only check out the one commit we want to build
            Fake.Tools.Git.CommandHelper.gitCommand
                "."
                (sprintf "clone --branch %s --depth 1 %s %s" project.Tag project.GitUrl relativeProjectDir)

            let fullProjectPath = sprintf "%s/%s" __SOURCE_DIRECTORY__ relativeProjectDir

            let buildStartInfo = project.BuildConfigurationFn fullProjectPath

            let buildExternalProject () =
                buildStartInfo.Arguments
                |> CreateProcess.fromRawCommand buildStartInfo.ProcessName
                |> CreateProcess.withWorkingDirectory relativeProjectDir
                |> CreateProcess.withTimeout (TimeSpan.FromMinutes 5.0)
                |> Proc.run

            let cleanResult = buildExternalProject ()

            if cleanResult.ExitCode <> 0 then
                failwithf
                    "Initial build of external project %s returned with a non-zero exit code"
                    project.DirectoryName

            let fantomasStartInfo = fantomasExecutableForExternalTests __SOURCE_DIRECTORY__

            let arguments =
                fantomasStartInfo.Arguments
                @ [ "--recurse"
                    project.SourceSubDirectory ]

            let invokeFantomas () =
                CreateProcess.fromRawCommand fantomasStartInfo.ProcessName arguments
                |> CreateProcess.withWorkingDirectory (sprintf "%s/%s" __SOURCE_DIRECTORY__ relativeProjectDir)
                |> CreateProcess.withTimeout (TimeSpan.FromMinutes 5.0)
                |> Proc.run

            let fantomasResult = invokeFantomas ()

            if fantomasResult.ExitCode <> 0 then
                Some
                <| sprintf "Fantomas invokation for %s returned with a non-zero exit code" project.DirectoryName
            else
                let formattedResult = buildExternalProject ()

                if formattedResult.ExitCode <> 0 then
                    Some
                    <| sprintf
                        "Build of external project after fantomas formatting failed for project %s"
                        project.DirectoryName
                else
                    printfn "Successfully built %s after reformatting" project.DirectoryName
                    None)
        |> List.choose id

    if not (List.isEmpty externalBuildErrors) then
        failwith (String.Join("\n", externalBuildErrors))

Target.create "TestExternalProjects" (fun _ -> testExternalProjects externalProjectsToTest)

Target.create "TestExternalProjectsFailing" (fun _ -> testExternalProjects externalProjectsToTestFailing)

// Workaround for https://github.com/fsharp/FAKE/issues/2242
let pushPackage nupkg =
    let args = [ yield "push"; yield nupkg ]

    CreateProcess.fromRawCommand "dotnet" ("paket" :: args)
    |> CreateProcess.disableTraceCommand
    |> CreateProcess.redirectOutput
    |> CreateProcess.withOutputEventsNotNull Trace.trace Trace.traceError
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

Target.create "Push" (fun _ ->
    Directory.EnumerateFiles("bin", "*.nupkg", SearchOption.TopDirectoryOnly)
    |> Seq.filter (fun nupkg -> not (nupkg.Contains("Fantomas.Client")))
    |> Seq.iter pushPackage)

Target.create "PushClient" (fun _ ->
    Directory.EnumerateFiles("bin", "Fantomas.Client.*.nupkg", SearchOption.TopDirectoryOnly)
    |> Seq.tryExactlyOne
    |> Option.iter pushPackage)

"Build" ==> "TestExternalProjects"
