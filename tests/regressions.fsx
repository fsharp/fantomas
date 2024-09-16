#r "nuget: CliWrap, 3.6.0"
#load "./configuration.fsx"

open System.IO
open System.Threading.Tasks
open CliWrap
open Configuration

let (</>) a b = Path.Combine(a, b)
let fantomasRoot = __SOURCE_DIRECTORY__ </> ".."
let fantomasProject = fantomasRoot </> "src" </> "Fantomas" </> "Fantomas.fsproj"

let fantomasBinary =
    fantomasRoot
    </> "src"
    </> "Fantomas"
    </> "bin"
    </> "Release"
    </> "net8.0"
    </> "fantomas.dll"

let repositoriesRoot = __SOURCE_DIRECTORY__ </> ".repositories"

type DirectoryInfo with

    member x.IsEmpty = Array.isEmpty (x.GetFiles()) && Array.isEmpty (x.GetDirectories())

let wrap workingDirectory file (arguments: string) =
    task {
        let! _result =
            Cli
                .Wrap(file)
                .WithWorkingDirectory(workingDirectory)
                .WithArguments(arguments)
                .WithStandardOutputPipe(PipeTarget.ToDelegate(printfn "%s"))
                .ExecuteAsync()
                .Task

        return ()
    }

let git (workingDirectory: string) (arguments: string) = wrap workingDirectory "git" arguments

let format (workingDirectory: string) (input: string list) =
    let input = String.concat " " input
    wrap workingDirectory "dotnet" $"{fantomasBinary} {input}"

let runCommands (workingDirectory: string) (commands: Command list) =
    task {
        for command in commands do
            let file =
                if command.File.Contains(".") then
                    workingDirectory </> command.File
                else
                    command.File

            do! wrap workingDirectory file command.Arguments
    }

printfn "Build Fantomas in Release"

let run (repository: Repository) : Task<unit> =
    task {
        printfn $"Start run for {repository.Name}"
        let directory = DirectoryInfo(repositoriesRoot </> repository.Name)
        directory.Create()
        let pwd = directory.FullName

        let git = git pwd

        if directory.IsEmpty then
            do! git "init"
            do! git $"remote add origin {repository.GitUrl}"

        do! git "fetch origin"
        do! git $"reset --hard {repository.Commit}"

        printfn $"Preparing {repository.Name}"
        do! runCommands pwd repository.Prepare

        printfn $"Building {repository.Name}"
        do! runCommands pwd repository.Build

        printfn $"Format {repository.Name} with latest Fantomas code"
        do! format pwd repository.Format

        printfn $"Rebuilding {repository.Name} after format"
        do! runCommands pwd repository.Build

        printfn "Revert format changes"
        do! git $"reset --hard {repository.Commit}"

        printfn $"Completed run for {repository.Name}"
    }

// Ensure Fantomas dll is built
(wrap fantomasRoot "dotnet" $"build -c Release {fantomasProject}").Wait()

// Run repositories
for repo in repositories do
    (run repo).Wait()
