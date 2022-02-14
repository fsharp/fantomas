namespace Fantomas.Extras

open System.IO
open MAB.DotIgnore

type IgnoreFile =
    { Location: FileInfo
      IgnoreList: IgnoreList }

[<RequireQualifiedAccess>]
module IgnoreFile =

    [<Literal>]
    let IgnoreFileName = ".fantomasignore"

    /// Find the `.fantomasignore` file above the given filepath, if one exists.
    /// Note that this is intended for use only in the daemon; the command-line tool
    /// does not support `.fantomasignore` files anywhere other than the current
    /// working directory.
    let find (filePath: string) : IgnoreFile option =
        let rec walkUp (currentDirectory: DirectoryInfo) : IgnoreFile option =
            if isNull currentDirectory then
                None
            else
                let potentialFile =
                    Path.Combine(currentDirectory.FullName, IgnoreFileName)
                    |> FileInfo

                if potentialFile.Exists then
                    { Location = potentialFile
                      IgnoreList = IgnoreList(potentialFile.FullName) }
                    |> Some
                else
                    walkUp currentDirectory.Parent

        walkUp (FileInfo(filePath).Directory)

    let private relativePathPrefix = sprintf ".%c" Path.DirectorySeparatorChar

    let private removeRelativePathPrefix (path: string) =
        if path.StartsWith(relativePathPrefix) then
            path.Substring(2)
        else
            path

    /// When executed from the command line, Fantomas only supports a `.fantomasignore` file in the
    /// current working directory. This is that `.fantomasignore` file.
    let current: Lazy<IgnoreFile option> =
        lazy
            let path =
                Path.Combine(System.Environment.CurrentDirectory, IgnoreFileName)
                |> FileInfo

            if path.Exists then
                { Location = path
                  IgnoreList = IgnoreList(path.FullName) }
                |> Some
            else
                None

    let isIgnoredFile (ignoreFile: IgnoreFile option) (file: string) : bool =
        match ignoreFile with
        | None -> false
        | Some ignoreFile ->
            let fullPath = Path.GetFullPath(file)

            try
                let path = removeRelativePathPrefix fullPath
                ignoreFile.IgnoreList.IsIgnored(path, false)
            with
            | ex ->
                printfn "%A" ex
                false
