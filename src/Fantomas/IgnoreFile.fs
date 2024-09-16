namespace Fantomas

open System.IO.Abstractions
open Ignore
open Fantomas.Logging

type AbsoluteFilePath =
    private
    | AbsoluteFilePath of string

    member x.Path =
        let (AbsoluteFilePath(path)) = x
        path

    static member Create (fs: IFileSystem) (filePath: string) =
        fs.Path.GetFullPath filePath |> AbsoluteFilePath

/// The string argument is taken relative to the location
/// of the ignore-file.
type IsPathIgnored = AbsoluteFilePath -> bool

[<NoComparison; NoEquality>]
type IgnoreFile =
    { Location: IFileInfo
      IsIgnored: IsPathIgnored }

[<RequireQualifiedAccess>]
module IgnoreFile =

    [<Literal>]
    let IgnoreFileName = ".fantomasignore"

    /// Find the `.fantomasignore` file above the given filepath, if one exists.
    /// Note that this is intended for use only in the daemon; the command-line tool
    /// does not support `.fantomasignore` files anywhere other than the current
    /// working directory.
    let find (fs: IFileSystem) (loadIgnoreList: string -> IsPathIgnored) (filePath: string) : IgnoreFile option =
        let rec walkUp (currentDirectory: IDirectoryInfo) : IgnoreFile option =
            if isNull currentDirectory then
                None
            else
                let potentialFile =
                    fs.Path.Combine(currentDirectory.FullName, IgnoreFileName) |> fs.FileInfo.New

                if potentialFile.Exists then
                    { Location = potentialFile
                      IsIgnored = loadIgnoreList potentialFile.FullName }
                    |> Some
                else
                    walkUp currentDirectory.Parent

        walkUp (fs.FileInfo.New(filePath).Directory)

    let loadIgnoreList (fs: IFileSystem) (ignoreFilePath: string) : IsPathIgnored =
        let lines = fs.File.ReadAllLines(ignoreFilePath)

        let fantomasIgnore =
            (Ignore(), lines)
            ||> Array.fold (fun (ig: Ignore) (line: string) -> ig.Add(line))

        fun (absoluteFilePath: AbsoluteFilePath) ->
            // See https://git-scm.com/docs/gitignore
            // We transform the incoming path relative to the .ignoreFilePath folder.
            // In a cli scenario that is the current directory, for the daemon it is the first found ignore file.
            // .gitignore uses forward slashes to path separators
            let relativePath =
                fs.Path
                    .GetRelativePath(fs.Directory.GetParent(ignoreFilePath).FullName, absoluteFilePath.Path)
                    .Replace("\\", "/")

            fantomasIgnore.IsIgnored(relativePath)

    let internal current' (fs: IFileSystem) (currentDirectory: string) (loadIgnoreList: string -> IsPathIgnored) =
        lazy find fs loadIgnoreList (fs.Path.Combine(currentDirectory, "_"))

    /// When executed from the command line, Fantomas will not dynamically locate
    /// the most appropriate `.fantomasignore` for each input file; it only finds
    /// a single `.fantomasignore` file. This is that file.
    let current: Lazy<IgnoreFile option> =
        let fs = FileSystem()
        current' fs System.Environment.CurrentDirectory (loadIgnoreList fs)

    let isIgnoredFile (ignoreFile: IgnoreFile option) (file: string) : bool =
        match ignoreFile with
        | None -> false
        | Some ignoreFile ->
            let fs = ignoreFile.Location.FileSystem
            let fullPath = AbsoluteFilePath.Create fs file

            try
                ignoreFile.IsIgnored fullPath
            with ex ->
                elog $"%O{ex}"
                false
