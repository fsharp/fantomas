namespace Fantomas.Core

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

[<Sealed>]
type CodeFormatter =
    static member ParseAsync(isSignature, source) : Async<(ParsedInput * string list) array> =
        CodeFormatterImpl.getSourceText source |> CodeFormatterImpl.parse isSignature

    static member FormatASTAsync(ast: ParsedInput, ?source, ?config) : Async<FormatResult> =
        let sourceText = Option.map CodeFormatterImpl.getSourceText source
        let config = Option.defaultValue FormatConfig.Default config

        CodeFormatterImpl.formatAST ast sourceText config None |> async.Return

    static member FormatDocumentAsync(isSignature, source, ?config, ?cursor: Position) =
        let config = Option.defaultValue FormatConfig.Default config
        CodeFormatterImpl.formatDocument config isSignature (CodeFormatterImpl.getSourceText source) cursor

    static member FormatSelectionAsync(isSignature, source, selection, config) =
        let config = Option.defaultValue FormatConfig.Default config

        CodeFormatterImpl.getSourceText source
        |> Selection.formatSelection config isSignature selection

    static member IsValidFSharpCodeAsync(isSignature: bool, source: string) =
        Validation.isValidFSharpCode isSignature source

    static member GetVersion() = Version.fantomasVersion.Value

    static member MakeRange(fileName, startLine, startCol, endLine, endCol) =
        Range.mkRange fileName (Position.mkPos startLine startCol) (Position.mkPos endLine endCol)

    static member MakeSomePosition(line, column) = Position.mkPos line column |> Some
