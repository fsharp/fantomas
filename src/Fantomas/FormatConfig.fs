module Fantomas.FormatConfig

open System
open System.ComponentModel

let satSolveMaxStepsMaxSteps = 100

type FormatException(msg: string) =
    inherit Exception(msg)

type Num = int

type MultilineFormatterType =
    | CharacterWidth
    | NumberOfItems

    static member ToConfigString(cfg: MultilineFormatterType) =
        match cfg with
        | MultilineFormatterType.CharacterWidth -> "character_width"
        | MultilineFormatterType.NumberOfItems -> "number_of_items"

    static member OfConfigString(cfgString: string) =
        match cfgString with
        | "character_width" -> Some(box MultilineFormatterType.CharacterWidth)
        | "number_of_items" -> Some(box MultilineFormatterType.NumberOfItems)
        | _ -> None

[<RequireQualifiedAccess>]
type EndOfLineStyle =
    | LF
    | CR
    | CRLF
    member x.NewLineString =
        match x with
        | LF -> "\n"
        | CR -> "\r"
        | CRLF -> "\r\n"

    static member FromEnvironment =
        match Environment.NewLine with
        | "\n" -> LF
        | "\r\n" -> CRLF
        | other -> failwithf "Unknown system newline string found: %s" other

    static member ToConfigString(eol: EndOfLineStyle) =
        match eol with
        | EndOfLineStyle.LF -> "lf"
        | EndOfLineStyle.CR -> "cr"
        | EndOfLineStyle.CRLF -> "crlf"

    static member OfConfigString(eolString: string) =
        match eolString with
        | "lf" -> Some(EndOfLineStyle.LF)
        | "cr" -> failwith "Carriage returns are not valid for F# code, please use one of 'lf' or 'crlf'"
        | "crlf" -> Some(EndOfLineStyle.CRLF)
        | _ -> None

// NOTE: try to keep this list below in sync with the docs (e.g. Documentation.md)
type FormatConfig =
    { [<Category("Indentation")>]
      [<DisplayName("Indent spaces")>]
      [<Description("Number of spaces to use for indentation")>]
      IndentSize: Num

      [<Category("Boundaries")>]
      [<DisplayName("Maximum line length")>]
      [<Description("The column where we break to new lines")>]
      MaxLineLength: Num

      [<Category("Convention")>]
      [<DisplayName("Semicolon at end-of-line")>]
      [<Description("Forces a semicolon to be added to the end of a line")>]
      SemicolonAtEndOfLine: bool

      [<Category("Spacing")>]
      [<DisplayName("Before parameter")>]
      SpaceBeforeParameter: bool

      [<Category("Spacing")>]
      [<DisplayName("Before lowercase invocation")>]
      SpaceBeforeLowercaseInvocation: bool

      [<Category("Spacing")>]
      [<DisplayName("Before uppercase invocation")>]
      SpaceBeforeUppercaseInvocation: bool

      [<Category("Spacing")>]
      [<DisplayName("Before class constructor")>]
      SpaceBeforeClassConstructor: bool

      [<Category("Spacing")>]
      [<DisplayName("Before member")>]
      SpaceBeforeMember: bool

      [<Category("Spacing")>]
      [<DisplayName("Before colon")>]
      SpaceBeforeColon: bool

      [<Category("Spacing")>]
      [<DisplayName("After comma")>]
      SpaceAfterComma: bool

      [<Category("Spacing")>]
      [<DisplayName("Before semicolon")>]
      SpaceBeforeSemicolon: bool

      [<Category("Spacing")>]
      [<DisplayName("After semicolon")>]
      SpaceAfterSemicolon: bool

      [<Category("Indentation")>]
      [<DisplayName("Indent try-with")>]
      IndentOnTryWith: bool

      [<Category("Spacing")>]
      [<DisplayName("Around delimiter")>]
      SpaceAroundDelimiter: bool

      [<Category("Boundaries")>]
      [<DisplayName("Maximum if-then-else width")>]
      MaxIfThenElseShortWidth: Num

      [<Category("Boundaries")>]
      [<DisplayName("Maximum infix-operator expression")>]
      MaxInfixOperatorExpression: Num

      [<Category("Boundaries")>]
      [<DisplayName("Maximum record width")>]
      MaxRecordWidth: Num

      [<Category("Boundaries")>]
      [<DisplayName("Maximum items in a record")>]
      MaxRecordNumberOfItems: Num

      [<Category("Boundaries")>]
      [<DisplayName("Multi-line formatter for records")>]
      RecordMultilineFormatter: MultilineFormatterType

      [<Category("Boundaries")>]
      [<DisplayName("Maximum array or list width")>]
      MaxArrayOrListWidth: Num

      [<Category("Boundaries")>]
      [<DisplayName("Maximum number of items in array/list")>]
      MaxArrayOrListNumberOfItems: Num

      [<Category("Boundaries")>]
      [<DisplayName("Multi-line formatter for array/list")>]
      ArrayOrListMultilineFormatter: MultilineFormatterType

      [<Category("Boundaries")>]
      [<DisplayName("Maximum value-binding width")>]
      MaxValueBindingWidth: Num

      [<Category("Boundaries")>]
      [<DisplayName("Maximum function-binding width")>]
      MaxFunctionBindingWidth: Num

      [<Category("Boundaries")>]
      [<DisplayName("Maximum dot get expression width")>]
      MaxDotGetExpressionWidth: Num

      [<Category("Boundaries")>]
      [<DisplayName("Multiline-block brackets on same column")>]
      MultilineBlockBracketsOnSameColumn: bool

      [<Category("Convention")>]
      [<DisplayName("Newline between type definition and members")>]
      NewlineBetweenTypeDefinitionAndMembers: bool

      [<Category("Convention")>]
      [<DisplayName("Keep If-Then in same line")>]
      KeepIfThenInSameLine: bool

      [<Category("Elmish")>]
      [<DisplayName("Maximum width for elmish syntax")>]
      MaxElmishWidth: Num

      [<Category("Convention")>]
      [<DisplayName("Single-argument web mode")>]
      SingleArgumentWebMode: bool

      [<Category("Convention")>]
      [<DisplayName("Align function signature to indentation")>]
      AlignFunctionSignatureToIndentation: bool

      [<Category("Convention")>]
      [<DisplayName("Alternative long member definitions")>]
      AlternativeLongMemberDefinitions: bool

      [<Category("Boundaries")>]
      [<DisplayName("MultiLine-lambda has closing newline")>]
      MultiLineLambdaClosingNewline: bool

      [<Category("Elmish")>]
      [<DisplayName("Disable support for elmish syntax")>]
      DisableElmishSyntax: bool

      [<Category("Boundaries")>]
      [<DisplayName("Line-ending style")>]
      EndOfLine: EndOfLineStyle

      [<Category("Indentation")>]
      [<DisplayName("Keep indent in branch")>]
      KeepIndentInBranch: bool

      [<Category("Convention")>]
      [<DisplayName("Keep empty lines around nested multi-line expressions")>]
      BlankLinesAroundNestedMultilineExpressions: bool

      [<Category("Convention")>]
      [<DisplayName("Add a bar before DU declarations")>]
      BarBeforeDiscriminatedUnionDeclaration: bool

      [<Category("Convention")>]
      [<DisplayName("Strict mode")>]
      [<Description("Pretty printing based on ASTs only")>]
      StrictMode: bool }

    static member Default =
        { IndentSize = 4
          MaxLineLength = 120
          SemicolonAtEndOfLine = false
          SpaceBeforeParameter = true
          SpaceBeforeLowercaseInvocation = true
          SpaceBeforeUppercaseInvocation = false
          SpaceBeforeClassConstructor = false
          SpaceBeforeMember = false
          SpaceBeforeColon = false
          SpaceAfterComma = true
          SpaceBeforeSemicolon = false
          SpaceAfterSemicolon = true
          IndentOnTryWith = false
          SpaceAroundDelimiter = true
          MaxIfThenElseShortWidth = 40
          MaxInfixOperatorExpression = 50
          MaxRecordWidth = 40
          MaxRecordNumberOfItems = 1
          RecordMultilineFormatter = MultilineFormatterType.CharacterWidth
          MaxArrayOrListWidth = 40
          MaxArrayOrListNumberOfItems = 1
          ArrayOrListMultilineFormatter = MultilineFormatterType.CharacterWidth
          MaxValueBindingWidth = 80
          MaxFunctionBindingWidth = 40
          MaxDotGetExpressionWidth = 50
          MultilineBlockBracketsOnSameColumn = false
          NewlineBetweenTypeDefinitionAndMembers = false
          KeepIfThenInSameLine = false
          MaxElmishWidth = 40
          SingleArgumentWebMode = false
          AlignFunctionSignatureToIndentation = false
          AlternativeLongMemberDefinitions = false
          MultiLineLambdaClosingNewline = false
          DisableElmishSyntax = false
          EndOfLine = EndOfLineStyle.FromEnvironment
          KeepIndentInBranch = false
          BlankLinesAroundNestedMultilineExpressions = true
          BarBeforeDiscriminatedUnionDeclaration = false
          StrictMode = false }
