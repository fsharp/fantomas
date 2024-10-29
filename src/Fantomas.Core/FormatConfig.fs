namespace Fantomas.Core

open System
open System.ComponentModel
open Fantomas.FCS.Parse

exception ParseException of diagnostics: FSharpParserDiagnostic list

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
        | "character_width" -> Some MultilineFormatterType.CharacterWidth
        | "number_of_items" -> Some MultilineFormatterType.NumberOfItems
        | _ -> None

type PatternMatchStyle =
    | LineSpecific
    | Consistent

    static member ToConfigString(cfg: PatternMatchStyle) =
        match cfg with
        | LineSpecific -> "line_specific"
        | Consistent -> "consistent"

    static member OfConfigString(cfgString: string) =
        match cfgString with
        | "line_specific" -> Some LineSpecific
        | "consistent" -> Some Consistent
        | _ -> None

type MultilineBracketStyle =
    | Cramped
    | Aligned
    | Stroustrup

    static member ToConfigString(cfg: MultilineBracketStyle) =
        match cfg with
        | Cramped -> "cramped"
        | Aligned -> "aligned"
        | Stroustrup -> "stroustrup"

    static member OfConfigString(cfgString: string) =
        match cfgString with
        | "cramped" -> Some Cramped
        | "aligned" -> Some Aligned
        | "stroustrup" -> Some Stroustrup
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
        | "lf" -> Some EndOfLineStyle.LF
        | "cr" -> failwith "Carriage returns are not valid for F# code, please use one of 'lf' or 'crlf'"
        | "crlf" -> Some EndOfLineStyle.CRLF
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

      [<Category("Boundaries")>]
      [<DisplayName("Line-ending style")>]
      EndOfLine: EndOfLineStyle

      [<Category("Boundaries")>]
      [<DisplayName("Insert final newline")>]
      InsertFinalNewline: bool

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

      [<Category("Spacing")>]
      [<DisplayName("Around delimiter")>]
      SpaceAroundDelimiter: bool

      [<Category("Boundaries")>]
      [<DisplayName("Maximum if-then width")>]
      MaxIfThenShortWidth: Num

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

      [<Category("Convention")>]
      [<DisplayName("Newline between type definition and members")>]
      NewlineBetweenTypeDefinitionAndMembers: bool

      [<Category("Convention")>]
      [<DisplayName("Align function signature to indentation")>]
      AlignFunctionSignatureToIndentation: bool

      [<Category("Convention")>]
      [<DisplayName("Alternative long member definitions")>]
      AlternativeLongMemberDefinitions: bool

      [<Category("Boundaries")>]
      [<DisplayName("MultiLine-lambda has closing newline")>]
      MultiLineLambdaClosingNewline: bool

      [<Category("Indentation")>]
      [<DisplayName("Keep indent in branch")>]
      [<Description("Experimental feature, use at your own risk.")>]
      ExperimentalKeepIndentInBranch: bool

      [<Category("Convention")>]
      [<DisplayName("Keep empty lines around nested multi-line expressions")>]
      BlankLinesAroundNestedMultilineExpressions: bool

      [<Category("Convention")>]
      [<DisplayName("Add a bar before Discriminated Union declarations")>]
      BarBeforeDiscriminatedUnionDeclaration: bool

      [<Category("Convention")>]
      [<DisplayName("How to format bracket expressions (arrays, objects, etc.) that span multiple lines")>]
      [<Description("Possible options include cramped (default), aligned, and stroustrup")>]
      MultilineBracketStyle: MultilineBracketStyle

      [<Category("Convention")>]
      [<DisplayName("Maximum number of consecutive blank lines to keep")>]
      KeepMaxNumberOfBlankLines: Num

      [<Category("Convention")>]
      [<DisplayName("Insert a newline before a computation expression that spans multiple lines")>]
      NewlineBeforeMultilineComputationExpression: bool

      [<Category("Convention")>]
      [<DisplayName("Applies the Stroustrup style to the final (two) array or list argument(s) in a function application")>]
      ExperimentalElmish: bool

      [<Category("Indentation")>]
      [<DisplayName("How to format pattern match expression")>]
      [<Description("Possible options include each line judged separately (default), or consistent (either all single line or all multiline)")>]
      ExperimentalPatternMatchStyle: PatternMatchStyle }

    member x.IsStroustrupStyle = x.MultilineBracketStyle = Stroustrup

    static member Default =
        { IndentSize = 4
          MaxLineLength = 120
          EndOfLine = EndOfLineStyle.FromEnvironment
          InsertFinalNewline = true
          SpaceBeforeParameter = true
          SpaceBeforeLowercaseInvocation = true
          SpaceBeforeUppercaseInvocation = false
          SpaceBeforeClassConstructor = false
          SpaceBeforeMember = false
          SpaceBeforeColon = false
          SpaceAfterComma = true
          SpaceBeforeSemicolon = false
          SpaceAfterSemicolon = true
          SpaceAroundDelimiter = true
          MaxIfThenShortWidth = 0
          MaxIfThenElseShortWidth = 60
          MaxInfixOperatorExpression = 80
          MaxRecordWidth = 40
          MaxRecordNumberOfItems = 1
          RecordMultilineFormatter = MultilineFormatterType.CharacterWidth
          MaxArrayOrListWidth = 80
          MaxArrayOrListNumberOfItems = 1
          ArrayOrListMultilineFormatter = MultilineFormatterType.CharacterWidth
          MaxValueBindingWidth = 80
          MaxFunctionBindingWidth = 40
          MaxDotGetExpressionWidth = 80
          NewlineBetweenTypeDefinitionAndMembers = true
          AlignFunctionSignatureToIndentation = false
          AlternativeLongMemberDefinitions = false
          MultiLineLambdaClosingNewline = false
          ExperimentalKeepIndentInBranch = false
          BlankLinesAroundNestedMultilineExpressions = true
          BarBeforeDiscriminatedUnionDeclaration = false
          MultilineBracketStyle = Cramped
          KeepMaxNumberOfBlankLines = 100
          NewlineBeforeMultilineComputationExpression = true
          ExperimentalElmish = false
          ExperimentalPatternMatchStyle = LineSpecific }
