module Fantomas.Context

open System
open FSharp.Compiler.Range
open Fantomas
open Fantomas.FormatConfig
open Fantomas.TriviaTypes

type WriterEvent =
    | Write of string
    | WriteLine
    | WriteLineInsideStringConst
    | WriteBeforeNewline of string
    | IndentBy of int
    | UnIndentBy of int
    | SetIndent of int
    | RestoreIndent of int
    | SetAtColumn of int
    | RestoreAtColumn of int

type WriterModel = {
    /// lines of resulting text, in reverse order (to allow more efficient adding line to end)
    Lines : string list
    /// current indentation
    Indent : int
    /// helper indentation information, if AtColumn > Indent after NewLine, Indent will be set to AtColumn
    AtColumn : int
    /// text to be written before next newline
    WriteBeforeNewline : string
    /// dummy = "fake" writer used in `autoNln`, `autoNlnByFuture`
    IsDummy : bool
} with
    member x.Column = List.head x.Lines |> String.length 

module WriterModel =
    let init = {
        Lines = [""]
        Indent = 0
        AtColumn = 0
        WriteBeforeNewline = ""
        IsDummy = false
    }
    
    let update cmd m =
        let doNewline m =
            let m = { m with Indent = max m.Indent m.AtColumn }
            { m with
               Lines = String.replicate m.Indent " " :: (List.head m.Lines + m.WriteBeforeNewline) :: (List.tail m.Lines) 
               WriteBeforeNewline = "" }
        match cmd with
        | WriteLine -> doNewline m
        | WriteLineInsideStringConst ->
            { m with Lines = "" :: m.Lines }
        | Write s -> { m with Lines = (List.head m.Lines + s) :: (List.tail m.Lines) }
        | WriteBeforeNewline s -> { m with WriteBeforeNewline = s }
        | IndentBy x -> { m with Indent = if m.AtColumn >= m.Indent + x then m.AtColumn + x else m.Indent + x }
        | UnIndentBy x -> { m with Indent = max m.AtColumn <| m.Indent - x }
        | SetAtColumn c -> { m with AtColumn = c }
        | RestoreAtColumn c -> { m with AtColumn = c }
        | SetIndent c -> { m with Indent = c }
        | RestoreIndent c -> { m with Indent = c }
        
    let updateAll cmds m = cmds |> List.fold (fun m c -> update c m) m
    
module WriterEvents =
    let normalize ev =
        match ev with
        | Write s when String.normalizeThenSplitNewLine s |> Array.length > 1 ->
            String.normalizeThenSplitNewLine s |> Seq.map (fun x -> [Write x]) |> Seq.reduce (fun x y -> x @ [ WriteLineInsideStringConst] @ y) |> Seq.toList
        | _ -> [ev]
        
    let isMultiline evs =
        evs |> List.exists (function | WriteLine -> true | _ -> false)

type internal Context = 
    { Config : FormatConfig; 
      WriterInitModel : WriterModel
      WriterEvents : WriterEvent list;
      BreakLines : bool;
      BreakOn : string -> bool;
      /// The original source string to query as a last resort 
      Content : string; 
      /// Positions of new lines in the original source string
      Positions : int []; 
      Trivia : TriviaNode list
      RecordBraceStart: int list }

    /// Initialize with a string writer and use space as delimiter
    static member Default = 
        { Config = FormatConfig.Default
          WriterInitModel = WriterModel.init
          WriterEvents = []
          BreakLines = true; BreakOn = (fun _ -> false) 
          Content = ""
          Positions = [||]
          Trivia = []
          RecordBraceStart = [] }

    static member create config defines (content : string) maybeAst =
        let content = String.normalizeNewLine content
        let positions = 
            content.Split('\n')
            |> Seq.map (fun s -> String.length s + 1)
            |> Seq.scan (+) 0
            |> Seq.toArray

        let (tokens, lineCount) = TokenParser.tokenize defines content
        let trivia =
            match maybeAst, config.StrictMode with
            | Some ast, false -> Trivia.collectTrivia config tokens lineCount ast
            | _ -> Context.Default.Trivia

        { Context.Default with 
            Config = config
            Content = content
            Positions = positions 
            Trivia = trivia }

    member x.MemoizeProjection = x.WriterInitModel, x.WriterEvents, x.Trivia, x.BreakLines, x.RecordBraceStart
    
    member x.WithDummy(writerCommands, ?keepPageWidth) =
        let keepPageWidth = keepPageWidth |> Option.defaultValue false
        // Use infinite column width to encounter worst-case scenario
        let m = WriterModel.updateAll x.WriterEvents x.WriterInitModel
        let model = { m with IsDummy = true; Lines = [String.replicate m.Column " "]; WriteBeforeNewline = "" }
        let config = { x.Config with PageWidth = if keepPageWidth then x.Config.PageWidth else Int32.MaxValue }
        { x with WriterInitModel = model; WriterEvents = writerCommands; Config = config }

let internal writerEvent e ctx = { ctx with WriterEvents = ctx.WriterEvents @ (WriterEvents.normalize e) }
let internal applyWriterEvents (ctx: Context) =
    let m = WriterModel.updateAll ctx.WriterEvents ctx.WriterInitModel
    if m.WriteBeforeNewline <> "" then WriterModel.update (Write m.WriteBeforeNewline) m else m
let internal dump (ctx: Context) =
    let m = applyWriterEvents ctx
    m.Lines |> List.rev |> String.concat Environment.NewLine

let internal dumpAndContinue (ctx: Context) =
    let m = applyWriterEvents ctx
    let lines = m.Lines |> List.rev
    let code = String.concat Environment.NewLine lines
#if DEBUG
    printfn "%s" code
#endif
    ctx

type Context with    
    member x.Column = (applyWriterEvents x).Column
    member x.ApplyWriterEvents = applyWriterEvents x

// A few utility functions from https://github.com/fsharp/powerpack/blob/master/src/FSharp.Compiler.CodeDom/generator.fs

/// Indent one more level based on configuration
let internal indent (ctx : Context) = 
    // if atColumn is bigger then after indent, then we use atColumn as base for indent
    writerEvent (IndentBy ctx.Config.IndentSpaceNum) ctx

/// Unindent one more level based on configuration
let internal unindent (ctx : Context) = 
    writerEvent (UnIndentBy ctx.Config.IndentSpaceNum) ctx

/// Increase indent by i spaces
let internal incrIndent i (ctx : Context) = 
    writerEvent (IndentBy i) ctx

/// Decrease indent by i spaces
let internal decrIndent i (ctx : Context) = 
    writerEvent (UnIndentBy i) ctx

/// Apply function f at an absolute indent level (use with care)
let internal atIndentLevel alsoSetIndent level (f : Context -> Context) (ctx: Context) =
    if level < 0 then
        invalidArg "level" "The indent level cannot be negative."
    let m = applyWriterEvents ctx
    let oldIndent = m.Indent
    let oldColumn = m.AtColumn
    (writerEvent (SetAtColumn level)
    >> if alsoSetIndent then writerEvent (SetIndent level) else id
    >> f
    >> writerEvent (RestoreAtColumn oldColumn)
    >> writerEvent (RestoreIndent oldIndent)) ctx

/// Set minimal indentation (`atColumn`) at current column position - next newline will be indented on `max indent atColumn`
/// Example:
/// { X = // indent=0, atColumn=2
///     "some long string" // indent=4, atColumn=2
///   Y = 1 // indent=0, atColumn=2
/// }
/// `atCurrentColumn` was called on `X`, then `indent` was called, but "some long string" have indent only 4, because it is bigger than `atColumn` (2).
let internal atCurrentColumn (f : _ -> Context) (ctx : Context) =
    atIndentLevel false ctx.Column f ctx

/// Like atCurrentColumn, but use current column after applying prependF
let internal atCurrentColumnWithPrepend (prependF : _ -> Context) (f : _ -> Context) (ctx : Context) =
    let col = ctx.Column
    (prependF >> atIndentLevel false col f) ctx

/// Write everything at current column indentation, set `indent` and `atColumn` on current column position
/// /// Example (same as above):
/// { X = // indent=2, atColumn=2
///       "some long string" // indent=6, atColumn=2
///   Y = 1 // indent=2, atColumn=2
/// }
/// `atCurrentColumn` was called on `X`, then `indent` was called, "some long string" have indent 6, because it is indented from `atCurrentColumn` pos (2).
let internal atCurrentColumnIndent (f : _ -> Context) (ctx : Context) =
    atIndentLevel true ctx.Column f ctx

/// Function composition operator
let internal (+>) (ctx : Context -> Context) (f : _ -> Context) x =
    f (ctx x)

/// Break-line and append specified string
let internal (++) (ctx : Context -> Context) (str : string) x =
    ctx x
    |> writerEvent WriteLine
    |> writerEvent (Write str)

/// Break-line if config says so
let internal (+-) (ctx : Context -> Context) (str : string) x =
    let c = ctx x
    let c =
        if c.BreakOn str then 
            writerEvent WriteLine c
        else
            writerEvent (Write " ") c
    writerEvent (Write str) c

/// Append specified string without line-break
let internal (--) (ctx : Context -> Context) (str : string) x =
    ctx x
    |> writerEvent (Write str)

/// Break-line unless we are on empty line
let internal (+~) (ctx : Context -> Context) (str : string) x =
    let addNewline ctx =
        let lines = dump ctx |> String.normalizeThenSplitNewLine
        lines
        |> Array.tryLast
        |> Option.map (fun (line:string) -> not(System.String.IsNullOrWhiteSpace(line)))
        |> Option.defaultValue false
    let c = ctx x
    let c =
        if addNewline c then 
            writerEvent WriteLine c
        else c
    writerEvent (Write str) c

let internal (!-) (str : string) = id -- str 
let internal (!+) (str : string) = id ++ str 
let internal (!+-) (str : string) = id +- str 
let internal (!+~) (str : string) = id +~ str 

/// Print object converted to string
let internal str (o : 'T) (ctx : Context) =
    ctx |> writerEvent (Write (o.ToString()))

/// Similar to col, and supply index as well
let internal coli f' (c : seq<'T>) f (ctx : Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let mutable i = 0
    let e = c.GetEnumerator()   
    while (e.MoveNext()) do
        if tryPick then tryPick <- false else st <- f' st
        st <- f i (e.Current) st
        i  <- i + 1
    st

/// Process collection - keeps context through the whole processing
/// calls f for every element in sequence and f' between every two elements 
/// as a separator. This is a variant that works on typed collections.
let internal col f' (c : seq<'T>) f (ctx : Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let e = c.GetEnumerator()   
    while (e.MoveNext()) do
        if tryPick then tryPick <- false else st <- f' st
        st <- f (e.Current) st
    st

// Similar to col but pass the item of 'T to f' as well
let internal colEx f' (c : seq<'T>) f (ctx: Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let e = c.GetEnumerator()   
    while (e.MoveNext()) do
        if tryPick then tryPick <- false else st <- f' e.Current st
        st <- f (e.Current) st
    st

/// Similar to col, apply one more function f2 at the end if the input sequence is not empty
let internal colPost f2 f1 (c : seq<'T>) f (ctx : Context) =
    if Seq.isEmpty c then ctx
    else f2 (col f1 c f ctx)

/// Similar to col, apply one more function f2 at the beginning if the input sequence is not empty
let internal colPre f2 f1 (c : seq<'T>) f (ctx : Context) =
    if Seq.isEmpty c then ctx
    else col f1 c f (f2 ctx)

let internal colPreEx f2 f1 (c : seq<'T>) f (ctx : Context) =
    if Seq.isEmpty c then ctx
    else colEx f1 c f (f2 ctx)

/// If there is a value, apply f and f' accordingly, otherwise do nothing
let internal opt (f' : Context -> _) o f (ctx : Context) =
    match o with
    | Some x -> f' (f x ctx)
    | None -> ctx

/// Similar to opt, but apply f2 at the beginning if there is a value
let internal optPre (f2 : _ -> Context) (f1 : Context -> _) o f (ctx : Context) =
    match o with
    | Some x -> f1 (f x (f2 ctx))
    | None -> ctx

/// b is true, apply f1 otherwise apply f2
let internal ifElse b (f1 : Context -> Context) f2 (ctx : Context) =
    if b then f1 ctx else f2 ctx

let internal ifElseCtx cond (f1 : Context -> Context) f2 (ctx : Context) =
    if cond ctx then f1 ctx else f2 ctx

/// Repeat application of a function n times
let internal rep n (f : Context -> Context) (ctx : Context) =
    [1..n] |> List.fold (fun c _ -> f c) ctx

let internal wordAnd = !- " and "
let internal wordOr = !- " or "
let internal wordOf = !- " of "   

// Separator functions
        
let internal sepDot = !- "."
let internal sepSpace =
    // ignore multiple spaces, space on start of file, after newline
    // TODO: this is inefficient - maybe remember last char written?
    fun (ctx: Context) ->
        if (not ctx.WriterInitModel.IsDummy && let s = dump ctx in s = "" || s.EndsWith " " || s.EndsWith Environment.NewLine) then ctx
        else (!- " ") ctx      
let internal sepNln = !+ ""
let internal sepStar = !- " * "
let internal sepEq = !- " ="
let internal sepArrow = !- " -> "
let internal sepWild = !- "_"
let internal sepNone = id
let internal sepBar = !- "| "

/// opening token of list
let internal sepOpenL (ctx : Context) =  
    if ctx.Config.SpaceAroundDelimiter then str "[ " ctx else str "[" ctx 

/// closing token of list
let internal sepCloseL (ctx : Context) =
    if ctx.Config.SpaceAroundDelimiter then str " ]" ctx else str "]" ctx 

/// opening token of list
let internal sepOpenLFixed = !- "["

/// closing token of list
let internal sepCloseLFixed = !- "]"

/// opening token of array
let internal sepOpenA (ctx : Context) =
    if ctx.Config.SpaceAroundDelimiter then str "[| " ctx else str "[|" ctx 

/// closing token of array
let internal sepCloseA (ctx : Context) = 
    if ctx.Config.SpaceAroundDelimiter then str " |]" ctx else str "|]" ctx 

/// opening token of list
let internal sepOpenAFixed = !- "[|"
/// closing token of list
let internal sepCloseAFixed = !- "|]"

/// opening token of sequence or record
let internal sepOpenS (ctx : Context) = 
    if ctx.Config.SpaceAroundDelimiter then str "{ " ctx else str "{" ctx 

/// closing token of sequence or record
let internal sepCloseS (ctx : Context) = 
    if ctx.Config.SpaceAroundDelimiter then str " }" ctx else str "}" ctx

/// opening token of anon record
let internal sepOpenAnonRecd (ctx : Context) =
    if ctx.Config.SpaceAroundDelimiter then str "{| " ctx else str "{|" ctx 

/// closing token of anon record
let internal sepCloseAnonRecd (ctx : Context) =
    if ctx.Config.SpaceAroundDelimiter then str " |}" ctx else str "|}" ctx

/// opening token of sequence
let internal sepOpenSFixed = !- "{"

/// closing token of sequence
let internal sepCloseSFixed = !- "}"

/// opening token of tuple
let internal sepOpenT = !- "("

/// closing token of tuple
let internal sepCloseT = !- ")"
let internal eventsWithoutMultilineWrite ctx =
    { ctx with WriterEvents =  ctx.WriterEvents |> List.filter (function | Write s when s.Contains ("\n") -> false | _ -> true) }

let internal autoNlnCheck (f: _ -> Context) sep (ctx : Context) =
    if not ctx.BreakLines then false else
    // Create a dummy context to evaluate length of current operation
    let dummyCtx = ctx.WithDummy([]) |> sep |> f 
    // This isn't accurate if we go to new lines
    dummyCtx.Column > ctx.Config.PageWidth

let internal futureNlnCheckMem = Cache.memoizeBy (fun (f, ctx : Context) -> Cache.LambdaEqByRef f, ctx.MemoizeProjection) <| fun (f, ctx) ->
    if ctx.WriterInitModel.IsDummy || not ctx.BreakLines then (false, false) else
    // Create a dummy context to evaluate length of current operation
    let dummyCtx : Context = ctx.WithDummy([], keepPageWidth = true) |> f
    WriterEvents.isMultiline dummyCtx.WriterEvents, dummyCtx.Column > ctx.Config.PageWidth

let internal futureNlnCheck f (ctx : Context) =
    let (isMultiLine, isLong) = futureNlnCheckMem (f, ctx)
    isMultiLine || isLong

let internal futureNlnCheckLazy f (ctx : Context) =
    let (isMultiLine, isLong) = futureNlnCheckMem (f, ctx)
    if isMultiLine then false else isLong

let internal autoNlnByFuture f = ifElseCtx (futureNlnCheck f) (sepNln +> f) f
let internal autoIndentNlnByFuture f = ifElseCtx (futureNlnCheck f) (indent +> sepNln +> f +> unindent) f

/// like autoNlnByFuture but don't do nln if there is another nln inside f
let internal autoNlnByFutureLazy f = ifElseCtx (futureNlnCheckLazy f) (sepNln +> f) f

/// similar to futureNlnCheck but validates whether the expression is going over the max page width
/// This functions is does not use any caching
let internal exceedsWidth maxWidth f (ctx: Context) =
    let dummyCtx : Context = ctx.WithDummy([], keepPageWidth = true)
    let currentColumn = dummyCtx.Column
    let ctxAfter : Context = f dummyCtx
    (ctxAfter.Column - currentColumn) > maxWidth

/// Set a checkpoint to break at an appropriate column
let internal autoNlnOrAddSep f sep (ctx : Context) =
    let isNln = autoNlnCheck f sep ctx
    if isNln then
       f (sepNln ctx)
    else
       f (sep ctx)

let internal autoNln f (ctx : Context) = autoNlnOrAddSep f sepNone ctx

let internal autoNlnOrSpace f (ctx : Context) = autoNlnOrAddSep f sepSpace ctx

/// Similar to col, skip auto newline for index 0
let internal colAutoNlnSkip0i f' (c : seq<'T>) f (ctx : Context) = 
    coli f' c (fun i c -> if i = 0 then f i c else autoNln (f i c)) ctx

/// Similar to col, skip auto newline for index 0
let internal colAutoNlnSkip0 f' c f = colAutoNlnSkip0i f' c (fun _ -> f)

/// Skip all auto-breaking newlines
let internal noNln f (ctx : Context) : Context = 
    let res = f { ctx with BreakLines = false }
    { res with BreakLines = ctx.BreakLines }

let internal sepColon (ctx : Context) = 
    if ctx.Config.SpaceBeforeColon then str " : " ctx else str ": " ctx

let internal sepColonFixed = !- ":"

let internal sepColonWithSpacesFixed = !- " : "

let internal sepComma (ctx : Context) = 
    if ctx.Config.SpaceAfterComma then str ", " ctx else str "," ctx

let internal sepSemi (ctx : Context) = 
    if ctx.Config.SpaceAfterSemicolon then str "; " ctx else str ";" ctx

let internal sepSemiNln (ctx : Context) =
    // sepNln part is essential to indentation
    if ctx.Config.SemicolonAtEndOfLine then (!- ";" +> sepNln) ctx else sepNln ctx

let internal sepBeforeArg (ctx : Context) = 
    if ctx.Config.SpaceBeforeArgument then str " " ctx else str "" ctx

/// Conditional indentation on with keyword
let internal indentOnWith (ctx : Context) =
    if ctx.Config.IndentOnTryWith then indent ctx else ctx

/// Conditional unindentation on with keyword
let internal unindentOnWith (ctx : Context) =
    if ctx.Config.IndentOnTryWith then unindent ctx else ctx

let internal sortAndDeduplicate by l (ctx : Context) =
    if ctx.Config.ReorderOpenDeclaration then
        l |> Seq.distinctBy by |> Seq.sortBy by |> List.ofSeq
    else l

/// Don't put space before and after these operators
let internal NoSpaceInfixOps = set ["?"]

/// Always break into newlines on these operators
let internal NewLineInfixOps = set ["|>"; "||>"; "|||>"; ">>"; ">>="]

/// Never break into newlines on these operators
let internal NoBreakInfixOps = set ["="; ">"; "<";]

let internal printTriviaContent (c: TriviaContent) (ctx: Context) =
    let currentLastLine =
        let m = applyWriterEvents ctx
        m.Lines
        |> List.tryHead

    // Some items like #if of Newline should be printed on a newline
    // It is hard to always get this right in CodePrinter, so we detect it based on the current code.
    let addNewline =
        currentLastLine
        |> Option.map(fun line -> line.Trim().Length > 0)
        |> Option.defaultValue false

    let addSpace =
        currentLastLine
        |> Option.bind(fun line -> Seq.tryLast line |> Option.map (fun lastChar -> lastChar <> ' '))
        |> Option.defaultValue false

    match c with
    | Comment(LineCommentAfterSourceCode s) ->
        let comment = sprintf "%s%s" (if addSpace then " " else String.empty) s
        writerEvent (WriteBeforeNewline comment)
    | Comment(BlockComment(s, before, after)) ->
        ifElse (before && addNewline) sepNln sepNone
        +> sepSpace -- s +> sepSpace
        +> ifElse after sepNln sepNone
    | Newline ->
        (ifElse addNewline (sepNln +> sepNln) sepNln)
    | Keyword _
    | Number _
    | StringContent _
    | IdentOperatorAsWord _
    | IdentBetweenTicks _
    | NewlineAfter
    | CharContent _
         -> sepNone // don't print here but somewhere in CodePrinter
    | Directive(s)
    | Comment(LineCommentOnSingleLine s) ->
        (ifElse addNewline sepNln sepNone) +> !- s +> sepNln
    <| ctx

let private removeNodeFromContext triviaNode (ctx: Context) =
    let newNodes = List.filter (fun tn -> tn <> triviaNode) ctx.Trivia
    { ctx with Trivia = newNodes }

let internal printContentBefore triviaNode =
    // Make sure content is not being printed twice.
    let removeBeforeContentOfTriviaNode =
        fun (ctx:Context) ->
            let trivia =
                ctx.Trivia
                |> List.map (fun tn ->
                    let contentBefore =
                        tn.ContentBefore
                        |> List.filter(fun cb ->
                            match cb with
                            | Keyword _
                            | Number _
                            | StringContent _
                            | IdentOperatorAsWord _ ->
                                true
                            | _ -> false)
                    if tn = triviaNode then
                        { tn with ContentBefore = contentBefore }
                    else
                        tn
                ) 
            { ctx with Trivia = trivia }
        
    col sepNone triviaNode.ContentBefore printTriviaContent +> removeBeforeContentOfTriviaNode

let internal printContentAfter triviaNode =
    col sepNone triviaNode.ContentAfter printTriviaContent

let private findTriviaMainNodeFromRange nodes (range:range) =
    nodes
    |> List.tryFind(fun n ->
        Trivia.isMainNode n && n.Range.Start = range.Start && n.Range.End = range.End)

let private findTriviaMainNodeOrTokenOnStartFromRange nodes (range:range) =
    nodes
    |> List.tryFind(fun n ->
        Trivia.isMainNode n && n.Range.Start = range.Start && n.Range.End = range.End
        || Trivia.isToken n && n.Range.Start = range.Start)

let private findTriviaMainNodeOrTokenOnEndFromRange nodes (range:range) =
    nodes
    |> List.tryFind(fun n ->
        Trivia.isMainNode n && n.Range.Start = range.Start && n.Range.End = range.End
        || Trivia.isToken n && n.Range.End = range.End)

let private findTriviaTokenFromRange nodes (range:range) =
    nodes
    |> List.tryFind(fun n -> Trivia.isToken n && n.Range.Start = range.Start && n.Range.End = range.End)

let internal findTriviaTokenFromName (range: range) nodes (tokenName:string) =
    nodes
    |> List.tryFind(fun n ->
        match n.Type with
        | Token(tn) when tn.TokenInfo.TokenName = tokenName ->
            RangeHelpers.``range contains`` range n.Range
        | _ -> false)

let internal enterNodeWith f x (ctx: Context) =
    match f ctx.Trivia x with
    | Some triviaNode ->
        (printContentBefore triviaNode) ctx
    | None -> ctx
let internal enterNode (range: range) (ctx: Context) = enterNodeWith findTriviaMainNodeOrTokenOnStartFromRange range ctx
let internal enterNodeToken (range: range) (ctx: Context) = enterNodeWith findTriviaTokenFromRange range ctx
let internal enterNodeTokenByName (range: range) (tokenName:string) (ctx: Context) = enterNodeWith (findTriviaTokenFromName range) tokenName ctx

let internal leaveNodeWith f x (ctx: Context) =
    match f ctx.Trivia x with
    | Some triviaNode ->
        ((printContentAfter triviaNode) +> (removeNodeFromContext triviaNode)) ctx
    | None -> ctx
let internal leaveNode (range: range) (ctx: Context) = leaveNodeWith findTriviaMainNodeOrTokenOnEndFromRange range ctx
let internal leaveNodeToken (range: range) (ctx: Context) = leaveNodeWith findTriviaTokenFromRange range ctx
let internal leaveNodeTokenByName (range: range) (tokenName:string) (ctx: Context) = leaveNodeWith (findTriviaTokenFromName range) tokenName ctx
    
let internal leaveEqualsToken (range: range) (ctx: Context) =
    ctx.Trivia
    |> List.filter(fun tn ->
        match tn.Type with
        | Token(tok) ->
            tok.TokenInfo.TokenName = "EQUALS" && tn.Range.StartLine = range.StartLine
        | _ -> false
    )
    |> List.tryHead
    |> fun tn ->
        match tn with
        | Some({ ContentAfter = [TriviaContent.Comment(LineCommentAfterSourceCode(lineComment))] } as tn) ->
            sepSpace +> !- lineComment +> removeNodeFromContext tn
        | _ ->
            id
    <| ctx

let internal leaveLeftToken (tokenName: string) (range: range) (ctx: Context) =
    ctx.Trivia
    |> List.tryFind(fun tn ->
        // Token is a left brace { at the beginning of the range.
        match tn.Type with
        | Token(tok) ->
            tok.TokenInfo.TokenName = tokenName && tn.Range.StartLine = range.StartLine && tn.Range.StartColumn = range.StartColumn
        | _ -> false
    )
    |> fun tn ->
        match tn with
        | Some({ ContentAfter = [TriviaContent.Comment(LineCommentAfterSourceCode(lineComment))] } as tn) ->
            !- lineComment +> sepNln +> removeNodeFromContext tn
        | _ ->
            id
    <| ctx

let internal leaveLeftBrace = leaveLeftToken "LBRACE"
let internal leaveLeftBrack = leaveLeftToken "LBRACK"
let internal leaveLeftBrackBar = leaveLeftToken "LBRACK_BAR"

let internal enterRightToken (tokenName: string) (range: range) (ctx: Context) =
    ctx.Trivia
    |> List.tryFind(fun tn ->
        // Token is a left brace { at the beginning of the range.
        match tn.Type with
        | Token(tok) ->
            (tok.TokenInfo.TokenName = tokenName)
            && tn.Range.EndLine = range.EndLine
            && (tn.Range.EndColumn = range.EndColumn || tn.Range.EndColumn + 1 = range.EndColumn)
        | _ -> false
    )
    |> fun tn ->
        match tn with
        | Some({ ContentBefore = [TriviaContent.Comment(LineCommentOnSingleLine(lineComment))] } as tn) ->
            let spacesBeforeComment =
                let braceSize = if tokenName = "RBRACK" then 1 else 2
                let spaceAround = if ctx.Config.SpaceAroundDelimiter then 1 else 0
                !- String.Empty.PadLeft(braceSize + spaceAround)

            let spaceAfterNewline = if ctx.Config.SpaceAroundDelimiter then sepSpace else sepNone
            sepNln +> spacesBeforeComment +> !- lineComment +> sepNln +> spaceAfterNewline +> removeNodeFromContext tn
        | _ ->
            id
    <| ctx

let internal enterRightBracket = enterRightToken "RBRACK"
let internal enterRightBracketBar = enterRightToken "BAR_RBRACK"
let internal hasPrintableContent (trivia: TriviaContent list) =
    trivia
    |> List.filter (fun tn ->
        match tn with
        | Comment(_) -> true
        | Newline -> true
        | _ -> false)
    |> List.isEmpty
    |> not
    
let private hasDirectiveBefore (trivia: TriviaContent list) =
    trivia
    |> List.filter (fun tn ->
        match tn with
        | Directive(_) -> true
        | _ -> false)
    |> List.isEmpty
    |> not

let internal sepConsideringTriviaContentBefore sepF (range: range) ctx =
    match findTriviaMainNodeFromRange ctx.Trivia range with
    | Some({ ContentBefore = (Comment(BlockComment(_,false,_)))::_ }) ->
        sepF ctx
    | Some({ ContentBefore = contentBefore }) when (hasPrintableContent contentBefore) ->
        ctx
    | _ -> sepF ctx

let internal sepNlnConsideringTriviaContentBefore (range:range) = sepConsideringTriviaContentBefore sepNln range

let internal sepNlnConsideringTriviaContentBeforeWithAttributes (ownRange:range) (attributeRanges: range seq) ctx =
    seq {
        yield ownRange
        yield! attributeRanges
    }
    |> Seq.choose (findTriviaMainNodeFromRange ctx.Trivia)
    |> Seq.exists (fun ({ ContentBefore = contentBefore }) -> hasPrintableContent contentBefore)
    |> fun hasContentBefore ->
        if hasContentBefore then ctx else sepNln ctx
    
let internal beforeElseKeyword (fullIfRange: range) (elseRange: range) (ctx: Context) =
    ctx.Trivia
    |> List.tryFind(fun tn ->
        match tn.Type with
        | Token(tok) ->
            tok.TokenInfo.TokenName = "ELSE" && (fullIfRange.StartLine < tn.Range.StartLine) && (tn.Range.StartLine >= elseRange.StartLine) 
        | _ -> false
    )
    |> fun tn ->
        match tn with
        | Some({ ContentBefore = [TriviaContent.Comment(LineCommentOnSingleLine(lineComment))] } as tn) ->
            sepNln +> !- lineComment +> removeNodeFromContext tn
        | _ ->
            id
    <| ctx

let internal genTriviaBeforeClausePipe (rangeOfClause:range) ctx =
    ctx.Trivia
    |> List.tryFind (fun t ->
        match t.Type with
        | Token({ TokenInfo = { TokenName = bar } }) ->
            bar = "BAR" && t.Range.StartColumn < rangeOfClause.StartColumn && t.Range.StartLine = rangeOfClause.StartLine
        | _ -> false
    )
    |> fun trivia ->
        match trivia with
        | Some trivia ->
            let containsOnlyDirectives =
                trivia.ContentBefore
                |> List.forall (fun tn -> match tn with | Directive(_) -> true | _ -> false)
            
            ifElse containsOnlyDirectives sepNln sepNone
            +> printContentBefore trivia
        | None -> id
    <| ctx
    
let internal genCommentsAfterInfix (rangePlusInfix: range option) (ctx: Context) =
    rangePlusInfix
    |> Option.bind (findTriviaMainNodeFromRange ctx.Trivia)
    |> Option.bind (fun trivia ->
        trivia.ContentAfter
        |> List.map (fun ca ->
            match ca with
            | TriviaContent.Comment(Comment.LineCommentAfterSourceCode(comment)) -> Some comment
            | _ -> None
        )
        |> List.choose id
        |> List.tryHead
    )
    |> Option.map (fun comment -> !- comment +> sepNln)
    |> Option.defaultValue id
    <| ctx
    
// Add a newline if there if trivia content before that requires it
let internal sepNlnIfTriviaBefore (range:range) (ctx:Context) =
    match findTriviaMainNodeFromRange ctx.Trivia range with
    | Some({ ContentBefore = contentBefore }) when (hasDirectiveBefore contentBefore) ->
        sepNln
    | _ -> sepNone
    <| ctx

let internal lastLineOnlyContains characters (ctx: Context) =
    let lastLine =
        List.tryHead ctx.ApplyWriterEvents.Lines
        |> Option.map (fun l -> l.Trim(characters))
    match lastLine with
    | Some l ->
        let length = String.length l
        length = 0 || length < ctx.Config.IndentSpaceNum
    | None -> false