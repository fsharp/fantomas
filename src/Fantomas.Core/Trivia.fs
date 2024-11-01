module internal Fantomas.Core.Trivia

open System
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text
open Fantomas.Core.SyntaxOak

type CommentTrivia with

    member x.Range =
        match x with
        | CommentTrivia.BlockComment m
        | CommentTrivia.LineComment m -> m

let internal collectTriviaFromCodeComments
    (source: ISourceText)
    (codeComments: CommentTrivia list)
    (codeRange: range)
    : TriviaNode list =
    codeComments
    |> List.filter (fun ct -> RangeHelpers.rangeContainsRange codeRange ct.Range)
    |> List.map (function
        | CommentTrivia.BlockComment r ->
            let content = source.GetSubTextFromRange r
            let startLine = source.GetLineString(r.StartLine - 1)
            let endLine = source.GetLineString(r.EndLine - 1)

            let contentBeforeComment =
                startLine.Substring(0, r.StartColumn).TrimStart(' ', ';').Length

            let contentAfterComment = endLine.Substring(r.EndColumn).TrimEnd(' ', ';').Length

            let content =
                if contentBeforeComment = 0 && contentAfterComment = 0 then
                    CommentOnSingleLine content
                else
                    BlockComment(content, false, false)

            TriviaNode(content, r)
        | CommentTrivia.LineComment r ->
            let content = source.GetSubTextFromRange r
            let index = r.StartLine - 1
            let line = source.GetLineString index

            let content =
                let trimmedLine = line.TrimStart(' ', ';')

                if index = 0 && String.startsWithOrdinal "#!" trimmedLine then // shebang
                    CommentOnSingleLine content
                else if String.startsWithOrdinal "//" trimmedLine then
                    CommentOnSingleLine content
                else
                    LineCommentAfterSourceCode content

            TriviaNode(content, r))

let internal collectTriviaFromBlankLines
    (config: FormatConfig)
    (source: ISourceText)
    (rootNode: Node)
    (codeComments: CommentTrivia list)
    (codeRange: range)
    : TriviaNode list =
    if codeRange.StartLine = 0 && codeRange.EndLine = 0 then
        // weird edge cases where there is no source code but only hash defines
        []
    else
        let fileIndex = codeRange.FileIndex

        let captureLinesIfMultiline (r: range) =
            if r.StartLine = r.EndLine then
                []
            else
                [ r.StartLine .. r.EndLine ]

        let multilineStringsLines =
            let rec visit (node: Node) (finalContinuation: int list -> int list) =
                let continuations: ((int list -> int list) -> int list) list =
                    Array.toList node.Children |> List.map visit

                let currentLines =
                    match node with
                    | :? StringNode as node -> captureLinesIfMultiline node.Range
                    | _ -> []

                let finalContinuation (lines: int list list) : int list =
                    List.collect id (currentLines :: lines) |> finalContinuation

                Continuation.sequence continuations finalContinuation

            visit rootNode id

        let blockCommentLines =
            codeComments
            |> List.collect (function
                | CommentTrivia.BlockComment r -> captureLinesIfMultiline r
                | CommentTrivia.LineComment _ -> [])

        let ignoreLines =
            Set(
                seq {
                    yield! multilineStringsLines
                    yield! blockCommentLines
                }
            )

        let min = System.Math.Max(0, codeRange.StartLine - 1)

        let max = System.Math.Min(source.Length - 1, codeRange.EndLine - 1)

        (min, [ min..max ])
        ||> List.chooseState (fun count idx ->
            if ignoreLines.Contains(idx + 1) then
                0, None
            else
                let line = source.GetLineString(idx)

                if String.isNotNullOrWhitespace line then
                    0, None
                else
                    let range =
                        let p = Position.mkPos (idx + 1) 0
                        Range.mkFileIndexRange fileIndex p p

                    if count < config.KeepMaxNumberOfBlankLines then
                        (count + 1), Some(TriviaNode(Newline, range))
                    else
                        count, None)

type ConditionalDirectiveTrivia with

    member x.Range =
        match x with
        | ConditionalDirectiveTrivia.If(_, m)
        | ConditionalDirectiveTrivia.Else m
        | ConditionalDirectiveTrivia.EndIf m -> m

let internal collectTriviaFromDirectives
    (source: ISourceText)
    (directives: ConditionalDirectiveTrivia list)
    (codeRange: range)
    : TriviaNode list =
    directives
    |> List.filter (fun cdt -> RangeHelpers.rangeContainsRange codeRange cdt.Range)
    |> List.map (fun cdt ->
        let m = cdt.Range
        let text = (source.GetSubTextFromRange m).TrimEnd()
        let content = Directive text
        TriviaNode(content, m))

let rec findNodeWhereRangeFitsIn (root: Node) (range: range) : Node option =
    let doesSelectionFitInNode = RangeHelpers.rangeContainsRange root.Range range

    if not doesSelectionFitInNode then
        None
    else
        // The more specific the node fits the selection, the better
        let betterChildNode =
            root.Children
            |> Array.choose (fun childNode -> findNodeWhereRangeFitsIn childNode range)
            |> Array.tryHead

        match betterChildNode with
        | Some betterChild -> Some betterChild
        | None -> Some root

let triviaBeforeOrAfterEntireTree (rootNode: Node) (trivia: TriviaNode) : unit =
    let isBefore = trivia.Range.EndLine < rootNode.Range.StartLine

    if isBefore then
        rootNode.AddBefore(trivia)
    else
        rootNode.AddAfter(trivia)

/// Find the last child node that will be the last node of the parent node.
let rec visitLastChildNode (node: Node) : Node =
    match node with
    | :? ExprIfThenNode
    | :? ExprIfThenElseNode
    | :? ExprIfThenElifNode
    | :? ExprAppNode
    // | :? ExprSameInfixAppsNode
    // | :? ExprInfixAppNode
    | :? ExprLambdaNode
    | :? ExprLetOrUseNode
    | :? ExprLetOrUseBangNode
    | :? ExprAndBang
    | :? BindingNode
    | :? TypeDefnEnumNode
    | :? TypeDefnUnionNode
    | :? TypeDefnRecordNode
    | :? TypeNameNode
    | :? TypeDefnAbbrevNode
    | :? TypeDefnExplicitNode
    | :? TypeDefnAugmentationNode
    | :? TypeDefnDelegateNode
    | :? TypeDefnRegularNode
    | :? ExprMatchNode
    | :? PatParameterNode
    | :? PatTupleNode
    | :? TypeTupleNode
    | :? TypeAppPrefixNode
    | :? TypeAppPostFixNode
    | :? TypeFunsNode
    | :? ExprTupleNode
    | :? MemberDefnInheritNode
    | :? OpenListNode
    | :? InheritConstructorTypeOnlyNode
    | :? InheritConstructorUnitNode
    | :? InheritConstructorParenNode
    | :? InheritConstructorOtherNode
    | :? FieldNode
    | :? BindingListNode
    | :? MemberDefnExplicitCtorNode
    | :? MemberDefnInterfaceNode
    | :? MemberDefnAutoPropertyNode
    | :? MemberDefnAbstractSlotNode
    | :? MemberDefnPropertyGetSetNode
    | :? MatchClauseNode
    | :? ExprCompExprBodyNode
    | :? NestedModuleNode
    | :? UnionCaseNode
    | :? EnumCaseNode
    | :? ValNode
    | :? BindingReturnInfoNode
    | :? PatLeftMiddleRight
    | :? MultipleAttributeListNode -> visitLastChildNode (Array.last node.Children)
    | :? PatLongIdentNode
    | :? ModuleOrNamespaceNode ->
        if Array.isEmpty node.Children then
            node
        else
            visitLastChildNode (Seq.last node.Children)
    | _ -> node

let lineCommentAfterSourceCodeToTriviaInstruction (containerNode: Node) (trivia: TriviaNode) : unit =
    let lineNumber = trivia.Range.StartLine

    let result =
        containerNode.Children
        |> Array.filter (fun node -> node.Range.EndLine = lineNumber)
        |> Array.sortByDescending (fun node -> node.Range.StartColumn)
        |> Array.tryHead

    result
    |> Option.iter (fun node ->
        let node = visitLastChildNode node
        node.AddAfter(trivia))

let simpleTriviaToTriviaInstruction (containerNode: Node) (trivia: TriviaNode) : unit =
    containerNode.Children
    |> Array.tryFind (fun node -> node.Range.StartLine > trivia.Range.StartLine)
    |> Option.map (fun n -> n.AddBefore)
    |> Option.orElseWith (fun () -> Array.tryLast containerNode.Children |> Option.map (fun n -> n.AddAfter))
    |> Option.iter (fun f -> f trivia)

let blockCommentToTriviaInstruction (containerNode: Node) (trivia: TriviaNode) : unit =
    let nodeAfter =
        containerNode.Children
        |> Seq.tryFind (fun tn ->
            let range = tn.Range

            (range.StartLine > trivia.Range.StartLine)
            || (range.StartLine = trivia.Range.StartLine
                && range.StartColumn > trivia.Range.StartColumn))

    let nodeBefore =
        containerNode.Children
        |> Seq.tryFindBack (fun tn ->
            let range = tn.Range

            range.EndLine <= trivia.Range.StartLine
            && range.EndColumn <= trivia.Range.StartColumn)
        |> Option.map visitLastChildNode

    let triviaWith newlineBefore newlineAfter =
        match trivia.Content with
        | BlockComment(content, _, _) ->
            let content = BlockComment(content, newlineBefore, newlineAfter)
            TriviaNode(content, trivia.Range)
        | _ -> trivia

    match nodeBefore, nodeAfter with
    | Some nb, None when nb.Range.EndLine = trivia.Range.StartLine -> nb.AddAfter(triviaWith false false)
    | None, Some na -> na.AddBefore(triviaWith true false)
    | Some nb, Some na ->
        if nb.Range.EndLine = trivia.Range.StartLine then
            // before (* comment *) after
            nb.AddAfter(triviaWith false false)
        elif
            (nb.Range.EndLine < trivia.Range.StartLine
             && trivia.Range.EndLine = na.Range.StartLine)
        then
            // before
            // (* comment *) after
            na.AddBefore(triviaWith false false)
    | _ -> ()

let addToTree (tree: Oak) (trivia: TriviaNode seq) =
    for trivia in trivia do
        let smallestNodeThatContainsTrivia = findNodeWhereRangeFitsIn tree trivia.Range

        match smallestNodeThatContainsTrivia with
        | None -> triviaBeforeOrAfterEntireTree tree trivia
        | Some parentNode ->
            match trivia.Content with
            | LineCommentAfterSourceCode _ -> lineCommentAfterSourceCodeToTriviaInstruction parentNode trivia
            | CommentOnSingleLine _
            | Newline
            | Directive _ -> simpleTriviaToTriviaInstruction parentNode trivia
            | BlockComment _
            | Cursor -> blockCommentToTriviaInstruction parentNode trivia

let enrichTree (config: FormatConfig) (sourceText: ISourceText) (ast: ParsedInput) (tree: Oak) : Oak =
    let fullTreeRange = tree.Range

    let directives, codeComments =
        match ast with
        | ParsedInput.ImplFile(ParsedImplFileInput(
            trivia = { ConditionalDirectives = directives
                       CodeComments = codeComments })) -> directives, codeComments
        | ParsedInput.SigFile(ParsedSigFileInput(
            trivia = { ConditionalDirectives = directives
                       CodeComments = codeComments })) -> directives, codeComments

    let trivia =
        let newlines =
            collectTriviaFromBlankLines config sourceText tree codeComments fullTreeRange

        let comments =
            match ast with
            | ParsedInput.ImplFile(ParsedImplFileInput(trivia = trivia)) ->
                collectTriviaFromCodeComments sourceText trivia.CodeComments fullTreeRange
            | ParsedInput.SigFile(ParsedSigFileInput(trivia = trivia)) ->
                collectTriviaFromCodeComments sourceText trivia.CodeComments fullTreeRange

        let directives = collectTriviaFromDirectives sourceText directives fullTreeRange

        [| yield! comments; yield! newlines; yield! directives |]
        |> Array.sortBy (fun n -> n.Range.Start.Line, n.Range.Start.Column)

    addToTree tree trivia
    tree

let insertCursor (tree: Oak) (cursor: pos) =
    let cursorRange = Range.mkRange (tree :> Node).Range.FileName cursor cursor
    let nodeWithCursor = findNodeWhereRangeFitsIn tree cursorRange

    match nodeWithCursor with
    | Some((:? SingleTextNode) as node) -> node.AddCursor cursor
    | _ -> addToTree tree [| TriviaNode(TriviaContent.Cursor, cursorRange) |]

    tree
