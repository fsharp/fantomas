﻿module internal Fantomas.Core.Trivia

open System.Collections.Generic
open System.Collections.Immutable
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open Fantomas.Core.ImmutableArray
open Fantomas.Core.ISourceTextExtensions
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
    : TriviaNode immarray =
    codeComments
    |> List.filter (fun ct -> RangeHelpers.rangeContainsRange codeRange ct.Range)
    |> ImmutableArray.mapList (function
        | CommentTrivia.BlockComment r ->
            let content = source.GetContentAt r
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
            let content = source.GetContentAt r
            let index = r.StartLine - 1
            let line = source.GetLineString index

            let content =
                let trimmedLine = line.TrimStart(' ', ';')

                if index = 0 && trimmedLine.StartsWith("#!") then // shebang
                    CommentOnSingleLine content
                else if trimmedLine.StartsWith("//") then
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
    : TriviaNode immarray =
    if codeRange.StartLine = 0 && codeRange.EndLine = 0 then
        // weird edge cases where there is no source code but only hash defines
        ImmutableArray.empty
    else
        let fileIndex = codeRange.FileIndex

        let ignoreLineBuilder: ImmutableArray<int>.Builder =
            ImmutableArray.CreateBuilder<int>()

        let captureLinesIfMultiline (r: range) =
            if r.StartLine <> r.EndLine then
                ignoreLineBuilder.AddRange([| r.StartLine .. r.EndLine |])

        let rec visit (nodes: Node immarray) =
            if nodes.IsEmpty then
                ()
            else
                let head = nodes.[0]
                let rest = nodes.Slice(1, nodes.Length - 1)

                match head with
                | :? StringNode as node -> captureLinesIfMultiline node.Range
                | _ -> ()

                visit rest

        visit (ImmutableArray.singleton rootNode)

        for codeComment in codeComments do
            match codeComment with
            | CommentTrivia.BlockComment r -> captureLinesIfMultiline r
            | CommentTrivia.LineComment _ -> ()

        let ignoreLines = Set(ignoreLineBuilder.ToImmutable())
        let min = System.Math.Max(0, codeRange.StartLine - 1)
        let max = System.Math.Min(source.Length - 1, codeRange.EndLine - 1)

        let triviaBuilder = ImmutableArray.CreateBuilder<TriviaNode>()
        let mutable count = min

        for idx in [ min..max ] do
            if ignoreLines.Contains(idx + 1) then
                count <- 0
            else
                let line = source.GetLineString(idx)

                if String.isNotNullOrWhitespace line then
                    count <- 0
                else
                    let range =
                        let p = Position.mkPos (idx + 1) 0
                        Range.mkFileIndexRange fileIndex p p

                    if count < config.KeepMaxNumberOfBlankLines then
                        count <- count + 1
                        triviaBuilder.Add(TriviaNode(Newline, range))
                    else
                        ()

        triviaBuilder.ToImmutable()

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
    : TriviaNode immarray =
    directives
    |> List.filter (fun cdt -> RangeHelpers.rangeContainsRange codeRange cdt.Range)
    |> ImmutableArray.mapList (fun cdt ->
        let m = cdt.Range
        let text = (source.GetContentAt m).TrimEnd()
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
            |> ImmutableArray.tryPick (fun childNode -> findNodeWhereRangeFitsIn childNode range)

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
    | :? ExprSameInfixAppsNode
    | :? ExprInfixAppNode
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
    | :? MultipleAttributeListNode -> visitLastChildNode (ImmutableArray.last node.Children)
    | :? PatLongIdentNode
    | :? ModuleOrNamespaceNode ->
        if node.Children.IsEmpty then
            node
        else
            visitLastChildNode (ImmutableArray.last node.Children)
    | _ -> node

let lineCommentAfterSourceCodeToTriviaInstruction (containerNode: Node) (trivia: TriviaNode) : unit =
    let lineNumber = trivia.Range.StartLine

    let result =
        containerNode.Children
        |> ImmutableArray.filter (fun node -> node.Range.EndLine = lineNumber)
        |> Seq.sortByDescending (fun node -> node.Range.StartColumn)
        |> Seq.tryHead

    result
    |> Option.iter (fun node ->
        let node = visitLastChildNode node
        node.AddAfter(trivia))

let simpleTriviaToTriviaInstruction (containerNode: Node) (trivia: TriviaNode) : unit =
    containerNode.Children
    |> ImmutableArray.tryPick (fun node ->
        if not (node.Range.StartLine > trivia.Range.StartLine) then
            None
        else
            Some node.AddBefore

    )
    |> Option.orElseWith (fun () ->
        ImmutableArray.tryLast containerNode.Children
        |> Option.map (fun n -> n.AddAfter))
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
            | Cursor _ -> blockCommentToTriviaInstruction parentNode trivia

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

        let comparer =
            { new IComparer<TriviaNode> with
                member this.Compare(x, y) =
                    Comparer<int * int>.Default
                        .Compare((x.Range.Start.Line, x.Range.Start.Column), (y.Range.Start.Line, y.Range.Start.Column)) }

        comments.AddRange(newlines).AddRange(directives).Sort(comparer)

    addToTree tree trivia
    tree

let insertCursor (tree: Oak) (cursor: pos) =
    let cursorRange = Range.mkRange (tree :> Node).Range.FileName cursor cursor
    let nodeWithCursor = findNodeWhereRangeFitsIn tree cursorRange

    match nodeWithCursor with
    | Some((:? SingleTextNode) as node) -> node.AddCursor cursor
    | _ -> addToTree tree [| TriviaNode(TriviaContent.Cursor, cursorRange) |]

    tree
