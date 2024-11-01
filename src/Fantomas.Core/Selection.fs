module internal Fantomas.Core.Selection

open Fantomas.FCS.Text
open Fantomas.Core.SyntaxOak

let correctSelection (fileIndex: int) (sourceText: ISourceText) (selection: range) =
    let lines =
        [| selection.StartLine .. selection.EndLine |]
        |> Array.choose (fun lineNumber ->
            let idx = lineNumber - 1

            if idx < 0 then
                None
            else
                let line = sourceText.GetLineString(idx)

                if String.isNotNullOrWhitespace line then
                    Some(lineNumber, line)
                else
                    None)

    match Array.tryHead lines, Array.tryLast lines with
    | Some(startLineNumber, startLine), Some(endLineNumber, endLine) ->
        let startColumn =
            // The selection is on the same line as the code but appears to be inside whitespace
            if startLineNumber = selection.StartLine then
                Seq.takeWhile System.Char.IsWhiteSpace startLine
                |> Seq.length
                |> fun firstCharOnLine -> System.Math.Max(firstCharOnLine, selection.StartColumn)
            else
                // The selection is on a different line than the code, take first non-whitespace character
                Seq.takeWhile System.Char.IsWhiteSpace startLine |> Seq.length

        let endColumn =
            // The selection is on the same line as the code but appears to be inside whitespace
            if endLineNumber = selection.EndLine then
                Seq.rev endLine
                |> Seq.takeWhile System.Char.IsWhiteSpace
                |> Seq.length
                |> fun trimmedEnd -> endLine.Length - trimmedEnd
                |> fun lastCharOnLine -> System.Math.Min(lastCharOnLine, selection.EndColumn)
            else
                // The selection is on a different line than the code, take first non-whitespace character
                Seq.rev endLine
                |> Seq.takeWhile System.Char.IsWhiteSpace
                |> Seq.length
                |> fun trimmedEnd -> endLine.Length - trimmedEnd

        if
            startLineNumber <> selection.StartLine
            || startColumn <> selection.StartColumn
            || endLineNumber <> selection.EndLine
            || endColumn <> selection.EndColumn
        then

            Range.mkFileIndexRange
                fileIndex
                (Position.mkPos startLineNumber startColumn)
                (Position.mkPos endLineNumber endColumn)
        else
            selection
    | _ -> selection

let findNode (selection: range) (node: Node) : Node option =
    let isExactSelection =
        selection.StartLine = node.Range.StartLine
        && selection.StartColumn = node.Range.StartColumn
        && selection.EndLine = node.Range.EndLine
        && selection.EndColumn = node.Range.EndColumn

    if isExactSelection then Some node else None

[<RequireQualifiedAccess; NoComparison>]
type TreeForSelection =
    /// Format this tree and return the entire result.
    | Standalone of Oak
    /// Format this tree, and extract the first node that matches this type.
    | RequiresExtraction of tree: Oak * nodeType: System.Type
    /// We currently don't support this type of node.
    | Unsupported

/// We can construct a tree using only the node which the user selected.
let mkOakFromModuleDecl (md: ModuleDecl) : TreeForSelection =
    let m = (ModuleDecl.Node md).Range
    TreeForSelection.Standalone(Oak([], [ ModuleOrNamespaceNode(None, [ md ], m) ], m))

/// The selected node by the user cannot be formatted as a standalone expression.
/// We need to format a tree that is an approximation of the selection.
/// This is typically a combination of the selection and some parent construct.
/// For example when formatting a type definition: `let (a: int   list) = []`
/// If the selection is `int    list` we cannot just drop that in an empty file and format it.
/// We can fake a binding (or type alias) and later try and select the formatted type.
let mkExtractableOakFromModule (md: ModuleDecl) (t: System.Type) =
    let m = (ModuleDecl.Node md).Range
    TreeForSelection.RequiresExtraction(Oak([], [ ModuleOrNamespaceNode(None, [ md ], m) ], m), t)

let dummyUnit: Expr =
    UnitNode(SingleTextNode("(", Range.Zero), SingleTextNode(")", Range.Zero), Range.Zero)
    |> Constant.Unit
    |> Expr.Constant

/// Wrap the selected node inside an anonymous module.
/// Keep the original trivia of the ParsedInput so code comments could still be restored.
let mkTreeWithSingleNode (node: Node) : TreeForSelection =
    let m = node.Range

    match node with
    | :? OpenListNode as node -> mkOakFromModuleDecl (ModuleDecl.OpenList node)
    | :? OpenModuleOrNamespaceNode as node ->
        let openMN = Open.ModuleOrNamespace node
        let openList = OpenListNode([ openMN ])
        mkOakFromModuleDecl (ModuleDecl.OpenList openList)
    | :? OpenTargetNode as node ->
        let openT = Open.Target node
        let openList = OpenListNode([ openT ])
        mkOakFromModuleDecl (ModuleDecl.OpenList openList)

    | :? HashDirectiveListNode as node -> mkOakFromModuleDecl (ModuleDecl.HashDirectiveList node)
    | :? ParsedHashDirectiveNode as node ->
        let nodeList = HashDirectiveListNode([ node ])
        mkOakFromModuleDecl (ModuleDecl.HashDirectiveList nodeList)
    | :? ModuleDeclAttributesNode as node -> mkOakFromModuleDecl (ModuleDecl.Attributes node)
    | :? AttributeListNode as node ->
        let attributes = MultipleAttributeListNode([ node ], m)

        let md =
            ModuleDecl.Attributes(ModuleDeclAttributesNode(Some attributes, dummyUnit, m))

        mkExtractableOakFromModule md (node.GetType())

    // ModuleDecl.Expr
    | :? ExprLazyNode as node ->
        let expr = Expr.Lazy node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprSingleNode as node ->
        let expr = Expr.Single node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? UnitNode as node ->
        let expr = Expr.Constant(Constant.Unit node)
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ConstantMeasureNode as node ->
        let expr = Expr.Constant(Constant.Measure node)
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? SingleTextNode as node ->
        let expr = Expr.Null node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprQuoteNode as node ->
        let expr = Expr.Quote node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTypedNode as node ->
        let expr = Expr.Typed node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprNewNode as node ->
        let expr = Expr.New node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTupleNode as node ->
        let expr = Expr.Tuple node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprStructTupleNode as node ->
        let expr = Expr.StructTuple node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprArrayOrListNode as node ->
        let expr = Expr.ArrayOrList node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprInheritRecordNode as node ->
        let expr = Expr.InheritRecord node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprAnonStructRecordNode as node ->
        let expr = Expr.AnonStructRecord node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprRecordNode as node ->
        let expr = Expr.Record node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprObjExprNode as node ->
        let expr = Expr.ObjExpr node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprWhileNode as node ->
        let expr = Expr.While node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprForNode as node ->
        let expr = Expr.For node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprForEachNode as node ->
        let expr = Expr.ForEach node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprNamedComputationNode as node ->
        let expr = Expr.NamedComputation node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprComputationNode as node ->
        let expr = Expr.Computation node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprCompExprBodyNode as node ->
        let expr = Expr.CompExprBody node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprJoinInNode as node ->
        let expr = Expr.JoinIn node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprParenLambdaNode as node ->
        let expr = Expr.ParenLambda node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprLambdaNode as node ->
        let expr = Expr.Lambda node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprMatchLambdaNode as node ->
        let expr = Expr.MatchLambda node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprMatchNode as node ->
        let expr = Expr.Match node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTraitCallNode as node ->
        let expr = Expr.TraitCall node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprParenFunctionNameWithStarNode as node ->
        let expr = Expr.ParenFunctionNameWithStar node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprParenNode as node ->
        let expr = Expr.Paren node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDynamicNode as node ->
        let expr = Expr.Dynamic node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprPrefixAppNode as node ->
        let expr = Expr.PrefixApp node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    //    | :? ExprSameInfixAppsNode as node ->
    //        let expr = Expr.SameInfixApps node
    //        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    //    | :? ExprInfixAppNode as node ->
    //        let expr = Expr.InfixApp node
    //        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprIndexWithoutDotNode as node ->
        let expr = Expr.IndexWithoutDot node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprChain as node ->
        let expr = Expr.Chain node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprAppLongIdentAndSingleParenArgNode as node ->
        let expr = Expr.AppLongIdentAndSingleParenArg node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprAppSingleParenArgNode as node ->
        let expr = Expr.AppSingleParenArg node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprAppWithLambdaNode as node ->
        let expr = Expr.AppWithLambda node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprNestedIndexWithoutDotNode as node ->
        let expr = Expr.NestedIndexWithoutDot node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprAppNode as node ->
        let expr = Expr.App node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTypeAppNode as node ->
        let expr = Expr.TypeApp node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTryWithSingleClauseNode as node ->
        let expr = Expr.TryWithSingleClause node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTryWithNode as node ->
        let expr = Expr.TryWith node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTryFinallyNode as node ->
        let expr = Expr.TryFinally node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprIfThenNode as node ->
        let expr = Expr.IfThen node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprIfThenElseNode as node ->
        let expr = Expr.IfThenElse node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprIfThenElifNode as node ->
        let expr = Expr.IfThenElif node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprOptVarNode as node ->
        let expr = Expr.OptVar node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprLongIdentSetNode as node ->
        let expr = Expr.LongIdentSet node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDotIndexedGetNode as node ->
        let expr = Expr.DotIndexedGet node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDotIndexedSetNode as node ->
        let expr = Expr.DotIndexedSet node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprNamedIndexedPropertySetNode as node ->
        let expr = Expr.NamedIndexedPropertySet node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDotNamedIndexedPropertySetNode as node ->
        let expr = Expr.DotNamedIndexedPropertySet node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprSetNode as node ->
        let expr = Expr.Set node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprLibraryOnlyStaticOptimizationNode as node ->
        let expr = Expr.LibraryOnlyStaticOptimization node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprInterpolatedStringExprNode as node ->
        let expr = Expr.InterpolatedStringExpr node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTripleNumberIndexRangeNode as node ->
        let expr = Expr.TripleNumberIndexRange node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprIndexRangeNode as node ->
        let expr = Expr.IndexRange node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprIndexFromEndNode as node ->
        let expr = Expr.IndexFromEnd node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExceptionDefnNode as node -> mkOakFromModuleDecl (ModuleDecl.Exception node)
    | :? ExternBindingNode as node -> mkOakFromModuleDecl (ModuleDecl.ExternBinding node)
    | :? BindingNode as node -> mkOakFromModuleDecl (ModuleDecl.TopLevelBinding node)
    | :? ModuleAbbrevNode as node -> mkOakFromModuleDecl (ModuleDecl.ModuleAbbrev node)
    | :? NestedModuleNode as node -> mkOakFromModuleDecl (ModuleDecl.NestedModule node)
    // TypeDefn
    | :? TypeDefnEnumNode as node ->
        let tdn = TypeDefn.Enum node
        mkOakFromModuleDecl (ModuleDecl.TypeDefn tdn)
    | :? TypeDefnUnionNode as node ->
        let tdn = TypeDefn.Union node
        mkOakFromModuleDecl (ModuleDecl.TypeDefn tdn)
    | :? TypeDefnRecordNode as node ->
        let tdn = TypeDefn.Record node
        mkOakFromModuleDecl (ModuleDecl.TypeDefn tdn)
    | :? TypeNameNode as node ->
        let tdn = TypeDefn.None node
        mkOakFromModuleDecl (ModuleDecl.TypeDefn tdn)
    | :? TypeDefnAbbrevNode as node ->
        let tdn = TypeDefn.Abbrev node
        mkOakFromModuleDecl (ModuleDecl.TypeDefn tdn)
    | :? TypeDefnExplicitNode as node ->
        let tdn = TypeDefn.Explicit node
        mkOakFromModuleDecl (ModuleDecl.TypeDefn tdn)
    | :? TypeDefnAugmentationNode as node ->
        let tdn = TypeDefn.Augmentation node
        mkOakFromModuleDecl (ModuleDecl.TypeDefn tdn)
    | :? TypeDefnDelegateNode as node ->
        let tdn = TypeDefn.Delegate node
        mkOakFromModuleDecl (ModuleDecl.TypeDefn tdn)
    | :? TypeDefnRegularNode as node ->
        let tdn = TypeDefn.Regular node
        mkOakFromModuleDecl (ModuleDecl.TypeDefn tdn)
    | :? ValNode as node -> mkOakFromModuleDecl (ModuleDecl.Val node)
    | _ ->
#if DEBUG
        failwithf $"%s{node.GetType().Name} is currently unsupported"
#endif
        TreeForSelection.Unsupported

#if DEBUG
let printTriviaNode (node: Node) : unit =
    let rec visit (level: int) (node: Node) =
        let name = node.GetType().Name
        printfn "%s%s: %A" ("".PadRight(level * 2)) name node.Range
        Array.iter (visit (level + 1)) node.Children

    visit 0 node
#endif

// Find the first node that matches the type
let rec findRangeOf (t: System.Type) (root: Node) : range option =
    if root.GetType() = t then
        Some root.Range
    else
        Array.choose (findRangeOf t) root.Children |> Array.tryHead

let formatSelection
    (config: FormatConfig)
    (isSignature: bool)
    (selection: range)
    (sourceText: ISourceText)
    : Async<string * range> =
    async {
        let baseUntypedTree, baseDiagnostics =
            Fantomas.FCS.Parse.parseFile isSignature sourceText []

        let isValid = Validation.noWarningOrErrorDiagnostics baseDiagnostics

        if not isValid then
            raise (FormatException $"Parsing failed with errors: %A{baseDiagnostics}")

        let rootNode = ASTTransformer.mkOak (Some sourceText) baseUntypedTree

#if DEBUG
        printTriviaNode rootNode
#endif

        let selection = correctSelection rootNode.Range.FileIndex sourceText selection

        let treeWithSelection =
            Trivia.findNodeWhereRangeFitsIn rootNode selection
            |> Option.bind (findNode selection)
            |> Option.map mkTreeWithSingleNode

        match treeWithSelection with
        | None -> return raise (FormatException("No suitable AST node was found for the given selection."))
        | Some tree ->
            let maxLineLength = config.MaxLineLength - selection.StartColumn

            let selectionConfig =
                { config with
                    InsertFinalNewline = false
                    MaxLineLength = maxLineLength }

            let formattedSelection =
                let context = Context.Context.Create selectionConfig

                match tree with
                | TreeForSelection.Unsupported ->
                    raise (FormatException("The current selection is not supported right now."))
                | TreeForSelection.Standalone tree ->
                    let enrichedTree = Trivia.enrichTree selectionConfig sourceText baseUntypedTree tree

                    CodePrinter.genFile enrichedTree context
                    |> Context.dump true
                    |> fun result -> result.Code
                | TreeForSelection.RequiresExtraction(tree, t) ->
                    let enrichedTree = Trivia.enrichTree selectionConfig sourceText baseUntypedTree tree

                    let { Code = formattedCode } =
                        CodePrinter.genFile enrichedTree context |> Context.dump true

                    let source = SourceText.ofString formattedCode
                    let formattedAST, _ = Fantomas.FCS.Parse.parseFile isSignature source []
                    let formattedTree = ASTTransformer.mkOak (Some source) formattedAST
                    let rangeOfSelection = findRangeOf t formattedTree

                    match rangeOfSelection with
                    | None ->
                        raise (FormatException("No suitable AST node could be extracted from formatted selection."))
                    | Some m -> source.GetSubTextFromRange m

            return formattedSelection.TrimEnd([| '\r'; '\n' |]), selection
    }
