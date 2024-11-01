module internal rec Fantomas.Core.CodePrinter

open System
open Fantomas.Core.Context
open Fantomas.Core.SyntaxOak
open Microsoft.FSharp.Core.CompilerServices

let arithmeticOperators = set [ "+" ]
let noBreakInfixOps = set [| "="; ">"; "<"; "%" |]
// let newLineInfixOps = set [ "|>"; "||>"; "|||>"; ">>"; ">>=" ]

let rec (|UppercaseType|LowercaseType|) (t: Type) : Choice<unit, unit> =
    let upperOrLower (v: string) =
        let isUpper = Seq.tryHead v |> Option.map Char.IsUpper |> Option.defaultValue false
        if isUpper then UppercaseType else LowercaseType

    match t with
    | Type.LongIdent node ->
        let lastIdent =
            List.tryFindBack
                (function
                | IdentifierOrDot.Ident _ -> true
                | _ -> false)
                node.Content

        match lastIdent with
        | Some(IdentifierOrDot.Ident ident) -> upperOrLower ident.Text
        | _ -> LowercaseType
    | Type.Var node -> upperOrLower (node.Text.Substring(1))
    | Type.AppPostfix node -> (|UppercaseType|LowercaseType|) node.First
    | Type.AppPrefix node -> (|UppercaseType|LowercaseType|) node.Identifier
    | _ -> failwithf $"Cannot determine if synType %A{t} is uppercase or lowercase"

let rec (|UppercaseExpr|LowercaseExpr|) (expr: Expr) =
    let upperOrLower (v: string) =
        let isUpper = Seq.tryHead v |> Option.map Char.IsUpper |> Option.defaultValue false
        if isUpper then UppercaseExpr else LowercaseExpr

    let lastFragmentInList (identList: IdentListNode) =
        match List.tryLast identList.Content with
        | None
        | Some(IdentifierOrDot.KnownDot _ | IdentifierOrDot.UnknownDot) -> LowercaseExpr
        | Some(IdentifierOrDot.Ident node) -> upperOrLower node.Text

    match expr with
    | Expr.Ident ident -> upperOrLower ident.Text
    | Expr.OptVar node -> lastFragmentInList node.Identifier
    | Expr.Chain node ->
        match List.tryLast node.Links with
        | None
        | Some(ChainLink.Dot _) -> LowercaseExpr
        | Some(ChainLink.Identifier e)
        | Some(ChainLink.Expr e) -> (|UppercaseExpr|LowercaseExpr|) e
        | Some(ChainLink.AppParen appParen) -> (|UppercaseExpr|LowercaseExpr|) appParen.FunctionName
        | Some(ChainLink.AppUnit appUnit) -> (|UppercaseExpr|LowercaseExpr|) appUnit.FunctionName
        // Questionable
        | Some(ChainLink.IndexExpr _) -> LowercaseExpr
    | Expr.DotIndexedGet node -> (|UppercaseExpr|LowercaseExpr|) node.ObjectExpr
    | Expr.TypeApp node -> (|UppercaseExpr|LowercaseExpr|) node.Identifier
    | Expr.Dynamic node -> (|UppercaseExpr|LowercaseExpr|) node.FuncExpr
    | Expr.AppSingleParenArg node -> (|UppercaseExpr|LowercaseExpr|) node.FunctionExpr
    | Expr.Paren node -> (|UppercaseExpr|LowercaseExpr|) node.Expr
    | Expr.App node -> (|UppercaseExpr|LowercaseExpr|) node.FunctionExpr
    | _ -> failwithf "cannot determine if Expr %A is uppercase or lowercase" expr

let (|ParenExpr|_|) (e: Expr) =
    match e with
    | Expr.Paren _
    | Expr.ParenLambda _
    | Expr.ParenFunctionNameWithStar _
    | Expr.Constant(Constant.Unit _) -> Some e
    | _ -> None

let genTrivia (node: Node) (trivia: TriviaNode) (ctx: Context) =
    let currentLastLine = ctx.WriterModel.Lines |> List.tryHead

    // Some items like #if or Newline should be printed on a newline
    // It is hard to always get this right in CodePrinter, so we detect it based on the current code.
    let addNewline =
        currentLastLine
        |> Option.map (fun line -> line.Trim().Length > 0)
        |> Option.defaultValue false

    let addSpace =
        currentLastLine
        |> Option.bind (fun line -> Seq.tryLast line |> Option.map (fun lastChar -> lastChar <> ' '))
        |> Option.defaultValue false

    let gen =
        match trivia.Content with
        | LineCommentAfterSourceCode s ->
            let comment = sprintf "%s%s" (if addSpace then " " else String.empty) s
            writerEvent (WriteBeforeNewline comment)
        | BlockComment(comment, before, after) ->
            ifElse (before && addNewline) sepNlnForTrivia sepNone
            +> sepSpace
            +> !-comment
            +> sepSpace
            +> ifElse after sepNlnForTrivia sepNone
        | CommentOnSingleLine s
        | Directive s -> (ifElse addNewline sepNlnForTrivia sepNone) +> !-s +> sepNlnForTrivia
        | Newline -> (ifElse addNewline (sepNlnForTrivia +> sepNlnForTrivia) sepNlnForTrivia)
        | Cursor ->
            fun ctx ->
                // TODO: this assumes the cursor is placed on the same line as the EndLine of the Node.
                let originalColumnOffset = trivia.Range.EndColumn - node.Range.EndColumn

                let formattedCursor =
                    Fantomas.FCS.Text.Position.mkPos ctx.WriterModel.Lines.Length (ctx.Column + originalColumnOffset)

                { ctx with
                    FormattedCursor = Some formattedCursor }

    gen ctx

let recordCursorNode f (node: Node) (ctx: Context) =
    match node.TryGetCursor with
    | None -> f ctx
    | Some cursor ->
        // TODO: this currently assume the node fits on a single line.
        // This won't be accurate in case of a multiline string.
        let currentStartLine = ctx.WriterModel.Lines.Length
        let currentStartColumn = ctx.Column

        let ctxAfter = f ctx

        let formattedCursor =
            let columnOffsetInSource = cursor.Column - node.Range.StartColumn
            Fantomas.FCS.Text.Position.mkPos currentStartLine (currentStartColumn + columnOffsetInSource)

        { ctxAfter with
            FormattedCursor = Some formattedCursor }

let enterNode<'n when 'n :> Node> (n: 'n) =
    col sepNone n.ContentBefore (genTrivia n)

let leaveNode<'n when 'n :> Node> (n: 'n) =
    col sepNone n.ContentAfter (genTrivia n)

let genNode<'n when 'n :> Node> (n: 'n) (f: Context -> Context) =
    enterNode n +> recordCursorNode f n +> leaveNode n

let genSingleTextNode (node: SingleTextNode) = !-node.Text |> genNode node

// Alternative for genSingleTextNode to avoid a double space when the node has line comment after it.
let genSingleTextNodeWithSpaceSuffix (addSpace: Context -> Context) (node: SingleTextNode) =
    (!-node.Text +> addSpace) |> genNode node

let genSingleTextNodeSuffixDelimiter (node: SingleTextNode) =
    genSingleTextNodeWithSpaceSuffix addSpaceIfSpaceAroundDelimiter node

let genSingleTextNodeWithLeadingDot (node: SingleTextNode) = !- $".%s{node.Text}" |> genNode node

let genMultipleTextsNode (node: MultipleTextsNode) =
    col sepSpace node.Content genSingleTextNode |> genNode node

let genIdentListNodeAux addLeadingDot (iln: IdentListNode) =
    coli sepNone iln.Content (fun idx identOrDot ->
        match identOrDot with
        | IdentifierOrDot.Ident ident ->
            if addLeadingDot && idx = 0 then
                genSingleTextNodeWithLeadingDot ident +> sepNlnWhenWriteBeforeNewlineNotEmpty
            else
                genSingleTextNode ident +> sepNlnWhenWriteBeforeNewlineNotEmpty
        | IdentifierOrDot.KnownDot dot -> genSingleTextNode dot
        | IdentifierOrDot.UnknownDot -> sepDot)
    |> genNode iln

let genIdentListNode iln = genIdentListNodeAux false iln
let genIdentListNodeWithDot iln = genIdentListNodeAux true iln

let genAccessOpt (nodeOpt: SingleTextNode option) =
    match nodeOpt with
    | None -> sepNone
    | Some node -> genSingleTextNode node +> sepSpace

let genXml (node: XmlDocNode option) =
    match node with
    | None -> sepNone
    | Some node -> col sepNln node.Lines (!-) +> sepNln |> genNode node

let addSpaceBeforeParenInPattern (node: IdentListNode) (ctx: Context) =
    node.Content
    |> List.tryFindBack (function
        | IdentifierOrDot.Ident node -> not (String.IsNullOrWhiteSpace node.Text)
        | _ -> false)
    |> fun identOrDot ->
        match identOrDot with
        | Some(IdentifierOrDot.Ident node) ->
            let parameterValue =
                if Char.IsUpper node.Text.[0] then
                    ctx.Config.SpaceBeforeUppercaseInvocation
                else
                    ctx.Config.SpaceBeforeLowercaseInvocation

            onlyIf parameterValue sepSpace ctx
        | _ -> sepSpace ctx

let genParsedHashDirective (phd: ParsedHashDirectiveNode) =
    let genArg =
        function
        | Choice1Of2(stn) -> genSingleTextNode stn
        | Choice2Of2(idl) -> genIdentListNode idl

    !- "#" +> !-phd.Ident +> sepSpace +> col sepSpace phd.Args genArg |> genNode phd

let genUnit (n: UnitNode) =
    genSingleTextNode n.OpeningParen +> genSingleTextNode n.ClosingParen
    |> genNode n

// genNode will should be called in the caller function.
let genConstant (c: Constant) =
    match c with
    | Constant.FromText n -> genSingleTextNode n
    | Constant.Unit n -> genUnit n
    | Constant.Measure n ->
        (genConstant n.Constant |> genNode (Constant.Node n.Constant))
        +> genSingleTextNode n.Measure.LessThan
        +> genMeasure n.Measure.Measure
        +> genSingleTextNode n.Measure.GreaterThan
        |> genNode n

let genMeasure (measure: Measure) =
    match measure with
    | Measure.Single n -> genSingleTextNode n
    | Measure.Operator n ->
        genMeasure n.LeftHandSide
        +> sepSpace
        +> genSingleTextNode n.Operator
        +> sepSpace
        +> genMeasure n.RightHandSide
        |> genNode n
    | Measure.Divide n ->
        optSingle genMeasure n.LeftHandSide
        +> sepSpace
        +> genSingleTextNode n.Operator
        +> sepSpace
        +> genMeasure n.RightHandSide
        |> genNode n
    | Measure.Power n ->
        genMeasure n.Measure +> genSingleTextNode n.Caret +> genRational n.Exponent
        |> genNode n
    | Measure.Seq n -> col sepSpace n.Measures genMeasure |> genNode n
    | Measure.Multiple n -> genIdentListNode n
    | Measure.Paren n ->
        genSingleTextNode n.OpeningParen
        +> genMeasure n.Measure
        +> genSingleTextNode n.ClosingParen
        |> genNode n

let genRational (rat: RationalConstNode) =
    match rat with
    | RationalConstNode.Integer i -> genSingleTextNode i
    | RationalConstNode.Negate negate -> genSingleTextNode negate.Minus +> genRational negate.Rational
    | RationalConstNode.Rational rationalNode ->
        genSingleTextNode rationalNode.OpeningParen
        +> genSingleTextNode rationalNode.Numerator
        +> genSingleTextNode rationalNode.DivOp
        +> genSingleTextNode rationalNode.Denominator
        +> genSingleTextNode rationalNode.ClosingParen
        |> genNode rationalNode

let genAttributesCore (ats: AttributeNode list) =
    let genAttributeExpr (attr: AttributeNode) =
        match attr.Expr with
        | None -> opt sepColon attr.Target genSingleTextNode +> genIdentListNode attr.TypeName
        | Some e ->
            let argSpacing = if e.HasParentheses then sepNone else sepSpace

            opt sepColon attr.Target genSingleTextNode
            +> genIdentListNode attr.TypeName
            +> argSpacing
            +> genExpr e
        |> genNode attr

    let shortExpression = atCurrentColumn (col sepSemi ats genAttributeExpr)
    let longExpression = atCurrentColumn (col (sepSemi +> sepNln) ats genAttributeExpr)
    ifElse ats.IsEmpty sepNone (expressionFitsOnRestOfLine shortExpression longExpression)

let genOnelinerAttributes (n: MultipleAttributeListNode option) =
    match n with
    | None -> sepNone
    | Some n ->
        let ats =
            List.collect (fun (al: AttributeListNode) -> al.Attributes) n.AttributeLists

        let openingToken =
            List.tryHead n.AttributeLists
            |> Option.map (fun (a: AttributeListNode) -> a.Opening)

        let closingToken =
            List.tryLast n.AttributeLists
            |> Option.map (fun (a: AttributeListNode) -> a.Closing)

        let genAttrs =
            optSingle genSingleTextNode openingToken
            +> genAttributesCore ats
            +> optSingle genSingleTextNode closingToken
            |> genNode n

        ifElse ats.IsEmpty sepNone (genAttrs +> sepSpace)

let genAttributes (node: MultipleAttributeListNode option) =
    match node with
    | None -> sepNone
    | Some node ->
        colPost sepNlnUnlessLastEventIsNewline sepNln node.AttributeLists (fun a ->
            genSingleTextNode a.Opening
            +> (genAttributesCore a.Attributes)
            +> genSingleTextNode a.Closing
            +> sepNlnWhenWriteBeforeNewlineNotEmpty
            |> genNode a)
        |> genNode node

// The inherit keyword should already be printed by the caller
let genInheritConstructor (ic: InheritConstructor) =
    match ic with
    | InheritConstructor.TypeOnly node -> genType node.Type
    | InheritConstructor.Unit node ->
        genType node.Type
        +> sepSpaceBeforeClassConstructor
        +> genSingleTextNode node.OpeningParen
        +> genSingleTextNode node.ClosingParen
    | InheritConstructor.Paren node ->
        genType node.Type
        +> sepSpaceBeforeClassConstructor
        +> expressionFitsOnRestOfLine (genExpr node.Expr) (genMultilineFunctionApplicationArguments node.Expr)
    | InheritConstructor.Other node ->
        genType node.Type
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr)

let mkExprParenNode openingParen e closingParen r =
    ExprParenNode(openingParen, e, closingParen, r) |> Expr.Paren

let isIfThenElse (e: Expr) =
    match e with
    | Expr.IfThen _
    | Expr.IfThenElif _
    | Expr.IfThenElse _ -> true
    | _ -> false

let (|IsIfThenElse|_|) (e: Expr) = if isIfThenElse e then Some e else None

let isLambdaOrIfThenElse (e: Expr) =
    match e with
    | Expr.Lambda _
    | IsIfThenElse _ -> true
    | _ -> false

let (|IsLambdaOrIfThenElse|_|) (e: Expr) =
    if isLambdaOrIfThenElse e then Some e else None

let genExpr (e: Expr) =
    match e with
    | Expr.Lazy node ->
        let genInfixExpr (ctx: Context) =
            isShortExpression
                ctx.Config.MaxInfixOperatorExpression
                // if this fits on the rest of line right after the lazy keyword, it should be wrapped in parenthesis.
                (sepOpenT +> genExpr node.Expr +> sepCloseT)
                // if it is multiline there is no need for parenthesis, because of the indentation
                (indent +> sepNln +> genExpr node.Expr +> unindent)
                ctx

        let genNonInfixExpr =
            autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr)

        genSingleTextNode node.LazyWord
        +> sepSpaceUnlessWriteBeforeNewlineNotEmpty
        +> ifElse node.ExprIsInfix genInfixExpr genNonInfixExpr
        |> genNode node
    | Expr.Single node ->
        genSingleTextNode node.Leading
        +> onlyIf node.AddSpace sepSpaceUnlessWriteBeforeNewlineNotEmpty
        +> ifElse
            node.SupportsStroustrup
            (autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Expr)
            (autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr))
        |> genNode node
    | Expr.Constant node -> genConstant node
    | Expr.Null node -> genSingleTextNode node
    | Expr.Quote node -> genQuoteExpr node
    | Expr.Typed node ->
        let short =
            let genOperator ctx =
                if node.Operator.Length > 1 || ctx.Config.SpaceBeforeColon then
                    (sepSpace +> !-node.Operator) ctx
                else
                    !- node.Operator ctx

            genExpr node.Expr +> genOperator +> sepSpace +> genType node.Type

        let long =
            genExpr node.Expr +> sepNln +> !-node.Operator +> sepSpace +> genType node.Type

        match node.Expr with
        | Expr.Lambda _ -> long
        | _ -> expressionFitsOnRestOfLine short long
        |> genNode node
    | Expr.New node ->
        match node.Arguments with
        | ParenExpr _ ->
            let sepSpaceBeforeArgs (ctx: Context) =
                match node.Type with
                | UppercaseType -> onlyIf ctx.Config.SpaceBeforeUppercaseInvocation sepSpace ctx
                | LowercaseType -> onlyIf ctx.Config.SpaceBeforeLowercaseInvocation sepSpace ctx

            let short =
                genSingleTextNode node.NewKeyword
                +> sepSpace
                +> genType node.Type
                +> sepSpaceBeforeArgs
                +> genExpr node.Arguments

            let long =
                genSingleTextNode node.NewKeyword
                +> sepSpace
                +> genType node.Type
                +> sepSpaceBeforeArgs
                +> genMultilineFunctionApplicationArguments node.Arguments

            expressionFitsOnRestOfLine short long
        | _ ->
            genSingleTextNode node.NewKeyword
            +> sepSpace
            +> genType node.Type
            +> sepSpace
            +> genExpr node.Arguments
        |> genNode node
    | Expr.Tuple node -> genTupleExpr node
    | Expr.StructTuple node ->
        genSingleTextNode node.Struct
        +> sepSpace
        +> sepOpenT
        +> genTupleExpr node.Tuple
        +> genSingleTextNode node.ClosingParen
        |> genNode node
    | Expr.ArrayOrList node -> fun ctx -> genArrayOrList (ctx.Config.MultilineBracketStyle = Cramped) node ctx
    | Expr.Record node ->
        let smallRecordExpr = genSmallRecordNode node
        let multilineRecordExpr = genMultilineRecord node
        genRecord smallRecordExpr multilineRecordExpr node
    | Expr.AnonStructRecord node ->
        let genStructPrefix = genSingleTextNodeWithSpaceSuffix sepSpace node.Struct
        let smallRecordExpr = genStructPrefix +> genSmallRecordNode node

        let multilineRecordExpr =
            if node.Struct.HasContentAfter then
                genStructPrefix +> indentSepNlnUnindent (genMultilineRecord node)
            else
                genStructPrefix +> genMultilineRecord node

        genRecord smallRecordExpr multilineRecordExpr node
    | Expr.InheritRecord node ->
        let genSmallInheritRecordExpr =
            genSmallRecordBaseExpr
                ((genSingleTextNode node.InheritConstructor.InheritKeyword
                  +> sepSpace
                  +> genInheritConstructor node.InheritConstructor
                  |> genNode (InheritConstructor.Node node.InheritConstructor))
                 +> onlyIf node.HasFields sepSemi)
                node

        let genMultilineInheritRecordExpr =
            let fieldsExpr genRecordField =
                genMultilineRecordFieldsExpr genRecordField node

            let genInheritInfo =
                (genSingleTextNode node.InheritConstructor.InheritKeyword
                 +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genInheritConstructor node.InheritConstructor)
                 |> genNode (InheritConstructor.Node node.InheritConstructor))
                +> onlyIf node.HasFields sepNln

            let genMultilineAlignBrackets =
                genSingleTextNode node.OpeningBrace
                +> indentSepNlnUnindent (genInheritInfo +> fieldsExpr genRecordFieldNameAligned)
                +> sepNln
                +> genSingleTextNode node.ClosingBrace

            let genMultilineCramped (ctx: Context) =
                // This is the column each field should start on.
                let targetColumn =
                    let openBraceLength = node.OpeningBrace.Text.Length

                    ctx.Column
                    + (if ctx.Config.SpaceAroundDelimiter then
                           openBraceLength + 1
                       else
                           openBraceLength)

                (genSingleTextNodeSuffixDelimiter node.OpeningBrace
                 +> atCurrentColumn genInheritInfo
                 +> col sepNln node.Fields (fun e ->
                     // Add spaces to ensure the record field (incl trivia) starts at the right column.
                     addFixedSpaces targetColumn
                     // Potential indentations will be in relation to the opening curly brace.
                     +> genRecordFieldNameCramped false e)
                 +> addSpaceIfSpaceAroundDelimiter
                 +> genSingleTextNode node.ClosingBrace)
                    ctx

            ifAlignOrStroustrupBrackets genMultilineAlignBrackets genMultilineCramped

        genRecord genSmallInheritRecordExpr genMultilineInheritRecordExpr node
    | Expr.ObjExpr node ->
        let param = optSingle genExpr node.Expr

        if node.Bindings.IsEmpty && node.Members.IsEmpty && node.Interfaces.IsEmpty then
            // See https://devblogs.microsoft.com/dotnet/announcing-f-5/#default-interface-member-consumption
            genSingleTextNode node.OpeningBrace
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.New
            +> sepSpace
            +> genType node.Type
            +> param
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.ClosingBrace
        else
            let genInterfaceImpl (node: InterfaceImplNode) =
                if node.Bindings.IsEmpty && node.Members.IsEmpty then
                    genSingleTextNode node.Interface +> sepSpace +> genType node.Type
                else
                    genSingleTextNode node.Interface
                    +> sepSpace
                    +> genType node.Type
                    +> sepSpace
                    +> optSingle genSingleTextNode node.With
                    +> indentSepNlnUnindent (genBindings false node.Bindings +> genMemberDefnList node.Members)
                |> genNode node

            let genBody =
                let sepNlnBeforeInterfaces =
                    match node.Interfaces with
                    | [] -> sepNone
                    | h :: _ -> sepNlnUnlessContentBefore h

                indentSepNlnUnindent (genBindings false node.Bindings +> genMemberDefnList node.Members)
                +> sepNlnBeforeInterfaces
                +> colEx sepNlnUnlessContentBefore node.Interfaces genInterfaceImpl

            let genObjExpr =
                genSingleTextNode node.OpeningBrace
                +> addSpaceIfSpaceAroundDelimiter
                +> atCurrentColumn (
                    genSingleTextNode node.New
                    +> sepSpace
                    +> genType node.Type
                    +> param
                    +> sepSpace
                    +> optSingle genSingleTextNode node.With
                    +> genBody
                )
                +> addSpaceIfSpaceAroundDelimiter
                +> genSingleTextNode node.ClosingBrace

            let genObjExprAlignBrackets =
                let genObjExpr =
                    atCurrentColumn (
                        genSingleTextNode node.New
                        +> sepSpace
                        +> genType node.Type
                        +> param
                        +> sepSpace
                        +> optSingle genSingleTextNode node.With
                        +> genBody
                    )

                atCurrentColumnIndent (
                    genSingleTextNode node.OpeningBrace
                    +> addSpaceIfSpaceAroundDelimiter
                    +> genObjExpr
                    +> sepNln
                    +> genSingleTextNode node.ClosingBrace
                )

            ifAlignOrStroustrupBrackets genObjExprAlignBrackets genObjExpr
        |> genNode node
    | Expr.While node ->
        atCurrentColumn (
            genSingleTextNode node.While
            +> sepSpace
            +> genExpr node.WhileExpr
            +> !- " do"
            +> indentSepNlnUnindent (genExpr node.DoExpr)
        )
        |> genNode node
    | Expr.For node ->
        atCurrentColumn (
            genSingleTextNode node.For
            +> sepSpace
            +> genSingleTextNode node.Ident
            +> sepSpace
            +> genSingleTextNode node.Equals
            +> sepSpace
            +> genExpr node.IdentBody
            +> ifElse node.Direction (!- " to ") (!- " downto ")
            +> genExpr node.ToBody
            +> !- " do"
            +> indent
            +> sepNln
            +> genExpr node.DoBody
            +> unindent
        )
        |> genNode node
    | Expr.ForEach node ->
        atCurrentColumn (
            genSingleTextNode node.For
            +> sepSpace
            +> genPat node.Pattern
            +> !- " in "
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.EnumExpr)
            +> ifElse
                node.IsArrow
                (sepArrow +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.BodyExpr))
                (!- " do" +> indent +> sepNln +> genExpr node.BodyExpr +> unindent)
        )
        |> genNode node
    | Expr.NamedComputation node ->
        let short =
            genExpr node.Name
            +> sepSpace
            +> genSingleTextNode node.OpeningBrace
            +> addSpaceIfSpaceAroundDelimiter
            +> genExpr node.Body
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.ClosingBrace

        let long =
            genExpr node.Name
            +> sepSpace
            +> genSingleTextNode node.OpeningBrace
            +> indentSepNlnUnindent (genExpr node.Body)
            +> sepNln
            +> genSingleTextNode node.ClosingBrace

        expressionFitsOnRestOfLine short long |> genNode node
    | Expr.Computation node ->
        genSingleTextNode node.OpeningBrace
        +> addSpaceIfSpaceAroundDelimiter
        +> genExpr node.Body
        +> addSpaceIfSpaceAroundDelimiter
        +> genSingleTextNode node.ClosingBrace
        |> genNode node
    | Expr.CompExprBody node ->
        let genStatements =
            node.Statements
            |> List.map (function
                | ComputationExpressionStatement.LetOrUseStatement node ->
                    let expr =
                        genBinding node.Binding
                        +> optSingle (fun inNode -> sepSpace +> genSingleTextNode inNode) node.In
                        |> genNode node

                    ColMultilineItem(expr, sepNlnUnlessContentBefore node)
                | ComputationExpressionStatement.LetOrUseBangStatement node ->
                    let expr =
                        genSingleTextNode node.LeadingKeyword
                        +> sepSpace
                        +> genPat node.Pattern
                        +> sepSpace
                        +> genSingleTextNode node.Equals
                        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Expression
                        |> genNode node

                    ColMultilineItem(expr, sepNlnUnlessContentBefore node)
                | ComputationExpressionStatement.AndBangStatement node ->
                    let expr =
                        genSingleTextNode node.LeadingKeyword
                        +> sepSpace
                        +> genPat node.Pattern
                        +> sepSpace
                        +> genSingleTextNode node.Equals
                        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Expression
                        |> genNode node

                    ColMultilineItem(expr, sepNlnUnlessContentBefore node)
                | ComputationExpressionStatement.OtherStatement e ->
                    ColMultilineItem(genExpr e, sepNlnUnlessContentBefore (Expr.Node e)))
            |> colWithNlnWhenItemIsMultilineUsingConfig
            |> genNode node

        match node.Statements with
        | [ ComputationExpressionStatement.LetOrUseStatement letOrUseNode
            ComputationExpressionStatement.OtherStatement inExpr ] when letOrUseNode.In.IsSome ->
            let short =
                (genBinding letOrUseNode.Binding
                 +> optSingle (fun inNode -> sepSpace +> genSingleTextNode inNode +> sepSpace) letOrUseNode.In
                 |> genNode letOrUseNode)
                +> sepSpace
                +> genExpr inExpr
                |> genNode node

            expressionFitsOnRestOfLine short genStatements
        | _ -> genStatements
    | Expr.JoinIn node ->
        genExpr node.LeftHandSide
        +> sepSpace
        +> genSingleTextNode node.In
        +> sepSpace
        +> atCurrentColumn (genExpr node.RightHandSide)
        |> genNode node
    | Expr.ParenLambda node ->
        genSingleTextNode node.OpeningParen
        +> leadingExpressionIsMultiline
            (genLambdaWithParen node.Lambda +> sepNlnWhenWriteBeforeNewlineNotEmpty)
            (fun isMultiline ctx ->
                onlyIf
                    (isMultiline
                     && ctx.Config.MultiLineLambdaClosingNewline
                     && not (isStroustrupStyleExpr ctx.Config node.Lambda.Expr))
                    sepNln
                    ctx)
        +> genSingleTextNode node.ClosingParen
        |> genNode node
    | Expr.Lambda node -> genLambda node
    | Expr.MatchLambda node ->
        genSingleTextNode node.Function +> sepNln +> genClauses node.Clauses
        |> genNode node
    | Expr.Match node ->
        atCurrentColumn (
            genControlExpressionStartCore (Choice1Of2 node.Match) node.MatchExpr node.With
            +> sepNln
            +> genClauses node.Clauses
        )
        |> genNode node
    | Expr.TraitCall node ->
        genType node.Type
        +> sepColon
        +> sepOpenT
        +> genMemberDefn node.MemberDefn
        +> sepCloseT
        +> sepSpace
        +> genExpr node.Expr
        |> genNode node
    | Expr.ParenILEmbedded node -> genSingleTextNode node
    | Expr.ParenFunctionNameWithStar node ->
        genSingleTextNode node.OpeningParen
        +> sepSpace
        +> genSingleTextNode node.FunctionName
        +> sepSpace
        +> genSingleTextNode node.ClosingParen
        |> genNode node
    | Expr.Paren node ->
        match node.Expr with
        | Expr.CompExprBody _ ->
            genSingleTextNode node.OpeningParen
            +> atCurrentColumn (genExpr node.Expr)
            +> genSingleTextNode node.ClosingParen
        | _ ->
            genSingleTextNode node.OpeningParen
            +> sepNlnWhenWriteBeforeNewlineNotEmpty
            +> genExpr node.Expr
            +> genSingleTextNode node.ClosingParen
        |> genNode node
    | Expr.Dynamic node -> genExpr node.FuncExpr +> !- "?" +> genExpr node.ArgExpr |> genNode node
    | Expr.PrefixApp node ->
        let fallback = genSingleTextNode node.Operator +> genExpr node.Expr

        match node.Expr with
        | Expr.Constant _
        | Expr.InterpolatedStringExpr _ when not (String.startsWithOrdinal "%" node.Operator.Text) ->
            genSingleTextNode node.Operator +> sepSpace +> genExpr node.Expr
        | Expr.AppSingleParenArg appNode ->
            genSingleTextNode node.Operator
            +> sepSpace
            +> genExpr appNode.FunctionExpr
            +> genExpr appNode.ArgExpr
        | Expr.AppLongIdentAndSingleParenArg appNode ->
            let mOptVarNode = appNode.FunctionName.Range

            genSingleTextNode node.Operator
            +> sepSpace
            +> genExpr (Expr.OptVar(ExprOptVarNode(false, appNode.FunctionName, mOptVarNode)))
            +> genExpr appNode.ArgExpr
        | Expr.App appNode ->
            match appNode.Arguments with
            | [ Expr.Constant(Constant.Unit _) as argExpr ] ->
                genSingleTextNode node.Operator
                +> sepSpace
                +> genExpr appNode.FunctionExpr
                +> genExpr argExpr
            | _ -> fallback
        | _ -> fallback
        |> genNode node
    // | Expr.SameInfixApps node ->
    //     let headIsSynExprLambdaOrIfThenElse = isLambdaOrIfThenElse node.LeadingExpr
    //
    //     let shortExpr =
    //         onlyIf headIsSynExprLambdaOrIfThenElse sepOpenT
    //         +> genExpr node.LeadingExpr
    //         +> onlyIf headIsSynExprLambdaOrIfThenElse sepCloseT
    //         +> sepSpace
    //         +> col sepSpace node.SubsequentExpressions (fun (operator, rhs) ->
    //             genSingleTextNode operator
    //             +> sepSpace
    //             +> onlyIf (isLambdaOrIfThenElse rhs) sepOpenT
    //             +> genExpr rhs
    //             +> onlyIf (isLambdaOrIfThenElse rhs) sepCloseT)
    //
    //     let multilineExpr =
    //         match node.SubsequentExpressions with
    //         | [] -> genExpr node.LeadingExpr
    //         | (operator, e2) :: es ->
    //             let m =
    //                 Fantomas.FCS.Text.Range.unionRanges (Expr.Node node.LeadingExpr).Range (Expr.Node e2).Range
    //
    //             genMultilineInfixExpr (ExprInfixAppNode(node.LeadingExpr, operator, e2, m))
    //             +> sepNln
    //             +> col sepNln es (fun (operator, e) ->
    //                 genSingleTextNode operator
    //                 +> sepNlnWhenWriteBeforeNewlineNotEmptyOr sepSpace
    //                 +> (fun ctx ->
    //                     match e with
    //                     | Expr.Lambda _ when
    //                         newLineInfixOps.Contains operator.Text
    //                         && ctx.Config.IndentSize <= operator.Text.Length
    //                         ->
    //                         // Special measure to account for https://github.com/fsprojects/fantomas/issues/870
    //                         (indent +> genExprInMultilineInfixExpr e +> unindent) ctx
    //                     | _ -> genExprInMultilineInfixExpr e ctx))
    //
    //     fun ctx ->
    //         genNode
    //             node
    //             (atCurrentColumn (isShortExpression ctx.Config.MaxInfixOperatorExpression shortExpr multilineExpr))
    //             ctx
    //
    // | Expr.InfixApp node ->
    //     let genOnelinerInfixExpr (node: ExprInfixAppNode) =
    //         let genExpr e =
    //             match e with
    //             | Expr.Record _
    //             | Expr.AnonStructRecord _ -> atCurrentColumnIndent (genExpr e)
    //             | _ -> genExpr e
    //
    //         genExpr node.LeftHandSide
    //         +> sepSpace
    //         +> genSingleTextNode node.Operator
    //         +> sepNlnWhenWriteBeforeNewlineNotEmpty
    //         +> sepSpace
    //         +> genExpr node.RightHandSide
    //
    //     if
    //         isLambdaOrIfThenElse node.LeftHandSide
    //         && newLineInfixOps.Contains node.Operator.Text
    //     then
    //         genNode node (genMultilineInfixExpr node)
    //     else
    //         fun ctx ->
    //             genNode
    //                 node
    //                 (isShortExpression
    //                     ctx.Config.MaxInfixOperatorExpression
    //                     (genOnelinerInfixExpr node)
    //                     (ifElse
    //                         (noBreakInfixOps.Contains(node.Operator.Text))
    //                         (genOnelinerInfixExpr node)
    //                         (genMultilineInfixExpr node)))
    //                 ctx

    | Expr.IndexWithoutDot node ->
        let genIdentifierExpr =
            match node.Identifier with
            | Expr.AppLongIdentAndSingleParenArg appNode -> genAppLongIdentAndSingleParenArgExpr sepNone appNode
            | Expr.AppSingleParenArg appNode -> genAppSingleParenArgExpr sepNone appNode
            | _ -> genExpr node.Identifier

        let genIndexExpr = genExpr node.Index

        genIdentifierExpr
        +> sepOpenLFixed
        +> expressionFitsOnRestOfLine genIndexExpr (atCurrentColumnIndent genIndexExpr)
        +> sepCloseLFixed
        |> genNode node

    | Expr.Chain node ->
        let genLink (isLastLink: bool) (link: ChainLink) =
            match link with
            | ChainLink.Identifier expr -> genExpr expr
            | ChainLink.Dot stn -> genSingleTextNode stn
            | ChainLink.Expr expr ->
                match expr with
                | Expr.App appNode ->
                    match appNode.Arguments with
                    | [ Expr.ArrayOrList _ as arrayOrList ] ->
                        // Edge case for something like .G[].
                        genExpr appNode.FunctionExpr +> genExpr arrayOrList
                    | _ -> genExpr expr
                | _ -> genExpr expr
            | ChainLink.AppUnit appUnitNode ->
                genExpr appUnitNode.FunctionName
                +> onlyIf
                    isLastLink
                    (sepSpaceBeforeParenInFuncInvocation
                        appUnitNode.FunctionName
                        (Expr.Constant(Constant.Unit appUnitNode.Unit)))
                +> genUnit appUnitNode.Unit
                |> genNode appUnitNode
            | ChainLink.AppParen appParen ->
                let short =
                    genExpr appParen.FunctionName
                    +> onlyIf
                        isLastLink
                        (sepSpaceBeforeParenInFuncInvocation appParen.FunctionName (Expr.Paren appParen.Paren))
                    +> genExpr (Expr.Paren appParen.Paren)

                let long =
                    match appParen.Paren.Expr with
                    | Expr.Lambda lambdaNode ->
                        genExpr appParen.FunctionName
                        +> onlyIf
                            isLastLink
                            (sepSpaceBeforeParenInFuncInvocation appParen.FunctionName (Expr.Paren appParen.Paren))
                        +> genSingleTextNode appParen.Paren.OpeningParen
                        +> genLambdaWithParen lambdaNode
                        +> onlyIfCtx
                            (fun ctx ->
                                ctx.Config.MultiLineLambdaClosingNewline
                                && (not (isStroustrupStyleExpr ctx.Config lambdaNode.Expr)))
                            sepNln
                        +> genSingleTextNode appParen.Paren.ClosingParen
                    | _ ->
                        genExpr appParen.FunctionName
                        +> onlyIf
                            isLastLink
                            (sepSpaceBeforeParenInFuncInvocation appParen.FunctionName (Expr.Paren appParen.Paren))
                        +> genMultilineFunctionApplicationArguments (Expr.Paren appParen.Paren)

                expressionFitsOnRestOfLine short long |> genNode appParen
            | ChainLink.IndexExpr e -> sepOpenLFixed +> genExpr e +> sepCloseLFixed

        let lastIndex = node.Links.Length - 1
        let short = coli sepNone node.Links (fun idx -> genLink (idx = lastIndex))

        let long =
            let (|SimpleChain|_|) (link: ChainLink) =
                match link with
                | ChainLink.Identifier _
                | ChainLink.IndexExpr _ -> Some link
                | _ -> None

            let (|LeadingSimpleChain|_|) (links: ChainLink list) =
                let leading = System.Collections.Generic.Queue(links.Length)
                let rest = System.Collections.Generic.Queue(links.Length)

                (None, links)
                ||> List.fold (fun lastDot link ->
                    if not (Seq.isEmpty rest) then
                        rest.Enqueue link
                        None
                    else
                        match link with
                        | SimpleChain _ ->
                            Option.iter leading.Enqueue lastDot
                            leading.Enqueue link
                            None
                        | ChainLink.Dot _ as dot -> Some dot
                        | _ ->
                            Option.iter rest.Enqueue lastDot
                            rest.Enqueue link
                            None)
                |> (fun _ ->
                    if Seq.isEmpty leading then
                        None
                    else
                        Some(Seq.toList leading, Seq.toList rest))

            let rec genIndentedLinks (lastLinkWasSimple: bool) (links: ChainLink list) (ctx: Context) : Context =
                match links with
                | [] -> ctx
                | ChainLink.Dot dot :: link :: rest ->
                    let isLast = List.isEmpty rest
                    let genDotAndLink = genSingleTextNode dot +> genLink isLast link
                    let currentIsSimple = ((|SimpleChain|_|) >> Option.isSome) link
                    let currentLinkFitsOnRestOfLine = not (futureNlnCheck genDotAndLink ctx)

                    if lastLinkWasSimple && currentLinkFitsOnRestOfLine then
                        // The last link was an identifier and the current link fits on the remainder of the current line.
                        genIndentedLinks currentIsSimple rest (genDotAndLink ctx)
                    else
                        let ctx' =
                            onlyIf
                                (not // Last link was `.Foo()`
                                    lastLinkWasSimple
                                 // `.Foo.Bar` but `Bar` crossed the max_line_length
                                 || (lastLinkWasSimple && currentIsSimple && not currentLinkFitsOnRestOfLine))
                                sepNlnUnlessLastEventIsNewline
                                ctx

                        genIndentedLinks
                            currentIsSimple
                            rest
                            // Print the current link
                            (genDotAndLink ctx')
                | _ -> failwith "Expected dot in chain at this point"

            let genFirstLinkAndIndentOther (firstLink: ChainLink) (others: ChainLink list) =
                genLink false firstLink +> indentSepNlnUnindent (genIndentedLinks false others)

            match node.Links with
            | [] -> sepNone
            | LeadingSimpleChain(leadingChain, links) ->
                match links with
                | [] ->
                    fun ctx ->
                        isShortExpression
                            ctx.Config.MaxDotGetExpressionWidth
                            short
                            (match leadingChain with
                             | [] -> sepNone
                             | head :: links -> genLink false head +> indent +> genIndentedLinks true links +> unindent)
                            ctx
                | _ ->
                    expressionFitsOnRestOfLine
                        (coli sepNone leadingChain (fun idx -> genLink (idx = lastIndex)))
                        (match leadingChain with
                         | [] -> sepNone
                         | [ head ] -> genLink false head
                         | head :: rest -> genLink false head +> indentSepNlnUnindent (genIndentedLinks true rest))
                    +> indentSepNlnUnindent (genIndentedLinks true links)

            | head :: links -> genFirstLinkAndIndentOther head links

        (fun ctx -> isShortExpression ctx.Config.MaxDotGetExpressionWidth short long ctx)
        |> genNode node

    // path.Replace("../../../", "....")
    | Expr.AppLongIdentAndSingleParenArg node ->
        let addSpace =
            sepSpaceBeforeParenInFuncInvocation
                (Expr.OptVar(ExprOptVarNode(false, node.FunctionName, node.FunctionName.Range)))
                node.ArgExpr

        genAppLongIdentAndSingleParenArgExpr addSpace node
    // fn (a, b, c)
    | Expr.AppSingleParenArg node ->
        let addSpace = sepSpaceBeforeParenInFuncInvocation node.FunctionExpr node.ArgExpr
        genAppSingleParenArgExpr addSpace node

    // functionName arg1 arg2 (fun x y z -> ...)
    | Expr.AppWithLambda node ->
        let sepSpaceAfterFunctionName =
            match node.Arguments with
            | [] ->
                // We create a temporary fake paren node only for the sepSpaceBeforeParenInFuncInvocation call.
                let parenExpr =
                    mkExprParenNode
                        node.OpeningParen
                        (Expr.Null(SingleTextNode("", Fantomas.FCS.Text.Range.Zero)))
                        node.ClosingParen
                        Fantomas.FCS.Text.Range.Zero

                sepSpaceBeforeParenInFuncInvocation node.FunctionName parenExpr
            | _ -> sepSpace

        genAppWithLambda sepSpaceAfterFunctionName node
    | Expr.NestedIndexWithoutDot node ->
        genExpr node.Identifier
        +> sepOpenLFixed
        +> genExpr node.Index
        +> sepCloseLFixed
        +> genExpr node.Argument
        |> genNode node

    | Expr.App node ->
        fun ctx ->
            match node with
            | EndsWithDualListApp ctx.Config (sequentialArgs: Expr list,
                                              firstList: ExprArrayOrListNode,
                                              lastList: ExprArrayOrListNode) ->
                let genArrayOrList = genArrayOrList false

                // check if everything else beside the last array/list fits on one line
                let singleLineTestExpr =
                    genExpr node.FunctionExpr
                    +> sepSpace
                    +> col sepSpace sequentialArgs genExpr
                    +> sepSpace
                    +> genArrayOrList firstList

                let short =
                    genExpr node.FunctionExpr
                    +> sepSpace
                    +> col sepSpace sequentialArgs genExpr
                    +> onlyIfNot sequentialArgs.IsEmpty sepSpace
                    +> genArrayOrList firstList
                    +> sepSpace
                    +> genArrayOrList lastList

                let long =
                    // check if everything besides both lists fits on one line
                    let singleLineTestExpr =
                        genExpr node.FunctionExpr +> sepSpace +> col sepSpace sequentialArgs genExpr

                    if futureNlnCheck singleLineTestExpr ctx then
                        genExpr node.FunctionExpr
                        +> indent
                        +> sepNln
                        +> col sepNln sequentialArgs genExpr
                        +> sepSpace
                        +> genArrayOrList firstList
                        +> sepSpace
                        +> genArrayOrList lastList
                        +> unindent
                    else
                        genExpr node.FunctionExpr
                        +> sepSpace
                        +> col sepSpace sequentialArgs genExpr
                        +> onlyIfNot sequentialArgs.IsEmpty sepSpace
                        +> genArrayOrList firstList
                        +> sepSpace
                        +> genArrayOrList lastList

                if futureNlnCheck singleLineTestExpr ctx then
                    long ctx
                else
                    short ctx

            | EndsWithSingleListApp ctx.Config (sequentialArgs: Expr list, arrayOrList: ExprArrayOrListNode) ->
                let genArrayOrList = genArrayOrList false

                // check if everything else beside the last array/list fits on one line
                let singleLineTestExpr =
                    genExpr node.FunctionExpr +> sepSpace +> col sepSpace sequentialArgs genExpr

                let short =
                    genExpr node.FunctionExpr
                    +> sepSpace
                    +> col sepSpace sequentialArgs genExpr
                    +> onlyIfNot sequentialArgs.IsEmpty sepSpace
                    +> genArrayOrList arrayOrList

                let long =
                    genExpr node.FunctionExpr
                    +> indent
                    +> sepNln
                    +> col sepNln sequentialArgs genExpr
                    +> onlyIfNot sequentialArgs.IsEmpty sepNln
                    +> genArrayOrList arrayOrList
                    +> unindent

                if futureNlnCheck singleLineTestExpr ctx then
                    long ctx
                else
                    short ctx

            | EndsWithSingleRecordApp ctx.Config (sequentialArgs: Expr list, recordOrAnonRecord) ->
                // check if everything else beside the last array/list fits on one line
                let singleLineTestExpr =
                    genExpr node.FunctionExpr +> sepSpace +> col sepSpace sequentialArgs genExpr

                let short =
                    genExpr node.FunctionExpr
                    +> sepSpace
                    +> col sepSpace sequentialArgs genExpr
                    +> onlyIfNot sequentialArgs.IsEmpty sepSpace
                    +> genExpr recordOrAnonRecord

                let long =
                    genExpr node.FunctionExpr
                    +> indent
                    +> sepNln
                    +> col sepNln sequentialArgs genExpr
                    +> onlyIfNot sequentialArgs.IsEmpty sepNln
                    +> genExpr recordOrAnonRecord
                    +> unindent

                if futureNlnCheck singleLineTestExpr ctx then
                    long ctx
                else
                    short ctx
            | _ ->
                let shortExpression =
                    let sep ctx =
                        match node.Arguments with
                        | [] -> ctx
                        | [ singleArg ] -> sepSpaceBeforeParenInFuncInvocation node.FunctionExpr singleArg ctx
                        | _ -> sepSpace ctx

                    genExpr node.FunctionExpr +> sep +> col sepSpace node.Arguments genExpr

                let longExpression (ctx: Context) =
                    let startColumn = ctx.Column

                    let ensureArgumentsAreNotAlignedWithFunctionName f (ctx: Context) =
                        let nextColumn =
                            Math.Max(ctx.WriterModel.AtColumn, ctx.WriterModel.Indent)
                            + ctx.Config.IndentSize

                        if startColumn = nextColumn then
                            (indent +> indent +> sepNln +> f +> unindent +> unindent) ctx
                        else
                            indentSepNlnUnindent f ctx

                    let genFuncExpr =
                        match node.FunctionExpr with
                        | Expr.TypeApp node -> genTypeApp true node
                        | _ -> genExpr node.FunctionExpr

                    (genFuncExpr
                     +> ensureArgumentsAreNotAlignedWithFunctionName (col sepNln node.Arguments genExpr))
                        ctx

                expressionFitsOnRestOfLine shortExpression longExpression ctx

        |> genNode node
    | Expr.TypeApp node -> expressionFitsOnRestOfLine (genTypeApp false node) (genTypeApp true node)
    | Expr.TryWithSingleClause node ->
        let genClause =
            let clauseNode = node.Clause

            leadingExpressionResult
                (enterNode clauseNode
                 +> optSingle (fun bar -> genNode bar sepNone) clauseNode.Bar)
                (fun ((linesBefore, _), (linesAfter, _)) ->
                    onlyIfCtx (fun ctx -> linesAfter > linesBefore || hasWriteBeforeNewlineContent ctx) sepBar)
            +> genPatInClause clauseNode.Pattern
            +> optSingle
                (fun e ->
                    !- " when"
                    +> expressionFitsOnRestOfLine (sepSpace +> genExpr e) (fun ctx ->
                        // See https://github.com/fsprojects/fantomas/issues/2784
                        let doubleIndent = ctx.Config.IndentSize < 4

                        (indent
                         +> onlyIf doubleIndent indent
                         +> sepNln
                         +> genExpr e
                         +> unindent
                         +> onlyIf doubleIndent unindent)
                            ctx))
                clauseNode.WhenExpr
            +> sepSpace
            +> genSingleTextNodeWithSpaceSuffix sepSpace clauseNode.Arrow
            +> indentSepNlnUnindentUnlessStroustrup genExpr clauseNode.BodyExpr
            +> leaveNode clauseNode

        atCurrentColumn (
            genSingleTextNode node.Try
            +> indent
            +> sepNln
            +> genExpr node.TryExpr
            +> unindent
            +> sepNln
            +> genSingleTextNode node.With
            +> sepSpace
            +> genClause
        )
        |> genNode node
    | Expr.TryWith node ->
        atCurrentColumn (
            genSingleTextNode node.Try
            +> indent
            +> sepNln
            +> genExpr node.TryExpr
            +> unindent
            +> sepNln
            +> genSingleTextNode node.With
            +> sepNln
            +> col sepNln node.Clauses (genClause false)
        )
        |> genNode node
    | Expr.TryFinally node ->
        atCurrentColumn (
            genSingleTextNode node.Try
            +> indentSepNlnUnindent (genExpr node.TryExpr)
            +> sepNln
            +> genSingleTextNode node.Finally
            +> indentSepNlnUnindent (genExpr node.FinallyExpr)
        )
        |> genNode node
    | Expr.IfThen node ->
        leadingExpressionResult
            (genControlExpressionStartCore (Choice2Of2 node.If) node.IfExpr node.Then)
            (fun ((lineCountBefore, columnBefore), (lineCountAfter, columnAfter)) ctx ->
                // Check if the `if expr then` is already multiline or cross the max_line_length.
                let isMultiline =
                    lineCountAfter > lineCountBefore || columnAfter > ctx.Config.MaxLineLength

                if isMultiline then
                    indentSepNlnUnindent (genExpr node.ThenExpr) ctx
                else
                    // Check if the entire expression is will still fit on one line, respecting MaxIfThenShortWidth
                    let remainingMaxLength =
                        ctx.Config.MaxIfThenShortWidth - (columnAfter - columnBefore)

                    isShortExpression
                        remainingMaxLength
                        (sepSpace +> genExpr node.ThenExpr)
                        (indentSepNlnUnindent (genExpr node.ThenExpr))
                        ctx)
        |> atCurrentColumnIndent
        |> genNode node
    | Expr.IfThenElse node ->
        leadingExpressionResult
            (genControlExpressionStartCore (Choice2Of2 node.If) node.IfExpr node.Then)
            (fun ((lineCountBefore, columnBefore), (lineCountAfter, columnAfter)) ctx ->
                let long =
                    indentSepNlnUnindent (genExpr node.ThenExpr)
                    +> sepNln
                    +> genSingleTextNode node.Else
                    +> genKeepIdentIfThenElse node.If.Node node.Else node.ElseExpr

                // Check if the `if expr then` is already multiline or cross the max_line_length.
                let isMultiline =
                    lineCountAfter > lineCountBefore || columnAfter > ctx.Config.MaxLineLength

                // If the `thenExpr` is also an SynExpr.IfThenElse, it will not be valid code if put on one line.
                // ex: if cond then if a then b else c else e2
                let thenExprIsIfThenElse = isIfThenElse node.ThenExpr

                if isMultiline || thenExprIsIfThenElse then
                    long ctx
                else
                    // Check if the entire expression is will still fit on one line, respecting MaxIfThenShortWidth
                    let remainingMaxLength =
                        ctx.Config.MaxIfThenElseShortWidth - (columnAfter - columnBefore)

                    isShortExpression
                        remainingMaxLength
                        (sepSpace
                         +> genExpr node.ThenExpr
                         +> sepSpace
                         +> genSingleTextNode node.Else
                         +> sepSpace
                         +> genExpr node.ElseExpr)
                        long
                        ctx)
        // |> atCurrentColumnIndent
        |> genNode node
    | Expr.IfThenElif node ->
        // multiple branches but no else expr
        // use the same threshold check as for if-then
        // Everything should fit on one line
        let areAllShort ctx =
            let anyThenExprIsIfThenElse =
                node.Branches |> List.exists (fun node -> isIfThenElse node.ThenExpr)

            let checkIfLine (node: ExprIfThenNode) =
                genControlExpressionStartCore (Choice2Of2 node.If) node.IfExpr node.Then
                +> sepSpace
                +> genExpr node.ThenExpr

            let linesToCheck =
                match node.Else with
                | None -> List.map checkIfLine node.Branches
                | Some(elseNode, elseExpr) ->
                    // This may appear a bit odd that we are adding the `else elseExpr` before the `if expr then expr` lines but purely for this check this doesn't matter.
                    // Each lines needs to fit on one line in order for us to format the short way
                    (genSingleTextNode elseNode +> sepSpace +> genExpr elseExpr)
                    :: (List.map checkIfLine node.Branches)

            let lineCheck () =
                linesToCheck
                |> List.forall (fun lineCheck ->
                    let maxWidth =
                        if node.Else.IsSome then
                            ctx.Config.MaxIfThenElseShortWidth
                        else
                            ctx.Config.MaxIfThenShortWidth

                    not (exceedsWidth maxWidth lineCheck ctx))

            not anyThenExprIsIfThenElse && lineCheck ()

        let shortExpr =
            col sepNln node.Branches (fun (node: ExprIfThenNode) ->
                genControlExpressionStartCore (Choice2Of2 node.If) node.IfExpr node.Then
                +> sepSpace
                +> genExpr node.ThenExpr
                |> genNode node)
            +> optSingle
                (fun (elseNode, elseExpr) -> sepNln +> genSingleTextNode elseNode +> sepSpace +> genExpr elseExpr)
                node.Else

        let longExpr =
            col sepNln node.Branches (fun (node: ExprIfThenNode) ->
                genControlExpressionStartCore (Choice2Of2 node.If) node.IfExpr node.Then
                +> indentSepNlnUnindent (genExpr node.ThenExpr)
                |> genNode node)
            +> optSingle
                (fun (elseNode, elseExpr) ->
                    let genKeepIdent =
                        let branch = List.last node.Branches
                        genKeepIdentIfThenElse branch.If.Node elseNode elseExpr

                    sepNln +> genSingleTextNode elseNode +> genKeepIdent)
                node.Else

        ifElseCtx areAllShort shortExpr longExpr
        |> atCurrentColumnIndent
        |> genNode node
    | Expr.Ident node -> genSingleTextNode node
    | Expr.OptVar node ->
        onlyIf node.IsOptional (!- "?") +> genIdentListNode node.Identifier
        |> genNode node
    | Expr.LongIdentSet node ->
        genIdentListNode node.Identifier
        +> sepArrowRev
        +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Expr
        |> genNode node
    | Expr.DotIndexedGet node ->
        let genDotIndexedGet =
            let isParen =
                match node.ObjectExpr with
                | Expr.Paren _ -> true
                | _ -> false

            ifElse isParen (genExpr node.ObjectExpr) (addParenIfAutoNln node.ObjectExpr genExpr)
            +> !- "."
            +> sepOpenLFixed
            +> genExpr node.IndexExpr
            +> sepCloseLFixed

        let genDotIndexedGetWithApp funcExpr argExpr (appNode: Node) =
            let short = funcExpr +> genExpr argExpr |> genNode appNode

            let long =
                funcExpr +> genMultilineFunctionApplicationArguments argExpr |> genNode appNode

            let idx = !- "." +> sepOpenLFixed +> genExpr node.IndexExpr +> sepCloseLFixed
            expressionFitsOnRestOfLine (short +> idx) (long +> idx)

        match node.ObjectExpr with
        | Expr.App appNode ->
            match appNode.Arguments with
            | [ Expr.Constant(Constant.Unit _) as ux ] ->
                genDotIndexedGetWithApp (genExpr appNode.FunctionExpr) ux appNode
            | _ -> genDotIndexedGet
        | Expr.AppSingleParenArg appNode ->
            genDotIndexedGetWithApp (genExpr appNode.FunctionExpr) appNode.ArgExpr appNode

        | Expr.AppLongIdentAndSingleParenArg appNode ->
            genDotIndexedGetWithApp (genIdentListNode appNode.FunctionName) appNode.ArgExpr appNode
        | _ -> genDotIndexedGet
        |> genNode node
    | Expr.DotIndexedSet node ->
        let genDotIndexedSet =
            addParenIfAutoNln node.ObjectExpr genExpr
            +> !- ".["
            +> genExpr node.Index
            +> !- "] <- "
            +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Value

        let genDotIndexedSetWithApp funcExpr argExpr (appNode: Node) =
            let short = funcExpr +> genExpr argExpr |> genNode appNode

            let long =
                funcExpr +> genMultilineFunctionApplicationArguments argExpr |> genNode appNode

            let idx =
                !- "." +> sepOpenLFixed +> genExpr node.Index +> sepCloseLFixed +> sepArrowRev

            expressionFitsOnRestOfLine
                (short +> idx +> genExpr node.Value)
                (long
                 +> idx
                 +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Value)

        match node.ObjectExpr with
        | Expr.App appNode ->
            match appNode.Arguments with
            | [ Expr.Constant(Constant.Unit _) as ux ] ->
                genDotIndexedSetWithApp (genExpr appNode.FunctionExpr) ux appNode
            | _ -> genDotIndexedSet

        | Expr.AppSingleParenArg appNode ->
            genDotIndexedSetWithApp (genExpr appNode.FunctionExpr) appNode.ArgExpr appNode

        | Expr.AppLongIdentAndSingleParenArg appNode ->
            genDotIndexedSetWithApp (genIdentListNode appNode.FunctionName) appNode.ArgExpr appNode

        | _ -> genDotIndexedSet
        |> genNode node
    | Expr.NamedIndexedPropertySet node ->
        match node.Index with
        | Expr.ArrayOrList arrayNode when arrayNode.Elements.Length = 1 ->
            genIdentListNode node.Identifier
            +> genSingleTextNode arrayNode.Opening
            +> col sepNone arrayNode.Elements genExpr
            +> genSingleTextNode arrayNode.Closing
            +> sepArrowRev
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Value)
        | _ ->
            let sep =
                match node.Index with
                | Expr.Constant _
                | Expr.Ident _ -> sepSpace
                | _ -> sepNone

            genIdentListNode node.Identifier
            +> sep
            +> genExpr node.Index
            +> sepArrowRev
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Value)
        |> genNode node
    | Expr.DotNamedIndexedPropertySet node ->
        genExpr node.Identifier
        +> sepDot
        +> genIdentListNode node.Name
        +> genExpr node.Property
        +> sepArrowRev
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Set)
        |> genNode node
    | Expr.Set node ->
        addParenIfAutoNln node.Identifier genExpr
        +> sepArrowRev
        +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Set
        |> genNode node
    | Expr.LibraryOnlyStaticOptimization node ->
        genExpr node.OptimizedExpr
        +> onlyIfNot node.Constraints.IsEmpty (!- " when ")
        +> col sepSpace node.Constraints (function
            | StaticOptimizationConstraint.WhenTyparTyconEqualsTycon n ->
                genSingleTextNode n.TypeParameter +> sepColon +> sepSpace +> genType n.Type
            | StaticOptimizationConstraint.WhenTyparIsStruct t -> genSingleTextNode t)
        +> sepEq
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr)
        |> genNode node
    | Expr.InterpolatedStringExpr node ->
        let genInterpolatedFillExpr expr =
            fun ctx ->
                let currentConfig = ctx.Config

                let interpolatedConfig =
                    { currentConfig with
                        // override the max line length for the interpolated expression.
                        // this is to avoid scenarios where the long / multiline format of the expression will be used
                        // where the construct is this short
                        // see unit test ``construct url with Fable``
                        MaxLineLength = ctx.WriterModel.Column + ctx.Config.MaxLineLength }

                genExpr expr { ctx with Config = interpolatedConfig }
                // Restore the existing configuration after printing the interpolated expression
                |> fun ctx -> { ctx with Config = currentConfig }
            |> atCurrentColumnIndent

        col sepNone node.Parts (fun part ->
            match part with
            | Choice1Of2 stringNode -> genSingleTextNode stringNode
            | Choice2Of2 fillNode ->
                fun ctx ->
                    let genFill =
                        genInterpolatedFillExpr fillNode.Expr
                        +> optSingle (fun format -> sepColonFixed +> genSingleTextNode format) fillNode.Ident

                    genFill ctx)
        |> genNode node
    | Expr.IndexRangeWildcard node -> genSingleTextNode node
    | Expr.TripleNumberIndexRange node ->
        let isNegativeFloat (text: string) =
            text.StartsWith("-", StringComparison.Ordinal) && text.Contains(".")

        let genDots (before: SingleTextNode) (dots: SingleTextNode) (after: SingleTextNode) =
            if not (isNegativeFloat before.Text) && not (isNegativeFloat after.Text) then
                genSingleTextNode dots
            else
                sepSpace +> genSingleTextNode dots +> sepSpace

        genSingleTextNode node.Start
        +> genDots node.Start node.StartDots node.Center
        +> genSingleTextNode node.Center
        +> genDots node.Center node.EndDots node.EndDots
        +> genSingleTextNode node.End
        |> genNode node
    | Expr.IndexRange node ->
        optSingle genExpr node.From
        +> genSingleTextNode node.Dots
        +> optSingle genExpr node.To
        |> genNode node
    | Expr.IndexFromEnd node -> !- "^" +> genExpr node.Expr |> genNode node
    | Expr.Typar node -> genSingleTextNode node
    | Expr.DotLambda node ->
        let genDotLambdaExpr expr =
            match expr with
            | Expr.AppSingleParenArg p -> genAppSingleParenArgExpr sepNone p // be always atomic, see 3050
            | Expr.AppLongIdentAndSingleParenArg p -> genAppLongIdentAndSingleParenArgExpr sepNone p // see 3120
            | _ -> genExpr expr

        genSingleTextNode node.Underscore
        +> genSingleTextNode node.Dot
        +> genDotLambdaExpr node.Expr
        |> genNode node
    | Expr.BeginEnd node ->
        let short =
            genSingleTextNode node.Begin
            +> sepSpace
            +> genExpr node.Expr
            +> sepSpace
            +> genSingleTextNode node.End

        let long =
            genSingleTextNode node.Begin
            +> indentSepNlnUnindent (genExpr node.Expr)
            +> sepNln
            +> genSingleTextNode node.End

        expressionFitsOnRestOfLine short long |> genNode node
    | Expr.ExplicitConstructorThenExpr node ->
        genSingleTextNode node.Then
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr)
        |> genNode node
    | Expr.SameInfixApps node ->
        genExpr node.LeadingExpr
        +> sepSpace
        +> colEx
            (fun (operator, expr) ctx ->
                ignore ((operator: SingleTextNode), expr)
                sepSpace ctx

            // if arithmeticOperators.Contains(operator.Text)
            //    && futureNlnCheck (genSingleTextNode operator +> genExpr expr) ctx then
            //     sepNln ctx
            // else
            //     sepSpace ctx
            )
            node.SubsequentExpressions
            (fun (operator, expr) ->
                genSingleTextNode operator
                +> expressionFitsOnRestOfLine (sepSpace +> genExpr expr) (sepNln +> genExpr expr))

    | Expr.InfixApp node ->
        let short =
            genExpr node.LeftHandSide
            +> sepSpace
            +> genSingleTextNode node.Operator
            +> sepSpace
            +> genExpr node.RightHandSide

        let long ctx =
            if
                arithmeticOperators.Contains(node.Operator.Text)
                && not (
                    match node.LeftHandSide with
                    | Expr.Match _ -> futureNlnCheck (genExpr node.LeftHandSide) ctx
                    | _ -> false
                )
            then
                (genExpr node.LeftHandSide
                 +> sepSpace
                 +> genSingleTextNode node.Operator
                 +> sepNln
                 +> genExpr node.RightHandSide)
                    ctx
            elif noBreakInfixOps.Contains(node.Operator.Text) then
                (genExpr node.LeftHandSide
                 +> sepSpace
                 +> genSingleTextNode node.Operator
                 +> indentSepNlnUnindent (genExpr node.RightHandSide))
                    ctx
            else
                (genExpr node.LeftHandSide
                 +> sepNln
                 +> indent
                 +> genSingleTextNode node.Operator
                 +> sepSpace
                 +> genExpr node.RightHandSide
                 +> unindent)
                    ctx

        expressionFitsOnRestOfLine short long

let genQuoteExpr (node: ExprQuoteNode) =
    genSingleTextNode node.OpenToken
    +> sepSpace
    +> expressionFitsOnRestOfLine (genExpr node.Expr) (indent +> sepNln +> genExpr node.Expr +> unindent +> sepNln)
    +> sepSpace
    +> genSingleTextNode node.CloseToken
    |> genNode node

/// <summary>
/// Prints the inside of an update record expression.
/// This function does not print the opening and closing braces.
/// </summary>
/// <param name="addAdditionalIndent">Should there be an additional indent after the `with` keyword.</param>
/// <param name="fieldsExpr">Record fields.</param>
/// <param name="copyExpr">Expression before the `with` keyword.</param>
let genMultilineRecordCopyExpr (addAdditionalIndent: bool) fieldsExpr copyExpr =
    atCurrentColumnIndent (genExpr copyExpr)
    +> !- " with"
    +> indent
    +> onlyIf addAdditionalIndent indent
    +> sepNln
    +> fieldsExpr
    +> onlyIf addAdditionalIndent unindent
    +> unindent

/// Special case for record fields in Cramped mode.
/// The caller should have already verified that the settings do indeed specify Cramped.
let genRecordFieldNameCramped (alreadyIndentedFurther: bool) (node: RecordFieldNode) =
    atCurrentColumn (
        enterNode node
        +> genIdentListNode node.FieldName
        +> sepSpace
        +> genSingleTextNode node.Equals
    )
    +> (fun ctx ->
        // See: https://github.com/fsprojects/fantomas/issues/2801
        let addAdditionIndent = not alreadyIndentedFurther && ctx.Config.IndentSize < 3

        let genBodyExpr e =
            if addAdditionIndent then
                sepSpaceOrDoubleIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)
            else
                sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)

        genBodyExpr node.Expr ctx)
    +> leaveNode node

let genRecordFieldNameAligned (node: RecordFieldNode) =
    atCurrentColumn (
        enterNode node
        +> genIdentListNode node.FieldName
        +> sepSpace
        +> genSingleTextNode node.Equals
    )
    +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Expr
    +> leaveNode node

let genMultilineRecordFieldsExpr
    (genRecordField: RecordFieldNode -> Context -> Context)
    (node: ExprRecordBaseNode)
    : Context -> Context =
    col sepNln node.Fields genRecordField

/// <summary>
/// Print a (anonymous) record with additional information as a single line.
/// </summary>
/// <param name="genExtra">Either the `expr with` or `inherit T`.</param>
/// <param name="node">ExprRecordBaseNode</param>
let genSmallRecordBaseExpr genExtra (node: ExprRecordBaseNode) =
    genSingleTextNode node.OpeningBrace
    +> addSpaceIfSpaceAroundDelimiter
    +> genExtra
    +> col sepSemi node.Fields (fun rf ->
        genIdentListNode rf.FieldName
        +> sepSpace
        +> genSingleTextNode rf.Equals
        +> sepSpace
        +> genExpr rf.Expr
        |> genNode rf) //genRecordFieldNameAligned
    +> addSpaceIfSpaceAroundDelimiter
    +> genSingleTextNode node.ClosingBrace

let genSmallRecordNode (node: ExprRecordNode) =
    genSmallRecordBaseExpr
        (match node.CopyInfo with
         | Some we -> genExpr we +> !- " with "
         | None -> sepNone)
        node

/// <summary>
/// Print a multiline ExprRecordNode.
/// <para>
/// Depending on the actual record, either regular or anonymous,
/// a different strategy needs to be provided to deal with the record fields in the Cramped style.
/// This is too avoid offset errors when using a smaller `indent_size`.
/// </para>
/// </summary>
/// <param name="node">The ExprRecordNode</param>
/// <param name="ctx">Context</param>
let genMultilineRecord (node: ExprRecordNode) (ctx: Context) =
    let expressionStartColumn = ctx.Column
    let openBraceLength = node.OpeningBrace.Text.Length

    let targetColumn =
        expressionStartColumn
        + (if ctx.Config.SpaceAroundDelimiter then
               openBraceLength + 1
           else
               openBraceLength)

    let genMultilineAlignBrackets =
        let fieldsExpr = genMultilineRecordFieldsExpr genRecordFieldNameAligned node

        match node.CopyInfo with
        | Some ci ->
            let additionalIndent = ctx.Config.IndentSize < 3

            genSingleTextNodeSuffixDelimiter node.OpeningBrace
            +> ifElseCtx
                (fun ctx -> ctx.Config.IsStroustrupStyle)
                (indent +> sepNln)
                sepNlnWhenWriteBeforeNewlineNotEmpty // comment after curly brace
            +> genMultilineRecordCopyExpr additionalIndent fieldsExpr ci
            +> onlyIfCtx (fun ctx -> ctx.Config.IsStroustrupStyle) unindent
            +> sepNln
            +> genSingleTextNode node.ClosingBrace
        | None ->
            genSingleTextNode node.OpeningBrace
            +> indentSepNlnUnindent fieldsExpr
            +> ifElseCtx lastWriteEventIsNewline sepNone sepNln
            +> genSingleTextNode node.ClosingBrace

    let genMultilineCramped =
        let genFields =
            match node.CopyInfo with
            | Some we ->
                let additionalIndent =
                    // Anonymous record
                    (openBraceLength = 2 && ctx.Config.IndentSize <= 3)
                    // Regular record
                    || ctx.Config.IndentSize < 3

                genMultilineRecordCopyExpr
                    additionalIndent
                    (genMultilineRecordFieldsExpr (genRecordFieldNameCramped additionalIndent) node)
                    we
            | None ->
                fun (ctx: Context) ->
                    col
                        sepNln
                        node.Fields
                        (fun e ->
                            // Add spaces to ensure the record field (incl trivia) starts at the right column.
                            addFixedSpaces targetColumn
                            // Potential indentations will be in relation to the opening curly brace.
                            +> genRecordFieldNameCramped false e)
                        ctx

        // Edge case scenario to make sure that the closing brace is not before the opening one
        // See unit test "multiline string before closing brace"
        let genClosingBrace ctx =
            let genClosingBrace =
                addFixedSpaces expressionStartColumn +> genSingleTextNode node.ClosingBrace

            ifElseCtx lastWriteEventIsNewline genClosingBrace (addSpaceIfSpaceAroundDelimiter +> genClosingBrace) ctx

        match node.CopyInfo with
        | Some _ ->
            genSingleTextNode node.OpeningBrace
            +> sepNlnWhenWriteBeforeNewlineNotEmptyOr addSpaceIfSpaceAroundDelimiter // comment after curly brace
            +> genFields
            +> genClosingBrace
        | None ->
            atCurrentColumn (
                genSingleTextNodeSuffixDelimiter node.OpeningBrace
                +> sepNlnWhenWriteBeforeNewlineNotEmpty // comment after curly brace
                +> genFields
                +> sepNlnWhenWriteBeforeNewlineNotEmpty
                +> genClosingBrace
            )

    ifAlignOrStroustrupBrackets genMultilineAlignBrackets genMultilineCramped ctx

let genRecord smallRecordExpr multilineRecordExpr (node: ExprRecordBaseNode) ctx =
    let size = getRecordSize ctx node.Fields
    genNode node (isSmallExpression size smallRecordExpr multilineRecordExpr) ctx

let genArrayOrList (preferMultilineCramped: bool) (node: ExprArrayOrListNode) =
    if node.Elements.IsEmpty then
        genSingleTextNode node.Opening +> genSingleTextNode node.Closing |> genNode node
    else
        let smallExpression =
            genSingleTextNode node.Opening
            +> addSpaceIfSpaceAroundDelimiter
            +> col sepSemi node.Elements genExpr
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.Closing

        let multilineExpression =
            let genMultiLineArrayOrListAlignBrackets =
                genSingleTextNode node.Opening
                +> indent
                +> sepNlnUnlessLastEventIsNewline
                +> col sepNln node.Elements genExpr
                +> unindent
                +> sepNlnUnlessLastEventIsNewline
                +> genSingleTextNode node.Closing

            let genMultiLineArrayOrListCramped =
                genSingleTextNodeSuffixDelimiter node.Opening
                +> atCurrentColumnIndent (
                    sepNlnWhenWriteBeforeNewlineNotEmpty
                    +> col sepNln node.Elements genExpr
                    +> (enterNode node.Closing
                        +> (fun ctx ->
                            let isFixed = lastWriteEventIsNewline ctx
                            (onlyIfNot isFixed sepSpace +> !-node.Closing.Text +> leaveNode node.Closing) ctx))
                )

            ifElse preferMultilineCramped genMultiLineArrayOrListCramped genMultiLineArrayOrListAlignBrackets

        fun ctx ->
            let alwaysMultiline =
                let isIfThenElse =
                    function
                    | Expr.IfThen _
                    | Expr.IfThenElse _ -> true
                    | _ -> false

                List.exists isIfThenElse node.Elements
                || List.forall isLambdaOrIfThenElse node.Elements

            if alwaysMultiline then
                multilineExpression ctx
            else
                let size = getListOrArrayExprSize ctx ctx.Config.MaxArrayOrListWidth node.Elements
                isSmallExpression size smallExpression multilineExpression ctx
        |> genNode node

let genMultilineFunctionApplicationArguments (argExpr: Expr) =
    let argsInsideParenthesis (parenNode: ExprParenNode) f =
        genSingleTextNode parenNode.OpeningParen
        +> indentSepNlnUnindent f
        +> sepNln
        +> genSingleTextNode parenNode.ClosingParen
        |> genNode parenNode

    let genExpr e =
        match e with
        // | Expr.InfixApp infixNode when (infixNode.Operator.Text = "=") -> genNamedArgumentExpr infixNode
        | _ -> genExpr e

    match argExpr with
    | Expr.Paren parenNode ->
        match parenNode.Expr with
        | Expr.Tuple tupleNode ->
            genTupleMultiline tupleNode
            |> genNode tupleNode
            |> argsInsideParenthesis parenNode
        | _ -> genExpr parenNode.Expr |> argsInsideParenthesis parenNode
    | _ -> genExpr argExpr

let genTupleExpr (node: ExprTupleNode) =
    // if a tuple element is an InfixApp with a lambda or if-then-else expression on the rhs,
    // we need to wrap the rhs in parenthesis to avoid a parse error caused by the higher precedence of "," over the rhs expression.
    // see 2819
    let wrapInfixAppRhsInParenIfNeeded expr =
        match expr with
        // | Expr.InfixApp exprInfixAppNode ->
        //     match exprInfixAppNode.RightHandSide with
        //     | IsLambdaOrIfThenElse e ->
        //         let parenNode =
        //             mkExprParenNode
        //                 (SingleTextNode("(", Fantomas.FCS.Text.Range.Zero))
        //                 e
        //                 (SingleTextNode(")", Fantomas.FCS.Text.Range.Zero))
        //                 Fantomas.FCS.Text.Range.Zero
        //
        //         ExprInfixAppNode(
        //             exprInfixAppNode.LeftHandSide,
        //             exprInfixAppNode.Operator,
        //             parenNode,
        //             Fantomas.FCS.Text.range.Zero
        //         )
        //         |> Expr.InfixApp
        //     | _ -> expr
        | _ -> expr

    let shortExpression =
        let lastIndex = Array.length node.Children - 1

        coli sepNone node.Items (fun i c ->
            match c with
            | Choice1Of2 e ->
                match e with
                | IsLambdaOrIfThenElse e when i <> lastIndex -> sepOpenT +> genExpr e +> sepCloseT
                | e -> genExpr (wrapInfixAppRhsInParenIfNeeded e)
            | Choice2Of2 comma -> genSingleTextNode comma +> addSpaceIfSpaceAfterComma)

    let longExpression = genTupleMultiline node

    atCurrentColumn (expressionFitsOnRestOfLine shortExpression longExpression)
    |> genNode node

let genTupleMultiline (node: ExprTupleNode) =
    let containsLambdaOrMatchExpr =
        // If the any items (expect the last) is a match/lambda
        node.Items
        |> List.chunkBySize 2
        |> List.exists (fun pair ->
            match pair with
            | [ Choice1Of2 e; Choice2Of2 _ ] ->
                match e with
                | Expr.Match _
                | Expr.Lambda _ -> true
                // | Expr.InfixApp node ->
                //     match node.RightHandSide with
                //     | Expr.Match _
                //     | Expr.Lambda _ -> true
                //     | _ -> false
                // | Expr.SameInfixApps node ->
                //     match List.last node.SubsequentExpressions with
                //     | _, Expr.Lambda _ -> true
                //     | _ -> false
                | _ -> false
            | _ -> false)

    let lastIndex = List.length node.Items - 1

    let genItem idx =
        function
        | Choice1Of2 e ->
            match e with
            | IsIfThenElse _ when (idx < lastIndex) -> autoParenthesisIfExpressionExceedsPageWidth (genExpr e)
            // | Expr.InfixApp node when (node.Operator.Text = "=") -> genNamedArgumentExpr node
            | _ -> genExpr e
        | Choice2Of2 comma ->
            if containsLambdaOrMatchExpr then
                sepNln +> genSingleTextNode comma +> sepSpace
            else
                genSingleTextNode comma +> sepNln

    coli sepNone node.Items genItem

// let genNamedArgumentExpr (node: ExprInfixAppNode) =
//     let short =
//         genExpr node.LeftHandSide
//         +> sepSpace
//         +> genSingleTextNode node.Operator
//         +> sepSpace
//         +> genExpr node.RightHandSide
//
//     let long =
//         genExpr node.LeftHandSide
//         +> sepSpace
//         +> genSingleTextNode node.Operator
//         +> indentSepNlnUnindentUnlessStroustrup (fun e -> sepSpace +> genExpr e) node.RightHandSide
//
//     expressionFitsOnRestOfLine short long |> genNode node

let genLambdaAux (includeClosingParen: bool) (node: ExprLambdaNode) =
    let genPats =
        let shortPats = sepSpace +> col sepSpace node.Parameters genPat

        let longPats (ctx: Context) =
            // If the current column already is larger than the next indent,
            // we need to write the parameters fixed on the current column.
            if ctx.Column > ctx.WriterModel.Indent + ctx.Config.IndentSize then
                (sepSpace +> atCurrentColumn (sepNln +> col sepNln node.Parameters genPat)) ctx
            else
                indentSepNlnUnindent (col sepNln node.Parameters genPat) ctx

        expressionFitsOnRestOfLine shortPats longPats

    genSingleTextNode node.Fun
    +> genPats
    +> sepSpace
    +> genSingleTextNode node.Arrow
    +> (fun ctx ->
        let maxLineLength = ctx.Config.MaxLineLength

        let ctx =
            // In this check we want to write the lambda body in one line.
            // Depending on `includeClosingParen` we want to check if the closing parenthesis also still fits on the remainder of the current line.
            // ...fun a -> expr)
            // This is to prevent the edge case where the lambda fits on one line but the closing parenthesis would go over the max_line_length.
            if includeClosingParen then
                { ctx with
                    Config =
                        { ctx.Config with
                            MaxLineLength = maxLineLength - 1 } }
            else
                ctx

        if hasWriteBeforeNewlineContent ctx then
            indentSepNlnUnindent (genExpr node.Expr) ctx
        else
            sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Expr ctx
        |> fun ctx ->
            // Afterwards we do need to reset the max_line_length to the original value.
            { ctx with
                Config =
                    { ctx.Config with
                        MaxLineLength = maxLineLength } })
    |> genNode node

let genLambda = genLambdaAux false
let genLambdaWithParen = genLambdaAux true

let genAppLongIdentAndSingleParenArgExpr (addSpace: Context -> Context) (node: ExprAppLongIdentAndSingleParenArgNode) =
    let shortLids = genIdentListNode node.FunctionName
    let short = shortLids +> addSpace +> genExpr node.ArgExpr

    let long =
        let args =
            addSpace
            +> expressionFitsOnRestOfLine (genExpr node.ArgExpr) (genMultilineFunctionApplicationArguments node.ArgExpr)

        ifElseCtx
            (futureNlnCheck shortLids)
            (genFunctionNameWithMultilineLids args node.FunctionName)
            (shortLids +> args)

    expressionFitsOnRestOfLine short long |> genNode node

let genAppSingleParenArgExpr (addSpace: Context -> Context) (node: ExprAppSingleParenArgNode) =
    let short = genExpr node.FunctionExpr +> addSpace +> genExpr node.ArgExpr

    let long ctx =
        let genDefaultLong =
            genExpr node.FunctionExpr
            +> addSpace
            +> genMultilineFunctionApplicationArguments node.ArgExpr

        match node.ArgExpr with
        | Expr.Paren parenNode when parenNode.HasContentBefore ->
            // We make a copy of the parenthesis argument (without the trivia being copied).
            // Then we check if that is multiline or not.
            let parenNode' =
                mkExprParenNode parenNode.OpeningParen parenNode.Expr parenNode.ClosingParen parenNode.Range

            let isSingleLineWithoutTriviaBefore = futureNlnCheck (genExpr parenNode') ctx

            if not isSingleLineWithoutTriviaBefore then
                (genExpr node.FunctionExpr +> indentSepNlnUnindent (genExpr node.ArgExpr)) ctx
            else
                (genExpr node.FunctionExpr
                 +> indentSepNlnUnindent (genMultilineFunctionApplicationArguments node.ArgExpr))
                    ctx
        | _ -> genDefaultLong ctx

    expressionFitsOnRestOfLine short long |> genNode node

/// When called from `SynExpr.App` we need to ensure the node.GreaterThan is placed one space further than the start column.
/// This is to ensure the application remains an application.
let genTypeApp (addAdditionalColumnOffset: bool) (node: ExprTypeAppNode) (ctx: Context) : Context =
    let startColumn = ctx.Column + (if addAdditionalColumnOffset then 1 else 0)

    genNode
        node
        (genExpr node.Identifier
         +> genSingleTextNode node.LessThan
         +> colGenericTypeParameters node.TypeParameters
         // we need to make sure each expression in the function application has offset at least greater than
         // See: https://github.com/fsprojects/fantomas/issues/1611
         +> addFixedSpaces startColumn
         +> genSingleTextNode node.GreaterThan)
        ctx

let genClauses (clauses: MatchClauseNode list) =
    let lastIndex = clauses.Length - 1

    coli sepNln clauses (fun idx clause ->
        let isLastItem = lastIndex = idx
        genClause isLastItem clause)

let genClause (isLastItem: bool) (node: MatchClauseNode) =
    let genBar =
        match node.Bar with
        | Some barNode -> genSingleTextNodeWithSpaceSuffix sepSpace barNode
        | None -> sepBar

    let genWhenAndBody =
        sepSpace
        +> leadingExpressionIsMultiline
            (optSingle
                (fun e ->
                    !- "when"
                    +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr e)
                    +> sepSpace)
                node.WhenExpr)
            (fun isMultiline ctx ->
                if isMultiline then
                    indentSepNlnUnindent (genSingleTextNode node.Arrow +> sepNln +> genExpr node.BodyExpr) ctx
                else
                    let genKeepIndentInBranch =
                        let long =
                            let startNode =
                                match node.Bar with
                                | None -> Pattern.Node node.Pattern
                                | Some bar -> bar

                            genKeepIdentMatchClause startNode node.BodyExpr

                        expressionFitsOnRestOfLine (sepSpace +> genExpr node.BodyExpr) long

                    let sepArrow =
                        if not node.Arrow.HasContentBefore then
                            genSingleTextNodeWithSpaceSuffix sepSpace node.Arrow
                        else
                            // In the weird case where the arrow has content before it and there is no multiline whenExpr,
                            // we need to ensure a space was written before the arrow to avoid offset errors.
                            // See https://github.com/fsprojects/fantomas/issues/2888
                            !- $" %s{node.Arrow.Text} " |> genNode node.Arrow

                    (sepArrow
                     +> ifElse
                         (ctx.Config.ExperimentalKeepIndentInBranch && isLastItem)
                         genKeepIndentInBranch
                         (autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.BodyExpr))
                        ctx)

    let genPatAndBody ctx =
        if isStroustrupStyleExpr ctx.Config node.BodyExpr then
            let startColumn = ctx.Column
            (genPatInClause node.Pattern +> atIndentLevel false startColumn genWhenAndBody) ctx
        else
            (genPatInClause node.Pattern +> genWhenAndBody) ctx

    genBar +> genPatAndBody |> genNode node

let genControlExpressionStartCore
    (startKeyword: Choice<SingleTextNode, IfKeywordNode>)
    (innerExpr: Expr)
    (endKeyword: SingleTextNode)
    =
    let enterStart =
        match startKeyword with
        | Choice1Of2 n -> enterNode n
        | Choice2Of2 n -> enterNode n.Node

    let genStart =
        match startKeyword with
        | Choice1Of2 node -> !-node.Text
        | Choice2Of2 ifKw ->
            match ifKw with
            | IfKeywordNode.SingleWord node -> !-node.Text
            | IfKeywordNode.ElseIf _ -> !- "else if"

    let leaveStart =
        match startKeyword with
        | Choice1Of2 n -> leaveNode n
        | Choice2Of2 n -> leaveNode n.Node

    let shortIfExpr =
        genStart
        +> leaveStart
        +> sepNlnWhenWriteBeforeNewlineNotEmptyOr sepSpace
        +> genExpr innerExpr
        +> sepSpace
        +> enterNode endKeyword
        +> !-endKeyword.Text

    let longIfExpr =
        genStart
        +> leaveStart
        +> indentSepNlnUnindent (genExpr innerExpr)
        +> sepNln
        +> enterNode endKeyword
        +> !-endKeyword.Text

    // A code comment before the start keyword should not make the expression long.
    enterStart
    +> expressionFitsOnRestOfLine shortIfExpr longIfExpr
    +> leaveNode endKeyword

// Caller of this function is responsible for genNode!
// let genMultilineInfixExpr (node: ExprInfixAppNode) =
//     let genLhs (ctx: Context) =
//         match node.LeftHandSide with
//         | IsIfThenElse _ when (ctx.Config.IndentSize - 1 <= node.Operator.Text.Length) ->
//             autoParenthesisIfExpressionExceedsPageWidth (genExpr node.LeftHandSide) ctx
//         | Expr.Match _ when (ctx.Config.IndentSize - 1 <= node.Operator.Text.Length) ->
//             let ctxAfterMatch = genExpr node.LeftHandSide ctx
//
//             let lastClauseIsSingleLine =
//                 Queue.rev ctxAfterMatch.WriterEvents
//                 |> Seq.skipWhile (fun e ->
//                     match e with
//                     | RestoreIndent _
//                     | RestoreAtColumn _ -> true
//                     | _ -> false)
//                 // In case the last clause was multiline an UnIndent event should follow
//                 |> Seq.tryHead
//                 |> fun e ->
//                     match e with
//                     | Some(UnIndentBy _) -> false
//                     | _ -> true
//
//             if lastClauseIsSingleLine then
//                 ctxAfterMatch
//             else
//                 autoParenthesisIfExpressionExceedsPageWidth (genExpr node.LeftHandSide) ctx
//         | lhsExpr -> genExpr lhsExpr ctx
//
//     atCurrentColumn (
//         genLhs
//         +> sepNln
//         +> genSingleTextNode node.Operator
//         +> sepNlnWhenWriteBeforeNewlineNotEmpty
//         +> sepSpace
//         +> genExprInMultilineInfixExpr node.RightHandSide
//     )

// let genExprInMultilineInfixExpr (e: Expr) =
//     match e with
//     | Expr.CompExprBody node ->
//         let areLetOrUseStatementsEndingWithOtherStatement =
//             node.Statements
//             |> List.mapWithLast
//                 (function
//                 | ComputationExpressionStatement.LetOrUseStatement _ -> true
//                 | _ -> false)
//                 (function
//                  | ComputationExpressionStatement.OtherStatement _ -> true
//                  | _ -> false)
//             |> List.reduce (&&)
//
//         if not areLetOrUseStatementsEndingWithOtherStatement then
//             genExpr e
//         else
//             colWithNlnWhenMappedNodeIsMultiline
//                 true
//                 ComputationExpressionStatement.Node
//                 (fun ces ->
//                     match ces with
//                     | ComputationExpressionStatement.LetOrUseStatement letOrUseNode ->
//                         let genIn =
//                             match letOrUseNode.In with
//                             | None -> !- "in"
//                             | Some inNode -> genSingleTextNode inNode
//
//                         genBinding letOrUseNode.Binding +> sepSpace +> genIn +> sepSpace
//                         |> genNode letOrUseNode
//                     | ComputationExpressionStatement.OtherStatement otherNode -> genExpr otherNode
//                     | _ -> failwith "unexpected ComputationExpressionStatement")
//                 node.Statements
//             |> atCurrentColumn
//             |> genNode node
//     | Expr.Paren parenNode ->
//         match parenNode.Expr with
//         | Expr.Match _ as mex ->
//             fun ctx ->
//                 if ctx.Config.MultiLineLambdaClosingNewline then
//                     genNode
//                         parenNode
//                         (genSingleTextNode parenNode.OpeningParen
//                          +> indentSepNlnUnindent (genExpr mex)
//                          +> sepNln
//                          +> genSingleTextNode parenNode.ClosingParen)
//                         ctx
//                 else
//                     genNode
//                         parenNode
//                         (genSingleTextNode parenNode.OpeningParen
//                          +> atCurrentColumnIndent (genExpr mex)
//                          +> genSingleTextNode parenNode.ClosingParen)
//                         ctx
//         // | Expr.InfixApp infixNode ->
//         //     match infixNode.LeftHandSide with
//         //     | Expr.Chain _ -> atCurrentColumnIndent (genExpr e)
//         //     | _ -> genExpr e
//         | Expr.Chain _
//         | Expr.Record _ -> atCurrentColumnIndent (genExpr e)
//         | _ -> genExpr e
//     | Expr.MatchLambda matchLambdaNode ->
//         genSingleTextNode matchLambdaNode.Function
//         +> indentSepNlnUnindent (genClauses matchLambdaNode.Clauses)
//         |> genNode matchLambdaNode
//     | Expr.Record _ -> atCurrentColumnIndent (genExpr e)
//     | _ -> genExpr e

let genKeepIdentIfThenElse (ifKeyword: Node) (elseKeyword: Node) (e: Expr) ctx =
    let exprNode = Expr.Node e

    if
        ctx.Config.ExperimentalKeepIndentInBranch
        && (elseKeyword.Range.StartColumn = exprNode.Range.StartColumn
            || ifKeyword.Range.StartColumn = exprNode.Range.StartColumn)
    then
        (sepNln +> sepNlnUnlessContentBefore exprNode +> genExpr e) ctx
    else
        indentSepNlnUnindent (genExpr e) ctx

let genKeepIdentMatchClause (startNode: Node) (e: Expr) ctx =
    let exprNode = Expr.Node e

    if
        ctx.Config.ExperimentalKeepIndentInBranch
        && startNode.Range.StartColumn = exprNode.Range.StartColumn
    then
        (sepNln +> sepNlnUnlessContentBefore exprNode +> genExpr e) ctx
    else
        indentSepNlnUnindent (genExpr e) ctx

let colGenericTypeParameters typeParameters =
    let genParameters sep =
        coli sep typeParameters (fun idx t ->
            let leadingSpace =
                match t with
                | Type.Var n when idx = 0 && String.startsWithOrdinal "^" n.Text -> sepSpace
                | _ -> sepNone

            leadingSpace +> genType t)

    let long = indentSepNlnUnindent (genParameters (sepComma +> sepNln)) +> sepNln
    let short = genParameters sepComma

    // Multiline text type params should be unmodified
    match typeParameters with
    | [ Type.StaticConstant(Constant.FromText textNode) ] when textNode.Text.Contains("\n") -> short
    | _ -> expressionFitsOnRestOfLine short long

let genFunctionNameWithMultilineLids (trailing: Context -> Context) (longIdent: IdentListNode) =
    match longIdent.Content with
    | IdentifierOrDot.Ident identNode :: t ->
        genSingleTextNode identNode
        +> indentSepNlnUnindent (
            colEx
                (function
                | IdentifierOrDot.Ident _ -> sepNone
                | IdentifierOrDot.KnownDot _
                | IdentifierOrDot.UnknownDot -> sepNln)
                t
                (function
                 | IdentifierOrDot.Ident identNode -> genSingleTextNode identNode
                 | IdentifierOrDot.KnownDot dot -> genSingleTextNode dot
                 | IdentifierOrDot.UnknownDot -> sepDot)
            +> trailing
        )
    | _ -> sepNone

let (|EndsWithDualListApp|_|) (config: FormatConfig) (appNode: ExprAppNode) =
    if not (config.ExperimentalElmish || config.IsStroustrupStyle) then
        None
    else
        let mutable otherArgs = ListCollector<Expr>()

        let rec visit (args: Expr list) =
            match args with
            | [] -> None
            | [ Expr.ArrayOrList firstList; Expr.ArrayOrList lastList ] -> Some(otherArgs.Close(), firstList, lastList)
            | arg :: args ->
                otherArgs.Add(arg)
                visit args

        visit appNode.Arguments

let (|EndsWithSingleListApp|_|) (config: FormatConfig) (appNode: ExprAppNode) =
    if not (config.ExperimentalElmish || config.IsStroustrupStyle) then
        None
    else
        let mutable otherArgs = ListCollector<Expr>()

        let rec visit (args: Expr list) =
            match args with
            | [] -> None
            | [ Expr.ArrayOrList singleList ] -> Some(otherArgs.Close(), singleList)
            | arg :: args ->
                otherArgs.Add(arg)
                visit args

        visit appNode.Arguments

let (|EndsWithSingleRecordApp|_|) (config: FormatConfig) (appNode: ExprAppNode) =
    if not config.IsStroustrupStyle then
        None
    else
        let mutable otherArgs = ListCollector<Expr>()

        let rec visit (args: Expr list) =
            match args with
            | [] -> None
            | [ Expr.Record _ | Expr.AnonStructRecord _ as singleRecord ] -> Some(otherArgs.Close(), singleRecord)
            | arg :: args ->
                otherArgs.Add(arg)
                visit args

        visit appNode.Arguments

let genAppWithLambda sep (node: ExprAppWithLambdaNode) =
    let short =
        genExpr node.FunctionName
        +> sep
        +> col sepSpace node.Arguments genExpr
        +> onlyIf (List.isNotEmpty node.Arguments) sepSpace
        +> (genSingleTextNode node.OpeningParen
            +> (match node.Lambda with
                | Choice1Of2 lambdaNode -> genLambdaWithParen lambdaNode |> genNode lambdaNode
                | Choice2Of2 matchLambdaNode ->
                    genSingleTextNode matchLambdaNode.Function
                    +> indentSepNlnUnindent (genClauses matchLambdaNode.Clauses)
                    |> genNode matchLambdaNode)
            +> sepNlnWhenWriteBeforeNewlineNotEmpty
            +> genSingleTextNode node.ClosingParen)

    let long (ctx: Context) : Context =
        if ctx.Config.MultiLineLambdaClosingNewline then
            let genArguments =
                match node.Arguments with
                | [] ->
                    match node.Lambda with
                    | Choice1Of2 lambdaNode ->
                        genSingleTextNode node.OpeningParen
                        +> (genLambdaWithParen lambdaNode |> genNode lambdaNode)
                        +> onlyIf (not (isStroustrupStyleExpr ctx.Config lambdaNode.Expr)) sepNln
                        +> genSingleTextNode node.ClosingParen
                    | Choice2Of2 matchLambdaNode ->
                        genSingleTextNode node.OpeningParen
                        +> indentSepNlnUnindent (
                            genSingleTextNode matchLambdaNode.Function
                            +> sepNln
                            +> genClauses matchLambdaNode.Clauses
                            |> genNode matchLambdaNode
                        )
                        +> sepNln
                        +> genSingleTextNode node.ClosingParen
                | es ->
                    col sepNln es genExpr
                    +> sepNln
                    +> (match node.Lambda with
                        | Choice1Of2 lambdaNode ->
                            leadingExpressionIsMultiline
                                (genSingleTextNode node.OpeningParen
                                 +> (genLambdaWithParen lambdaNode |> genNode lambdaNode))
                                (fun isMultiline ->
                                    onlyIf
                                        (isMultiline && not (isStroustrupStyleExpr ctx.Config lambdaNode.Expr))
                                        sepNln
                                    +> genSingleTextNode node.ClosingParen)
                        | Choice2Of2 matchLambdaNode ->
                            (genSingleTextNode node.OpeningParen
                             +> (genSingleTextNode matchLambdaNode.Function
                                 +> sepNln
                                 +> genClauses matchLambdaNode.Clauses
                                 |> genNode matchLambdaNode)
                             +> sepNln
                             +> genSingleTextNode node.ClosingParen))
                    +> unindent

            (genExpr node.FunctionName
             +> ifElse (List.isEmpty node.Arguments) sep (indent +> sepNln)
             +> genArguments)
                ctx
        else
            match node.Lambda with
            | Choice1Of2 lambdaNode ->
                let singleLineTestExpr =
                    genExpr node.FunctionName
                    +> sep
                    +> col sepSpace node.Arguments genExpr
                    +> sep
                    +> genSingleTextNode node.OpeningParen
                    +> enterNode lambdaNode
                    +> genSingleTextNode lambdaNode.Fun
                    +> sepSpace
                    +> col sepSpace lambdaNode.Parameters genPat
                    +> sepSpace
                    +> genSingleTextNode lambdaNode.Arrow

                let singleLine (ctx: Context) =
                    let startColumn = ctx.WriterModel.Indent

                    (genExpr node.FunctionName
                     +> sep
                     +> col sepSpace node.Arguments genExpr
                     +> sep
                     +> genSingleTextNode node.OpeningParen
                     +> (genLambdaWithParen lambdaNode |> genNode lambdaNode)
                     +> sepNlnWhenWriteBeforeNewlineNotEmpty
                     +> addFixedSpaces startColumn
                     +> genSingleTextNode node.ClosingParen)
                        ctx

                let multiLine =
                    genExpr node.FunctionName
                    +> indentSepNlnUnindent (fun ctx ->
                        let startColumn = ctx.WriterModel.Indent

                        (col sepNln node.Arguments genExpr
                         +> onlyIfNot (List.isEmpty node.Arguments) sepNln
                         +> genSingleTextNode node.OpeningParen
                         +> (genLambdaWithParen lambdaNode |> genNode lambdaNode)
                         +> addFixedSpaces startColumn
                         +> genSingleTextNode node.ClosingParen)
                            ctx)

                if futureNlnCheck singleLineTestExpr ctx then
                    multiLine ctx
                else
                    singleLine ctx

            | Choice2Of2 matchLambdaNode ->
                let singleLineTestExpr =
                    genExpr node.FunctionName
                    +> sep
                    +> col sepSpace node.Arguments genExpr
                    +> genSingleTextNode node.OpeningParen
                    +> enterNode matchLambdaNode
                    +> genSingleTextNode matchLambdaNode.Function

                let singleLine =
                    genExpr node.FunctionName
                    +> sep
                    +> col sepSpace node.Arguments genExpr
                    +> sepSpace
                    +> genSingleTextNode node.OpeningParen
                    +> (genSingleTextNode matchLambdaNode.Function
                        +> indentSepNlnUnindent (genClauses matchLambdaNode.Clauses)
                        |> genNode matchLambdaNode)
                    +> sepNlnWhenWriteBeforeNewlineNotEmpty
                    +> genSingleTextNode node.ClosingParen

                let multiLine =
                    genExpr node.FunctionName
                    +> indentSepNlnUnindent (
                        col sepNln node.Arguments genExpr
                        +> sepNln
                        +> genSingleTextNode node.OpeningParen
                        +> atCurrentColumn (
                            genSingleTextNode matchLambdaNode.Function
                            +> sepNln
                            +> genClauses matchLambdaNode.Clauses
                            |> genNode matchLambdaNode
                        )
                        +> genSingleTextNode node.ClosingParen
                    )

                if futureNlnCheck singleLineTestExpr ctx then
                    multiLine ctx
                else
                    singleLine ctx

    expressionFitsOnRestOfLine short long |> genNode node

let sepSpaceBeforeParenInFuncInvocation (functionExpr: Expr) (argExpr: Expr) ctx =
    match functionExpr, argExpr with
    | Expr.DotLambda _, _ -> ctx
    | Expr.Constant _, _ -> sepSpace ctx
    | ParenExpr _, _ -> sepSpace ctx
    | UppercaseExpr, ParenExpr _ -> onlyIf ctx.Config.SpaceBeforeUppercaseInvocation sepSpace ctx
    | LowercaseExpr, ParenExpr _ -> onlyIf ctx.Config.SpaceBeforeLowercaseInvocation sepSpace ctx
    | Expr.Ident _, Expr.Ident _ -> sepSpace ctx
    | _ -> sepSpace ctx

// end expressions

let genPatLeftMiddleRight (node: PatLeftMiddleRight) =
    genPat node.LeftHandSide
    +> sepSpace
    +> (match node.Middle with
        | Choice1Of2 node -> genSingleTextNode node
        | Choice2Of2 text -> !-text)
    +> sepSpace
    +> genPat node.RightHandSide
    |> genNode node

let genTyparDecl (isFirstTypeParam: bool) (td: TyparDeclNode) =
    genOnelinerAttributes td.Attributes
    +> onlyIf (isFirstTypeParam && String.startsWithOrdinal "^" td.TypeParameter.Text) sepSpace
    +> genSingleTextNode td.TypeParameter
    +> colPre sepSpace sepSpace td.IntersectionConstraints (function
        | Choice1Of2 t -> genType t
        | Choice2Of2 amp -> genSingleTextNode amp)
    |> genNode td

let genTyparDecls (td: TyparDecls) =
    match td with
    | TyparDecls.PostfixList node ->
        genSingleTextNode node.LessThan
        +> coli sepComma node.Decls (fun i -> genTyparDecl (i = 0))
        +> onlyIf (List.isNotEmpty node.Constraints) (sepSpace +> genTypeConstraints node.Constraints)
        +> genSingleTextNode node.GreaterThan
        |> genNode node
    | TyparDecls.PrefixList node ->
        genSingleTextNode node.OpeningParen
        +> coli sepComma node.Decls (fun i -> genTyparDecl (i = 0))
        +> genSingleTextNode node.ClosingParen
        |> genNode node
    | TyparDecls.SinglePrefix node -> genTyparDecl true node

let genTuplePatLong (node: PatTupleNode) =
    let padUntilAtCurrentColumn ctx =
        addFixedSpaces ctx.WriterModel.AtColumn ctx

    col padUntilAtCurrentColumn node.Items (function
        | Choice1Of2 p -> genPat p
        | Choice2Of2 comma -> genSingleTextNode comma +> sepNln)

let genTuplePat (node: PatTupleNode) =
    let short =
        col sepNone node.Items (function
            | Choice1Of2 p -> genPat p
            | Choice2Of2 comma -> genSingleTextNode comma +> addSpaceIfSpaceAfterComma)

    atCurrentColumn (expressionFitsOnRestOfLine short (atCurrentColumn (genTuplePatLong node)))
    |> genNode node

let genPat (p: Pattern) =
    match p with
    | Pattern.OptionalVal n -> genSingleTextNode n
    | Pattern.Or node -> genPatLeftMiddleRight node
    | Pattern.Ands node -> col (!- " & ") node.Patterns genPat |> genNode node
    | Pattern.Null node
    | Pattern.Wild node -> genSingleTextNode node
    | Pattern.Parameter node ->
        genOnelinerAttributes node.Attributes
        +> genPat node.Pattern
        +> optSingle
            (fun t ->
                sepColon
                +> autoIndentAndNlnIfExpressionExceedsPageWidth (atCurrentColumnIndent (genType t)))
            node.Type
        |> genNode node
    | Pattern.NamedParenStarIdent node ->
        genAccessOpt node.Accessibility
        +> genSingleTextNode node.OpeningParen
        +> sepSpace
        +> genSingleTextNode node.Name
        +> sepSpace
        +> genSingleTextNode node.ClosingParen
        |> genNode node
    | Pattern.Named node -> genAccessOpt node.Accessibility +> genSingleTextNode node.Name |> genNode node
    | Pattern.As node
    | Pattern.ListCons node -> genPatLeftMiddleRight node
    | Pattern.NamePatPairs node ->
        let genPatWithIdent (node: NamePatPair) =
            genSingleTextNode node.Ident
            +> sepSpace
            +> genSingleTextNode node.Equals
            +> sepSpace
            +> genPat node.Pattern
            |> genNode node

        let pats =
            expressionFitsOnRestOfLine
                (atCurrentColumn (col sepSemi node.Pairs genPatWithIdent))
                (atCurrentColumn (col sepNln node.Pairs genPatWithIdent))

        genIdentListNode node.Identifier
        +> optSingle genTyparDecls node.TyparDecls
        +> addSpaceBeforeParenInPattern node.Identifier
        +> genSingleTextNode node.OpeningParen
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (
            sepNlnWhenWriteBeforeNewlineNotEmpty
            +> pats
            +> sepNlnWhenWriteBeforeNewlineNotEmpty
        )
        +> genSingleTextNode node.ClosingParen
        |> genNode node

    | Pattern.LongIdent node ->
        let genName =
            genAccessOpt node.Accessibility
            +> genIdentListNode node.Identifier
            +> optSingle genTyparDecls node.TyparDecls

        let genParameters =
            match node.Parameters with
            | [] -> sepNone
            | [ Pattern.Paren _ | Pattern.Unit _ as parameter ] ->
                addSpaceBeforeParenInPattern node.Identifier +> genPat parameter
            | ps -> sepSpace +> atCurrentColumn (col sepSpace ps genPat)

        genName +> genParameters |> genNode node
    | Pattern.Unit n -> genUnit n
    | Pattern.Paren node ->
        genSingleTextNode node.OpeningParen
        +> genPat node.Pattern
        +> genSingleTextNode node.ClosingParen
        |> genNode node
    | Pattern.Tuple node -> genTuplePat node
    | Pattern.StructTuple node ->
        !- "struct "
        +> sepOpenT
        +> atCurrentColumn (colAutoNlnSkip0 sepComma node.Patterns genPat)
        +> sepCloseT
        |> genNode node
    | Pattern.ArrayOrList node ->
        let genPats =
            let short = colAutoNlnSkip0 sepSemi node.Patterns genPat

            let long (ctx: Context) =
                match node.Patterns with
                | [ Pattern.Or _ ] ->
                    let column = ctx.Column + 1
                    atIndentLevel false column (col sepNln node.Patterns genPatInClause) ctx
                | _ -> col sepNln node.Patterns genPatInClause ctx

            expressionFitsOnRestOfLine short long

        ifElse
            node.Patterns.IsEmpty
            (genSingleTextNode node.OpenToken +> genSingleTextNode node.CloseToken)
            (genSingleTextNode node.OpenToken
             +> addSpaceIfSpaceAroundDelimiter
             +> atCurrentColumn genPats
             +> addSpaceIfSpaceAroundDelimiter
             +> genSingleTextNode node.CloseToken)
        |> genNode node
    | Pattern.Record node ->
        let smallRecordExpr =
            genSingleTextNode node.OpeningNode
            +> addSpaceIfSpaceAroundDelimiter
            +> col sepSemi node.Fields genPatRecordFieldName
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.ClosingNode

        let multilineRecordExpr =
            genSingleTextNode node.OpeningNode
            +> addSpaceIfSpaceAroundDelimiter
            +> atCurrentColumn (col sepNln node.Fields genPatRecordFieldName)
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.ClosingNode

        let multilineRecordExprAlignBrackets =
            genSingleTextNode node.OpeningNode
            +> indent
            +> sepNln
            +> atCurrentColumn (col sepNln node.Fields genPatRecordFieldName)
            +> unindent
            +> sepNln
            +> genSingleTextNode node.ClosingNode
            |> atCurrentColumnIndent

        let multilineExpressionIfAlignBrackets =
            ifAlignOrStroustrupBrackets multilineRecordExprAlignBrackets multilineRecordExpr

        fun ctx ->
            let size = getRecordSize ctx node.Fields
            genNode node (isSmallExpression size smallRecordExpr multilineExpressionIfAlignBrackets) ctx
    | Pattern.Const c -> genConstant c
    | Pattern.IsInst node -> genSingleTextNode node.Token +> sepSpace +> genType node.Type |> genNode node
    | Pattern.QuoteExpr node -> genQuoteExpr node

let genPatInClause (pat: Pattern) =
    let rec genPatMultiline p =
        match p with
        | Pattern.Or p ->
            let genBar =
                match p.Middle with
                | Choice1Of2 barNode -> genSingleTextNode barNode +> sepSpace
                | Choice2Of2 _ -> sepBar

            genPatMultiline p.LeftHandSide +> sepNln +> genBar +> genPat p.RightHandSide

        | Pattern.As p ->
            let genAs =
                match p.Middle with
                | Choice1Of2 asNode -> genSingleTextNode asNode
                | Choice2Of2 _ -> !- "as"

            genPatMultiline p.LeftHandSide
            +> sepSpace
            +> genAs
            +> sepSpace
            +> genPat p.RightHandSide

        | p -> atCurrentColumn (genPat p)

    genPatMultiline pat

let genPatRecordFieldName (node: PatRecordField) =
    match node.Prefix with
    | None ->
        genSingleTextNode node.FieldName
        +> sepSpace
        +> genSingleTextNode node.Equals
        +> sepSpace
        +> genPat node.Pattern
    | Some prefix ->
        genIdentListNode prefix
        +> sepDot
        +> genSingleTextNode node.FieldName
        +> sepSpace
        +> genSingleTextNode node.Equals
        +> sepSpace
        +> genPat node.Pattern

let genReturnTypeBinding (node: BindingReturnInfoNode option) =
    match node with
    | None -> sepNone
    | Some node ->
        onlyIfCtx (fun ctx -> ctx.Config.SpaceBeforeColon) sepSpace
        +> genSingleTextNode node.Colon
        +> sepSpace
        +> genType node.Type

let genBinding (b: BindingNode) (ctx: Context) : Context =
    let spaceBefore, alternativeSyntax =
        let keywords =
            List.map (fun (st: SingleTextNode) -> st.Text) b.LeadingKeyword.Content

        match keywords with
        | [ "member" ]
        | [ "override" ]
        | [ "static"; "member" ]
        | [ "abstract"; "member" ]
        | [ "default" ] -> ctx.Config.SpaceBeforeMember, ctx.Config.AlternativeLongMemberDefinitions
        | _ -> ctx.Config.SpaceBeforeParameter, ctx.Config.AlignFunctionSignatureToIndentation

    let isRecursiveLetOrUseFunction =
        match b.LeadingKeyword.Content with
        | [ singleText ] -> singleText.Text = "and"
        | _ -> false

    let binding =
        match b.FunctionName with
        | Choice1Of2 functionName when List.isNotEmpty b.Parameters ->
            let genAttrIsFirstChild =
                onlyIf (not isRecursiveLetOrUseFunction) (genAttributes b.Attributes)

            let genPref =
                if not isRecursiveLetOrUseFunction then
                    genMultipleTextsNode b.LeadingKeyword +> sepSpace
                else
                    genMultipleTextsNode b.LeadingKeyword
                    +> sepSpace
                    +> genOnelinerAttributes b.Attributes

            let afterLetKeyword =
                ifElse b.IsMutable (!- "mutable ") sepNone
                +> genInlineOpt b.Inline
                +> genAccessOpt b.Accessibility

            let genFunctionName =
                genIdentListNode functionName +> optSingle genTyparDecls b.GenericTypeParameters

            let genReturnType isFixed =
                match b.ReturnType with
                | None -> sepNone
                | Some returnTypeNode ->
                    onlyIfNot isFixed sepSpace
                    +> (genSingleTextNode returnTypeNode.Colon
                        +> sepSpace
                        +> atCurrentColumnIndent (genType returnTypeNode.Type)
                        |> genNode returnTypeNode)

            let genSignature =
                let spaceBeforeParameters =
                    match b.Parameters with
                    | [] -> sepNone
                    | [ p ] ->
                        let addSpaceBeforeParensInFunDef =
                            match functionName.Content, p with
                            | [ IdentifierOrDot.Ident newIdent ], _ when newIdent.Text = "new" -> false
                            | _, Pattern.Paren _
                            | _, Pattern.Unit _ -> spaceBefore
                            | _, Pattern.Named _
                            | _, Pattern.Wild _ -> true
                            | content, _ ->
                                match List.tryLast content with
                                | None -> false
                                | Some(IdentifierOrDot.KnownDot _)
                                | Some IdentifierOrDot.UnknownDot -> true
                                | Some(IdentifierOrDot.Ident ident) -> not (Char.IsUpper ident.Text.[0])

                        ifElse addSpaceBeforeParensInFunDef sepSpace sepNone
                    | _ -> sepSpace

                /// Format everything in one line:
                /// let fn p1 p2 : rt =
                let short =
                    afterLetKeyword
                    +> sepSpace
                    +> genFunctionName
                    +> spaceBeforeParameters
                    +> col sepSpace b.Parameters genPat
                    +> genReturnType false
                    +> sepSpace
                    +> genSingleTextNode b.Equals

                /// Format over multiple lines
                /// AlternativeLongMemberDefinitions or AlignFunctionSignatureToIndentation can influence the equals and return type.
                ///
                /// By default we go with:
                /// let fn
                ///     p1
                ///     p2
                ///     : rt =
                ///
                /// When `rt` is multiline, `=` is placed on the next line:
                /// let fn
                ///     p1
                ///     p2
                ///     : rt
                ///     =
                ///
                /// When the p2 is a multiline tuple, there are different rules formatting rules:
                /// let fn
                ///     p1
                ///     (
                ///         p2_1,
                ///         p2_2,
                ///         p2_3
                ///     ) : rt =
                ///
                /// AlignFunctionSignatureToIndentation will always place `rt` and `=` on their own lines:
                /// let fn
                ///     p1
                ///     p2
                ///     : rt
                ///     =
                ///
                /// This happens regardless whether p2 is a multiline tuple:
                /// let fn
                ///     p1
                ///     (
                ///         p2_1,
                ///         p2_2,
                ///         p2_3
                ///     )
                ///     : rt
                ///     =
                let long (ctx: Context) =
                    let endsWithMultilineTupleParameter =
                        match List.tryLast b.Parameters with
                        | Some(Pattern.Paren parenNode as p) ->
                            match parenNode.Pattern with
                            | Pattern.Tuple _ -> futureNlnCheck (genLongParenPatParameter p) ctx
                            | _ -> false
                        | _ -> false

                    let genParameters = col sepNln b.Parameters genLongParenPatParameter

                    let hasTriviaAfterLeadingKeyword =
                        let beforeInline =
                            match b.Inline with
                            | None -> false
                            | Some n -> n.HasContentBefore

                        let beforeIdentifier = functionName.HasContentBefore

                        let beforeAccessibility =
                            match b.Accessibility with
                            | None -> false
                            | Some n -> n.HasContentBefore

                        beforeInline || beforeIdentifier || beforeAccessibility

                    let nlnOnSeparateLine = not endsWithMultilineTupleParameter || alternativeSyntax

                    (onlyIf hasTriviaAfterLeadingKeyword indent
                     +> afterLetKeyword
                     +> sepSpace
                     +> genFunctionName
                     +> indent
                     +> sepNln
                     +> genParameters
                     +> onlyIf nlnOnSeparateLine sepNln
                     +> leadingExpressionIsMultiline (genReturnType nlnOnSeparateLine) (fun isMultiline ->
                         if (alternativeSyntax && Option.isSome b.ReturnType) || isMultiline then
                             sepNln +> genSingleTextNode b.Equals
                         else
                             sepSpace +> genSingleTextNode b.Equals)
                     +> unindent
                     +> onlyIf hasTriviaAfterLeadingKeyword unindent)
                        ctx

                expressionFitsOnRestOfLine short long

            let body = genExpr b.Expr

            let genExpr isMultiline =
                if isMultiline then
                    indentSepNlnUnindent body
                else
                    let short = sepSpace +> body

                    let long =
                        indentSepNlnUnindentUnlessStroustrup (fun e -> sepSpace +> genExpr e) b.Expr

                    isShortExpression ctx.Config.MaxFunctionBindingWidth short long

            (genXml b.XmlDoc
             +> genAttrIsFirstChild
             +> genPref
             +> leadingExpressionIsMultiline genSignature genExpr)

        | Choice2Of2(Pattern.Tuple _ as pat) ->
            let genAttrAndPref =
                if not isRecursiveLetOrUseFunction then
                    (genAttributes b.Attributes +> genMultipleTextsNode b.LeadingKeyword)
                else
                    (genMultipleTextsNode b.LeadingKeyword
                     +> sepSpace
                     +> genOnelinerAttributes b.Attributes)

            let afterLetKeyword =
                genAccessOpt b.Accessibility
                +> ifElse b.IsMutable (!- "mutable ") sepNone
                +> genInlineOpt b.Inline

            let genDestructedTuples =
                expressionFitsOnRestOfLine (genPat pat) (sepOpenT +> genPat pat +> sepCloseT)
                +> optSingle
                    (fun (rt: BindingReturnInfoNode) ->
                        genSingleTextNode rt.Colon
                        +> sepSpace
                        +> atCurrentColumnIndent (genType rt.Type))
                    b.ReturnType

            genXml b.XmlDoc
            +> genAttrAndPref
            +> sepSpace
            +> (fun ctx ->
                let prefix =
                    afterLetKeyword
                    +> sepSpace
                    +> genDestructedTuples
                    +> sepSpace
                    +> genSingleTextNode b.Equals

                let long = prefix +> indentSepNlnUnindent (genExpr b.Expr)
                let short = prefix +> sepSpace +> genExpr b.Expr
                isShortExpression ctx.Config.MaxValueBindingWidth short long ctx)

        | _ ->
            // old code of genSynBindingValue
            let genAttrIsFirstChild =
                onlyIf (not isRecursiveLetOrUseFunction) (genAttributes b.Attributes)

            let genPref =
                if not isRecursiveLetOrUseFunction then
                    genMultipleTextsNode b.LeadingKeyword +> sepSpace
                else
                    (genMultipleTextsNode b.LeadingKeyword
                     +> sepSpace
                     +> genOnelinerAttributes b.Attributes)

            let afterLetKeyword =
                ifElse b.IsMutable (!- "mutable ") sepNone
                +> genInlineOpt b.Inline
                +> genAccessOpt b.Accessibility

            let genValueName =
                match b.FunctionName with
                | Choice1Of2 lid -> genIdentListNode lid
                | Choice2Of2 pat -> genPat pat
                +> optSingle genTyparDecls b.GenericTypeParameters

            let genEqualsInBinding (ctx: Context) =
                (genSingleTextNode b.Equals +> sepSpaceUnlessWriteBeforeNewlineNotEmpty) ctx

            let genReturnType =
                match b.ReturnType with
                | Some rt ->
                    let hasGenerics = b.GenericTypeParameters.IsSome

                    autoIndentAndNlnIfExpressionExceedsPageWidth (
                        onlyIfCtx (fun ctx -> hasGenerics || ctx.Config.SpaceBeforeColon) sepSpace
                        +> genSingleTextNode rt.Colon
                        +> sepSpace
                        +> atCurrentColumnIndent (genType rt.Type)
                    )
                    +> sepSpaceUnlessWriteBeforeNewlineNotEmpty
                    +> autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty genEqualsInBinding
                | _ -> sepSpace +> genEqualsInBinding

            genXml b.XmlDoc
            +> genAttrIsFirstChild
            +> genPref
            +> (fun ctx ->
                let prefix = afterLetKeyword +> sepSpace +> genValueName +> genReturnType
                let short = prefix +> genExpr b.Expr
                let long = prefix +> indentSepNlnUnindentUnlessStroustrup genExpr b.Expr
                isShortExpression ctx.Config.MaxValueBindingWidth short long ctx)

    genNode b binding ctx

let genBindings withUseConfig (bs: BindingNode list) : Context -> Context =
    colWithNlnWhenNodeIsMultiline withUseConfig genBinding bs

let genExternBinding (externNode: ExternBindingNode) =
    let genParameters =
        let short =
            col sepComma externNode.Parameters (fun externParameter ->
                genOnelinerAttributes externParameter.Attributes
                +> onlyIf externParameter.Attributes.IsSome sepSpace
                +> optSingle genType externParameter.Type
                +> onlyIf externParameter.Pattern.IsSome sepSpace
                +> optSingle genPat externParameter.Pattern
                |> genNode externParameter)

        let long =
            indentSepNlnUnindent (
                col (sepComma +> sepNln) externNode.Parameters (fun externParameter ->
                    genOnelinerAttributes externParameter.Attributes
                    +> onlyIf externParameter.Attributes.IsSome sepSpace
                    +> optSingle (fun t -> genType t +> sepSpace) externParameter.Type
                    +> optSingle genPat externParameter.Pattern
                    |> genNode externParameter)
            )
            +> sepNln

        expressionFitsOnRestOfLine short long

    genXml externNode.XmlDoc
    +> genAttributes externNode.Attributes
    +> genSingleTextNode externNode.Extern
    +> sepSpace
    +> genOnelinerAttributes externNode.AttributesOfType
    +> genType externNode.Type
    +> sepSpace
    +> genAccessOpt externNode.Accessibility
    +> genIdentListNode externNode.Identifier
    +> genSingleTextNode externNode.OpeningParen
    +> genParameters
    +> genSingleTextNode externNode.ClosingParen
    |> genNode externNode

let genOpenList (openList: OpenListNode) =
    col sepNln openList.Opens (function
        | Open.ModuleOrNamespace node -> !- "open " +> genIdentListNode node.Name |> genNode node
        | Open.Target node -> !- "open type " +> genType node.Target |> genNode node)
    |> genNode openList

let genTypeConstraint (tc: TypeConstraint) =
    match tc with
    | TypeConstraint.Single node ->
        genSingleTextNode node.Typar +> sepColon +> genSingleTextNode node.Kind
        |> genNode node
    | TypeConstraint.DefaultsToType node ->
        genSingleTextNode node.Default
        +> sepSpace
        +> genSingleTextNode node.Typar
        +> sepColon
        +> genType node.Type
        |> genNode node
    | TypeConstraint.SubtypeOfType node ->
        genSingleTextNode node.Typar +> !- " :> " +> genType node.Type |> genNode node
    | TypeConstraint.SupportsMember node ->
        genType node.Type
        +> sepColon
        +> sepOpenT
        +> genMemberDefn node.MemberSig
        +> sepCloseT
        |> genNode node
    | TypeConstraint.EnumOrDelegate node ->
        genSingleTextNode node.Typar
        +> sepColon
        +> !- $"%s{node.Verb}<"
        +> col sepComma node.Types genType
        +> !- ">"
        |> genNode node
    | TypeConstraint.WhereSelfConstrained t -> genType t

let genTypeConstraints (tcs: TypeConstraint list) =
    let short = colPre (sepSpace +> !- "when ") wordAnd tcs genTypeConstraint

    let long =
        colPre (!- "when ") (sepNln +> wordAndFixed +> sepSpace) tcs genTypeConstraint

    autoIndentAndNlnIfExpressionExceedsPageWidth (expressionFitsOnRestOfLine short long)

let genType (t: Type) =
    match t with
    | Type.Funs node ->
        let short =
            col sepNone node.Parameters (fun (t, arrow) ->
                genType t
                +> sepSpace
                +> genSingleTextNode arrow
                +> sepSpace
                +> sepNlnWhenWriteBeforeNewlineNotEmpty)
            +> genType node.ReturnType

        let long =
            match node.Parameters with
            | [] -> genType node.ReturnType
            | (ht, ha) :: rest ->
                genType ht
                +> indentSepNlnUnindent (
                    genSingleTextNode ha
                    +> sepSpace
                    +> col sepNone rest (fun (t, arrow) -> genType t +> sepNln +> genSingleTextNode arrow +> sepSpace)
                    +> genType node.ReturnType
                )

        expressionFitsOnRestOfLine short long |> genNode node
    | Type.Tuple node -> genSynTupleTypeSegments node.Path |> genNode node
    | Type.HashConstraint node -> genSingleTextNode node.Hash +> genType node.Type |> genNode node
    | Type.MeasurePower node -> genType node.BaseMeasure +> !- "^" +> genRational node.Exponent |> genNode node
    | Type.StaticConstant c -> genConstant c
    | Type.StaticConstantExpr node ->
        let addSpace =
            match node.Expr with
            | ParenExpr _ -> sepNone
            | _ -> sepSpace

        genSingleTextNode node.Const +> addSpace +> genExpr node.Expr |> genNode node
    | Type.StaticConstantNamed node ->
        genType node.Identifier
        +> !- "="
        +> addSpaceIfSynTypeStaticConstantHasAtSignBeforeString node.Value
        +> genType node.Value
        |> genNode node
    | Type.Array node ->
        genType node.Type +> !- "[" +> rep (node.Rank - 1) (!- ",") +> !- "]"
        |> genNode node
    | Type.Anon node -> genSingleTextNode node
    | Type.Var node -> genSingleTextNode node
    | Type.AppPostfix node -> genType node.First +> sepSpace +> genType node.Last |> genNode node
    | Type.AppPrefix node ->
        let addExtraSpace =
            match node.Arguments with
            | [] -> sepNone
            | Type.Var node :: _ when String.startsWithOrdinal "^" node.Text -> sepSpace
            | t :: _ -> addSpaceIfSynTypeStaticConstantHasAtSignBeforeString t

        genType node.Identifier
        +> optSingle genIdentListNodeWithDot node.PostIdentifier
        +> genSingleTextNode node.LessThen
        +> addExtraSpace
        +> leadingExpressionIsMultiline (colGenericTypeParameters node.Arguments) (fun isMultiline ->
            onlyIf isMultiline (!- " "))
        +> addExtraSpace
        // TODO: I think we need to add a space here
        +> genSingleTextNode node.GreaterThan
        |> genNode node
    | Type.StructTuple node ->
        genSingleTextNode node.Keyword
        +> sepSpace
        +> sepOpenT
        +> genSynTupleTypeSegments node.Path
        +> genSingleTextNode node.ClosingParen
        |> genNode node
    | Type.WithSubTypeConstraint tc -> genTypeConstraint tc
    | Type.WithGlobalConstraints node -> genType node.Type +> genTypeConstraints node.TypeConstraints |> genNode node
    | Type.LongIdent idn -> genIdentListNode idn
    | Type.AnonRecord node ->
        let genStruct =
            match node.Struct with
            | None -> sepNone
            | Some n -> genSingleTextNode n +> sepSpace

        let genOpening =
            match node.Opening with
            | None -> sepOpenAnonRecdFixed +> addSpaceIfSpaceAroundDelimiter
            | Some n -> genSingleTextNode n +> addSpaceIfSpaceAroundDelimiter

        let genAnonRecordFieldType (identifier, t) =
            genSingleTextNode identifier
            +> sepColon
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genType t)

        let smallExpression =
            genStruct
            +> genOpening
            +> col sepSemi node.Fields genAnonRecordFieldType
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.Closing

        let longExpression =
            let genAnonRecordFields = col sepNln node.Fields genAnonRecordFieldType

            ifAlignOrStroustrupBrackets
                (genStruct
                 +> sepOpenAnonRecdFixed
                 +> indentSepNlnUnindent (atCurrentColumnIndent genAnonRecordFields)
                 +> sepNln
                 +> genSingleTextNode node.Closing)
                (genStruct
                 +> genOpening
                 +> atCurrentColumn genAnonRecordFields
                 +> addSpaceIfSpaceAroundDelimiter
                 +> genSingleTextNode node.Closing)

        fun (ctx: Context) ->
            let size = getRecordSize ctx node.Fields
            genNode node (isSmallExpression size smallExpression longExpression) ctx

    | Type.Paren node ->
        genSingleTextNode node.OpeningParen
        +> genType node.Type
        +> genSingleTextNode node.ClosingParen
        |> genNode node
    | Type.SignatureParameter node ->
        genOnelinerAttributes node.Attributes
        +> optSingle (fun id -> genSingleTextNode id +> sepColon) node.Identifier
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genType node.Type)
        |> genNode node
    | Type.Or node ->
        genType node.LeftHandSide
        +> sepSpace
        +> genSingleTextNode node.Or
        +> sepSpace
        +> genType node.RightHandSide
        |> genNode node
    | Type.LongIdentApp node ->
        genType node.AppType +> sepDot +> genIdentListNode node.LongIdent
        |> genNode node
    | Type.Intersection node ->
        col sepSpace node.TypesAndSeparators (function
            | Choice1Of2 t -> genType t
            | Choice2Of2 amp -> genSingleTextNode amp)
        |> genNode node

let genSynTupleTypeSegments (path: Choice<Type, SingleTextNode> list) =
    let genTs addNewline =
        col sepSpace path (fun t ->
            match t with
            | Choice1Of2 t -> genType t
            | Choice2Of2 node -> genSingleTextNode node +> onlyIf addNewline sepNln)

    expressionFitsOnRestOfLine (genTs false) (genTs true)

let addSpaceIfSynTypeStaticConstantHasAtSignBeforeString (t: Type) =
    match t with
    | Type.StaticConstant sc ->
        match sc with
        | Constant.FromText node -> onlyIf (String.startsWithOrdinal "@" node.Text) sepSpace
        | _ -> sepNone
    | _ -> sepNone

let sepNlnBetweenTypeAndMembers (node: ITypeDefn) (ctx: Context) : Context =
    match node.Members with
    | [] -> sepNone ctx
    | firstMember :: _ ->
        match node.TypeName.WithKeyword with
        | Some node when node.HasContentBefore -> enterNode node ctx
        | _ ->
            if ctx.Config.NewlineBetweenTypeDefinitionAndMembers then
                sepNlnUnlessContentBefore (MemberDefn.Node firstMember) ctx
            else
                ctx

[<return: Struct>]
let inline (|ParameterWithTupleTypePattern|_|) (pat: Pattern) =
    match pat with
    | Pattern.Parameter parameterNode ->
        match parameterNode.Type with
        | Some t ->
            match t with
            | Type.Tuple _ -> ValueSome()
            | _ -> ValueNone
        | None -> ValueNone
    | _ -> ValueNone

/// Format a long parentheses parameter pattern in a binding or constructor.
/// Alternate formatting will applied when a paren tuple does not fit on the remainder of the line.
let genLongParenPatParameter (pat: Pattern) =
    match pat with
    | Pattern.Paren patParen ->
        match patParen.Pattern with
        | ParameterWithTupleTypePattern
        | Pattern.Tuple _ ->
            genSingleTextNode patParen.OpeningParen
            +> expressionFitsOnRestOfLine
                (genPat patParen.Pattern)
                (indentSepNlnUnindent (genPat patParen.Pattern) +> sepNln)
            +> genSingleTextNode patParen.ClosingParen
            |> genNode patParen
        | _ -> genPat pat
    | _ -> genPat pat

let genImplicitConstructor (node: ImplicitConstructorNode) =
    let short =
        genXml node.XmlDoc
        +> onlyIf node.Attributes.IsSome sepSpace
        +> genOnelinerAttributes node.Attributes
        +> onlyIf node.Accessibility.IsSome sepSpace
        +> genAccessOpt node.Accessibility
        +> genPat node.Pattern

    let long =
        indentSepNlnUnindent (
            genXml node.XmlDoc
            +> genOnelinerAttributes node.Attributes
            +> onlyIf node.Attributes.IsSome sepNln
            +> expressionFitsOnRestOfLine
                (genAccessOpt node.Accessibility +> genLongParenPatParameter node.Pattern)
                (genAccessOpt node.Accessibility
                 +> optSingle (fun _ -> sepNln) node.Accessibility
                 +> genLongParenPatParameter node.Pattern)
            +> (fun ctx -> onlyIf ctx.Config.AlternativeLongMemberDefinitions sepNln ctx)
        )

    expressionFitsOnRestOfLine short long
    +> optSingle
        (fun (selfNode: AsSelfIdentifierNode) ->
            sepSpace
            +> onlyIf selfNode.HasContentBefore indent
            +> (genSingleTextNode selfNode.As +> sepSpace +> genSingleTextNode selfNode.Self
                |> genNode selfNode)
            +> onlyIf selfNode.HasContentBefore unindent
            +> sepSpace)
        node.Self

let hasTriviaAfterLeadingKeyword (identifier: IdentListNode) (accessibility: SingleTextNode option) =
    let beforeAccess =
        match accessibility with
        | Some n -> n.HasContentBefore
        | _ -> false

    let beforeIdentifier = identifier.HasContentBefore
    beforeAccess || beforeIdentifier

let genTypeDefn (td: TypeDefn) =
    let typeDefnNode = TypeDefn.TypeDefnNode td
    let typeName = typeDefnNode.TypeName

    let header =
        let implicitConstructor = typeName.ImplicitConstructor
        let hasAndKeyword = typeName.LeadingKeyword.Text = "and"

        // Workaround for https://github.com/fsprojects/fantomas/issues/628
        let hasTriviaAfterLeadingKeyword =
            hasTriviaAfterLeadingKeyword typeName.Identifier typeName.Accessibility

        genXml typeName.XmlDoc
        +> onlyIfNot hasAndKeyword (genAttributes typeName.Attributes)
        +> genSingleTextNode typeName.LeadingKeyword
        +> onlyIf hasTriviaAfterLeadingKeyword indent
        +> onlyIf hasAndKeyword (sepSpace +> genOnelinerAttributes typeName.Attributes)
        +> sepSpace
        +> genAccessOpt typeName.Accessibility
        +> genTypeAndParam (genIdentListNode typeName.Identifier) typeName.TypeParameters
        +> onlyIfNot typeName.Constraints.IsEmpty (sepSpace +> genTypeConstraints typeName.Constraints)
        +> onlyIf hasTriviaAfterLeadingKeyword unindent
        +> leadingExpressionIsMultiline
            (optSingle
                (fun imCtor -> sepSpaceBeforeClassConstructor +> genImplicitConstructor imCtor)
                implicitConstructor)
            (fun isMulti ctx ->
                if isMulti && ctx.Config.AlternativeLongMemberDefinitions then
                    (optSingle genSingleTextNode typeName.EqualsToken) ctx
                else
                    (sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (
                        optSingle genSingleTextNode typeName.EqualsToken
                    ))
                        ctx)
        |> genNode typeName

    let members = typeDefnNode.Members

    match td with
    | TypeDefn.Enum node ->
        let hasMembers = List.isNotEmpty members

        let genEnumCase (node: EnumCaseNode) =
            genXml node.XmlDoc
            +> (match node.Bar with
                | None -> sepBar
                | Some bar -> genSingleTextNode bar +> sepSpace)
            +> genOnelinerAttributes node.Attributes
            +> genSingleTextNode node.Identifier
            +> sepSpace
            +> genSingleTextNode node.Equals
            +> autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty (sepSpace +> genExpr node.Constant)
            |> genNode node

        header
        +> indentSepNlnUnindent (
            col sepNln node.EnumCases genEnumCase
            +> onlyIf hasMembers sepNln
            +> sepNlnBetweenTypeAndMembers typeDefnNode
            +> genMemberDefnList members
        )
        |> genNode node
    | TypeDefn.Union node ->
        let hasMembers = List.isNotEmpty members

        let unionCases (ctx: Context) =
            match node.UnionCases with
            | [] -> ctx
            | [ singleCase ] when not hasMembers ->
                let hasVerticalBar =
                    ctx.Config.BarBeforeDiscriminatedUnionDeclaration
                    || singleCase.Attributes.IsSome
                    || List.isEmpty singleCase.Fields

                let genCase hasVerticalBar =
                    opt sepSpace node.Accessibility (fun vis ->
                        genSingleTextNode vis +> onlyIfNot singleCase.XmlDoc.IsNone sepNln)
                    +> genUnionCase hasVerticalBar singleCase

                expressionFitsOnRestOfLine
                    (sepSpace +> genCase hasVerticalBar)
                    (indentSepNlnUnindent (genCase true))
                    ctx
            | xs ->
                indentSepNlnUnindent
                    (opt sepNln node.Accessibility genSingleTextNode
                     +> col sepNln xs (genUnionCase true))
                    ctx

        header
        +> unionCases
        +> onlyIf
            hasMembers
            (indentSepNlnUnindent (sepNlnBetweenTypeAndMembers typeDefnNode +> genMemberDefnList members))
        |> genNode node
    | TypeDefn.Record node ->
        let hasMembers = List.isNotEmpty members

        let multilineExpression (ctx: Context) =
            let genRecordFields =
                genSingleTextNode node.OpeningBrace
                +> indentSepNlnUnindent (atCurrentColumn (col sepNln node.Fields genField))
                +> sepNln
                +> genSingleTextNode node.ClosingBrace

            let genMembers =
                onlyIf hasMembers (sepNln +> sepNlnBetweenTypeAndMembers typeDefnNode +> genMemberDefnList members)

            let anyFieldHasXmlDoc =
                List.exists (fun (fieldNode: FieldNode) -> fieldNode.XmlDoc.IsSome) node.Fields

            let aligned =
                opt (indent +> sepNln) node.Accessibility genSingleTextNode
                +> genRecordFields
                +> optSingle (fun _ -> unindent) node.Accessibility
                +> genMembers

            let stroustrup =
                let withKw =
                    match typeName.WithKeyword with
                    | None -> !- "with"
                    | Some withNode -> genSingleTextNode withNode

                genAccessOpt node.Accessibility
                +> genRecordFields
                +> onlyIf hasMembers (sepSpace +> withKw +> indent +> genMembers +> unindent)

            let cramped =
                sepNlnUnlessLastEventIsNewline
                +> opt (indent +> sepNln) node.Accessibility genSingleTextNode
                +> genSingleTextNodeSuffixDelimiter node.OpeningBrace
                +> atCurrentColumn (sepNlnWhenWriteBeforeNewlineNotEmpty +> col sepNln node.Fields genField)
                +> addSpaceIfSpaceAroundDelimiter
                +> genSingleTextNode node.ClosingBrace
                +> optSingle (fun _ -> unindent) node.Accessibility
                +> genMembers

            match ctx.Config.MultilineBracketStyle with
            | Stroustrup -> stroustrup ctx
            | Aligned -> aligned ctx
            | Cramped when anyFieldHasXmlDoc -> aligned ctx
            | Cramped -> cramped ctx

        let bodyExpr size =
            if hasMembers then
                multilineExpression
            else
                let smallExpression =
                    sepSpace
                    +> genAccessOpt node.Accessibility
                    +> sepSpace
                    +> genSingleTextNode node.OpeningBrace
                    +> addSpaceIfSpaceAroundDelimiter
                    +> col sepSemi node.Fields genField
                    +> addSpaceIfSpaceAroundDelimiter
                    +> genSingleTextNode node.ClosingBrace

                isSmallExpression size smallExpression multilineExpression

        let genTypeDefinition (ctx: Context) =
            let size = getRecordSize ctx node.Fields
            let short = bodyExpr size

            if ctx.Config.IsStroustrupStyle && not node.OpeningBrace.HasContentBefore then
                (sepSpace +> short) ctx
            else
                isSmallExpression size short (indentSepNlnUnindent short) ctx

        header +> genTypeDefinition |> genNode node
    | TypeDefn.None _ -> header
    | TypeDefn.Abbrev node ->
        let hasMembers = List.isNotEmpty members

        fun (ctx: Context) ->
            (match node.Type with
             | Type.AnonRecord _ when not hasMembers && ctx.Config.IsStroustrupStyle ->
                 header
                 +> sepSpaceOrIndentAndNlnIfTypeExceedsPageWidthUnlessStroustrup genType node.Type
                 |> genNode node
             | _ ->
                 header
                 +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genType node.Type)
                 +> (match List.tryHead members, typeName.WithKeyword with
                     | Some firstMember, Some withNode ->
                         indentSepNlnUnindent (
                             genSingleTextNode withNode
                             +> onlyIfCtx
                                 (fun ctx -> ctx.Config.NewlineBetweenTypeDefinitionAndMembers)
                                 (sepNlnUnlessContentBefore (MemberDefn.Node firstMember))
                             +> indentSepNlnUnindent (genMemberDefnList members)
                         )
                     | _ -> sepNone)
                 |> genNode node)
                ctx
    | TypeDefn.Explicit node ->
        let bodyNode = node.Body

        let additionMembers =
            match members with
            | [] -> sepNone
            | h :: _ ->
                sepNlnUnlessContentBefore (MemberDefn.Node h)
                +> indentSepNlnUnindent (genMemberDefnList members)

        match bodyNode.Members, members with
        | [], [] ->
            // Empty class/struct/interface end
            let genBody =
                genSingleTextNode bodyNode.Kind +> sepSpace +> genSingleTextNode bodyNode.End
                |> genNode bodyNode

            header +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth genBody
        | _ ->
            header
            +> indentSepNlnUnindent (
                genSingleTextNode bodyNode.Kind
                +> onlyIfNot bodyNode.Members.IsEmpty (indentSepNlnUnindent (genMemberDefnList bodyNode.Members))
                +> sepNln
                +> genSingleTextNode bodyNode.End
                |> genNode bodyNode
            )
            +> additionMembers
        |> genNode node
    | TypeDefn.Augmentation node ->
        header
        +> sepSpace
        +> optSingle genSingleTextNode typeName.WithKeyword
        +> indentSepNlnUnindent (genMemberDefnList members)
        |> genNode node
    | TypeDefn.Delegate node ->
        header
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (
            genSingleTextNode node.DelegateNode
            +> sepSpace
            +> !- "of"
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genTypeList node.TypeList)
        )
        |> genNode node
    | TypeDefn.Regular node -> header +> indentSepNlnUnindent (genMemberDefnList members) |> genNode node

let genTypeList (node: TypeFunsNode) =
    let shortExpr =
        col sepSpace node.Parameters (fun (t, arrow) -> genType t +> sepSpace +> genSingleTextNode arrow)
        +> sepSpace
        +> genType node.ReturnType

    let longExpr =
        let rec visit parameters level (continuation: Context -> Context) =
            match parameters with
            | [] -> continuation
            | [ lastType, lastArrow ] ->
                continuation
                +> genType lastType
                +> sepSpace
                +> genSingleTextNode lastArrow
                +> indent
                +> sepNln
                +> genType node.ReturnType
                +> rep (level + 1) unindent
            | (t, arrow) :: tail ->
                let isTuple =
                    match t with
                    | Type.Tuple _ -> true
                    | _ -> false

                visit
                    tail
                    (level + if isTuple then 1 else 0)
                    (continuation
                     +> genType t
                     +> sepSpace
                     +> genSingleTextNode arrow
                     +> onlyIf isTuple indent
                     +> sepNln)

        visit node.Parameters 0 sepNone

    expressionFitsOnRestOfLine shortExpr longExpr |> genNode node

let genTypeInSignature (t: Type) =
    match t with
    | Type.WithGlobalConstraints node ->
        match node.Type with
        | Type.Funs funsNode ->
            let genConstraints =
                let short =
                    ifElse (List.isNotEmpty node.TypeConstraints) (!- "when ") sepSpace
                    +> col wordAnd node.TypeConstraints genTypeConstraint

                let long =
                    ifElse (List.isNotEmpty node.TypeConstraints) (!- "when ") sepSpace
                    +> col (sepNln +> wordAndFixed +> sepSpace) node.TypeConstraints genTypeConstraint

                expressionFitsOnRestOfLine short long

            autoIndentAndNlnIfExpressionExceedsPageWidth (
                leadingExpressionIsMultiline (genTypeList funsNode) (fun isMultiline ->
                    if isMultiline then
                        indentSepNlnUnindent genConstraints
                    else
                        sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth genConstraints)
            )
        | _ -> autoIndentAndNlnIfExpressionExceedsPageWidth (genType t)
    | Type.Funs funsNode -> autoIndentAndNlnIfExpressionExceedsPageWidth (genTypeList funsNode)
    | _ -> autoIndentAndNlnIfExpressionExceedsPageWidth (genType t)

let genField (node: FieldNode) =
    let genAccessAndFieldContent =
        genAccessOpt node.Accessibility
        +> (match node.Name with
            | None -> genType node.Type
            | Some name ->
                genSingleTextNode name
                +> sepColon
                +> autoIndentAndNlnTypeUnlessStroustrup genType node.Type)

    genXml node.XmlDoc
    +> genAttributes node.Attributes
    +> optSingle (fun lk -> genMultipleTextsNode lk +> sepSpace) node.LeadingKeyword
    +> optSingle (fun mk -> genSingleTextNode mk +> onlyIfNot mk.HasContentAfter sepSpace) node.MutableKeyword
    +> ifElseCtx hasWriteBeforeNewlineContent (indentSepNlnUnindent genAccessAndFieldContent) genAccessAndFieldContent
    |> genNode node

let genUnionCase (hasVerticalBar: bool) (node: UnionCaseNode) =
    let shortExpr = col sepStar node.Fields genField

    let longExpr =
        indentSepNlnUnindent (atCurrentColumn (col (sepStar +> sepNln) node.Fields genField))

    let genBar =
        match node.Bar with
        | Some bar -> ifElse hasVerticalBar (genSingleTextNodeWithSpaceSuffix sepSpace bar) (genNode bar sepNone)
        | None -> onlyIf hasVerticalBar sepBar

    genXml node.XmlDoc
    +> genBar
    +> atCurrentColumn (
        // If the bar has a comment after, add a newline and print the identifier on the same column on the next line.
        sepNlnWhenWriteBeforeNewlineNotEmpty
        +> genOnelinerAttributes node.Attributes
        +> genSingleTextNode node.Identifier
        +> onlyIf (List.isNotEmpty node.Fields) wordOf
    )
    +> onlyIf (List.isNotEmpty node.Fields) (expressionFitsOnRestOfLine shortExpr longExpr)
    |> genNode node

let genTypeAndParam (genTypeName: Context -> Context) (tds: TyparDecls option) =
    match tds with
    | None -> genTypeName
    | Some(TyparDecls.PostfixList _) -> genTypeName +> optSingle genTyparDecls tds
    | Some(TyparDecls.PrefixList _) -> optSingle (fun tds -> genTyparDecls tds +> sepSpace) tds +> genTypeName
    | Some(TyparDecls.SinglePrefix singlePrefixNode) -> genTyparDecl true singlePrefixNode +> sepSpace +> genTypeName

let genInlineOpt (inlineNode: SingleTextNode option) =
    match inlineNode with
    | None -> sepNone
    | Some inlineNode -> genSingleTextNodeWithSpaceSuffix sepSpace inlineNode

let genVal (node: ValNode) (optGetSet: MultipleTextsNode option) =
    let genOptExpr =
        match node.Equals, node.Expr with
        | Some eq, Some e -> sepSpace +> genSingleTextNode eq +> sepSpace +> genExpr e
        | _ -> sepNone

    genXml node.XmlDoc
    +> genAttributes node.Attributes
    +> optSingle (fun lk -> genMultipleTextsNode lk +> sepSpace) node.LeadingKeyword
    +> genInlineOpt node.Inline
    +> onlyIf node.IsMutable (!- "mutable ")
    +> genAccessOpt node.Accessibility
    +> genTypeAndParam (genSingleTextNode node.Identifier) node.TypeParams
    +> ifElse (Option.isSome node.TypeParams) sepColonWithSpacesFixed sepColon
    +> genTypeInSignature node.Type
    +> optSingle (fun gs -> sepSpace +> genMultipleTextsNode gs) optGetSet
    +> genOptExpr
    |> genNode node

let genMemberDefnList mds =
    match mds with
    | [] -> sepNone
    | _ -> colWithNlnWhenMappedNodeIsMultiline false MemberDefn.Node genMemberDefn mds

let genMemberDefn (md: MemberDefn) =
    match md with
    | MemberDefn.ImplicitInherit ic ->
        genSingleTextNode ic.InheritKeyword
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genInheritConstructor ic)
        |> genNode (InheritConstructor.Node ic)
    | MemberDefn.Inherit node ->
        genSingleTextNode node.Inherit +> sepSpace +> genType node.BaseType
        |> genNode node
    | MemberDefn.ValField node -> genField node
    | MemberDefn.Member node -> genBinding node
    | MemberDefn.ExternBinding node -> genExternBinding node
    | MemberDefn.DoExpr node -> genExpr (Expr.Single node)
    | MemberDefn.ExplicitCtor node ->
        let short =
            genAccessOpt node.Accessibility
            +> genSingleTextNode node.New
            +> sepSpaceBeforeClassConstructor
            +> genPat node.Pattern
            +> optSingle (fun alias -> sepSpace +> !- "as" +> sepSpace +> genSingleTextNode alias) node.Alias
            +> sepSpace
            +> genSingleTextNode node.Equals
            +> sepSpace
            +> genExpr node.Expr

        let long =
            leadingExpressionIsMultiline
                (genAccessOpt node.Accessibility
                 +> genSingleTextNode node.New
                 +> sepSpaceBeforeClassConstructor
                 +> autoIndentAndNlnIfExpressionExceedsPageWidth (genLongParenPatParameter node.Pattern)
                 +> optSingle (fun alias -> sepSpace +> !- "as" +> sepSpace +> genSingleTextNode alias) node.Alias)
                (fun isMultiline ctx ->
                    let genExpr = genExpr node.Expr
                    let short = genSingleTextNode node.Equals +> sepSpace +> genExpr

                    let long ctx =
                        if ctx.Config.AlternativeLongMemberDefinitions then
                            indentSepNlnUnindent (genSingleTextNode node.Equals +> sepNln +> genExpr) ctx
                        else
                            (sepSpace +> genSingleTextNode node.Equals +> indentSepNlnUnindent genExpr) ctx

                    if isMultiline then
                        long ctx
                    else
                        expressionFitsOnRestOfLine short long ctx)

        genXml node.XmlDoc
        +> genAttributes node.Attributes
        +> expressionFitsOnRestOfLine short long
        |> genNode (MemberDefn.Node md)
    | MemberDefn.LetBinding node -> genBindings true node.Bindings |> genNode (MemberDefn.Node md)
    | MemberDefn.Interface node ->
        genSingleTextNode node.Interface
        +> sepSpace
        +> genType node.Type
        +> optSingle
            (fun withNode ->
                sepSpace
                +> genSingleTextNode withNode
                +> indentSepNlnUnindent (genMemberDefnList node.Members))
            node.With
        |> genNode (MemberDefn.Node md)
    | MemberDefn.AutoProperty node ->
        genXml node.XmlDoc
        +> genAttributes node.Attributes
        +> genMultipleTextsNode node.LeadingKeyword
        +> sepSpace
        +> genAccessOpt node.Accessibility
        +> genSingleTextNode node.Identifier
        +> optPre sepColon sepNone node.Type genType
        +> sepSpace
        +> genSingleTextNode node.Equals
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (
            genExpr node.Expr
            +> optSingle (fun gs -> sepSpace +> genMultipleTextsNode gs) node.WithGetSet
        )
        |> genNode (MemberDefn.Node md)
    | MemberDefn.AbstractSlot node ->
        genXml node.XmlDoc
        +> genAttributes node.Attributes
        +> genMultipleTextsNode node.LeadingKeyword
        +> sepSpace
        +> genSingleTextNode node.Identifier
        +> optSingle genTyparDecls node.TypeParams
        +> ifElse node.TypeParams.IsSome sepColonWithSpacesFixed sepColon
        +> genTypeInSignature node.Type
        +> optSingle (fun gs -> sepSpace +> genMultipleTextsNode gs) node.WithGetSet
        |> genNode (MemberDefn.Node md)
    | MemberDefn.PropertyGetSet node ->
        let genProperty (node: PropertyGetSetBindingNode) =
            genInlineOpt node.Inline
            +> genOnelinerAttributes node.Attributes
            +> genAccessOpt node.Accessibility
            +> genSingleTextNode node.LeadingKeyword
            +> sepSpace
            +> col sepSpace node.Parameters genPat
            +> genReturnTypeBinding node.ReturnType
            +> sepSpace
            +> genSingleTextNode node.Equals
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr)

        genXml node.XmlDoc
        +> genAttributes node.Attributes
        +> genMultipleTextsNode node.LeadingKeyword
        +> sepSpace
        +> genInlineOpt node.Inline
        +> genAccessOpt node.Accessibility
        +> genIdentListNode node.MemberName
        +> indent
        +> sepNln
        +> genSingleTextNode node.WithKeyword
        +> sepSpace
        +> genProperty node.FirstBinding
        +> optSingle (fun a -> sepNln +> genSingleTextNode a +> sepSpace) node.AndKeyword
        +> optSingle genProperty node.LastBinding
        +> unindent
        |> genNode (MemberDefn.Node md)
    | MemberDefn.SigMember node -> genVal node.Val node.WithGetSet |> genNode node

let genException (node: ExceptionDefnNode) =
    genXml node.XmlDoc
    +> genAttributes node.Attributes
    +> !- "exception "
    +> genAccessOpt node.Accessibility
    +> genUnionCase false node.UnionCase
    +> onlyIf
        (not node.Members.IsEmpty)
        (sepSpace
         +> optSingle genSingleTextNode node.WithKeyword
         +> indentSepNlnUnindent (genMemberDefnList node.Members))
    |> genNode node

let genModuleDecl (md: ModuleDecl) =
    match md with
    | ModuleDecl.OpenList ol -> genOpenList ol
    | ModuleDecl.HashDirectiveList node -> col sepNln node.HashDirectives genParsedHashDirective |> genNode node
    | ModuleDecl.Attributes node -> genAttributes node.Attributes +> genExpr node.Expr |> genNode node
    | ModuleDecl.DeclExpr e -> genExpr e
    | ModuleDecl.Exception node -> genException node
    | ModuleDecl.ExternBinding node -> genExternBinding node
    | ModuleDecl.TopLevelBinding b -> genBinding b
    | ModuleDecl.ModuleAbbrev node ->
        genSingleTextNode node.Module
        +> sepSpace
        +> genSingleTextNode node.Name
        +> sepSpace
        +> sepEqFixed
        +> sepSpace
        +> genIdentListNode node.Alias
        |> genNode (ModuleDecl.Node md)
    | ModuleDecl.NestedModule node ->
        // Workaround for https://github.com/fsprojects/fantomas/issues/2867
        let hasTriviaAfterLeadingKeyword =
            hasTriviaAfterLeadingKeyword node.Identifier node.Accessibility

        genXml node.XmlDoc
        +> genAttributes node.Attributes
        +> genSingleTextNode node.Module
        +> sepSpace
        +> onlyIf hasTriviaAfterLeadingKeyword indent
        +> genAccessOpt node.Accessibility
        +> onlyIf node.IsRecursive (sepSpace +> !- "rec" +> sepSpace)
        +> genIdentListNode node.Identifier
        +> onlyIf hasTriviaAfterLeadingKeyword unindent
        +> sepSpace
        +> genSingleTextNode node.Equals
        +> indentSepNlnUnindent (
            ifElse
                (List.isEmpty node.Declarations)
                (!- "begin end")
                (colWithNlnWhenMappedNodeIsMultiline false ModuleDecl.Node genModuleDecl node.Declarations)
        )

        |> genNode (ModuleDecl.Node md)
    | ModuleDecl.TypeDefn td -> genTypeDefn td
    | ModuleDecl.Val node -> genVal node None

let sepNlnUnlessContentBefore (node: Node) =
    if not node.HasContentBefore then sepNln else sepNone

let colWithNlnWhenMappedNodeIsMultiline<'n>
    (withUseConfig: bool)
    (mapNode: 'n -> Node)
    (f: 'n -> Context -> Context)
    (nodes: 'n list)
    : Context -> Context =
    nodes
    |> List.map (fun n -> ColMultilineItem(f n, (mapNode >> sepNlnUnlessContentBefore) n))
    |> (if withUseConfig then
            colWithNlnWhenItemIsMultiline
        else
            colWithNlnWhenItemIsMultilineUsingConfig)

let colWithNlnWhenNodeIsMultiline<'n when 'n :> Node>
    (withUseConfig: bool)
    (f: 'n -> Context -> Context)
    (nodes: 'n list)
    : Context -> Context =
    colWithNlnWhenMappedNodeIsMultiline<'n> withUseConfig (fun n -> n :> Node) f nodes

let genModule (m: ModuleOrNamespaceNode) =
    let newline =
        match m.Declarations with
        | [] -> onlyIf m.HasContentAfter sepNln
        | h :: _ -> sepNln +> sepNlnUnlessContentBefore (ModuleDecl.Node h)

    optSingle
        (fun (header: ModuleOrNamespaceHeaderNode) ->
            (genXml header.XmlDoc
             +> genAttributes header.Attributes
             +> genMultipleTextsNode header.LeadingKeyword
             +> sepSpace
             +> genAccessOpt header.Accessibility
             +> onlyIf header.IsRecursive (sepSpace +> !- "rec" +> sepSpace)
             +> optSingle genIdentListNode header.Name
             |> genNode header)
            +> newline)
        m.Header
    +> colWithNlnWhenMappedNodeIsMultiline false ModuleDecl.Node genModuleDecl m.Declarations
    |> genNode m

let addFinalNewline ctx =
    let lastEvent = ctx.WriterEvents.TryHead

    match lastEvent with
    | Some WriteLineBecauseOfTrivia ->
        if ctx.Config.InsertFinalNewline then
            ctx
        else
            // Due to trivia the last event is a newline, if insert_final_newline is false, we need to remove it.
            { ctx with
                WriterEvents = ctx.WriterEvents.Tail
                WriterModel =
                    { ctx.WriterModel with
                        Lines = List.tail ctx.WriterModel.Lines } }
    | _ -> onlyIf ctx.Config.InsertFinalNewline sepNln ctx

let genFile (oak: Oak) =
    (col sepNln oak.ParsedHashDirectives genParsedHashDirective
     +> (if oak.ParsedHashDirectives.IsEmpty then sepNone else sepNln)
     +> col sepNln oak.ModulesOrNamespaces genModule
     |> genNode oak)
    +> addFinalNewline
