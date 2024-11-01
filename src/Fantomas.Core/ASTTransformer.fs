module internal rec Fantomas.Core.ASTTransformer

#nowarn "FS1182"

open System.Collections.Generic
open System.Text.RegularExpressions
open Fantomas.FCS.Text
open Fantomas.FCS.Text.Range
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Fantomas.Core.RangePatterns
open Fantomas.Core.SyntaxOak
open Microsoft.FSharp.Core

[<NoComparison>]
type CreationAide =
    { SourceText: ISourceText option }

    member x.TextFromSource fallback range =
        match x.SourceText with
        | None -> fallback ()
        | Some sourceText -> sourceText.GetSubTextFromRange range

let stn text range = SingleTextNode(text, range)

let mkIdent (ident: Ident) =
    let width = ident.idRange.EndColumn - ident.idRange.StartColumn

    let text =
        if ident.idText.Length + 4 = width then
            // add backticks
            $"``%s{ident.idText}``"
        else
            ident.idText

    stn text ident.idRange

let mkSynIdent (SynIdent(ident, trivia)) =
    match trivia with
    | None -> mkIdent ident
    | Some(IdentTrivia.OriginalNotation text) -> stn text ident.idRange
    | Some(IdentTrivia.OriginalNotationWithParen(_, text, _)) -> stn $"(%s{text})" ident.idRange
    | Some(IdentTrivia.HasParenthesis _) -> stn $"(%s{ident.idText})" ident.idRange

let mkSynLongIdent (sli: SynLongIdent) =
    match sli.IdentsWithTrivia with
    | [] -> IdentListNode.Empty
    | [ single ] -> IdentListNode([ IdentifierOrDot.Ident(mkSynIdent single) ], sli.Range)
    | head :: tail ->
        assert (tail.Length = sli.Dots.Length)

        let rest =
            (sli.Dots, tail)
            ||> List.zip
            |> List.collect (fun (dot, ident) ->
                [ IdentifierOrDot.KnownDot(stn "." dot)
                  IdentifierOrDot.Ident(mkSynIdent ident) ])

        IdentListNode(IdentifierOrDot.Ident(mkSynIdent head) :: rest, sli.Range)

let mkLongIdent (longIdent: LongIdent) : IdentListNode =
    match longIdent with
    | [] -> IdentListNode.Empty
    | [ single ] -> IdentListNode([ IdentifierOrDot.Ident(mkIdent single) ], single.idRange)
    | head :: tail ->
        let rest =
            tail
            |> List.collect (fun ident -> [ IdentifierOrDot.UnknownDot; IdentifierOrDot.Ident(mkIdent ident) ])

        let range =
            longIdent |> List.map (fun ident -> ident.idRange) |> List.reduce unionRanges

        IdentListNode(IdentifierOrDot.Ident(mkIdent head) :: rest, range)

let mkSynAccess (vis: SynAccess option) =
    match vis with
    | None -> None
    | Some(SynAccess.Internal range) -> Some(stn "internal" range)
    | Some(SynAccess.Private range) -> Some(stn "private" range)
    | Some(SynAccess.Public range) -> Some(stn "public" range)

let parseExpressionInSynBinding returnInfo expr =
    match returnInfo, expr with
    | Some(SynBindingReturnInfo(typeName = t1)), SynExpr.Typed(e, t2, _) when RangeHelpers.rangeEq t1.Range t2.Range ->
        e
    | _ -> expr

let mkConstString (creationAide: CreationAide) (stringKind: SynStringKind) (value: string) (range: range) =
    let escaped = Regex.Replace(value, "\"{1}", "\\\"")

    let fallback () =
        match stringKind with
        | SynStringKind.Regular -> sprintf "\"%s\"" escaped
        | SynStringKind.Verbatim -> sprintf "@\"%s\"" escaped
        | SynStringKind.TripleQuote -> sprintf "\"\"\"%s\"\"\"" escaped

    stn (creationAide.TextFromSource fallback range) range

let mkParsedHashDirective (creationAide: CreationAide) (ParsedHashDirective(ident, args, range)) =
    let args =
        args
        |> List.map (function
            | ParsedHashDirectiveArgument.String(value, stringKind, range) ->
                mkConstString creationAide stringKind value range |> Choice1Of2
            | ParsedHashDirectiveArgument.SourceIdentifier(identifier, _, range) -> stn identifier range |> Choice1Of2
            | ParsedHashDirectiveArgument.Int32(value, range) ->
                let text = creationAide.TextFromSource (fun () -> $"%A{value}") range
                stn text range |> Choice1Of2
            | ParsedHashDirectiveArgument.Ident(value = ident) -> mkIdent ident |> Choice1Of2
            | ParsedHashDirectiveArgument.LongIdent(value = lid) -> mkSynLongIdent lid |> Choice2Of2)

    ParsedHashDirectiveNode(ident, args, range)

let mkConstant (creationAide: CreationAide) c r : Constant =
    let orElse fallback =
        stn (creationAide.TextFromSource (fun () -> fallback) r) r |> Constant.FromText

    match c with
    | SynConst.Unit -> mkUnit r |> Constant.Unit
    | SynConst.Bool b -> stn (if b then "true" else "false") r |> Constant.FromText
    | SynConst.Byte v -> orElse $"%A{v}"
    | SynConst.SByte v -> orElse $"%A{v}"
    | SynConst.Int16 v -> orElse $"%A{v}"
    | SynConst.Int32 v -> orElse $"%A{v}"
    | SynConst.Int64 v -> orElse $"%A{v}"
    | SynConst.UInt16 v -> orElse $"%A{v}"
    | SynConst.UInt16s v -> orElse $"%A{v}"
    | SynConst.UInt32 v -> orElse $"%A{v}"
    | SynConst.UInt64 v -> orElse $"%A{v}"
    | SynConst.Double v -> orElse $"%A{v}"
    | SynConst.Single v -> orElse $"%A{v}"
    | SynConst.Decimal v -> orElse $"%A{v}"
    | SynConst.IntPtr v -> orElse $"%A{v}"
    | SynConst.UIntPtr v -> orElse $"%A{v}"
    | SynConst.UserNum(v, s) ->
        let fallback () = $"%s{v}%s{s}"
        stn (creationAide.TextFromSource fallback r) r |> Constant.FromText
    | SynConst.String(value, stringKind, r) -> mkConstString creationAide stringKind value r |> Constant.FromText
    | SynConst.Char c ->
        let escapedChar =
            match c with
            | '\r' -> @"'\r'"
            | '\n' -> @"'\n'"
            | '\t' -> @"'\t'"
            | '\\' -> @"'\\'"
            | '\b' -> @"'\b'"
            | '\f' -> @"'\f'"
            | _ -> $"'%c{c}'"

        orElse escapedChar
    | SynConst.Bytes(bytes, _, r) ->
        let fallback () =
            let content =
                System.String(Array.map (fun (byte: byte) -> System.Convert.ToChar(byte)) bytes)

            $"\"%s{content}\"B"

        stn (creationAide.TextFromSource fallback r) r |> Constant.FromText
    | SynConst.Measure(c, numberRange, measure, trivia) ->
        let uOfMRange =
            mkRange trivia.LessRange.FileName trivia.LessRange.Start trivia.GreaterRange.End

        let unitOfMeasure =
            UnitOfMeasureNode(
                stn "<" trivia.LessRange,
                mkMeasure creationAide measure,
                stn ">" trivia.GreaterRange,
                uOfMRange
            )

        ConstantMeasureNode(mkConstant creationAide c numberRange, unitOfMeasure, r)
        |> Constant.Measure
    | SynConst.SourceIdentifier(c, _, r) -> stn c r |> Constant.FromText

let mkMeasure (creationAide: CreationAide) (measure: SynMeasure) : Measure =
    match measure with
    | SynMeasure.Var(typar, _) -> mkSynTypar typar |> Measure.Single
    | SynMeasure.Anon m -> stn "_" m |> Measure.Single
    | SynMeasure.One m -> stn "1" m |> Measure.Single
    | SynMeasure.Product(m1, mAsterisk, m2, m) ->
        MeasureOperatorNode(mkMeasure creationAide m1, stn "*" mAsterisk, mkMeasure creationAide m2, m)
        |> Measure.Operator
    | SynMeasure.Divide(m1, mSlash, m2, m) ->
        let lhs = m1 |> Option.map (mkMeasure creationAide)

        MeasureDivideNode(lhs, stn "/" mSlash, mkMeasure creationAide m2, m)
        |> Measure.Divide
    | SynMeasure.Power(ms, mCaret, rat, m) ->
        MeasurePowerNode(mkMeasure creationAide ms, stn "^" mCaret, mkSynRationalConst creationAide rat, m)
        |> Measure.Power
    | SynMeasure.Named(lid, _) -> mkLongIdent lid |> Measure.Multiple
    | SynMeasure.Paren(measure, StartEndRange 1 (mOpen, m, mClose)) ->
        MeasureParenNode(stn "(" mOpen, mkMeasure creationAide measure, stn ")" mClose, m)
        |> Measure.Paren
    | SynMeasure.Seq(ms, m) -> MeasureSequenceNode(List.map (mkMeasure creationAide) ms, m) |> Measure.Seq

let mkAttribute (creationAide: CreationAide) (a: SynAttribute) =
    let expr =
        match a.ArgExpr with
        | UnitExpr _ -> None
        | e -> mkExpr creationAide e |> Some

    AttributeNode(mkSynLongIdent a.TypeName, expr, Option.map mkIdent a.Target, a.Range)

let mkAttributeList (creationAide: CreationAide) (al: SynAttributeList) : AttributeListNode =
    let attributes = List.map (mkAttribute creationAide) al.Attributes

    let opening, closing =
        match al.Range with
        | StartEndRange 2 (s, _, e) -> stn "[<" s, stn ">]" e

    AttributeListNode(opening, attributes, closing, al.Range)

let mkAttributes (creationAide: CreationAide) (al: SynAttributeList list) : MultipleAttributeListNode option =
    match al with
    | [] -> None
    | _ ->
        let attributeLists = List.map (mkAttributeList creationAide) al
        let range = attributeLists |> List.map (fun al -> al.Range) |> combineRanges
        Some(MultipleAttributeListNode(attributeLists, range))

/// Only used to get items of SynExpr.ArrayOrListComputed
/// We can safely assume the SequentialTrivia.SeparatorRange is not going to be `then`.
let (|Sequentials|_|) e =
    let rec visit (e: SynExpr) (finalContinuation: SynExpr list -> SynExpr list) : SynExpr list =
        match e with
        | SynExpr.Sequential(expr1 = e1; expr2 = e2) ->

            visit e2 (fun xs -> e1 :: xs |> finalContinuation)
        | e -> finalContinuation [ e ]

    match e with
    | SynExpr.Sequential(expr1 = e1; expr2 = e2) ->
        let xs = visit e2 id
        Some(e1 :: xs)
    | _ -> None

let mkOpenAndCloseForArrayOrList isArray range =
    if isArray then
        let (StartEndRange 2 (mO, _, mC)) = range
        stn "[|" mO, stn "|]" mC
    else
        let (StartEndRange 1 (mO, _, mC)) = range
        stn "[" mO, stn "]" mC

let mkInheritConstructor (creationAide: CreationAide) (t: SynType) (e: SynExpr) (mInherit: range) (m: range) =
    let inheritNode = stn "inherit" mInherit
    let m = unionRanges mInherit m

    match e with
    | SynExpr.Const(constant = SynConst.Unit; range = StartEndRange 1 (mOpen, unitRange, mClose)) ->
        // The unit expression could have been added artificially.
        if unitRange.StartColumn + 2 = unitRange.EndColumn then
            InheritConstructorUnitNode(inheritNode, mkType creationAide t, stn "(" mOpen, stn ")" mClose, m)
            |> InheritConstructor.Unit
        else
            InheritConstructorTypeOnlyNode(inheritNode, mkType creationAide t, m)
            |> InheritConstructor.TypeOnly
    | SynExpr.Paren _ as px ->
        InheritConstructorParenNode(inheritNode, mkType creationAide t, mkExpr creationAide px, m)
        |> InheritConstructor.Paren
    | _ ->
        InheritConstructorOtherNode(inheritNode, mkType creationAide t, mkExpr creationAide e, m)
        |> InheritConstructor.Other

let mkTuple (creationAide: CreationAide) (exprs: SynExpr list) (commas: range list) (m: range) =
    match exprs with
    | [] -> failwith "SynExpr.Tuple with no elements"
    | head :: tail ->
        let rest =
            assert (tail.Length = commas.Length)

            List.zip commas tail
            |> List.collect (fun (c, e) -> [ yield Choice2Of2(stn "," c); yield Choice1Of2(mkExpr creationAide e) ])

        ExprTupleNode([ yield Choice1Of2(mkExpr creationAide head); yield! rest ], m)

/// Unfold a list of let bindings
/// Recursive and use properties have to be determined at this point
let rec (|LetOrUses|_|) =
    function
    | SynExpr.LetOrUse(_, _, xs, LetOrUses(ys, e), _, trivia) ->
        let xs' = List.mapWithLast (fun b -> b, None) (fun b -> b, trivia.InKeyword) xs
        Some(xs' @ ys, e)
    | SynExpr.LetOrUse(_, _, xs, e, _, trivia) ->
        let xs' = List.mapWithLast (fun b -> b, None) (fun b -> b, trivia.InKeyword) xs
        Some(xs', e)
    | _ -> None

let rec collectComputationExpressionStatements
    (creationAide: CreationAide)
    (e: SynExpr)
    (finalContinuation: ComputationExpressionStatement list -> ComputationExpressionStatement list)
    : ComputationExpressionStatement list =
    match e with
    | LetOrUses(bindings, body) ->
        let bindings =
            bindings
            |> List.map (fun (b, inNode) ->
                let b: BindingNode = mkBinding creationAide b

                let inNode, m =
                    match inNode with
                    | None -> None, b.Range
                    | Some mIn -> Some(stn "in" mIn), unionRanges b.Range mIn

                ExprLetOrUseNode(b, inNode, m)
                |> ComputationExpressionStatement.LetOrUseStatement)

        collectComputationExpressionStatements creationAide body (fun bodyStatements ->
            [ yield! bindings; yield! bodyStatements ] |> finalContinuation)
    | SynExpr.LetOrUseBang(_,
                           isUse,
                           _,
                           pat,
                           expr,
                           andBangs,
                           body,
                           StartRange 4 (mLeading, _m),
                           { EqualsRange = Some mEq }) ->
        let letOrUseBang =
            ExprLetOrUseBangNode(
                stn (if isUse then "use!" else "let!") mLeading,
                mkPat creationAide pat,
                stn "=" mEq,
                mkExpr creationAide expr,
                unionRanges mLeading expr.Range
            )
            |> ComputationExpressionStatement.LetOrUseBangStatement

        let andBangs =
            andBangs
            |> List.map (fun (SynExprAndBang(_, _, _, ap, ae, StartRange 4 (mAnd, m), trivia)) ->
                ExprAndBang(
                    stn "and!" mAnd,
                    mkPat creationAide ap,
                    stn "=" trivia.EqualsRange,
                    mkExpr creationAide ae,
                    m
                )
                |> ComputationExpressionStatement.AndBangStatement)

        collectComputationExpressionStatements creationAide body (fun bodyStatements ->
            [ letOrUseBang; yield! andBangs; yield! bodyStatements ] |> finalContinuation)
    | SynExpr.Sequential(expr1 = e1; expr2 = e2; trivia = trivia) ->
        let continuations
            : ((ComputationExpressionStatement list -> ComputationExpressionStatement list)
                  -> ComputationExpressionStatement list) list =
            let c2 =
                match trivia.SeparatorRange with
                // detect then keyword in explicit constructor
                | Some mThen when mThen.StartColumn + 4 = mThen.EndColumn ->
                    let thenNode = stn "then" mThen
                    let expr = mkExpr creationAide e2
                    let m = unionRanges mThen e2.Range
                    let node = ExprExplicitConstructorThenExpr(thenNode, expr, m)

                    fun finalContinuation ->
                        finalContinuation
                            [ ComputationExpressionStatement.OtherStatement(Expr.ExplicitConstructorThenExpr node) ]
                | _ -> collectComputationExpressionStatements creationAide e2

            [ collectComputationExpressionStatements creationAide e1; c2 ]

        let finalContinuation (nodes: ComputationExpressionStatement list list) : ComputationExpressionStatement list =
            List.collect id nodes |> finalContinuation

        Continuation.sequence continuations finalContinuation
    | expr -> finalContinuation [ ComputationExpressionStatement.OtherStatement(mkExpr creationAide expr) ]

/// Process compiler-generated matches in an appropriate way
let rec private skipGeneratedLambdas expr =
    match expr with
    | SynExpr.Lambda(inLambdaSeq = true; body = bodyExpr) -> skipGeneratedLambdas bodyExpr
    | _ -> expr

and skipGeneratedMatch expr =
    match expr with
    | SynExpr.Match(_, _, [ SynMatchClause.SynMatchClause(resultExpr = innerExpr) as clause ], matchRange, _) when
        matchRange.Start = clause.Range.Start
        ->
        skipGeneratedMatch innerExpr
    | _ -> expr

let inline private getLambdaBodyExpr expr =
    let skippedLambdas = skipGeneratedLambdas expr
    skipGeneratedMatch skippedLambdas

let mkLambda creationAide pats mArrow body (StartRange 3 (mFun, m)) : ExprLambdaNode =
    let body = getLambdaBodyExpr body
    ExprLambdaNode(stn "fun" mFun, List.map (mkPat creationAide) pats, stn "->" mArrow, mkExpr creationAide body, m)

let mkSynMatchClause creationAide (SynMatchClause(p, eo, e, range, _, trivia)) : MatchClauseNode =
    let fullRange =
        match trivia.BarRange with
        | None -> range
        | Some barRange -> unionRanges barRange range

    let arrowRange =
        match trivia.ArrowRange with
        | Some r -> r
        | None -> failwith $"unable to get the arrow range from trivia in %s{nameof mkSynMatchClause}"

    MatchClauseNode(
        Option.map (stn "|") trivia.BarRange,
        mkPat creationAide p,
        Option.map (mkExpr creationAide) eo,
        stn "->" arrowRange,
        mkExpr creationAide e,
        fullRange
    )

let (|ColonColonInfixApp|_|) =
    function
    | SynExpr.App(
        isInfix = true
        funcExpr = SynExpr.LongIdent(
            longDotId = SynLongIdent([ operatorIdent ], [], [ Some(IdentTrivia.OriginalNotation "::") ]))
        argExpr = SynExpr.Tuple(exprs = [ e1; e2 ])) -> Some(e1, stn "::" operatorIdent.idRange, e2)
    | _ -> None

let (|InfixApp|_|) synExpr =
    match synExpr with
    | ColonColonInfixApp(lhs, operator, rhs) -> Some(lhs, operator, rhs)
    | SynExpr.App(
        funcExpr = SynExpr.App(
            isInfix = true
            funcExpr = SynExpr.LongIdent(
                longDotId = SynLongIdent([ operatorIdent ], [], [ Some(IdentTrivia.OriginalNotation operator) ]))
            argExpr = e1)
        argExpr = e2) -> Some(e1, stn operator operatorIdent.idRange, e2)
    | _ -> None

let (|IndexWithoutDot|_|) expr =
    match expr with
    | SynExpr.App(ExprAtomicFlag.Atomic, false, identifierExpr, SynExpr.ArrayOrListComputed(false, indexExpr, _), _) ->
        Some(identifierExpr, indexExpr)
    | SynExpr.App(ExprAtomicFlag.NonAtomic,
                  false,
                  identifierExpr,
                  (SynExpr.ArrayOrListComputed(isArray = false; expr = indexExpr) as argExpr),
                  _) when (RangeHelpers.isAdjacentTo identifierExpr.Range argExpr.Range) ->
        Some(identifierExpr, indexExpr)
    | _ -> None

// :: a ::
let (|MultipleConsInfixApps|_|) expr =
    let rec visit expr (headAndLastOperator: (SynExpr * SingleTextNode) option) (xs: Queue<SingleTextNode * SynExpr>) =
        match expr with
        | ColonColonInfixApp(lhs, operator, rhs) ->
            match headAndLastOperator with
            | None -> visit rhs (Some(lhs, operator)) xs
            | Some(head, lastOperator) ->
                xs.Enqueue(lastOperator, lhs)
                visit rhs (Some(head, operator)) xs
        | e ->
            match headAndLastOperator with
            | None -> e, xs
            | Some(head, lastOperator) ->
                xs.Enqueue(lastOperator, e)
                head, xs

    match expr with
    | ColonColonInfixApp _ ->
        let head, xs = visit expr None (Queue())
        if xs.Count < 2 then None else Some(head, Seq.toList xs)
    | _ -> None

let rightOperators = set [| "@"; "**"; "^"; ":=" |]

let (|SameInfixApps|_|) expr =
    let rec visitLeft sameOperator expr continuation =
        match expr with
        | InfixApp(lhs, operator, rhs) when operator.Text = sameOperator ->
            visitLeft sameOperator lhs (fun (head, xs: Queue<SingleTextNode * SynExpr>) ->
                xs.Enqueue(operator, rhs)
                continuation (head, xs))
        | e -> continuation (e, Queue())

    let rec visitRight
        (sameOperator: string)
        (expr: SynExpr)
        (headAndLastOperator: (SynExpr * SingleTextNode) option)
        (xs: Queue<SingleTextNode * SynExpr>)
        : SynExpr * Queue<SingleTextNode * SynExpr> =
        match expr with
        | SynExpr.App(ExprAtomicFlag.NonAtomic,
                      false,
                      SynExpr.App(
                          isInfix = true
                          funcExpr = SynExpr.LongIdent(
                              longDotId = SynLongIdent(
                                  id = [ operatorIdent ]; trivia = [ Some(IdentTrivia.OriginalNotation lhsOperator) ]))
                          argExpr = leftExpr),
                      rhs,
                      _) when (lhsOperator = sameOperator) ->
            let operator = stn lhsOperator operatorIdent.idRange

            match headAndLastOperator with
            | None ->
                // Start of the infix chain, the leftExpr is the utmost left
                visitRight sameOperator rhs (Some(leftExpr, operator)) xs
            | Some(head, lastOperator) ->
                // Continue collecting
                xs.Enqueue(lastOperator, leftExpr)
                visitRight sameOperator rhs (Some(head, operator)) xs
        | e ->
            match headAndLastOperator with
            | None -> e, xs
            | Some(head, lastOperator) ->
                xs.Enqueue(lastOperator, e)
                head, xs

    match expr with
    | InfixApp(_, operator, _) ->
        let isRight =
            Set.exists (fun (rOp: string) -> String.startsWithOrdinal rOp operator.Text) rightOperators

        let head, xs =
            if isRight then
                visitRight operator.Text expr None (Queue())
            else
                visitLeft operator.Text expr id

        if xs.Count < 2 then None else Some(head, Seq.toList xs)
    | _ -> None

let newLineInfixOps = set [ "|>"; "||>"; "|||>"; ">>"; ">>=" ]

let (|NewlineInfixApps|_|) expr =
    let rec visit expr continuation =
        match expr with
        | InfixApp(lhs, operator, rhs) when newLineInfixOps.Contains operator.Text ->
            visit lhs (fun (head, xs: Queue<SingleTextNode * SynExpr>) ->
                xs.Enqueue(operator, rhs)
                continuation (head, xs))
        | e -> continuation (e, Queue())

    match expr with
    | InfixApp _ ->
        let head, xs = visit expr id
        if xs.Count < 2 then None else Some(head, Seq.toList xs)
    | _ -> None

let rec (|ElIf|_|) =
    function
    | SynExpr.IfThenElse(e1,
                         e2,
                         Some(ElIf((elifNode: Choice<SingleTextNode, range * range>, eshE1, eshThenKw, eshE2) :: es,
                                   elseInfo)),
                         _,
                         _,
                         _,
                         trivia) ->
        let ifNode =
            stn (if trivia.IsElif then "elif" else "if") trivia.IfKeyword |> Choice1Of2

        let elifNode =
            match trivia.ElseKeyword with
            | None -> elifNode
            | Some mElse ->
                match elifNode with
                | Choice1Of2 ifNode -> Choice2Of2(mElse, ifNode.Range)
                | Choice2Of2 _ -> failwith "Cannot merge a second else keyword into existing else if"

        Some(
            (ifNode, e1, stn "then" trivia.ThenKeyword, e2)
            :: (elifNode, eshE1, eshThenKw, eshE2)
            :: es,
            elseInfo
        )

    | SynExpr.IfThenElse(e1, e2, e3, _, _, _, trivia) ->
        let elseInfo =
            match trivia.ElseKeyword, e3 with
            | Some elseKw, Some elseExpr -> Some(stn "else" elseKw, elseExpr)
            | _ -> None

        let ifNode =
            stn (if trivia.IsElif then "elif" else "if") trivia.IfKeyword |> Choice1Of2

        Some([ (ifNode, e1, stn "then" trivia.ThenKeyword, e2) ], elseInfo)
    | _ -> None

let (|ConstNumberExpr|_|) =
    function
    | SynExpr.Const(SynConst.Double v, m) -> Some(string<double> v, m)
    | SynExpr.Const(SynConst.Decimal v, m) -> Some(string<decimal> v, m)
    | SynExpr.Const(SynConst.Single v, m) -> Some(string<single> v, m)
    | SynExpr.Const(SynConst.Int16 v, m) -> Some(string<int16> v, m)
    | SynExpr.Const(SynConst.Int32 v, m) -> Some(string<int> v, m)
    | SynExpr.Const(SynConst.Int64 v, m) -> Some(string<int64> v, m)
    | _ -> None

let (|App|_|) e =
    let rec visit expr continuation =
        match expr with
        | SynExpr.App(funcExpr = funcExpr; argExpr = argExpr) ->
            visit funcExpr (fun (head, xs: Queue<SynExpr>) ->
                xs.Enqueue(argExpr)
                continuation (head, xs))
        | e -> continuation (e, Queue())

    let head, xs = visit e id
    if xs.Count = 0 then None else Some(head, Seq.toList xs)

let (|ParenLambda|_|) e =
    match e with
    | ParenExpr(lpr, SynExpr.Lambda(_, _, _, _, Some(pats, body), mLambda, { ArrowRange = Some mArrow }), rpr, _) ->
        Some(lpr, pats, mArrow, body, mLambda, rpr)
    | _ -> None

let (|ParenMatchLambda|_|) e =
    match e with
    | ParenExpr(lpr, SynExpr.MatchLambda(_, mFunction, clauses, _, mMatchLambda), rpr, _) ->
        Some(lpr, mFunction, clauses, mMatchLambda, rpr)
    | _ -> None

let mkMatchLambda creationAide mFunction cs m =
    ExprMatchLambdaNode(stn "function" mFunction, List.map (mkSynMatchClause creationAide) cs, m)

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type LinkExpr =
    | Identifier of
        // Could be SynExpr.LongIdent or SynExpr.TypeApp(LongIdent)
        SynExpr
    | Dot of range
    | Expr of SynExpr
    | AppParenLambda of functionName: SynExpr * parenLambda: SynExpr
    | AppParen of functionName: SynExpr * lpr: range * e: SynExpr * rpr: range * pr: range
    | AppUnit of functionName: SynExpr * unit: range
    | IndexExpr of indexExpr: SynExpr

let mkLinksFromSynLongIdent (sli: SynLongIdent) : LinkExpr list =
    let idents =
        List.map (mkLongIdentExprFromSynIdent >> LinkExpr.Identifier) sli.IdentsWithTrivia

    let dots = List.map LinkExpr.Dot sli.Dots

    [ yield! idents; yield! dots ]
    |> List.sortBy (function
        | LinkExpr.Identifier identifierExpr -> identifierExpr.Range.StartLine, identifierExpr.Range.StartColumn
        | LinkExpr.Dot m -> m.StartLine, m.StartColumn
        | LinkExpr.Expr _
        | LinkExpr.AppParenLambda _
        | LinkExpr.AppParen _
        | LinkExpr.AppUnit _
        | LinkExpr.IndexExpr _ -> -1, -1)

let (|UnitExpr|_|) e =
    match e with
    | SynExpr.Const(constant = SynConst.Unit) -> Some e.Range
    | _ -> None

let (|ParenExpr|_|) e =
    match e with
    | SynExpr.Paren(e, lpr, Some rpr, pr) -> Some(lpr, e, rpr, pr)
    | _ -> None

let mkLongIdentExprFromSynIdent (SynIdent(ident, identTrivia)) =
    SynExpr.LongIdent(false, SynLongIdent([ ident ], [], [ identTrivia ]), None, ident.idRange)

let mkLinksFromFunctionName (mkLinkFromExpr: SynExpr -> LinkExpr) (functionName: SynExpr) : LinkExpr list =
    match functionName with
    | SynExpr.TypeApp(SynExpr.LongIdent(longDotId = sli),
                      lessRange,
                      typeArgs,
                      commaRanges,
                      Some greaterRange,
                      typeArgsRange,
                      _) ->
        match sli.IdentsWithTrivia with
        | []
        | [ _ ] -> [ mkLinkFromExpr functionName ]
        | synIdents ->
            let leftLinks = mkLinksFromSynLongIdent sli
            let lastSynIdent = List.last synIdents

            let m =
                let (SynIdent(ident, _)) = lastSynIdent
                unionRanges ident.idRange greaterRange

            let typeAppExpr =
                SynExpr.TypeApp(
                    mkLongIdentExprFromSynIdent lastSynIdent,
                    lessRange,
                    typeArgs,
                    commaRanges,
                    Some greaterRange,
                    typeArgsRange,
                    m
                )

            [ yield! List.cutOffLast leftLinks; yield mkLinkFromExpr typeAppExpr ]

    | SynExpr.LongIdent(longDotId = sli) ->
        match sli.IdentsWithTrivia with
        | []
        | [ _ ] -> [ mkLinkFromExpr functionName ]
        | synIdents ->
            let leftLinks = mkLinksFromSynLongIdent sli
            let lastSynIdent = List.last synIdents

            [ yield! List.cutOffLast leftLinks
              yield (mkLongIdentExprFromSynIdent lastSynIdent |> mkLinkFromExpr) ]
    | e -> [ mkLinkFromExpr e ]

let (|ChainExpr|_|) (e: SynExpr) : LinkExpr list option =
    let rec visit (e: SynExpr) (continuation: LinkExpr list -> LinkExpr list) =
        match e with
        | SynExpr.App(
            isInfix = false
            funcExpr = SynExpr.TypeApp(SynExpr.DotGet _ as funcExpr,
                                       lessRange,
                                       typeArgs,
                                       commaRanges,
                                       Some greaterRange,
                                       typeArgsRange,
                                       _)
            argExpr = ParenExpr _ | UnitExpr _ as argExpr) ->
            visit funcExpr (fun leftLinks ->
                let lastLink =
                    match List.tryLast leftLinks with
                    | Some(LinkExpr.Identifier identifierExpr) ->
                        let typeApp =
                            SynExpr.TypeApp(
                                identifierExpr,
                                lessRange,
                                typeArgs,
                                commaRanges,
                                Some greaterRange,
                                typeArgsRange,
                                unionRanges identifierExpr.Range greaterRange
                            )

                        match argExpr with
                        | UnitExpr mUnit -> [ LinkExpr.AppUnit(typeApp, mUnit) ]
                        | ParenExpr(lpr, innerExpr, rpr, pr) -> [ LinkExpr.AppParen(typeApp, lpr, innerExpr, rpr, pr) ]
                        | _ -> []
                    | _ -> []

                let leftLinks = List.cutOffLast leftLinks

                continuation [ yield! leftLinks; yield! lastLink ])

        | SynExpr.TypeApp(SynExpr.DotGet _ as dotGet,
                          lessRange,
                          typeArgs,
                          commaRanges,
                          Some greaterRange,
                          typeArgsRange,
                          _) ->
            visit dotGet (fun leftLinks ->
                let lastLink =
                    match List.tryLast leftLinks with
                    | Some(LinkExpr.Identifier property) ->
                        [ SynExpr.TypeApp(
                              property,
                              lessRange,
                              typeArgs,
                              commaRanges,
                              Some greaterRange,
                              typeArgsRange,
                              unionRanges property.Range greaterRange
                          )
                          |> LinkExpr.Identifier ]
                    | _ -> []

                let leftLinks = List.cutOffLast leftLinks
                continuation [ yield! leftLinks; yield! lastLink ])

        // Transform `x().y[0]` into `x()` , `dot`, `y[0]`
        | IndexWithoutDot(SynExpr.DotGet(expr, mDot, sli, _), indexExpr) ->
            visit expr (fun leftLinks ->
                let middleLinks, lastExpr =
                    match List.tryLast sli.IdentsWithTrivia with
                    | None -> [], indexExpr
                    | Some lastMiddleLink ->
                        let middleLinks = mkLinksFromSynLongIdent sli |> List.cutOffLast

                        let indexWithDotExpr =
                            let identifierExpr = mkLongIdentExprFromSynIdent lastMiddleLink

                            // Create an adjacent range for the `[`,`]` in the index expression.
                            let adjacentRange =
                                mkRange
                                    indexExpr.Range.FileName
                                    (Position.mkPos
                                        identifierExpr.Range.StartLine
                                        (identifierExpr.Range.StartColumn + 1))
                                    (Position.mkPos indexExpr.Range.EndLine (indexExpr.Range.EndColumn - 1))

                            SynExpr.App(
                                ExprAtomicFlag.Atomic,
                                false,
                                identifierExpr,
                                SynExpr.ArrayOrListComputed(false, indexExpr, adjacentRange),
                                unionRanges identifierExpr.Range indexExpr.Range
                            )

                        middleLinks, indexWithDotExpr

                continuation
                    [ yield! leftLinks
                      yield LinkExpr.Dot mDot
                      yield! middleLinks
                      yield LinkExpr.Expr lastExpr ])

        | SynExpr.App(isInfix = false; funcExpr = SynExpr.DotGet _ as funcExpr; argExpr = argExpr) ->
            visit funcExpr (fun leftLinks ->
                match List.tryLast leftLinks with
                | Some(LinkExpr.Identifier(identifierExpr)) ->
                    match argExpr with
                    | UnitExpr mUnit ->
                        let leftLinks = List.cutOffLast leftLinks

                        // Compose a function application by taking the last identifier of the SynExpr.DotGet
                        // and the following argument expression.
                        // Example: X().Y() -> Take `Y` as function name and `()` as argument.
                        let rightLink = LinkExpr.AppUnit(identifierExpr, mUnit)

                        continuation [ yield! leftLinks; yield rightLink ]

                    | ParenExpr(lpr, e, rpr, pr) ->
                        let leftLinks = List.cutOffLast leftLinks
                        // Example: A().B(fun b -> b)
                        let rightLink = LinkExpr.AppParen(identifierExpr, lpr, e, rpr, pr)
                        continuation [ yield! leftLinks; yield rightLink ]

                    | _ -> visit argExpr (fun rightLinks -> continuation [ yield! leftLinks; yield! rightLinks ])
                | _ -> visit argExpr (fun rightLinks -> continuation [ yield! leftLinks; yield! rightLinks ]))

        | SynExpr.DotGet(expr, rangeOfDot, longDotId, _) ->
            visit expr (fun links ->
                continuation
                    [ yield! links
                      yield LinkExpr.Dot rangeOfDot
                      yield! mkLinksFromSynLongIdent longDotId ])

        | SynExpr.App(isInfix = false; funcExpr = funcExpr; argExpr = UnitExpr mUnit) ->
            mkLinksFromFunctionName (fun e -> LinkExpr.AppUnit(e, mUnit)) funcExpr
            |> continuation

        | SynExpr.App(isInfix = false; funcExpr = funcExpr; argExpr = ParenExpr(lpr, e, rpr, pr)) ->
            mkLinksFromFunctionName (fun f -> LinkExpr.AppParen(f, lpr, e, rpr, pr)) funcExpr
            |> continuation

        | SynExpr.App(ExprAtomicFlag.Atomic,
                      false,
                      (SynExpr.LongIdent _ as funcExpr),
                      (SynExpr.ArrayOrList _ as argExpr),
                      _) ->
            visit funcExpr (fun leftLinks ->
                let app =
                    match List.tryLast leftLinks with
                    | Some(LinkExpr.Identifier identifier) ->
                        [ SynExpr.App(
                              ExprAtomicFlag.Atomic,
                              false,
                              identifier,
                              argExpr,
                              unionRanges identifier.Range argExpr.Range
                          )
                          |> LinkExpr.Expr ]
                    | _ -> []

                let leftLinks = List.cutOffLast leftLinks
                continuation [ yield! leftLinks; yield! app ])

        | SynExpr.TypeApp _ as typeApp -> mkLinksFromFunctionName LinkExpr.Identifier typeApp |> continuation

        | SynExpr.LongIdent(longDotId = sli) -> continuation (mkLinksFromSynLongIdent sli)

        | SynExpr.Ident _ -> continuation [ LinkExpr.Identifier e ]

        | SynExpr.DotIndexedGet(objectExpr, indexArgs, dotRange, _) ->
            visit objectExpr (fun leftLinks ->
                continuation
                    [ yield! leftLinks
                      yield LinkExpr.Dot dotRange
                      yield LinkExpr.IndexExpr indexArgs ])

        | other -> continuation [ LinkExpr.Expr other ]

    match e with
    // An identifier only application with a parenthesis lambda expression.
    // ex: `List.map (fun n -> n)` or `MailboxProcessor<string>.Start (fun n -> n)
    // Because the identifier is not complex we don't consider it a chain.
    | SynExpr.App(
        isInfix = false
        funcExpr = SynExpr.LongIdent _ | SynExpr.Ident _ | SynExpr.DotGet(expr = SynExpr.TypeApp(expr = SynExpr.Ident _))
        argExpr = ParenExpr(_, SynExpr.Lambda _, _, _)) -> None
    | SynExpr.App(
        isInfix = false
        funcExpr = SynExpr.DotGet _ | SynExpr.TypeApp(expr = SynExpr.DotGet _)
        argExpr = UnitExpr _ | ParenExpr _)
    | SynExpr.DotGet _
    | SynExpr.TypeApp(expr = SynExpr.DotGet _)
    | SynExpr.DotIndexedGet(objectExpr = SynExpr.App(funcExpr = SynExpr.DotGet _) | SynExpr.DotGet _) ->
        Some(visit e id)
    | _ -> None

let (|AppSingleParenArg|_|) =
    function
    | App(SynExpr.DotGet _, [ (SynExpr.Paren(expr = SynExpr.Tuple _)) ]) -> None
    | App(e, [ UnitExpr _ as px ]) -> Some(e, px)
    | App(e, [ SynExpr.Paren(expr = singleExpr) as px ]) ->
        match singleExpr with
        | SynExpr.Lambda _
        | SynExpr.MatchLambda _ -> None
        | _ -> Some(e, px)
    | _ -> None

let mkParenExpr creationAide lpr e rpr m =
    ExprParenNode(stn "(" lpr, mkExpr creationAide e, stn ")" rpr, m)

let mkExpr (creationAide: CreationAide) (e: SynExpr) : Expr =
    let exprRange = e.Range

    match e with
    | SynExpr.Lazy(e, StartRange 4 (lazyKeyword, _range)) ->
        ExprLazyNode(stn "lazy" lazyKeyword, mkExpr creationAide e, exprRange)
        |> Expr.Lazy
    | SynExpr.InferredDowncast(e, StartRange 8 (downcastKeyword, _range)) ->
        ExprSingleNode(stn "downcast" downcastKeyword, true, false, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.InferredUpcast(e, StartRange 6 (upcastKeyword, _range)) ->
        ExprSingleNode(stn "upcast" upcastKeyword, true, false, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.Assert(e, StartRange 6 (assertKeyword, _range)) ->
        ExprSingleNode(stn "assert" assertKeyword, true, false, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.AddressOf(true, e, _, StartRange 1 (ampersandToken, _range)) ->
        ExprSingleNode(stn "&" ampersandToken, false, false, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.AddressOf(false, e, _, StartRange 2 (ampersandToken, _range)) ->
        ExprSingleNode(stn "&&" ampersandToken, false, false, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.YieldOrReturn((true, _), e, StartRange 5 (yieldKeyword, _range)) ->
        ExprSingleNode(stn "yield" yieldKeyword, true, true, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.YieldOrReturn((false, _), e, StartRange 6 (returnKeyword, _range)) ->
        ExprSingleNode(stn "return" returnKeyword, true, true, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.YieldOrReturnFrom((true, _), e, StartRange 6 (yieldBangKeyword, _range)) ->
        ExprSingleNode(stn "yield!" yieldBangKeyword, true, true, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.YieldOrReturnFrom((false, _), e, StartRange 7 (returnBangKeyword, _range)) ->
        ExprSingleNode(stn "return!" returnBangKeyword, true, true, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.Do(e, StartRange 2 (doKeyword, _range)) ->
        ExprSingleNode(stn "do" doKeyword, true, true, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.DoBang(e, StartRange 3 (doBangKeyword, _range)) ->
        ExprSingleNode(stn "do!" doBangKeyword, true, true, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.Fixed(e, StartRange 5 (fixedKeyword, _range)) ->
        ExprSingleNode(stn "fixed" fixedKeyword, true, false, mkExpr creationAide e, exprRange)
        |> Expr.Single
    | SynExpr.Const(c, r) -> mkConstant creationAide c r |> Expr.Constant
    | SynExpr.Null _ -> stn "null" exprRange |> Expr.Null
    | SynExpr.Quote(_, isRaw, e, _, range) -> mkExprQuote creationAide isRaw e range |> Expr.Quote
    | SynExpr.TypeTest(e, t, _) ->
        ExprTypedNode(mkExpr creationAide e, ":?", mkType creationAide t, exprRange)
        |> Expr.Typed
    | SynExpr.Downcast(e, t, _) ->
        ExprTypedNode(mkExpr creationAide e, ":?>", mkType creationAide t, exprRange)
        |> Expr.Typed
    | SynExpr.Upcast(e, t, _) ->
        ExprTypedNode(mkExpr creationAide e, ":>", mkType creationAide t, exprRange)
        |> Expr.Typed
    | SynExpr.Typed(e, t, _) ->
        ExprTypedNode(mkExpr creationAide e, ":", mkType creationAide t, exprRange)
        |> Expr.Typed
    | SynExpr.New(_, t, e, StartRange 3 (newRange, _)) ->
        ExprNewNode(stn "new" newRange, mkType creationAide t, mkExpr creationAide e, exprRange)
        |> Expr.New
    | SynExpr.Tuple(false, exprs, commas, _) -> mkTuple creationAide exprs commas exprRange |> Expr.Tuple
    | SynExpr.Tuple(true, exprs, commas, StartRange 6 (mStruct, _) & EndRange 1 (mClosing, _)) ->
        let mTuple =
            match List.tryHead exprs, List.tryLast exprs with
            | Some e1, Some e2 -> unionRanges e1.Range e2.Range
            | _ -> failwith "SynExpr.Tuple with no elements"

        ExprStructTupleNode(stn "struct" mStruct, mkTuple creationAide exprs commas mTuple, stn ")" mClosing, exprRange)
        |> Expr.StructTuple
    | SynExpr.ArrayOrListComputed(isArray, Sequentials xs, range)
    | SynExpr.ArrayOrList(isArray, xs, range) ->
        let o, c = mkOpenAndCloseForArrayOrList isArray range

        ExprArrayOrListNode(o, List.map (mkExpr creationAide) xs, c, exprRange)
        |> Expr.ArrayOrList
    | SynExpr.ArrayOrListComputed(isArray, singleExpr, range) ->
        let o, c = mkOpenAndCloseForArrayOrList isArray range

        ExprArrayOrListNode(o, [ mkExpr creationAide singleExpr ], c, exprRange)
        |> Expr.ArrayOrList
    | SynExpr.Record(baseInfo, copyInfo, recordFields, StartEndRange 1 (mOpen, _, mClose)) ->
        let fieldNodes =
            recordFields
            |> List.choose (function
                | SynExprRecordField((fieldName, _), Some mEq, Some expr, _) ->
                    let m = unionRanges fieldName.Range expr.Range
                    Some(RecordFieldNode(mkSynLongIdent fieldName, stn "=" mEq, mkExpr creationAide expr, m))
                | _ -> None)

        match baseInfo, copyInfo with
        | Some _, Some _ -> failwith "Unexpected that both baseInfo and copyInfo are present in SynExpr.Record"
        | Some(t, e, m, _, mInherit), None ->
            let inheritCtor = mkInheritConstructor creationAide t e mInherit m

            ExprInheritRecordNode(stn "{" mOpen, inheritCtor, fieldNodes, stn "}" mClose, exprRange)
            |> Expr.InheritRecord
        | None, Some(copyExpr, _) ->
            let copyExpr = mkExpr creationAide copyExpr

            ExprRecordNode(stn "{" mOpen, Some copyExpr, fieldNodes, stn "}" mClose, exprRange)
            |> Expr.Record
        | None, None ->
            ExprRecordNode(stn "{" mOpen, None, fieldNodes, stn "}" mClose, exprRange)
            |> Expr.Record
    | SynExpr.AnonRecd(true,
                       copyInfo,
                       recordFields,
                       (StartRange 6 (mStruct, _) & EndRange 2 (mClose, _)),
                       { OpeningBraceRange = mOpen }) ->
        let fields =
            recordFields
            |> List.choose (function
                | sli, Some mEq, e ->
                    let m = unionRanges sli.Range e.Range
                    let longIdent = mkSynLongIdent sli

                    Some(RecordFieldNode(longIdent, stn "=" mEq, mkExpr creationAide e, m))
                | _ -> None)

        ExprAnonStructRecordNode(
            stn "struct" mStruct,
            stn "{|" mOpen,
            Option.map (fst >> mkExpr creationAide) copyInfo,
            fields,
            stn "|}" mClose,
            exprRange
        )
        |> Expr.AnonStructRecord
    | SynExpr.AnonRecd(false, copyInfo, recordFields, EndRange 2 (mClose, _), { OpeningBraceRange = mOpen }) ->
        let fields =
            recordFields
            |> List.choose (function
                | sli, Some mEq, e ->
                    let m = unionRanges sli.Range e.Range
                    let longIdent = mkSynLongIdent sli
                    Some(RecordFieldNode(longIdent, stn "=" mEq, mkExpr creationAide e, m))
                | _ -> None)

        ExprRecordNode(
            stn "{|" mOpen,
            Option.map (fst >> mkExpr creationAide) copyInfo,
            fields,
            stn "|}" mClose,
            exprRange
        )
        |> Expr.Record
    | SynExpr.ObjExpr(t, eio, withKeyword, bd, members, ims, StartRange 3 (mNew, _), StartEndRange 1 (mOpen, _, mClose)) ->
        let interfaceNodes =
            ims
            |> List.map (fun (SynInterfaceImpl(t, mWith, bs, members, StartRange 9 (mInterface, m))) ->
                InterfaceImplNode(
                    stn "interface" mInterface,
                    mkType creationAide t,
                    Option.map (stn "with") mWith,
                    List.map (mkBinding creationAide) bs,
                    List.map (mkMemberDefn creationAide) members,
                    m
                ))

        ExprObjExprNode(
            stn "{" mOpen,
            stn "new" mNew,
            mkType creationAide t,
            Option.map (fun (e, _) -> mkExpr creationAide e) eio,
            Option.map (stn "with") withKeyword,
            List.map (mkBinding creationAide) bd,
            List.map (mkMemberDefn creationAide) members,
            interfaceNodes,
            stn "}" mClose,
            exprRange
        )
        |> Expr.ObjExpr
    | SynExpr.While(_, ew, ed, StartRange 5 (mWhile, _)) ->
        ExprWhileNode(stn "while" mWhile, mkExpr creationAide ew, mkExpr creationAide ed, exprRange)
        |> Expr.While
    | SynExpr.WhileBang(_, ew, ed, StartRange 6 (mWhileBang, _)) ->
        ExprWhileNode(stn "while!" mWhileBang, mkExpr creationAide ew, mkExpr creationAide ed, exprRange)
        |> Expr.While
    | SynExpr.For(_, _, ident, Some equalsRange, e1, isUp, e2, e3, StartRange 3 (mFor, _)) ->
        ExprForNode(
            stn "for" mFor,
            mkIdent ident,
            stn "=" equalsRange,
            mkExpr creationAide e1,
            isUp,
            mkExpr creationAide e2,
            mkExpr creationAide e3,
            exprRange
        )
        |> Expr.For
    | SynExpr.ForEach(_,
                      _,
                      SeqExprOnly true,
                      _,
                      pat,
                      e1,
                      SynExpr.YieldOrReturn((true, _), e2, _),
                      StartRange 3 (mFor, _)) ->
        ExprForEachNode(
            stn "for" mFor,
            mkPat creationAide pat,
            mkExpr creationAide e1,
            true,
            mkExpr creationAide e2,
            exprRange
        )
        |> Expr.ForEach
    | SynExpr.ForEach(_, _, SeqExprOnly isArrow, _, pat, e1, e2, StartRange 3 (mFor, _)) ->
        ExprForEachNode(
            stn "for" mFor,
            mkPat creationAide pat,
            mkExpr creationAide e1,
            isArrow,
            mkExpr creationAide e2,
            exprRange
        )
        |> Expr.ForEach
    | SynExpr.App(ExprAtomicFlag.NonAtomic,
                  false,
                  (SynExpr.App _ | SynExpr.TypeApp _ | SynExpr.Ident _ | SynExpr.LongIdent _ as nameExpr),
                  SynExpr.ComputationExpr(_, expr, StartEndRange 1 (openingBrace, _range, closingBrace)),
                  _) ->
        ExprNamedComputationNode(
            mkExpr creationAide nameExpr,
            stn "{" openingBrace,
            mkExpr creationAide expr,
            stn "}" closingBrace,
            exprRange
        )
        |> Expr.NamedComputation
    | SynExpr.ComputationExpr(_, expr, StartEndRange 1 (openingBrace, _range, closingBrace)) ->
        ExprComputationNode(stn "{" openingBrace, mkExpr creationAide expr, stn "}" closingBrace, exprRange)
        |> Expr.Computation

    | SynExpr.LetOrUse _
    | SynExpr.LetOrUseBang _
    | SynExpr.Sequential _ ->
        ExprCompExprBodyNode(collectComputationExpressionStatements creationAide e id, exprRange)
        |> Expr.CompExprBody

    | SynExpr.JoinIn(e1, mIn, e2, _) ->
        ExprJoinInNode(mkExpr creationAide e1, stn "in" mIn, mkExpr creationAide e2, exprRange)
        |> Expr.JoinIn

    | ParenLambda(lpr, pats, mArrow, body, mLambda, rpr) ->
        let lambdaNode = mkLambda creationAide pats mArrow body mLambda

        ExprParenLambdaNode(stn "(" lpr, lambdaNode, stn ")" rpr, exprRange)
        |> Expr.ParenLambda

    | SynExpr.Lambda(_, _, _, _, Some(pats, body), _, { ArrowRange = Some mArrow }) ->
        mkLambda creationAide pats mArrow body exprRange |> Expr.Lambda

    | SynExpr.MatchLambda(_, mFunction, cs, _, _) ->
        mkMatchLambda creationAide mFunction cs exprRange |> Expr.MatchLambda

    | SynExpr.Match(_, e, cs, _, trivia) ->
        ExprMatchNode(
            stn "match" trivia.MatchKeyword,
            mkExpr creationAide e,
            stn "with" trivia.WithKeyword,
            List.map (mkSynMatchClause creationAide) cs,
            exprRange
        )
        |> Expr.Match
    | SynExpr.MatchBang(_, e, cs, _, trivia) ->
        ExprMatchNode(
            stn "match!" trivia.MatchBangKeyword,
            mkExpr creationAide e,
            stn "with" trivia.WithKeyword,
            List.map (mkSynMatchClause creationAide) cs,
            exprRange
        )
        |> Expr.Match
    | SynExpr.TraitCall(tps, msg, expr, _) ->
        ExprTraitCallNode(mkType creationAide tps, mkMemberSig creationAide msg, mkExpr creationAide expr, exprRange)
        |> Expr.TraitCall

    | SynExpr.Paren(expr = SynExpr.LibraryOnlyILAssembly(range = m)) ->
        stn (creationAide.TextFromSource (fun () -> "") m) m |> Expr.ParenILEmbedded
    | SynExpr.LongIdent(longDotId = SynLongIdent([ ident ], [], [ Some(ParenStarSynIdent(lpr, originalNotation, rpr)) ])) ->
        ExprParenFunctionNameWithStarNode(stn "(" lpr, stn originalNotation ident.idRange, stn ")" rpr, exprRange)
        |> Expr.ParenFunctionNameWithStar
    | ParenExpr(lpr, e, rpr, pr) ->
        let hasBeginKeyword = (lpr.EndColumn - lpr.StartColumn) = 5
        let hasEndKeyword = (rpr.EndColumn - rpr.StartColumn) = 3

        if hasBeginKeyword && hasEndKeyword then
            ExprBeginEndNode(stn "begin" lpr, mkExpr creationAide e, stn "end" rpr, pr)
            |> Expr.BeginEnd
        else
            mkParenExpr creationAide lpr e rpr pr |> Expr.Paren
    | SynExpr.Dynamic(funcExpr, _, argExpr, _) ->
        ExprDynamicNode(mkExpr creationAide funcExpr, mkExpr creationAide argExpr, exprRange)
        |> Expr.Dynamic
    | SynExpr.App(_,
                  false,
                  SynExpr.LongIdent(_,
                                    SynLongIdent([ ident ], [], [ Some(IdentTrivia.OriginalNotation operatorName) ]),
                                    _,
                                    _),
                  e2,
                  _) when
        PrettyNaming.IsValidPrefixOperatorDefinitionName(
            PrettyNaming.ConvertValLogicalNameToDisplayNameCore ident.idText
        )
        ->
        ExprPrefixAppNode(stn operatorName ident.idRange, mkExpr creationAide e2, exprRange)
        |> Expr.PrefixApp

    // | NewlineInfixApps(head, xs)
    // | MultipleConsInfixApps(head, xs)
    | SameInfixApps(head, xs) ->
        let rest = xs |> List.map (fun (operator, e) -> operator, mkExpr creationAide e)

        ExprSameInfixAppsNode(mkExpr creationAide head, rest, exprRange)
        |> Expr.SameInfixApps

    | InfixApp(e1, operator, e2) ->
        ExprInfixAppNode(mkExpr creationAide e1, operator, mkExpr creationAide e2, exprRange)
        |> Expr.InfixApp

    | IndexWithoutDot(identifierExpr, indexExpr) ->
        ExprIndexWithoutDotNode(mkExpr creationAide identifierExpr, mkExpr creationAide indexExpr, exprRange)
        |> Expr.IndexWithoutDot

    | ChainExpr links ->
        let chainLinks =
            links
            |> List.map (function
                | LinkExpr.Identifier identifierExpr -> mkExpr creationAide identifierExpr |> ChainLink.Identifier
                | LinkExpr.Dot mDot -> stn "." mDot |> ChainLink.Dot
                | LinkExpr.Expr e -> mkExpr creationAide e |> ChainLink.Expr
                | LinkExpr.AppUnit(f, mUnit) ->
                    LinkSingleAppUnit(mkExpr creationAide f, mkUnit mUnit, unionRanges f.Range mUnit)
                    |> ChainLink.AppUnit
                | LinkExpr.AppParen(f, lpr, e, rpr, pr) ->
                    LinkSingleAppParen(
                        mkExpr creationAide f,
                        mkParenExpr creationAide lpr e rpr pr,
                        unionRanges f.Range pr
                    )
                    |> ChainLink.AppParen
                | LinkExpr.IndexExpr e -> mkExpr creationAide e |> ChainLink.IndexExpr
                | link -> failwithf "cannot map %A" link)

        ExprChain(chainLinks, exprRange) |> Expr.Chain

    | AppSingleParenArg(SynExpr.LongIdent(longDotId = longDotId), px) ->
        ExprAppLongIdentAndSingleParenArgNode(mkSynLongIdent longDotId, mkExpr creationAide px, exprRange)
        |> Expr.AppLongIdentAndSingleParenArg
    | AppSingleParenArg(e, px) ->
        ExprAppSingleParenArgNode(mkExpr creationAide e, mkExpr creationAide px, exprRange)
        |> Expr.AppSingleParenArg

    | SynExpr.App(funcExpr = App(fe, args); argExpr = ParenLambda(lpr, pats, mArrow, body, mLambda, rpr)) ->
        let lambdaNode = mkLambda creationAide pats mArrow body mLambda

        ExprAppWithLambdaNode(
            mkExpr creationAide fe,
            List.map (mkExpr creationAide) args,
            stn "(" lpr,
            Choice1Of2 lambdaNode,
            stn ")" rpr,
            exprRange
        )
        |> Expr.AppWithLambda
    | SynExpr.App(funcExpr = fe; argExpr = ParenLambda(lpr, pats, mArrow, body, mLambda, rpr)) ->
        let lambdaNode = mkLambda creationAide pats mArrow body mLambda

        ExprAppWithLambdaNode(mkExpr creationAide fe, [], stn "(" lpr, Choice1Of2 lambdaNode, stn ")" rpr, exprRange)
        |> Expr.AppWithLambda

    | SynExpr.App(funcExpr = App(fe, args); argExpr = ParenMatchLambda(lpr, mFunction, clauses, mMatchLambda, rpr)) ->
        let lambdaNode = mkMatchLambda creationAide mFunction clauses mMatchLambda

        ExprAppWithLambdaNode(
            mkExpr creationAide fe,
            List.map (mkExpr creationAide) args,
            stn "(" lpr,
            Choice2Of2 lambdaNode,
            stn ")" rpr,
            exprRange
        )
        |> Expr.AppWithLambda

    | SynExpr.App(funcExpr = fe; argExpr = ParenMatchLambda(lpr, mFunction, clauses, mMatchLambda, rpr)) ->
        let lambdaNode = mkMatchLambda creationAide mFunction clauses mMatchLambda

        ExprAppWithLambdaNode(mkExpr creationAide fe, [], stn "(" lpr, Choice2Of2 lambdaNode, stn ")" rpr, exprRange)
        |> Expr.AppWithLambda
    | SynExpr.App(ExprAtomicFlag.NonAtomic,
                  false,
                  SynExpr.App(ExprAtomicFlag.Atomic,
                              false,
                              identifierExpr,
                              SynExpr.ArrayOrListComputed(false, indexExpr, _),
                              _),
                  argExpr,
                  _) ->
        ExprNestedIndexWithoutDotNode(
            mkExpr creationAide identifierExpr,
            mkExpr creationAide indexExpr,
            mkExpr creationAide argExpr,
            exprRange
        )
        |> Expr.NestedIndexWithoutDot
    | SynExpr.App(ExprAtomicFlag.NonAtomic,
                  false,
                  SynExpr.App(ExprAtomicFlag.NonAtomic,
                              false,
                              identifierExpr,
                              (SynExpr.ArrayOrListComputed(isArray = false; expr = indexExpr) as indexArgExpr),
                              _),
                  argExpr,
                  _) when (RangeHelpers.isAdjacentTo identifierExpr.Range indexArgExpr.Range) ->
        ExprNestedIndexWithoutDotNode(
            mkExpr creationAide identifierExpr,
            mkExpr creationAide indexExpr,
            mkExpr creationAide argExpr,
            exprRange
        )
        |> Expr.NestedIndexWithoutDot

    | App(fe, args) ->
        ExprAppNode(mkExpr creationAide fe, List.map (mkExpr creationAide) args, exprRange)
        |> Expr.App

    | SynExpr.TypeApp(identifier, lessRange, ts, _, Some greaterRange, _, _) ->
        ExprTypeAppNode(
            mkExpr creationAide identifier,
            stn "<" lessRange,
            List.map (mkType creationAide) ts,
            stn ">" greaterRange,
            exprRange
        )
        |> Expr.TypeApp
    | SynExpr.TryWith(e, [ SynMatchClause(pat = pat) as c ], _, _, _, trivia) ->
        match pat with
        | SynPat.Or _
        | SynPat.As(SynPat.Or _, _, _) ->
            ExprTryWithNode(
                stn "try" trivia.TryKeyword,
                mkExpr creationAide e,
                stn "with" trivia.WithKeyword,
                [ mkSynMatchClause creationAide c ],
                exprRange
            )
            |> Expr.TryWith
        | _ ->
            ExprTryWithSingleClauseNode(
                stn "try" trivia.TryKeyword,
                mkExpr creationAide e,
                stn "with" trivia.WithKeyword,
                mkSynMatchClause creationAide c,
                exprRange
            )
            |> Expr.TryWithSingleClause
    | SynExpr.TryWith(e, clauses, _, _, _, trivia) ->
        ExprTryWithNode(
            stn "try" trivia.TryKeyword,
            mkExpr creationAide e,
            stn "with" trivia.WithKeyword,
            List.map (mkSynMatchClause creationAide) clauses,
            exprRange
        )
        |> Expr.TryWith

    | SynExpr.TryFinally(e1, e2, _, _, _, trivia) ->
        ExprTryFinallyNode(
            stn "try" trivia.TryKeyword,
            mkExpr creationAide e1,
            stn "finally" trivia.FinallyKeyword,
            mkExpr creationAide e2,
            exprRange
        )
        |> Expr.TryFinally

    | SynExpr.IfThenElse(ifExpr = ifExpr; thenExpr = thenExpr; elseExpr = None; trivia = trivia) ->
        ExprIfThenNode(
            IfKeywordNode.SingleWord(stn "if" trivia.IfKeyword),
            mkExpr creationAide ifExpr,
            stn "then" trivia.ThenKeyword,
            mkExpr creationAide thenExpr,
            exprRange
        )
        |> Expr.IfThen

    | ElIf([ elifKw, ifExpr, thenKw, thenExpr ], Some(elseKw, elseExpr)) ->
        let ifExprNode = mkExpr creationAide ifExpr

        let ifKwNode: IfKeywordNode =
            match elifKw with
            | Choice1Of2 stn -> IfKeywordNode.SingleWord stn
            | Choice2Of2(mElse, mIf) ->
                ElseIfNode(mElse, mIf, Expr.Node ifExprNode, unionRanges mElse mIf)
                |> IfKeywordNode.ElseIf

        ExprIfThenElseNode(
            ifKwNode,
            ifExprNode,
            thenKw,
            mkExpr creationAide thenExpr,
            elseKw,
            mkExpr creationAide elseExpr,
            exprRange
        )
        |> Expr.IfThenElse

    | ElIf(elifs, elseOpt) ->
        let elifs =
            elifs
            |> List.map (fun (elifKw, ifExpr, thenNode, thenExpr) ->
                let ifExprNode = mkExpr creationAide ifExpr

                let ifKwNode: IfKeywordNode =
                    match elifKw with
                    | Choice1Of2 stn -> IfKeywordNode.SingleWord stn
                    | Choice2Of2(mElse, mIf) ->
                        ElseIfNode(mElse, mIf, Expr.Node ifExprNode, unionRanges mElse mIf)
                        |> IfKeywordNode.ElseIf

                let m = unionRanges ifKwNode.Range thenExpr.Range
                ExprIfThenNode(ifKwNode, ifExprNode, thenNode, mkExpr creationAide thenExpr, m))

        let optElse =
            match elseOpt with
            | None -> None
            | Some(elseNode, e) -> Some(elseNode, mkExpr creationAide e)

        ExprIfThenElifNode(elifs, optElse, exprRange) |> Expr.IfThenElif

    | SynExpr.Ident ident -> mkIdent ident |> Expr.Ident
    | SynExpr.LongIdent(isOpt, synLongIdent, _, m) ->
        ExprOptVarNode(isOpt, mkSynLongIdent synLongIdent, m) |> Expr.OptVar
    | SynExpr.LongIdentSet(synLongIdent, e, _) ->
        ExprLongIdentSetNode(mkSynLongIdent synLongIdent, mkExpr creationAide e, exprRange)
        |> Expr.LongIdentSet
    | SynExpr.DotIndexedGet(objectExpr, indexArgs, _, _) ->
        ExprDotIndexedGetNode(mkExpr creationAide objectExpr, mkExpr creationAide indexArgs, exprRange)
        |> Expr.DotIndexedGet

    | SynExpr.DotIndexedSet(objectExpr, indexExpr, valueExpr, _, _, _) ->
        ExprDotIndexedSetNode(
            mkExpr creationAide objectExpr,
            mkExpr creationAide indexExpr,
            mkExpr creationAide valueExpr,
            exprRange
        )
        |> Expr.DotIndexedSet
    | SynExpr.NamedIndexedPropertySet(synLongIdent, e1, e2, _) ->
        ExprNamedIndexedPropertySetNode(
            mkSynLongIdent synLongIdent,
            mkExpr creationAide e1,
            mkExpr creationAide e2,
            exprRange
        )
        |> Expr.NamedIndexedPropertySet
    | SynExpr.DotNamedIndexedPropertySet(e, synLongIdent, e1, e2, _) ->
        ExprDotNamedIndexedPropertySetNode(
            mkExpr creationAide e,
            mkSynLongIdent synLongIdent,
            mkExpr creationAide e1,
            mkExpr creationAide e2,
            exprRange
        )
        |> Expr.DotNamedIndexedPropertySet
    | SynExpr.DotSet(e1, synLongIdent, e2, _) ->
        let mDot =
            mkRange
                e1.Range.FileName
                (Position.mkPos e1.Range.EndLine (e1.Range.EndColumn + 1))
                (Position.mkPos synLongIdent.Range.StartLine (synLongIdent.Range.StartColumn - 1))

        let dotGet =
            SynExpr.DotGet(e1, mDot, synLongIdent, unionRanges e1.Range synLongIdent.Range)

        ExprSetNode(mkExpr creationAide dotGet, mkExpr creationAide e2, exprRange)
        |> Expr.Set
    | SynExpr.Set(e1, e2, _) ->
        ExprSetNode(mkExpr creationAide e1, mkExpr creationAide e2, exprRange)
        |> Expr.Set

    | SynExpr.LibraryOnlyStaticOptimization(constraints, e, optExpr, _) ->
        let constraints =
            constraints
            |> List.map (function
                | SynStaticOptimizationConstraint.WhenTyparTyconEqualsTycon(t1, t2, _) ->
                    StaticOptimizationConstraintWhenTyparTyconEqualsTyconNode(
                        mkSynTypar t1,
                        mkType creationAide t2,
                        unionRanges t1.Range t2.Range
                    )
                    |> StaticOptimizationConstraint.WhenTyparTyconEqualsTycon
                | SynStaticOptimizationConstraint.WhenTyparIsStruct(t, _) ->
                    mkSynTypar t |> StaticOptimizationConstraint.WhenTyparIsStruct)

        ExprLibraryOnlyStaticOptimizationNode(
            mkExpr creationAide optExpr,
            constraints,
            mkExpr creationAide e,
            exprRange
        )
        |> Expr.LibraryOnlyStaticOptimization
    | SynExpr.InterpolatedString(parts, _, _) ->
        let lastIndex = parts.Length - 1

        let parts =
            parts
            |> List.mapi (fun idx part ->
                match part with
                | SynInterpolatedStringPart.String(v, r) ->
                    stn
                        (creationAide.TextFromSource
                            (fun () ->
                                if idx = 0 && not (String.startsWithOrdinal "$" v) then
                                    $"$\"%s{v}{{"
                                elif idx = lastIndex && not (String.endsWithOrdinal "\"" v) then
                                    $"}}%s{v}\""
                                else
                                    $"}}%s{v}{{")
                            r)
                        r
                    |> Choice1Of2
                | SynInterpolatedStringPart.FillExpr(fillExpr, qualifiers) ->
                    let m =
                        match qualifiers with
                        | None -> fillExpr.Range
                        | Some ident -> unionRanges fillExpr.Range ident.idRange

                    FillExprNode(mkExpr creationAide fillExpr, Option.map mkIdent qualifiers, m)
                    |> Choice2Of2)

        ExprInterpolatedStringExprNode(parts, exprRange) |> Expr.InterpolatedStringExpr
    | SynExpr.IndexRange(None, _, None, _, _, _) -> stn "*" exprRange |> Expr.IndexRangeWildcard
    | SynExpr.IndexRange(Some(SynExpr.IndexRange(Some(ConstNumberExpr(c1, mC1)),
                                                 mDots1,
                                                 Some(ConstNumberExpr(c2, mC2)),
                                                 _,
                                                 _,
                                                 _)),
                         mDots2,
                         Some(ConstNumberExpr(c3, mC3)),
                         _,
                         _,
                         _) ->
        let c1Node = stn (creationAide.TextFromSource (fun () -> c1) mC1) mC1
        let c2Node = stn (creationAide.TextFromSource (fun () -> c2) mC2) mC2
        let c3Node = stn (creationAide.TextFromSource (fun () -> c3) mC3) mC3

        let dotText =
            if String.endsWithOrdinal "." c1Node.Text || String.endsWithOrdinal "." c2Node.Text then
                " .. "
            else
                ".."

        let startDots = stn dotText mDots1
        let endDots = stn dotText mDots2

        ExprTripleNumberIndexRangeNode(c1Node, startDots, c2Node, endDots, c3Node, exprRange)
        |> Expr.TripleNumberIndexRange
    | SynExpr.IndexRange(expr1 = e1; opm = mDots; expr2 = e2) ->
        let dotsNode =
            let hasSpaces =
                let rec (|AtomicExpr|_|) e =
                    match e with
                    | ConstNumberExpr(v, _) when String.startsWithOrdinal "-" v -> None
                    | SynExpr.Ident _
                    | SynExpr.Const(SynConst.Int32 _, _)
                    | SynExpr.IndexRange(expr1 = Some(AtomicExpr _); expr2 = Some(AtomicExpr _))
                    | SynExpr.IndexFromEnd(AtomicExpr _, _) -> Some e
                    | _ -> None

                match e1, e2 with
                | Some(AtomicExpr _), None
                | None, Some(AtomicExpr _)
                | Some(AtomicExpr _), Some(AtomicExpr _) -> false
                | _ -> true

            let dots =
                if hasSpaces && e1.IsSome && e2.IsSome then " .. "
                elif hasSpaces && e1.IsNone && e2.IsSome then ".. "
                elif hasSpaces && e1.IsSome && e2.IsNone then " .."
                else ".."

            stn dots mDots

        ExprIndexRangeNode(
            Option.map (mkExpr creationAide) e1,
            dotsNode,
            Option.map (mkExpr creationAide) e2,
            exprRange
        )
        |> Expr.IndexRange
    | SynExpr.IndexFromEnd(e, _) -> ExprIndexFromEndNode(mkExpr creationAide e, exprRange) |> Expr.IndexFromEnd
    | SynExpr.Typar(typar, _) -> mkSynTypar typar |> Expr.Typar
    | SynExpr.DotLambda(
        expr = e
        trivia = { DotRange = mDot
                   UnderscoreRange = mUnderscore }) ->
        ExprDotLambda(stn "_" mUnderscore, stn "." mDot, mkExpr creationAide e, exprRange)
        |> Expr.DotLambda
    | _ -> failwithf "todo for %A" e

let mkExprQuote creationAide isRaw e range : ExprQuoteNode =
    let startToken, endToken =
        let sText, length, eText = if isRaw then "<@@", 3, "@@>" else "<@", 2, "@>"

        match range with
        | StartEndRange length (startRange, _, endRange) -> stn sText startRange, stn eText endRange

    ExprQuoteNode(startToken, mkExpr creationAide e, endToken, range)

let (|ParenStarSynIdent|_|) =
    function
    | IdentTrivia.OriginalNotationWithParen(lpr, originalNotation, rpr) ->
        if originalNotation.Length > 1 && String.startsWithOrdinal "*" originalNotation then
            Some(lpr, originalNotation, rpr)
        else
            None
    | _ -> None

let (|PatParameter|_|) (p: SynPat) =
    match p with
    | SynPat.Typed(pat = pat; targetType = t) -> Some([], pat, Some t)
    | SynPat.Attrib(pat = SynPat.Typed(pat = pat; targetType = t); attributes = attributes) ->
        Some(attributes, pat, Some t)
    | SynPat.Attrib(pat = pat; attributes = attributes) -> Some(attributes, pat, None)
    | _ -> None

let mkUnit (StartEndRange 1 (lpr, m, rpr)) = UnitNode(stn "(" lpr, stn ")" rpr, m)

let mkTuplePat (creationAide: CreationAide) (pats: SynPat list) (commas: range list) (m: range) =
    match pats with
    | [] -> failwith "SynPat.Tuple with no elements"
    | head :: tail ->
        let rest =
            if tail.Length <> commas.Length then
                failwith
                    $"Number of elements in tail of tuple (%i{tail.Length}) was not equal to number of commas (%i{commas.Length}), at range %O{m}."

            List.zip commas tail
            |> List.collect (fun (c, e) -> [ yield Choice2Of2(stn "," c); yield Choice1Of2(mkPat creationAide e) ])

        PatTupleNode([ yield Choice1Of2(mkPat creationAide head); yield! rest ], m)

let mkPat (creationAide: CreationAide) (p: SynPat) =
    let patternRange = p.Range

    match p with
    | SynPat.OptionalVal(ident, _) ->
        let identNode = mkIdent ident
        SingleTextNode($"?%s{identNode.Text}", patternRange) |> Pattern.OptionalVal
    | PatParameter(ats, pat, t) ->
        PatParameterNode(
            mkAttributes creationAide ats,
            mkPat creationAide pat,
            Option.map (mkType creationAide) t,
            patternRange
        )
        |> Pattern.Parameter
    | SynPat.Or(p1, p2, _, trivia) ->
        PatLeftMiddleRight(
            mkPat creationAide p1,
            Choice1Of2(stn "|" trivia.BarRange),
            mkPat creationAide p2,
            patternRange
        )
        |> Pattern.Or
    | SynPat.Ands(ps, _) -> PatAndsNode(List.map (mkPat creationAide) ps, patternRange) |> Pattern.Ands
    | SynPat.Null _ -> stn "null" patternRange |> Pattern.Null
    | SynPat.Wild _ -> stn "_" patternRange |> Pattern.Wild
    | SynPat.Named(accessibility = ao; ident = SynIdent(ident, Some(ParenStarSynIdent(lpr, op, rpr)))) ->
        PatNamedParenStarIdentNode(mkSynAccess ao, stn "(" lpr, stn op ident.idRange, stn ")" rpr, patternRange)
        |> Pattern.NamedParenStarIdent
    | SynPat.Named(accessibility = ao; ident = ident) ->
        PatNamedNode(mkSynAccess ao, mkSynIdent ident, patternRange) |> Pattern.Named
    | SynPat.As(p1, p2, _) ->
        PatLeftMiddleRight(mkPat creationAide p1, Choice2Of2 "as", mkPat creationAide p2, patternRange)
        |> Pattern.As
    | SynPat.ListCons(p1, p2, _, trivia) ->
        PatLeftMiddleRight(
            mkPat creationAide p1,
            Choice1Of2(stn "::" trivia.ColonColonRange),
            mkPat creationAide p2,
            patternRange
        )
        |> Pattern.ListCons
    | SynPat.LongIdent(synLongIdent,
                       _,
                       vtdo,
                       SynArgPats.NamePatPairs(nps, _, { ParenRange = StartEndRange 1 (lpr, _, rpr) }),
                       _,
                       _) ->
        let typarDecls = mkSynValTyparDecls creationAide vtdo

        let pairs =
            nps
            |> List.choose (fun (ident, eq, pat) ->
                eq
                |> Option.map (fun eq ->
                    NamePatPair(
                        mkIdent ident,
                        stn "=" eq,
                        mkPat creationAide pat,
                        unionRanges ident.idRange pat.Range
                    )))

        PatNamePatPairsNode(mkSynLongIdent synLongIdent, typarDecls, stn "(" lpr, pairs, stn ")" rpr, patternRange)
        |> Pattern.NamePatPairs
    | SynPat.LongIdent(synLongIdent, _, vtdo, SynArgPats.Pats pats, ao, _) ->
        let typarDecls = mkSynValTyparDecls creationAide vtdo

        PatLongIdentNode(
            mkSynAccess ao,
            mkSynLongIdent synLongIdent,
            typarDecls,
            List.map (mkPat creationAide) pats,
            patternRange
        )
        |> Pattern.LongIdent
    | SynPat.Paren(SynPat.Const(SynConst.Unit, _), mUnit) -> mkUnit mUnit |> Pattern.Unit
    | SynPat.Paren(p, StartEndRange 1 (lpr, _, rpr)) ->
        PatParenNode(stn "(" lpr, mkPat creationAide p, stn ")" rpr, patternRange)
        |> Pattern.Paren
    | SynPat.Tuple(false, ps, commas, _) -> mkTuplePat creationAide ps commas patternRange |> Pattern.Tuple
    | SynPat.Tuple(true, ps, _, _) ->
        PatStructTupleNode(List.map (mkPat creationAide) ps, patternRange)
        |> Pattern.StructTuple
    | SynPat.ArrayOrList(isArray, ps, range) ->
        let openToken, closeToken = mkOpenAndCloseForArrayOrList isArray range

        PatArrayOrListNode(openToken, List.map (mkPat creationAide) ps, closeToken, patternRange)
        |> Pattern.ArrayOrList
    | SynPat.Record(fields, StartEndRange 1 (o, _, c)) ->
        let fields =
            fields
            |> List.map (fun ((lid, ident), eq, pat) ->
                let prefix = if lid.IsEmpty then None else Some(mkLongIdent lid)

                let range =
                    match prefix with
                    | None -> unionRanges ident.idRange pat.Range
                    | Some prefix -> unionRanges prefix.Range pat.Range

                let eqNode = stn "=" (Option.defaultValue Range.Zero eq)
                PatRecordField(prefix, mkIdent ident, eqNode, mkPat creationAide pat, range))

        PatRecordNode(stn "{" o, fields, stn "}" c, patternRange) |> Pattern.Record
    | SynPat.Const(c, r) -> mkConstant creationAide c r |> Pattern.Const
    | SynPat.IsInst(t, StartRange 2 (tokenRange, _)) ->
        PatIsInstNode(stn ":?" tokenRange, mkType creationAide t, patternRange)
        |> Pattern.IsInst
    | SynPat.QuoteExpr(SynExpr.Quote(_, isRaw, e, _, _), _) ->
        mkExprQuote creationAide isRaw e patternRange |> Pattern.QuoteExpr
    | pat -> failwith $"unexpected pattern: %A{pat}"

let mkBindingReturnInfo creationAide (returnInfo: SynBindingReturnInfo option) =
    Option.bind
        (fun (SynBindingReturnInfo(typeName = t; trivia = trivia)) ->
            trivia.ColonRange
            |> Option.map (fun mColon ->
                let m = unionRanges mColon t.Range
                BindingReturnInfoNode(stn ":" mColon, mkType creationAide t, m)))
        returnInfo

let (|OperatorWithStar|_|) (si: SynIdent) =
    match si with
    | SynIdent(ident, Some(ParenStarSynIdent(_, text, _))) ->
        Some(IdentifierOrDot.Ident(stn $"( %s{text} )" ident.idRange))
    | _ -> None

let mkBinding
    (creationAide: CreationAide)
    (SynBinding(_, _, _, isMutable, attributes, xmlDoc, _, pat, returnInfo, expr, _, _, trivia))
    =
    let mkFunctionName (sli: SynLongIdent) : IdentListNode =
        match sli.IdentsWithTrivia with
        | [ prefix; OperatorWithStar operatorNode ] ->
            IdentListNode(
                [ IdentifierOrDot.Ident(mkSynIdent prefix)
                  IdentifierOrDot.UnknownDot
                  operatorNode ],
                sli.Range
            )
        | [ OperatorWithStar operatorNode ] -> IdentListNode([ operatorNode ], sli.Range)
        | _ -> mkSynLongIdent sli

    let ao, functionName, genericParameters, parameters =
        match pat with
        | SynPat.LongIdent(accessibility = ao; longDotId = lid; typarDecls = typarDecls; argPats = SynArgPats.Pats ps) ->
            ao,
            Choice1Of2(mkFunctionName lid),
            mkSynValTyparDecls creationAide typarDecls,
            List.map (mkPat creationAide) ps
        | SynPat.Named(accessibility = ao; ident = si) ->
            let name =
                match si with
                | OperatorWithStar operatorNode -> operatorNode
                | _ -> IdentifierOrDot.Ident(mkSynIdent si)

            let m =
                let (SynIdent(ident, _)) = si
                ident.idRange

            ao, Choice1Of2(IdentListNode([ name ], m)), None, []
        | _ -> None, Choice2Of2(mkPat creationAide pat), None, []

    let equals =
        let equalsRange =
            match trivia.EqualsRange with
            | Some r -> r
            | None -> failwith $"failed to get equals range in %s{nameof mkBinding}"

        stn "=" equalsRange

    let e = parseExpressionInSynBinding returnInfo expr

    let returnTypeNode = mkBindingReturnInfo creationAide returnInfo

    let range =
        let start =
            if not xmlDoc.IsEmpty then
                xmlDoc.Range
            elif not attributes.IsEmpty then
                attributes.Head.Range
            else
                match trivia.LeadingKeyword, pat with
                | SynLeadingKeyword.Member _, SynPat.LongIdent(extraId = Some _) -> pat.Range
                | _ -> trivia.LeadingKeyword.Range

        unionRanges start e.Range

    BindingNode(
        mkXmlDoc xmlDoc,
        mkAttributes creationAide attributes,
        mkSynLeadingKeyword trivia.LeadingKeyword,
        isMutable,
        Option.map (stn "inline") trivia.InlineKeyword,
        mkSynAccess ao,
        functionName,
        genericParameters,
        parameters,
        returnTypeNode,
        equals,
        (mkExpr creationAide e),
        range
    )

let mkExternBinding
    (creationAide: CreationAide)
    (SynBinding(
        accessibility = accessibility
        attributes = attributes
        xmlDoc = xmlDoc
        headPat = pat
        returnInfo = returnInfo
        range = range
        trivia = trivia))
    : ExternBindingNode =
    let m =
        if not xmlDoc.IsEmpty then
            unionRanges xmlDoc.Range pat.Range
        else
            match attributes with
            | [] -> range
            | head :: _ -> unionRanges head.Range pat.Range

    let externNode =
        match trivia.LeadingKeyword with
        | SynLeadingKeyword.Extern mExtern -> stn "extern" mExtern
        | _ -> failwith "Leading keyword should be extern"

    let attributesOfReturnType, returnType =
        match returnInfo with
        | None -> failwith "return info in extern binding should be present"
        | Some(SynBindingReturnInfo(typeName = t; attributes = a)) ->
            let attrs = mkAttributes creationAide a

            let returnType =
                match t with
                | SynType.App(typeName = t) -> mkType creationAide t
                | _ -> mkType creationAide t

            attrs, returnType

    let (|Ampersand|_|) (it: IdentTrivia) =
        match it with
        | IdentTrivia.OriginalNotation "&" -> Some "&"
        | _ -> None

    let (|Star|_|) (it: IdentTrivia) =
        match it with
        | IdentTrivia.OriginalNotation "*" -> Some "*"
        | _ -> None

    let (|ArrayText|_|) (it: Ident) =
        if it.idText = "[]" then Some "[]" else None

    let rec mkExternType t =
        match t with
        | SynType.App(typeName = t; isPostfix = false; typeArgs = []) -> mkType creationAide t
        | SynType.App(
            typeName = SynType.LongIdent(SynLongIdent([ _ ], [], [ Some(IdentTrivia.OriginalNotation "void*") ]))
            isPostfix = true
            typeArgs = []) ->
            IdentListNode([ IdentifierOrDot.Ident(stn "void*" t.Range) ], t.Range)
            |> Type.LongIdent
        | SynType.App(
            typeName = SynType.LongIdent(SynLongIdent([ _ ], [], [ Some(Ampersand suffix | Star suffix) ]) | SynLongIdent(
                id = [ ArrayText suffix ]))
            isPostfix = true
            typeArgs = [ SynType.App(typeName = SynType.LongIdent argLid; isPostfix = false; typeArgs = []) ]) ->
            let lid = mkSynLongIdent argLid

            let lidPieces =
                lid.Content
                |> List.mapWithLast id (function
                    | IdentifierOrDot.KnownDot dot -> IdentifierOrDot.KnownDot dot
                    | IdentifierOrDot.UnknownDot -> IdentifierOrDot.UnknownDot
                    | IdentifierOrDot.Ident ident -> IdentifierOrDot.Ident(stn $"%s{ident.Text}%s{suffix}" ident.Range))

            Type.LongIdent(IdentListNode(lidPieces, t.Range))
        | SynType.App(typeName = typeName; isPostfix = true; typeArgs = [ argType ]) ->
            TypeAppPostFixNode(mkExternType argType, mkExternType typeName, t.Range)
            |> Type.AppPostfix
        | _ -> mkType creationAide t

    let mkExternPat pat =
        match pat with
        | SynPat.Attrib(pat = SynPat.Typed(pat = SynPat.Null _ | SynPat.Wild _; targetType = t); attributes = attributes) ->
            ExternBindingPatternNode(mkAttributes creationAide attributes, Some(mkExternType t), None, pat.Range)
        | SynPat.Attrib(pat = SynPat.Typed(pat = innerPat; targetType = t); attributes = attributes) ->
            ExternBindingPatternNode(
                mkAttributes creationAide attributes,
                Some(mkExternType t),
                Some(mkPat creationAide innerPat),
                pat.Range
            )
        | _ -> ExternBindingPatternNode(None, None, Some(mkPat creationAide pat), pat.Range)

    let identifier, openNode, parameters, closeNode =
        match pat with
        | SynPat.LongIdent(
            longDotId = longDotId
            argPats = SynArgPats.Pats [ SynPat.Tuple(_, ps, _, StartEndRange 1 (mOpen, _, mClose)) ]) ->
            mkSynLongIdent longDotId, stn "(" mOpen, List.map mkExternPat ps, stn ")" mClose
        | _ -> failwith "expecting a SynPat.LongIdent for extern binding"

    ExternBindingNode(
        mkXmlDoc xmlDoc,
        mkAttributes creationAide attributes,
        externNode,
        attributesOfReturnType,
        returnType,
        mkSynAccess accessibility,
        identifier,
        openNode,
        parameters,
        closeNode,
        m
    )

let mkXmlDoc (px: PreXmlDoc) =
    if px.IsEmpty then
        None
    else
        let xmlDoc = px.ToXmlDoc(false, None)
        let lines = Array.map (sprintf "///%s") xmlDoc.UnprocessedLines
        Some(XmlDocNode(lines, xmlDoc.Range))

let mkModuleDecl (creationAide: CreationAide) (decl: SynModuleDecl) =
    let declRange = decl.Range

    match decl with
    | SynModuleDecl.Expr(e, _) -> mkExpr creationAide e |> ModuleDecl.DeclExpr
    | SynModuleDecl.Exception(SynExceptionDefn(SynExceptionDefnRepr(attrs, caseName, _, xmlDoc, vis, _),
                                               withKeyword,
                                               ms,
                                               _),
                              _) ->
        ExceptionDefnNode(
            mkXmlDoc xmlDoc,
            mkAttributes creationAide attrs,
            mkSynAccess vis,
            mkSynUnionCase creationAide caseName,
            Option.map (stn "with") withKeyword,
            List.map (mkMemberDefn creationAide) ms,
            declRange
        )
        |> ModuleDecl.Exception
    | SynModuleDecl.Let(_, [ SynBinding(trivia = { LeadingKeyword = SynLeadingKeyword.Extern _ }) as binding ], _) ->
        mkExternBinding creationAide binding |> ModuleDecl.ExternBinding
    | SynModuleDecl.Let(bindings = [ singleBinding ]) ->
        mkBinding creationAide singleBinding |> ModuleDecl.TopLevelBinding
    | SynModuleDecl.ModuleAbbrev(ident, lid, StartRange 6 (mModule, _)) ->
        ModuleAbbrevNode(stn "module" mModule, mkIdent ident, mkLongIdent lid, declRange)
        |> ModuleDecl.ModuleAbbrev
    | SynModuleDecl.NestedModule(SynComponentInfo(ats, _, _, lid, px, _, ao, _),
                                 isRecursive,
                                 decls,
                                 _,
                                 _,
                                 { ModuleKeyword = Some mModule
                                   EqualsRange = Some mEq }) ->
        NestedModuleNode(
            mkXmlDoc px,
            mkAttributes creationAide ats,
            stn "module" mModule,
            mkSynAccess ao,
            isRecursive,
            mkLongIdent lid,
            stn "=" mEq,
            mkModuleDecls creationAide decls id,
            declRange
        )
        |> ModuleDecl.NestedModule
    | decl -> failwithf $"Failed to create ModuleDecl for %A{decl}"

let mkSynTyparDecl
    (creationAide: CreationAide)
    (SynTyparDecl(attributes = attrs; typar = typar; intersectionConstraints = intersectionConstraints; trivia = trivia))
    =
    let m =
        match List.tryHead attrs with
        | None -> typar.Range
        | Some a -> unionRanges a.Range typar.Range

    let intersectionConstraintNodes =
        if intersectionConstraints.Length <> trivia.AmpersandRanges.Length then
            failwith "Unexpected mismatch in SynTyparDecl between intersectionConstraints and AmpersandRanges"
        else
            (trivia.AmpersandRanges, intersectionConstraints)
            ||> List.zip
            |> List.collect (fun (amp, t) -> [ Choice2Of2(stn "&" amp); Choice1Of2(mkType creationAide t) ])

    TyparDeclNode(mkAttributes creationAide attrs, mkSynTypar typar, intersectionConstraintNodes, m)

let mkSynTyparDecls (creationAide: CreationAide) (tds: SynTyparDecls) : TyparDecls =
    match tds with
    | SynTyparDecls.PostfixList(decls, constraints, StartEndRange 1 (mOpen, m, mClose)) ->
        let decls = List.map (mkSynTyparDecl creationAide) decls
        let constraints = List.map (mkSynTypeConstraint creationAide) constraints

        TyparDeclsPostfixListNode(stn "<" mOpen, decls, constraints, stn ">" mClose, m)
        |> TyparDecls.PostfixList
    | SynTyparDecls.PrefixList(decls, StartEndRange 1 (mOpen, m, mClose)) ->
        let decls = List.map (mkSynTyparDecl creationAide) decls

        TyparDeclsPrefixListNode(stn "(" mOpen, decls, stn ")" mClose, m)
        |> TyparDecls.PrefixList
    | SynTyparDecls.SinglePrefix(decl, _) -> mkSynTyparDecl creationAide decl |> TyparDecls.SinglePrefix

let mkSynValTyparDecls (creationAide: CreationAide) (vt: SynValTyparDecls option) : TyparDecls option =
    match vt with
    | None -> None
    | Some(SynValTyparDecls(tds, _)) -> Option.map (mkSynTyparDecls creationAide) tds

let mkSynRationalConst (creationAide: CreationAide) rc =
    let rec visit rc =
        match rc with
        | SynRationalConst.Integer(i, range) ->
            stn (creationAide.TextFromSource (fun () -> string<int> i) range) range
            |> RationalConstNode.Integer

        | SynRationalConst.Paren(SynRationalConst.Rational(numerator,
                                                           numeratorRange,
                                                           divRange,
                                                           denominator,
                                                           denominatorRange,
                                                           _),
                                 range) ->
            let openingParen =
                let r =
                    withEnd (Position.mkPos range.Start.Line (range.StartRange.StartColumn + 1)) range.StartRange

                stn "(" r

            let n =
                stn (creationAide.TextFromSource (fun () -> string<int> numerator) numeratorRange) numeratorRange

            let div = stn "/" divRange

            let d =
                stn (creationAide.TextFromSource (fun () -> string<int> denominator) denominatorRange) denominatorRange

            let closingParen =
                let r =
                    withStart (Position.mkPos range.End.Line (range.End.Column - 1)) range.EndRange

                stn ")" r

            RationalConstNode.Rational(RationalNode(openingParen, n, div, d, closingParen, range))
        | SynRationalConst.Paren(innerRc, _) -> visit innerRc
        | SynRationalConst.Negate(innerRc, range) ->
            RationalConstNode.Negate(NegateRationalNode(stn "-" range.StartRange, visit innerRc, range))
        | SynRationalConst.Rational _ -> failwith "SynRationalConst.Rational not wrapped in SynRationalConst.Paren"

    visit rc

let mkSynTypar (SynTypar(ident, req, _)) =
    let range =
        mkRange
            ident.idRange.FileName
            (Position.mkPos ident.idRange.StartLine (ident.idRange.StartColumn - 1))
            ident.idRange.End

    let identText =
        let width = ident.idRange.EndColumn - ident.idRange.StartColumn
        // 5 because of ^ or ' and `` on each side
        if ident.idText.Length + 5 = width then
            $"``%s{ident.idText}``"
        else
            ident.idText

    match req with
    | TyparStaticReq.None -> stn $"'%s{identText}" range
    | TyparStaticReq.HeadType -> stn $"^%s{identText}" range

let mkSynTypeConstraint (creationAide: CreationAide) (tc: SynTypeConstraint) : TypeConstraint =
    match tc with
    | SynTypeConstraint.WhereTyparIsValueType(tp, EndRange 6 (mKeyword, m)) ->
        TypeConstraintSingleNode(mkSynTypar tp, stn "struct" mKeyword, m)
        |> TypeConstraint.Single
    | SynTypeConstraint.WhereTyparIsReferenceType(tp, EndRange 10 (mKeyword, m)) ->
        TypeConstraintSingleNode(mkSynTypar tp, stn "not struct" mKeyword, m)
        |> TypeConstraint.Single
    | SynTypeConstraint.WhereTyparIsUnmanaged(tp, EndRange 9 (mKeyword, m)) ->
        TypeConstraintSingleNode(mkSynTypar tp, stn "unmanaged" mKeyword, m)
        |> TypeConstraint.Single
    | SynTypeConstraint.WhereTyparSupportsNull(tp, EndRange 4 (mKeyword, m)) ->
        TypeConstraintSingleNode(mkSynTypar tp, stn "null" mKeyword, m)
        |> TypeConstraint.Single
    | SynTypeConstraint.WhereTyparIsComparable(tp, EndRange 10 (mKeyword, m)) ->
        TypeConstraintSingleNode(mkSynTypar tp, stn "comparison" mKeyword, m)
        |> TypeConstraint.Single
    | SynTypeConstraint.WhereTyparIsEquatable(tp, EndRange 8 (mKeyword, m)) ->
        TypeConstraintSingleNode(mkSynTypar tp, stn "equality" mKeyword, m)
        |> TypeConstraint.Single
    | SynTypeConstraint.WhereTyparDefaultsToType(tp, t, StartRange 7 (mDefaults, m)) ->
        TypeConstraintDefaultsToTypeNode(stn "default" mDefaults, mkSynTypar tp, mkType creationAide t, m)
        |> TypeConstraint.DefaultsToType
    | SynTypeConstraint.WhereTyparSubtypeOfType(tp, t, m) ->
        TypeConstraintSubtypeOfTypeNode(mkSynTypar tp, mkType creationAide t, m)
        |> TypeConstraint.SubtypeOfType
    | SynTypeConstraint.WhereTyparSupportsMember(tps, msg, m) ->
        TypeConstraintSupportsMemberNode(mkType creationAide tps, mkMemberSig creationAide msg, m)
        |> TypeConstraint.SupportsMember
    | SynTypeConstraint.WhereTyparIsEnum(tp, ts, m) ->
        TypeConstraintEnumOrDelegateNode(mkSynTypar tp, "enum", List.map (mkType creationAide) ts, m)
        |> TypeConstraint.EnumOrDelegate
    | SynTypeConstraint.WhereTyparIsDelegate(tp, ts, m) ->
        TypeConstraintEnumOrDelegateNode(mkSynTypar tp, "delegate", List.map (mkType creationAide) ts, m)
        |> TypeConstraint.EnumOrDelegate
    | SynTypeConstraint.WhereSelfConstrained(t, _) -> mkType creationAide t |> TypeConstraint.WhereSelfConstrained

// Arrow type is right-associative
let rec (|TFuns|_|) =
    function
    | SynType.Fun(t1, TFuns(ts, ret), _, trivia) -> Some((t1, trivia.ArrowRange) :: ts, ret)
    | SynType.Fun(t1, t2, _, trivia) -> Some([ t1, trivia.ArrowRange ], t2)
    | _ -> None

let mkTypeList creationAide ts rt m =
    let parameters =
        ts |> List.map (fun (t, mArrow) -> mkType creationAide t, stn "->" mArrow)

    TypeFunsNode(parameters, mkType creationAide rt, m)

let mkType (creationAide: CreationAide) (t: SynType) : Type =
    let typeRange = t.Range

    match t with
    | TFuns(ts, rt) -> mkTypeList creationAide ts rt typeRange |> Type.Funs
    | SynType.Tuple(false, ts, _) ->
        let path =
            ts
            |> List.map (function
                | SynTupleTypeSegment.Type t -> Choice1Of2(mkType creationAide t)
                | SynTupleTypeSegment.Slash m -> Choice2Of2(stn "/" m)
                | SynTupleTypeSegment.Star m -> Choice2Of2(stn "*" m))

        TypeTupleNode(path, typeRange) |> Type.Tuple
    | SynType.Tuple(true, ts, (StartRange 6 (mStruct, _) & StartEndRange 1 (_, _, closingParen))) ->
        let path =
            ts
            |> List.map (function
                | SynTupleTypeSegment.Type t -> Choice1Of2(mkType creationAide t)
                | SynTupleTypeSegment.Slash m -> Choice2Of2(stn "/" m)
                | SynTupleTypeSegment.Star m -> Choice2Of2(stn "*" m))

        TypeStructTupleNode(stn "struct" mStruct, path, stn ")" closingParen, typeRange)
        |> Type.StructTuple
    | SynType.HashConstraint(t, StartRange 1 (mHash, _)) ->
        TypeHashConstraintNode(stn "#" mHash, mkType creationAide t, typeRange)
        |> Type.HashConstraint
    | SynType.MeasurePower(t, rc, _) ->
        TypeMeasurePowerNode(mkType creationAide t, mkSynRationalConst creationAide rc, typeRange)
        |> Type.MeasurePower
    | SynType.StaticConstant(SynConst.String(null, kind, mString), r) ->
        mkConstant creationAide (SynConst.String("null", kind, mString)) r
        |> Type.StaticConstant
    | SynType.StaticConstant(c, r) -> mkConstant creationAide c r |> Type.StaticConstant
    | SynType.StaticConstantExpr(e, StartRange 5 (mConst, _)) ->
        TypeStaticConstantExprNode(stn "const" mConst, mkExpr creationAide e, typeRange)
        |> Type.StaticConstantExpr
    | SynType.StaticConstantNamed(t1, t2, _) ->
        TypeStaticConstantNamedNode(mkType creationAide t1, mkType creationAide t2, typeRange)
        |> Type.StaticConstantNamed
    | SynType.Array(rank, t, _) -> TypeArrayNode(mkType creationAide t, rank, typeRange) |> Type.Array
    | SynType.Anon _ -> stn "_" typeRange |> Type.Anon
    | SynType.Var(tp, _) -> mkSynTypar tp |> Type.Var
    | SynType.App(t1, None, [ t2 ], _commaRanges, None, true, _) ->
        TypeAppPostFixNode(mkType creationAide t2, mkType creationAide t1, typeRange)
        |> Type.AppPostfix
    | SynType.App(t, Some mLt, args, _commaRanges, Some mGt, false, _) ->
        TypeAppPrefixNode(
            mkType creationAide t,
            None,
            stn "<" mLt,
            List.map (mkType creationAide) args,
            stn ">" mGt,
            typeRange
        )
        |> Type.AppPrefix
    | SynType.LongIdentApp(t, lid, Some mLt, args, _, Some mGt, _) ->
        TypeAppPrefixNode(
            mkType creationAide t,
            Some(mkSynLongIdent lid),
            stn "<" mLt,
            List.map (mkType creationAide) args,
            stn ">" mGt,
            typeRange
        )
        |> Type.AppPrefix
    | SynType.LongIdentApp(t, lid, None, [], _, None, _) ->
        TypeLongIdentAppNode(mkType creationAide t, mkSynLongIdent lid, typeRange)
        |> Type.LongIdentApp
    | SynType.WithGlobalConstraints(SynType.Var _, [ SynTypeConstraint.WhereTyparSubtypeOfType _ as tc ], _) ->
        mkSynTypeConstraint creationAide tc |> Type.WithSubTypeConstraint
    | SynType.WithGlobalConstraints(t, tcs, _) ->
        TypeWithGlobalConstraintsNode(mkType creationAide t, List.map (mkSynTypeConstraint creationAide) tcs, typeRange)
        |> Type.WithGlobalConstraints
    | SynType.LongIdent lid -> Type.LongIdent(mkSynLongIdent lid)
    | SynType.AnonRecd(isStruct, fields, StartEndRange 2 (_, r, mClosing)) ->
        let structNode, openingNode =
            if isStruct then
                match r with
                | StartRange 6 (mStruct, _) -> Some(stn "struct" mStruct), None
            else
                match r with
                | StartRange 2 (mOpening, _) -> None, Some(stn "{|" mOpening)

        let fields = fields |> List.map (fun (i, t) -> mkIdent i, mkType creationAide t)

        TypeAnonRecordNode(structNode, openingNode, fields, stn "|}" mClosing, typeRange)
        |> Type.AnonRecord
    | SynType.Paren(innerType, StartEndRange 1 (lpr, _, rpr)) ->
        TypeParenNode(stn "(" lpr, mkType creationAide innerType, stn ")" rpr, typeRange)
        |> Type.Paren
    | SynType.SignatureParameter(attrs, isOptional, identOpt, t, _) ->
        let identNode =
            identOpt
            |> Option.map (fun ident ->
                let identNode = mkIdent ident

                if not isOptional then
                    identNode
                else
                    SingleTextNode($"?%s{identNode.Text}", ident.idRange))

        TypeSignatureParameterNode(mkAttributes creationAide attrs, identNode, mkType creationAide t, typeRange)
        |> Type.SignatureParameter
    | SynType.Or(lhs, rhs, _, trivia) ->
        TypeOrNode(mkType creationAide lhs, stn "or" trivia.OrKeyword, mkType creationAide rhs, typeRange)
        |> Type.Or
    | SynType.Intersection(optTypar, ts, m, trivia) ->
        let typesAndSeparators =
            let headNode, ts =
                match optTypar with
                | Some typar ->
                    // We model the typar as Type.Var out of convenience
                    Type.Var(mkSynTypar typar), ts
                | None ->
                    match ts with
                    | [] -> failwith "SynType.Intersection does not contain typar or any intersectionConstraints"
                    | head :: tail -> mkType creationAide head, tail

            assert (ts.Length = trivia.AmpersandRanges.Length)

            [ yield Choice1Of2 headNode
              for t, mAmp in List.zip ts trivia.AmpersandRanges do
                  yield Choice2Of2(stn "&" mAmp)
                  yield Choice1Of2(mkType creationAide t) ]

        TypeIntersectionNode(typesAndSeparators, m) |> Type.Intersection
    | t -> failwith $"unexpected type: %A{t}"

let rec (|OpenL|_|) =
    function
    | SynModuleDecl.Open(target, range) :: OpenL(xs, ys) -> Some((target, range) :: xs, ys)
    | SynModuleDecl.Open(target, range) :: ys -> Some([ target, range ], ys)
    | _ -> None

let mkOpenNodeForImpl (creationAide: CreationAide) (target, range) : Open =
    match target with
    | SynOpenDeclTarget.ModuleOrNamespace(longId, _) ->
        OpenModuleOrNamespaceNode(mkSynLongIdent longId, range)
        |> Open.ModuleOrNamespace
    | SynOpenDeclTarget.Type(typeName, _) -> OpenTargetNode(mkType creationAide typeName, range) |> Open.Target

let rec (|HashDirectiveL|_|) =
    function
    | SynModuleDecl.HashDirective(p, _) :: HashDirectiveL(xs, ys) -> Some(p :: xs, ys)
    | SynModuleDecl.HashDirective(p, _) :: ys -> Some([ p ], ys)
    | _ -> None

let mkSynLeadingKeyword (lk: SynLeadingKeyword) =
    let mtn v =
        v
        |> List.map (fun (t, r) -> stn t r)
        |> fun nodes -> MultipleTextsNode(nodes, lk.Range)

    match lk with
    | SynLeadingKeyword.Let letRange -> mtn [ "let", letRange ]
    | SynLeadingKeyword.LetRec(letRange, recRange) -> mtn [ "let", letRange; "rec", recRange ]
    | SynLeadingKeyword.And andRange -> mtn [ "and", andRange ]
    | SynLeadingKeyword.Use useRange -> mtn [ "use", useRange ]
    | SynLeadingKeyword.UseRec(useRange, recRange) -> mtn [ "use", useRange; "rec", recRange ]
    | SynLeadingKeyword.Extern externRange -> mtn [ "extern", externRange ]
    | SynLeadingKeyword.Member memberRange -> mtn [ "member", memberRange ]
    | SynLeadingKeyword.MemberVal(memberRange, valRange) -> mtn [ "member", memberRange; "val", valRange ]
    | SynLeadingKeyword.Override overrideRange -> mtn [ "override", overrideRange ]
    | SynLeadingKeyword.OverrideVal(overrideRange, valRange) -> mtn [ "override", overrideRange; "val", valRange ]
    | SynLeadingKeyword.Abstract abstractRange -> mtn [ "abstract", abstractRange ]
    | SynLeadingKeyword.AbstractMember(abstractRange, memberRange) ->
        mtn [ "abstract", abstractRange; "member", memberRange ]
    | SynLeadingKeyword.Static staticRange -> mtn [ "static", staticRange ]
    | SynLeadingKeyword.StaticMember(staticRange, memberRange) -> mtn [ "static", staticRange; "member", memberRange ]
    | SynLeadingKeyword.StaticMemberVal(staticRange, memberRange, valRange) ->
        mtn [ "static", staticRange; "member", memberRange; "val", valRange ]
    | SynLeadingKeyword.StaticAbstract(staticRange, abstractRange) ->
        mtn [ "static", staticRange; "abstract", abstractRange ]
    | SynLeadingKeyword.StaticAbstractMember(staticRange, abstractMember, memberRange) ->
        mtn [ "static", staticRange; "abstract", abstractMember; "member", memberRange ]
    | SynLeadingKeyword.StaticVal(staticRange, valRange) -> mtn [ "static", staticRange; "val", valRange ]
    | SynLeadingKeyword.StaticLet(staticRange, letRange) -> mtn [ "static", staticRange; "let", letRange ]
    | SynLeadingKeyword.StaticLetRec(staticRange, letRange, recRange) ->
        mtn [ "static", staticRange; "let", letRange; "rec", recRange ]
    | SynLeadingKeyword.StaticDo(staticRange, doRange) -> mtn [ "static", staticRange; "do", doRange ]
    | SynLeadingKeyword.Default defaultRange -> mtn [ "default", defaultRange ]
    | SynLeadingKeyword.DefaultVal(defaultRange, valRange) -> mtn [ "default", defaultRange; "val", valRange ]
    | SynLeadingKeyword.Val valRange -> mtn [ "val", valRange ]
    | SynLeadingKeyword.New newRange -> mtn [ "new", newRange ]
    | SynLeadingKeyword.Do doRange -> mtn [ "do", doRange ]
    | SynLeadingKeyword.Synthetic -> failwith "Unexpected SynLeadingKeyword.Synthetic"

let mkSynField
    (creationAide: CreationAide)
    (SynField(ats,
              _isStatic,
              ido,
              t,
              _,
              px,
              ao,
              range,
              { LeadingKeyword = lk
                MutableKeyword = mk }))
    =
    FieldNode(
        mkXmlDoc px,
        mkAttributes creationAide ats,
        Option.map mkSynLeadingKeyword lk,
        Option.map (stn "mutable") mk,
        mkSynAccess ao,
        Option.map mkIdent ido,
        mkType creationAide t,
        range
    )

let mkSynUnionCase
    (creationAide: CreationAide)
    (SynUnionCase(attributes, ident, caseType, xmlDoc, _vis, m, trivia))
    : UnionCaseNode =
    let fullRange =
        if not xmlDoc.IsEmpty then
            m
        else
            match trivia.BarRange with
            | None -> m
            | Some barRange -> unionRanges barRange m

    let fields =
        match caseType with
        | SynUnionCaseKind.FullType _ -> []
        | SynUnionCaseKind.Fields cases -> List.map (mkSynField creationAide) cases

    UnionCaseNode(
        mkXmlDoc xmlDoc,
        mkAttributes creationAide attributes,
        Option.map (stn "|") trivia.BarRange,
        mkSynIdent ident,
        fields,
        fullRange
    )

let mkImplicitCtor
    (creationAide: CreationAide)
    (vis: SynAccess option)
    (attrs: SynAttributeList list)
    (pat: SynPat)
    (self: (range * Ident) option)
    (xmlDoc: PreXmlDoc)
    : ImplicitConstructorNode =
    let range =
        let startRange =
            if not xmlDoc.IsEmpty then xmlDoc.Range
            else if not attrs.IsEmpty then attrs.[0].Range
            else pat.Range

        let endRange =
            match self with
            | Some(_, self) -> self.idRange
            | None -> pat.Range

        unionRanges startRange endRange

    let asSelfNode =
        match self with
        | None -> None
        | Some(mAs, self) ->
            let m = unionRanges mAs self.idRange
            Some(AsSelfIdentifierNode(stn "as" mAs, mkIdent self, m))

    ImplicitConstructorNode(
        mkXmlDoc xmlDoc,
        mkAttributes creationAide attrs,
        mkSynAccess vis,
        mkPat creationAide pat,
        asSelfNode,
        range
    )

let mkTypeDefn
    (creationAide: CreationAide)
    (SynTypeDefn(typeInfo, typeRepr, members, implicitConstructor, range, trivia))
    : TypeDefn =
    let typeNameNode =
        match typeInfo with
        | SynComponentInfo(ats, tds, tcs, lid, px, _preferPostfix, ao, _) ->
            let identifierNode = mkLongIdent lid
            let mIdentifierNode = identifierNode.Range

            let leadingKeyword =
                match trivia.LeadingKeyword with
                | SynTypeDefnLeadingKeyword.Type mType -> stn "type" mType
                | SynTypeDefnLeadingKeyword.And mAnd -> stn "and" mAnd
                | SynTypeDefnLeadingKeyword.StaticType _
                | SynTypeDefnLeadingKeyword.Synthetic -> failwithf "unexpected %A" trivia.LeadingKeyword

            let implicitConstructorNode =
                match implicitConstructor with
                | Some(SynMemberDefn.ImplicitCtor(vis, attrs, pats, self, xmlDoc, _, trivia)) ->
                    let self =
                        match self, trivia.AsKeyword with
                        | Some self, Some mAs -> Some(mAs, self)
                        | _ -> None

                    mkImplicitCtor creationAide vis attrs pats self xmlDoc |> Some
                | _ -> None

            let m =
                let startRange =
                    if not px.IsEmpty then
                        px.Range
                    elif leadingKeyword.Text = "and" then
                        leadingKeyword.Range
                    else
                        match ats with
                        | [] -> leadingKeyword.Range
                        | firstAttr :: _ -> firstAttr.Range

                let endRange =
                    match trivia.EqualsRange with
                    | None -> mIdentifierNode
                    | Some mEq -> mEq

                unionRanges startRange endRange

            TypeNameNode(
                mkXmlDoc px,
                mkAttributes creationAide ats,
                leadingKeyword,
                mkSynAccess ao,
                identifierNode,
                Option.map (mkSynTyparDecls creationAide) tds,
                List.map (mkSynTypeConstraint creationAide) tcs,
                implicitConstructorNode,
                Option.map (stn "=") trivia.EqualsRange,
                Option.map (stn "with") trivia.WithKeyword,
                m
            )

    let members = List.map (mkMemberDefn creationAide) members
    let typeDefnRange = unionRanges typeNameNode.Range range

    match typeRepr with
    | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.Enum(ecs, _)) ->
        let enumCases =
            ecs
            |> List.map (fun (SynEnumCase(attributes, ident, valueExpr, xmlDoc, range, trivia)) ->
                EnumCaseNode(
                    mkXmlDoc xmlDoc,
                    Option.map (stn "|") trivia.BarRange,
                    mkAttributes creationAide attributes,
                    mkSynIdent ident,
                    stn "=" trivia.EqualsRange,
                    mkExpr creationAide valueExpr,
                    range
                ))

        TypeDefnEnumNode(typeNameNode, enumCases, members, typeDefnRange)
        |> TypeDefn.Enum

    | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.Union(ao, cases, _)) ->
        let unionCases = cases |> List.map (mkSynUnionCase creationAide)

        TypeDefnUnionNode(typeNameNode, mkSynAccess ao, unionCases, members, typeDefnRange)
        |> TypeDefn.Union

    | SynTypeDefnRepr.Simple(
        simpleRepr = SynTypeDefnSimpleRepr.Record(ao, fs, StartEndRange 1 (openingBrace, _, closingBrace))) ->
        let fields = List.map (mkSynField creationAide) fs

        TypeDefnRecordNode(
            typeNameNode,
            mkSynAccess ao,
            stn "{" openingBrace,
            fields,
            stn "}" closingBrace,
            members,
            typeDefnRange
        )
        |> TypeDefn.Record

    | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.TypeAbbrev(rhsType = t)) ->
        TypeDefn.Abbrev(TypeDefnAbbrevNode(typeNameNode, mkType creationAide t, members, typeDefnRange))

    | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.None _) -> TypeDefn.None typeNameNode

    | SynTypeDefnRepr.ObjectModel(
        kind = SynTypeDefnKind.Class | SynTypeDefnKind.Interface | SynTypeDefnKind.Struct as tdk
        members = objectMembers
        range = range) ->
        let kindNode =
            match tdk, range with
            | SynTypeDefnKind.Class, StartRange 5 (mClass, _) -> stn "class" mClass
            | SynTypeDefnKind.Interface, StartRange 9 (mInterface, _) -> stn "interface" mInterface
            | SynTypeDefnKind.Struct, StartRange 6 (mStruct, _) -> stn "struct" mStruct
            | _ -> failwith "unexpected kind"

        let objectMembers =
            objectMembers
            |> List.filter (function
                | SynMemberDefn.ImplicitCtor _ -> false
                | _ -> true)
            |> List.map (mkMemberDefn creationAide)

        let endNode =
            match range with
            | EndRange 3 (mEnd, _) -> stn "end" mEnd

        let body = TypeDefnExplicitBodyNode(kindNode, objectMembers, endNode, range)

        TypeDefnExplicitNode(typeNameNode, body, members, typeDefnRange)
        |> TypeDefn.Explicit

    | SynTypeDefnRepr.ObjectModel(kind = SynTypeDefnKind.Augmentation mWith) ->
        let typeNameNode =
            TypeNameNode(
                typeNameNode.XmlDoc,
                typeNameNode.Attributes,
                typeNameNode.LeadingKeyword,
                typeNameNode.Accessibility,
                typeNameNode.Identifier,
                typeNameNode.TypeParameters,
                typeNameNode.Constraints,
                None,
                None,
                Some(stn "with" mWith),
                typeNameNode.Range
            )

        TypeDefnAugmentationNode(typeNameNode, members, typeDefnRange)
        |> TypeDefn.Augmentation

    | SynTypeDefnRepr.ObjectModel(
        kind = SynTypeDefnKind.Delegate(signature = TFuns(ts, rt) as st); range = StartRange 8 (mDelegate, _)) ->
        let typeList = mkTypeList creationAide ts rt st.Range

        TypeDefnDelegateNode(typeNameNode, stn "delegate" mDelegate, typeList, typeDefnRange)
        |> TypeDefn.Delegate

    | SynTypeDefnRepr.ObjectModel(members = objectMembers) ->

        let allMembers =
            let objectMembers =
                objectMembers
                |> List.filter (function
                    | SynMemberDefn.ImplicitCtor _ -> false
                    | _ -> true)
                |> List.map (mkMemberDefn creationAide)

            [ yield! objectMembers; yield! members ]

        TypeDefnRegularNode(typeNameNode, allMembers, typeDefnRange) |> TypeDefn.Regular
    | _ -> failwithf "Could not create a TypeDefn for %A" typeRepr

let mkWithGetSet (withKeyword: range option) (getSet: GetSetKeywords option) =
    match withKeyword, getSet with
    | Some mWith, Some gs ->
        let withNode = stn "with" mWith
        let m = unionRanges mWith gs.Range

        match gs with
        | GetSetKeywords.Get mGet -> Some(MultipleTextsNode([ withNode; stn "get" mGet ], m))
        | GetSetKeywords.Set mSet -> Some(MultipleTextsNode([ withNode; stn "set" mSet ], m))
        | GetSetKeywords.GetSet(mGet, mSet) ->
            if rangeBeforePos mGet mSet.Start then
                Some(MultipleTextsNode([ withNode; stn "get," mGet; stn "set" mSet ], m))
            else
                Some(MultipleTextsNode([ withNode; stn "set," mSet; stn "get" mGet ], m))
    | _ -> None

let mkPropertyGetSetBinding
    (creationAide: CreationAide)
    (withOrAndKeyword: range)
    (accessibility: SynAccess option)
    (leadingKeyword: SingleTextNode)
    (binding: SynBinding)
    : PropertyGetSetBindingNode =
    match binding with
    | SynBinding(
        attributes = attributes
        headPat = SynPat.LongIdent(extraId = Some extraIdent; argPats = SynArgPats.Pats ps)
        returnInfo = returnInfo
        expr = expr
        trivia = { EqualsRange = Some mEq
                   InlineKeyword = inlineKw }) ->
        // Attribute are not accurate in this case.
        // The binding could contain attributes for the entire member and the getter or setter member.
        // We use the `with` or `and` keyword to filter them.
        let attributes =
            attributes
            |> List.map (fun al ->
                { al with
                    Attributes =
                        al.Attributes
                        |> List.filter (fun a -> Position.posGt a.Range.Start withOrAndKeyword.End) })

        let e = parseExpressionInSynBinding returnInfo expr
        let returnTypeNode = mkBindingReturnInfo creationAide returnInfo

        let pats =
            match ps with
            | [ SynPat.Tuple(false, ps, commas, _) ] when
                // This is the case for an indexer setter.
                // The AST is weird in this case and doesn't properly reflect what the user wrote.
                // It will represent `set (x: int, y: int, z: int) v` as a single tuple with 4 patterns and 2 commas.
                ps.Length - 2 = commas.Length
                ->

                let tuplePat =
                    let tuplePs = List.take (ps.Length - 1) ps
                    let mTuple = tuplePs |> List.map (fun p -> p.Range) |> List.reduce unionRanges

                    match tuplePs with
                    // If there is only a single element, it does not need any additional parentheses.
                    | [ singlePat ] -> singlePat
                    | _ -> SynPat.Paren(SynPat.Tuple(false, tuplePs, commas, mTuple), mTuple)
                    |> mkPat creationAide

                [ tuplePat
                  match List.tryLast ps with
                  | None -> failwith ""
                  | Some indexerPat -> mkPat creationAide indexerPat ]
            | [ SynPat.Tuple(false, [ p1; p2 ], _, _) ] -> [ mkPat creationAide p1; mkPat creationAide p2 ]
            | ps -> List.map (mkPat creationAide) ps

        let range = unionRanges extraIdent.idRange e.Range

        PropertyGetSetBindingNode(
            Option.map (stn "inline") inlineKw,
            mkAttributes creationAide attributes,
            mkSynAccess accessibility,
            leadingKeyword,
            pats,
            returnTypeNode,
            stn "=" mEq,
            mkExpr creationAide e,
            range
        )
    | _ -> failwith "SynBinding does not expected information for PropertyGetSetBinding"

let mkMemberDefn (creationAide: CreationAide) (md: SynMemberDefn) =
    let memberDefinitionRange = md.Range

    match md with
    | SynMemberDefn.ImplicitInherit(t, e, _, StartRange 7 (mInherit, _)) ->
        mkInheritConstructor creationAide t e mInherit memberDefinitionRange
        |> MemberDefn.ImplicitInherit

    // Transforms: `member this.Y with get() = "meh"` into `member this.Y = "meh"`
    | SynMemberDefn.GetSetMember(Some(SynBinding(_,
                                                 kind,
                                                 isInline,
                                                 isMutable,
                                                 ats,
                                                 px,
                                                 valData,
                                                 SynPat.LongIdent(lid,
                                                                  extraId,
                                                                  typarDecls,
                                                                  SynArgPats.Pats [ SynPat.Paren(
                                                                                        pat = SynPat.Const(
                                                                                            constant = SynConst.Unit)) ],
                                                                  ao,
                                                                  mPat),
                                                 ri,
                                                 e,
                                                 bindingRange,
                                                 dp,
                                                 trivia)),
                                 None,
                                 _,
                                 { GetKeyword = Some _ }) ->

        let pat = SynPat.LongIdent(lid, extraId, typarDecls, SynArgPats.Pats([]), ao, mPat)

        mkBinding
            creationAide
            (SynBinding(None, kind, isInline, isMutable, ats, px, valData, pat, ri, e, bindingRange, dp, trivia))
        |> MemberDefn.Member
    | SynMemberDefn.Member(
        memberDefn = SynBinding(
            attributes = ats
            xmlDoc = px
            valData = SynValData(memberFlags = Some { MemberKind = SynMemberKind.Constructor }; thisIdOpt = ido)
            headPat = SynPat.LongIdent(
                longDotId = SynLongIdent(id = [ newIdent ])
                argPats = SynArgPats.Pats [ SynPat.Paren _ as pat ]
                accessibility = ao)
            expr = expr
            trivia = { EqualsRange = Some mEq })) when (newIdent.idText = "new") ->
        let exprNode = mkExpr creationAide expr

        MemberDefnExplicitCtorNode(
            mkXmlDoc px,
            mkAttributes creationAide ats,
            mkSynAccess ao,
            mkIdent newIdent,
            mkPat creationAide pat,
            Option.map mkIdent ido,
            stn "=" mEq,
            exprNode,
            memberDefinitionRange
        )
        |> MemberDefn.ExplicitCtor
    | SynMemberDefn.Member(memberDefn, _) -> mkBinding creationAide memberDefn |> MemberDefn.Member
    | SynMemberDefn.Inherit(baseType, _, StartRange 7 (mInherit, _)) ->
        MemberDefnInheritNode(stn "inherit" mInherit, mkType creationAide baseType, memberDefinitionRange)
        |> MemberDefn.Inherit
    | SynMemberDefn.ValField(f, _) -> mkSynField creationAide f |> MemberDefn.ValField
    | SynMemberDefn.LetBindings(
        bindings = [ SynBinding(trivia = { LeadingKeyword = SynLeadingKeyword.Extern _ }) as binding ]) ->
        mkExternBinding creationAide binding |> MemberDefn.ExternBinding
    | SynMemberDefn.LetBindings(bindings = [ SynBinding(kind = SynBindingKind.Do; expr = expr; trivia = trivia) ]) ->
        // This is a shortcut to support "static do"
        let leadingKw =
            (mkSynLeadingKeyword trivia.LeadingKeyword).Content
            |> List.map (fun stn -> stn.Text)
            |> String.concat " "

        ExprSingleNode(
            stn leadingKw trivia.LeadingKeyword.Range,
            true,
            false,
            mkExpr creationAide expr,
            memberDefinitionRange
        )
        |> MemberDefn.DoExpr
    | SynMemberDefn.LetBindings(bindings = bindings) ->
        BindingListNode(List.map (mkBinding creationAide) bindings, memberDefinitionRange)
        |> MemberDefn.LetBinding
    | SynMemberDefn.Interface(t, mWith, mdsOpt, _) ->
        let interfaceNode =
            match memberDefinitionRange with
            | StartRange 9 (mInterface, _) -> stn "interface" mInterface

        let members =
            match mdsOpt with
            | None -> []
            | Some mds -> List.map (mkMemberDefn creationAide) mds

        MemberDefnInterfaceNode(
            interfaceNode,
            mkType creationAide t,
            Option.map (stn "with") mWith,
            members,
            memberDefinitionRange
        )
        |> MemberDefn.Interface
    | SynMemberDefn.AutoProperty(ats,
                                 _isStatic,
                                 ident,
                                 typeOpt,
                                 _,
                                 _,
                                 _,
                                 px,
                                 ao,
                                 e,
                                 _,
                                 { LeadingKeyword = lk
                                   EqualsRange = Some mEq
                                   WithKeyword = mWith
                                   GetSetKeywords = mGS }) ->
        MemberDefnAutoPropertyNode(
            mkXmlDoc px,
            mkAttributes creationAide ats,
            mkSynLeadingKeyword lk,
            mkSynAccess ao,
            mkIdent ident,
            Option.map (mkType creationAide) typeOpt,
            stn "=" mEq,
            mkExpr creationAide e,
            mkWithGetSet mWith mGS,
            memberDefinitionRange
        )
        |> MemberDefn.AutoProperty
    | SynMemberDefn.AbstractSlot(SynValSig(ats, ident, tds, t, _, _, _, px, _ao, _, _, trivia), _, _, abstractSlotTrivia) ->
        MemberDefnAbstractSlotNode(
            mkXmlDoc px,
            mkAttributes creationAide ats,
            mkSynLeadingKeyword trivia.LeadingKeyword,
            mkSynIdent ident,
            mkSynValTyparDecls creationAide (Some tds),
            mkType creationAide t,
            mkWithGetSet trivia.WithKeyword abstractSlotTrivia.GetSetKeywords,
            memberDefinitionRange
        )
        |> MemberDefn.AbstractSlot
    | SynMemberDefn.GetSetMember(Some(SynBinding(
                                     attributes = ats
                                     xmlDoc = px
                                     headPat = SynPat.LongIdent(longDotId = memberName; accessibility = visGet)
                                     trivia = { LeadingKeyword = lk }) as getBinding),
                                 Some(SynBinding(headPat = SynPat.LongIdent(accessibility = visSet)) as setBinding),
                                 _,
                                 { InlineKeyword = inlineKw
                                   GetKeyword = Some getKeyword
                                   SetKeyword = Some setKeyword
                                   WithKeyword = withKeyword
                                   AndKeyword = Some andKeyword }) ->

        let firstAccessibility, firstBinding, firstKeyword, lastBinding, lastKeyword =
            if Position.posLt getKeyword.Start setKeyword.Start then
                visGet, getBinding, (stn "get" getKeyword), setBinding, (stn "set" setKeyword)
            else
                visSet, setBinding, (stn "set" setKeyword), getBinding, (stn "get" getKeyword)

        // Only use the accessibility of the first binding if the keyword came before the member identifier.
        let accessibility =
            firstAccessibility
            |> Option.bind (fun vis ->
                if rangeBeforePos vis.Range memberName.Range.Start then
                    Some vis
                else
                    None)

        let firstBinding =
            match firstBinding with
            | SynBinding(headPat = SynPat.LongIdent(accessibility = Some vis)) when
                rangeBeforePos memberName.Range vis.Range.Start
                ->
                mkPropertyGetSetBinding creationAide withKeyword (Some vis) firstKeyword firstBinding
            | _ -> mkPropertyGetSetBinding creationAide withKeyword None firstKeyword firstBinding

        let lastBinding =
            match lastBinding with
            | SynBinding(headPat = SynPat.LongIdent(accessibility = Some vis)) when
                rangeBeforePos memberName.Range vis.Range.Start
                ->
                mkPropertyGetSetBinding creationAide andKeyword (Some vis) lastKeyword lastBinding
            | _ -> mkPropertyGetSetBinding creationAide andKeyword None lastKeyword lastBinding

        // Attributes placed on the member will be included in both bindings for the getter and setter.
        // We need to filter out the attributes above the leading keyword (typically `member`).
        let memberAttributes =
            ats
            |> List.choose (fun al ->
                let filteredAttributeList =
                    { al with
                        Attributes =
                            al.Attributes
                            |> List.filter (fun a -> Position.posLt a.Range.End lk.Range.Start) }

                if filteredAttributeList.Attributes.IsEmpty then
                    None
                else
                    Some filteredAttributeList)

        MemberDefnPropertyGetSetNode(
            mkXmlDoc px,
            mkAttributes creationAide memberAttributes,
            mkSynLeadingKeyword lk,
            Option.map (stn "inline") inlineKw,
            mkSynAccess accessibility,
            mkSynLongIdent memberName,
            stn "with" withKeyword,
            firstBinding,
            Some(stn "and" andKeyword),
            Some lastBinding,
            memberDefinitionRange
        )
        |> MemberDefn.PropertyGetSet
    | SynMemberDefn.GetSetMember(None,
                                 Some(SynBinding(
                                     attributes = ats
                                     xmlDoc = px
                                     headPat = SynPat.LongIdent(longDotId = memberName; accessibility = ao)
                                     trivia = { LeadingKeyword = lk
                                                InlineKeyword = inlineKw }) as binding),
                                 _,
                                 { WithKeyword = withKeyword
                                   GetKeyword = getKeyword
                                   SetKeyword = setKeyword })
    | SynMemberDefn.GetSetMember(Some(SynBinding(
                                     attributes = ats
                                     xmlDoc = px
                                     headPat = SynPat.LongIdent(longDotId = memberName; accessibility = ao)
                                     trivia = { LeadingKeyword = lk
                                                InlineKeyword = inlineKw }) as binding),
                                 None,
                                 _,
                                 { WithKeyword = withKeyword
                                   GetKeyword = getKeyword
                                   SetKeyword = setKeyword }) ->

        let visMember, visProperty =
            match ao with
            | None -> None, None
            | Some ao ->
                if rangeBeforePos ao.Range memberName.Range.Start then
                    Some ao, None
                else
                    None, Some ao

        match getKeyword, setKeyword with
        | Some getKeyword, None ->
            let bindingNode =
                mkPropertyGetSetBinding creationAide withKeyword visProperty (stn "get" getKeyword) binding

            MemberDefnPropertyGetSetNode(
                mkXmlDoc px,
                mkAttributes creationAide ats,
                mkSynLeadingKeyword lk,
                Option.map (stn "inline") inlineKw,
                mkSynAccess visMember,
                mkSynLongIdent memberName,
                stn "with" withKeyword,
                bindingNode,
                None,
                None,
                memberDefinitionRange
            )
            |> MemberDefn.PropertyGetSet
        | None, Some setKeyword ->
            let bindingNode =
                mkPropertyGetSetBinding creationAide withKeyword visProperty (stn "set" setKeyword) binding

            MemberDefnPropertyGetSetNode(
                mkXmlDoc px,
                mkAttributes creationAide ats,
                mkSynLeadingKeyword lk,
                Option.map (stn "inline") inlineKw,
                mkSynAccess visMember,
                mkSynLongIdent memberName,
                stn "with" withKeyword,
                bindingNode,
                None,
                None,
                memberDefinitionRange
            )
            |> MemberDefn.PropertyGetSet
        | _ -> failwith "SynMemberDefn.GetSetMember cannot exist with get and without set"
    | _ -> failwithf "Unexpected SynMemberDefn: %A" md

let mkVal
    (creationAide: CreationAide)
    (SynValSig(ats, synIdent, vtd, t, _vi, _isInline, isMutable, px, ao, eo, range, trivia))
    : ValNode =
    let lk =
        match trivia.LeadingKeyword with
        | SynLeadingKeyword.New _ -> None
        | lk -> Some(mkSynLeadingKeyword lk)

    ValNode(
        mkXmlDoc px,
        mkAttributes creationAide ats,
        lk,
        Option.map (stn "inline") trivia.InlineKeyword,
        isMutable,
        mkSynAccess ao,
        mkSynIdent synIdent,
        mkSynValTyparDecls creationAide (Some vtd),
        mkType creationAide t,
        Option.map (stn "=") trivia.EqualsRange,
        Option.map (mkExpr creationAide) eo,
        range
    )

let mkMemberSig (creationAide: CreationAide) (ms: SynMemberSig) =
    let memberSigRange = ms.Range

    match ms with
    | SynMemberSig.Member(vs, _, _, memberTrivia) ->
        let (SynValSig(trivia = trivia)) = vs

        MemberDefnSigMemberNode(
            mkVal creationAide vs,
            mkWithGetSet trivia.WithKeyword memberTrivia.GetSetKeywords,
            memberSigRange
        )
        |> MemberDefn.SigMember
    | SynMemberSig.Interface(t, StartRange 9 (mInterface, _)) ->
        MemberDefnInterfaceNode(stn "interface" mInterface, mkType creationAide t, None, [], memberSigRange)
        |> MemberDefn.Interface

    | SynMemberSig.Inherit(t, StartRange 7 (mInherit, _)) ->
        MemberDefnInheritNode(stn "inherit" mInherit, mkType creationAide t, memberSigRange)
        |> MemberDefn.Inherit
    | SynMemberSig.ValField(f, _) -> mkSynField creationAide f |> MemberDefn.ValField
    | _ -> failwithf "Cannot construct node for %A" ms

let rec mkModuleDecls
    (creationAide: CreationAide)
    (decls: SynModuleDecl list)
    (finalContinuation: ModuleDecl list -> ModuleDecl list)
    =
    match decls with
    | [] -> finalContinuation []
    | OpenL(xs, ys) ->
        let openListNode =
            List.map (mkOpenNodeForImpl creationAide) xs
            |> OpenListNode
            |> ModuleDecl.OpenList

        mkModuleDecls creationAide ys (fun nodes -> openListNode :: nodes |> finalContinuation)

    | HashDirectiveL(xs, ys) ->
        let listNode =
            List.map (mkParsedHashDirective creationAide) xs
            |> HashDirectiveListNode
            |> ModuleDecl.HashDirectiveList

        mkModuleDecls creationAide ys (fun nodes -> listNode :: nodes |> finalContinuation)

    | SynModuleDecl.Types(typeDefns = typeDefns) :: rest ->
        let typeNodes =
            List.map (fun tdn -> mkTypeDefn creationAide tdn |> ModuleDecl.TypeDefn) typeDefns

        mkModuleDecls creationAide rest (fun nodes -> [ yield! typeNodes; yield! nodes ] |> finalContinuation)

    | SynModuleDecl.Attributes(a, _) :: SynModuleDecl.Expr(e, _) :: rest ->
        let attributes = mkAttributes creationAide a
        let expr = mkExpr creationAide e
        let range = unionRanges a.Head.Range (Expr.Node expr).Range
        let node = ModuleDeclAttributesNode(attributes, expr, range)
        mkModuleDecls creationAide rest (fun nodes -> ModuleDecl.Attributes node :: nodes |> finalContinuation)

    | SynModuleDecl.Let(bindings = bindings) :: rest when List.moreThanOne bindings ->
        let bindingNodes =
            List.map (fun b -> mkBinding creationAide b |> ModuleDecl.TopLevelBinding) bindings

        mkModuleDecls creationAide rest (fun nodes -> [ yield! bindingNodes; yield! nodes ] |> finalContinuation)

    | head :: tail ->
        mkModuleDecls creationAide tail (fun nodes -> mkModuleDecl creationAide head :: nodes |> finalContinuation)

let mkModuleOrNamespace
    (creationAide: CreationAide)
    (SynModuleOrNamespace(
        xmlDoc = xmlDoc
        attribs = attribs
        accessibility = accessibility
        longId = longId
        isRecursive = isRecursive
        kind = kind
        decls = decls
        trivia = trivia) as mn)
    =
    let leadingKeyword =
        match trivia.LeadingKeyword with
        | SynModuleOrNamespaceLeadingKeyword.Module mModule ->
            Some(MultipleTextsNode([ stn "module" mModule ], mModule))
        | SynModuleOrNamespaceLeadingKeyword.Namespace mNamespace ->
            match kind with
            | SynModuleOrNamespaceKind.GlobalNamespace ->
                Some(MultipleTextsNode([ stn "namespace" mNamespace; stn "global" Range.Zero ], mNamespace))
            | _ -> Some(MultipleTextsNode([ stn "namespace" mNamespace ], mNamespace))
        | SynModuleOrNamespaceLeadingKeyword.None -> None

    let name =
        match kind with
        | SynModuleOrNamespaceKind.AnonModule
        | SynModuleOrNamespaceKind.GlobalNamespace -> None
        | _ -> Some(mkLongIdent longId)

    let range: range = mkSynModuleOrNamespaceFullRange mn

    let header =
        match leadingKeyword with
        | None -> None
        | Some leadingKeyword ->
            match name with
            | None ->
                let m = mkFileIndexRange range.FileIndex range.Start leadingKeyword.Range.End

                ModuleOrNamespaceHeaderNode(
                    mkXmlDoc xmlDoc,
                    mkAttributes creationAide attribs,
                    leadingKeyword,
                    mkSynAccess accessibility,
                    isRecursive,
                    None,
                    m
                )
                |> Some
            | Some name ->
                let m = mkFileIndexRange range.FileIndex range.Start name.Range.End

                ModuleOrNamespaceHeaderNode(
                    mkXmlDoc xmlDoc,
                    mkAttributes creationAide attribs,
                    leadingKeyword,
                    mkSynAccess accessibility,
                    isRecursive,
                    Some name,
                    m
                )
                |> Some

    let decls = mkModuleDecls creationAide decls id

    ModuleOrNamespaceNode(header, decls, range)

let mkImplFile
    (creationAide: CreationAide)
    (ParsedImplFileInput(hashDirectives = hashDirectives; contents = contents))
    (m: range)
    =
    let phds = List.map (mkParsedHashDirective creationAide) hashDirectives
    let mds = List.map (mkModuleOrNamespace creationAide) contents
    Oak(phds, mds, m)

// start sig file
let rec (|OpenSigL|_|) =
    function
    | SynModuleSigDecl.Open(target, range) :: OpenSigL(xs, ys) -> Some((target, range) :: xs, ys)
    | SynModuleSigDecl.Open(target, range) :: ys -> Some([ target, range ], ys)
    | _ -> None

let rec (|HashDirectiveSigL|_|) =
    function
    | SynModuleSigDecl.HashDirective(p, _) :: HashDirectiveSigL(xs, ys) -> Some(p :: xs, ys)
    | SynModuleSigDecl.HashDirective(p, _) :: ys -> Some([ p ], ys)
    | _ -> None

let mkModuleSigDecl (creationAide: CreationAide) (decl: SynModuleSigDecl) =
    let declRange = decl.Range

    match decl with
    | SynModuleSigDecl.Exception(SynExceptionSig(SynExceptionDefnRepr(attrs, caseName, _, xmlDoc, vis, _),
                                                 withKeyword,
                                                 ms,
                                                 _),
                                 _) ->
        ExceptionDefnNode(
            mkXmlDoc xmlDoc,
            mkAttributes creationAide attrs,
            mkSynAccess vis,
            mkSynUnionCase creationAide caseName,
            Option.map (stn "with") withKeyword,
            List.map (mkMemberSig creationAide) ms,
            declRange
        )
        |> ModuleDecl.Exception
    | SynModuleSigDecl.ModuleAbbrev(ident, lid, StartRange 6 (mModule, _)) ->
        ModuleAbbrevNode(stn "module" mModule, mkIdent ident, mkLongIdent lid, declRange)
        |> ModuleDecl.ModuleAbbrev
    | SynModuleSigDecl.NestedModule(SynComponentInfo(ats, _, _, lid, px, _, ao, _),
                                    isRecursive,
                                    decls,
                                    _,
                                    { ModuleKeyword = Some mModule
                                      EqualsRange = Some mEq }) ->
        NestedModuleNode(
            mkXmlDoc px,
            mkAttributes creationAide ats,
            stn "module" mModule,
            mkSynAccess ao,
            isRecursive,
            mkLongIdent lid,
            stn "=" mEq,
            mkModuleSigDecls creationAide decls id,
            declRange
        )
        |> ModuleDecl.NestedModule
    | SynModuleSigDecl.Val(valSig, _) -> mkVal creationAide valSig |> ModuleDecl.Val
    | decl -> failwithf $"Failed to create ModuleDecl for %A{decl}"

let mkTypeDefnSig (creationAide: CreationAide) (SynTypeDefnSig(typeInfo, typeRepr, members, range, trivia)) : TypeDefn =
    let typeNameNode =
        match typeInfo with
        | SynComponentInfo(ats, tds, tcs, lid, px, _preferPostfix, ao, _) ->
            let identifierNode = mkLongIdent lid
            let mIdentifierNode = identifierNode.Range

            let leadingKeyword =
                match trivia.LeadingKeyword with
                | SynTypeDefnLeadingKeyword.Type mType -> stn "type" mType
                | SynTypeDefnLeadingKeyword.And mAnd -> stn "and" mAnd
                | SynTypeDefnLeadingKeyword.StaticType _
                | SynTypeDefnLeadingKeyword.Synthetic -> failwithf "unexpected %A" trivia.LeadingKeyword

            let m =
                if not px.IsEmpty then
                    unionRanges px.Range mIdentifierNode
                else
                    match ats with
                    | [] -> unionRanges leadingKeyword.Range mIdentifierNode
                    | firstAttr :: _ -> unionRanges firstAttr.Range mIdentifierNode

            TypeNameNode(
                mkXmlDoc px,
                mkAttributes creationAide ats,
                leadingKeyword,
                mkSynAccess ao,
                identifierNode,
                Option.map (mkSynTyparDecls creationAide) tds,
                List.map (mkSynTypeConstraint creationAide) tcs,
                None,
                Option.map (stn "=") trivia.EqualsRange,
                Option.map (stn "with") trivia.WithKeyword,
                m
            )

    let members = List.map (mkMemberSig creationAide) members
    let typeDefnRange = unionRanges typeNameNode.Range range

    match typeRepr with
    | SynTypeDefnSigRepr.Simple(repr = SynTypeDefnSimpleRepr.Enum(ecs, _)) ->
        let enumCases =
            ecs
            |> List.map (fun (SynEnumCase(attributes, ident, valueExpr, xmlDoc, range, trivia)) ->
                EnumCaseNode(
                    mkXmlDoc xmlDoc,
                    Option.map (stn "|") trivia.BarRange,
                    mkAttributes creationAide attributes,
                    mkSynIdent ident,
                    stn "=" trivia.EqualsRange,
                    mkExpr creationAide valueExpr,
                    range
                ))

        TypeDefnEnumNode(typeNameNode, enumCases, members, typeDefnRange)
        |> TypeDefn.Enum

    | SynTypeDefnSigRepr.Simple(repr = SynTypeDefnSimpleRepr.Union(ao, cases, _)) ->
        let unionCases = cases |> List.map (mkSynUnionCase creationAide)

        TypeDefnUnionNode(typeNameNode, mkSynAccess ao, unionCases, members, typeDefnRange)
        |> TypeDefn.Union

    | SynTypeDefnSigRepr.Simple(
        repr = SynTypeDefnSimpleRepr.Record(ao, fs, StartEndRange 1 (openingBrace, _, closingBrace))) ->
        let fields = List.map (mkSynField creationAide) fs

        TypeDefnRecordNode(
            typeNameNode,
            mkSynAccess ao,
            stn "{" openingBrace,
            fields,
            stn "}" closingBrace,
            members,
            typeDefnRange
        )
        |> TypeDefn.Record

    | SynTypeDefnSigRepr.Simple(repr = SynTypeDefnSimpleRepr.TypeAbbrev(rhsType = t)) ->
        TypeDefn.Abbrev(TypeDefnAbbrevNode(typeNameNode, mkType creationAide t, members, range))

    | SynTypeDefnSigRepr.Simple(repr = SynTypeDefnSimpleRepr.None _) when List.isNotEmpty members ->
        let typeNameNode =
            TypeNameNode(
                typeNameNode.XmlDoc,
                typeNameNode.Attributes,
                typeNameNode.LeadingKeyword,
                typeNameNode.Accessibility,
                typeNameNode.Identifier,
                typeNameNode.TypeParameters,
                typeNameNode.Constraints,
                None,
                None,
                typeNameNode.WithKeyword,
                typeNameNode.Range
            )

        TypeDefnAugmentationNode(typeNameNode, members, typeDefnRange)
        |> TypeDefn.Augmentation

    | SynTypeDefnSigRepr.Simple(repr = SynTypeDefnSimpleRepr.None _) -> TypeDefn.None typeNameNode

    | SynTypeDefnSigRepr.ObjectModel(
        kind = SynTypeDefnKind.Class | SynTypeDefnKind.Interface | SynTypeDefnKind.Struct as tdk
        memberSigs = objectMembers
        range = range) ->

        let kindNode =
            match tdk, range with
            | SynTypeDefnKind.Class, StartRange 5 (mClass, _) -> stn "class" mClass
            | SynTypeDefnKind.Interface, StartRange 9 (mInterface, _) -> stn "interface" mInterface
            | SynTypeDefnKind.Struct, StartRange 6 (mStruct, _) -> stn "struct" mStruct
            | _ -> failwith "unexpected kind"

        let objectMembers = objectMembers |> List.map (mkMemberSig creationAide)

        let endNode =
            match range with
            | EndRange 3 (mEnd, _) -> stn "end" mEnd

        let body = TypeDefnExplicitBodyNode(kindNode, objectMembers, endNode, range)

        TypeDefnExplicitNode(typeNameNode, body, members, typeDefnRange)
        |> TypeDefn.Explicit

    | SynTypeDefnSigRepr.ObjectModel(kind = SynTypeDefnKind.Augmentation mWith) ->
        let typeNameNode =
            TypeNameNode(
                typeNameNode.XmlDoc,
                typeNameNode.Attributes,
                typeNameNode.LeadingKeyword,
                typeNameNode.Accessibility,
                typeNameNode.Identifier,
                typeNameNode.TypeParameters,
                typeNameNode.Constraints,
                None,
                None,
                Some(stn "with" mWith),
                typeNameNode.Range
            )

        TypeDefnAugmentationNode(typeNameNode, members, typeDefnRange)
        |> TypeDefn.Augmentation

    | SynTypeDefnSigRepr.ObjectModel(
        kind = SynTypeDefnKind.Delegate(signature = TFuns(ts, rt) as st); range = StartRange 8 (mDelegate, _)) ->
        let typeList = mkTypeList creationAide ts rt st.Range

        TypeDefnDelegateNode(typeNameNode, stn "delegate" mDelegate, typeList, typeDefnRange)
        |> TypeDefn.Delegate

    | SynTypeDefnSigRepr.ObjectModel(memberSigs = objectMembers) ->
        let allMembers =
            let objectMembers = objectMembers |> List.map (mkMemberSig creationAide)

            [ yield! objectMembers; yield! members ]

        TypeDefnRegularNode(typeNameNode, allMembers, typeDefnRange) |> TypeDefn.Regular
    | _ -> failwithf "Could not create a TypeDefn for %A" typeRepr

let rec mkModuleSigDecls
    (creationAide: CreationAide)
    (decls: SynModuleSigDecl list)
    (finalContinuation: ModuleDecl list -> ModuleDecl list)
    : ModuleDecl list =
    match decls with
    | [] -> finalContinuation []
    | OpenSigL(xs, ys) ->
        let openListNode =
            List.map (mkOpenNodeForImpl creationAide) xs
            |> OpenListNode
            |> ModuleDecl.OpenList

        mkModuleSigDecls creationAide ys (fun nodes -> openListNode :: nodes |> finalContinuation)

    | HashDirectiveSigL(xs, ys) ->
        let listNode =
            List.map (mkParsedHashDirective creationAide) xs
            |> HashDirectiveListNode
            |> ModuleDecl.HashDirectiveList

        mkModuleSigDecls creationAide ys (fun nodes -> listNode :: nodes |> finalContinuation)

    | SynModuleSigDecl.Types(types = typeDefns) :: rest ->
        let typeNodes =
            List.map (fun tdn -> mkTypeDefnSig creationAide tdn |> ModuleDecl.TypeDefn) typeDefns

        mkModuleSigDecls creationAide rest (fun nodes -> [ yield! typeNodes; yield! nodes ] |> finalContinuation)

    | head :: tail ->
        mkModuleSigDecls creationAide tail (fun nodes ->
            mkModuleSigDecl creationAide head :: nodes |> finalContinuation)

let mkModuleOrNamespaceSig
    (creationAide: CreationAide)
    (SynModuleOrNamespaceSig(
        xmlDoc = xmlDoc
        attribs = attribs
        accessibility = accessibility
        isRecursive = isRecursive
        longId = longId
        kind = kind
        decls = decls
        trivia = trivia) as mn)
    =
    let leadingKeyword =
        match trivia.LeadingKeyword with
        | SynModuleOrNamespaceLeadingKeyword.Module mModule ->
            Some(MultipleTextsNode([ stn "module" mModule ], mModule))
        | SynModuleOrNamespaceLeadingKeyword.Namespace mNamespace ->
            match kind with
            | SynModuleOrNamespaceKind.GlobalNamespace ->
                Some(MultipleTextsNode([ stn "namespace" mNamespace; stn "global" Range.Zero ], mNamespace))
            | _ -> Some(MultipleTextsNode([ stn "namespace" mNamespace ], mNamespace))
        | SynModuleOrNamespaceLeadingKeyword.None -> None

    let name =
        match kind with
        | SynModuleOrNamespaceKind.AnonModule
        | SynModuleOrNamespaceKind.GlobalNamespace -> None
        | _ -> Some(mkLongIdent longId)

    let decls = mkModuleSigDecls creationAide decls id
    let range: range = mkSynModuleOrNamespaceSigFullRange mn

    let header =
        match leadingKeyword with
        | None -> None
        | Some leadingKeyword ->
            match name with
            | None ->
                let m = mkFileIndexRange range.FileIndex range.Start leadingKeyword.Range.End

                ModuleOrNamespaceHeaderNode(
                    mkXmlDoc xmlDoc,
                    mkAttributes creationAide attribs,
                    leadingKeyword,
                    mkSynAccess accessibility,
                    isRecursive,
                    None,
                    m
                )
                |> Some
            | Some name ->
                let m = mkFileIndexRange range.FileIndex range.Start name.Range.End

                ModuleOrNamespaceHeaderNode(
                    mkXmlDoc xmlDoc,
                    mkAttributes creationAide attribs,
                    leadingKeyword,
                    mkSynAccess accessibility,
                    isRecursive,
                    Some name,
                    m
                )
                |> Some

    ModuleOrNamespaceNode(header, decls, range)

let mkSigFile
    (creationAide: CreationAide)
    (ParsedSigFileInput(hashDirectives = hashDirectives; contents = contents))
    (m: range)
    =
    let phds = List.map (mkParsedHashDirective creationAide) hashDirectives
    let mds = List.map (mkModuleOrNamespaceSig creationAide) contents
    Oak(phds, mds, m)

let includeTrivia
    (baseRange: range)
    (comments: CommentTrivia list)
    (conditionDirectives: ConditionalDirectiveTrivia list)
    : range =
    let ranges =
        [ yield!
              List.map
                  (function
                  | CommentTrivia.LineComment m
                  | CommentTrivia.BlockComment m -> m)
                  comments
          yield!
              List.map
                  (function
                  | ConditionalDirectiveTrivia.If(range = range)
                  | ConditionalDirectiveTrivia.Else(range = range)
                  | ConditionalDirectiveTrivia.EndIf(range = range) -> range)
                  conditionDirectives ]

    (baseRange, ranges)
    ||> List.fold (fun acc triviaRange ->
        if acc.StartLine < triviaRange.StartLine && acc.EndLine > triviaRange.EndLine then
            acc
        elif triviaRange.EndLine > acc.EndLine then
            unionRanges acc triviaRange
        else
            unionRanges triviaRange acc)

let mkSynModuleOrNamespaceFullRange (mn: SynModuleOrNamespace) =
    match mn with
    | SynModuleOrNamespace(kind = SynModuleOrNamespaceKind.AnonModule; decls = decls) ->
        match List.tryHead decls, List.tryLast decls with
        | None, None -> Range.Zero
        | Some d, None
        | None, Some d -> d.Range
        | Some s, Some e -> unionRanges s.Range e.Range
    | _ -> mn.Range

let mkSynModuleOrNamespaceSigFullRange (mn: SynModuleOrNamespaceSig) =
    match mn with
    | SynModuleOrNamespaceSig(kind = SynModuleOrNamespaceKind.AnonModule; decls = decls) ->
        match List.tryHead decls, List.tryLast decls with
        | None, None -> Range.Zero
        | Some d, None
        | None, Some d -> d.Range
        | Some s, Some e -> unionRanges s.Range e.Range

    | _ -> mn.Range

let mkFullTreeRange ast =
    match ast with
    | ParsedInput.ImplFile(ParsedImplFileInput(hashDirectives = directives; contents = modules; trivia = trivia)) ->
        let startPos =
            match directives with
            | ParsedHashDirective(range = r) :: _ -> r
            | [] ->
                match modules with
                | m :: _ -> mkSynModuleOrNamespaceFullRange m
                | _ -> Range.Zero

        let endPos =
            match List.tryLast modules with
            | None ->
                match List.tryLast directives with
                | None -> Range.Zero
                | Some(ParsedHashDirective(range = r)) -> r
            | Some lastModule -> mkSynModuleOrNamespaceFullRange lastModule

        let astRange = unionRanges startPos endPos
        includeTrivia astRange trivia.CodeComments trivia.ConditionalDirectives

    | ParsedInput.SigFile(ParsedSigFileInput(hashDirectives = directives; contents = modules; trivia = trivia)) ->
        let startPos =
            match directives with
            | ParsedHashDirective(range = r) :: _ -> r
            | [] ->
                match modules with
                | m :: _ -> mkSynModuleOrNamespaceSigFullRange m
                | _ -> Range.Zero

        let endPos =
            match List.tryLast modules with
            | None ->
                match List.tryLast directives with
                | None -> Range.Zero
                | Some(ParsedHashDirective(range = r)) -> r
            | Some lastModule -> mkSynModuleOrNamespaceSigFullRange lastModule

        let astRange = unionRanges startPos endPos
        includeTrivia astRange trivia.CodeComments trivia.ConditionalDirectives

let mkOak (sourceText: ISourceText option) (ast: ParsedInput) =
    let creationAide = { SourceText = sourceText }

    let fullRange = mkFullTreeRange ast

    match ast with
    | ParsedInput.ImplFile parsedImplFileInput -> mkImplFile creationAide parsedImplFileInput fullRange
    | ParsedInput.SigFile parsedSigFileInput -> mkSigFile creationAide parsedSigFileInput fullRange
