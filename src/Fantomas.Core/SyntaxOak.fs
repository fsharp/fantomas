module rec Fantomas.Core.SyntaxOak

open System.Collections.Generic
open Fantomas.FCS.Text

type TriviaContent =
    | CommentOnSingleLine of string
    | LineCommentAfterSourceCode of comment: string
    | BlockComment of comment: string * newlineBefore: bool * newlineAfter: bool
    | Newline
    | Directive of string
    | Cursor

type TriviaNode(content: TriviaContent, range: range) =
    member val Content = content
    member val Range = range

[<Interface>]
type Node =
    abstract ContentBefore: TriviaNode seq
    abstract HasContentBefore: bool
    abstract ContentAfter: TriviaNode seq
    abstract HasContentAfter: bool
    abstract Range: range
    abstract Children: Node array
    abstract AddBefore: triviaNode: TriviaNode -> unit
    abstract AddAfter: triviaNode: TriviaNode -> unit
    abstract AddCursor: pos -> unit
    abstract TryGetCursor: pos option

[<AbstractClass>]
type NodeBase(range: range) =
    let mutable potentialCursor = None
    let nodesBefore = Queue<TriviaNode>(0)
    let nodesAfter = Queue<TriviaNode>(0)

    member _.ContentBefore: TriviaNode seq = nodesBefore

    member _.HasContentBefore =
        nodesBefore
        |> Seq.filter (fun tn ->
            match tn.Content with
            | Cursor -> false
            | _ -> true)
        |> Seq.isEmpty
        |> not

    member _.ContentAfter: TriviaNode seq = nodesAfter

    member _.HasContentAfter =
        nodesAfter
        |> Seq.filter (fun tn ->
            match tn.Content with
            | Cursor -> false
            | _ -> true)
        |> Seq.isEmpty
        |> not

    member _.Range = range
    member _.AddBefore triviaNode = nodesBefore.Enqueue triviaNode
    member _.AddAfter triviaNode = nodesAfter.Enqueue triviaNode
    abstract member Children: Node array
    member _.AddCursor cursor = potentialCursor <- Some cursor
    member _.TryGetCursor = potentialCursor

    interface Node with
        member x.ContentBefore = x.ContentBefore
        member x.HasContentBefore = x.HasContentBefore
        member x.ContentAfter = x.ContentAfter
        member x.HasContentAfter = x.HasContentAfter
        member x.Range = x.Range
        member x.AddBefore triviaNode = x.AddBefore triviaNode
        member x.AddAfter triviaNode = x.AddAfter triviaNode
        member x.Children = x.Children
        member x.AddCursor cursor = x.AddCursor cursor
        member x.TryGetCursor = x.TryGetCursor

type StringNode(content: string, range: range) =
    inherit NodeBase(range)
    member val Content = content
    override val Children = Array.empty

let noa<'n when 'n :> Node> (n: 'n option) =
    match n with
    | None -> Array.empty
    | Some n -> [| n :> Node |]

let nodes<'n when 'n :> Node> (ns: 'n seq) = Seq.cast<Node> ns

let nodeRange (n: Node) = n.Range

let combineRanges (ranges: range seq) =
    if Seq.isEmpty ranges then
        Range.Zero
    else
        Seq.reduce Range.unionRanges ranges

[<RequireQualifiedAccess; NoComparison>]
type IdentifierOrDot =
    | Ident of SingleTextNode
    | KnownDot of SingleTextNode
    | UnknownDot

    member x.Range =
        match x with
        | Ident n -> Some n.Range
        | KnownDot n -> Some n.Range
        | UnknownDot -> None

type IdentListNode(content: IdentifierOrDot list, range) =
    inherit NodeBase(range)
    member val IsEmpty = content.IsEmpty
    member val Content = content
    static member Empty = IdentListNode(List.empty, Range.Zero)

    override x.Children =
        x.Content
        |> List.choose (function
            | IdentifierOrDot.Ident n -> Some(n :> Node)
            | IdentifierOrDot.KnownDot n -> Some(n :> Node)
            | _ -> None)
        |> Array.ofList

type SingleTextNode(idText: string, range: range) =
    inherit NodeBase(range)
    member val Text = idText
    override val Children = Array.empty

type MultipleTextsNode(content: SingleTextNode list, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield! nodes content |]
    member val Content = content

type XmlDocNode(lines: string array, range) =

    inherit NodeBase(range)
    override val Children = Array.empty
    member val Lines = lines

type Oak(parsedHashDirectives: ParsedHashDirectiveNode list, modulesOrNamespaces: ModuleOrNamespaceNode list, m: range)
    =
    inherit NodeBase(m)

    member val ParsedHashDirectives = parsedHashDirectives
    member val ModulesOrNamespaces = modulesOrNamespaces

    override val Children: Node array = [| yield! nodes parsedHashDirectives; yield! nodes modulesOrNamespaces |]

type ParsedHashDirectiveNode(ident: string, args: Choice<SingleTextNode, IdentListNode> list, range) =
    inherit NodeBase(range)
    member val Ident = ident
    member val Args = args

    override val Children: Node array =
        [| for arg in args do
               match arg with
               | Choice1Of2(node) -> node
               | Choice2Of2(node) -> node |]

type ModuleOrNamespaceHeaderNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: MultipleTextsNode,
        accessibility: SingleTextNode option,
        isRecursive: bool,
        name: IdentListNode option,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield leadingKeyword
           yield! noa accessibility
           yield! noa name |]

    member val XmlDoc = xmlDoc
    member val Attributes = attributes
    member val LeadingKeyword = leadingKeyword
    member val Accessibility = accessibility
    member val IsRecursive = isRecursive
    member val Name = name

type ModuleOrNamespaceNode(header: ModuleOrNamespaceHeaderNode option, decls: ModuleDecl list, range) =
    inherit NodeBase(range)
    member val Declarations = decls
    member val IsNamed = Option.isSome header

    override val Children: Node array = [| yield! noa header; yield! List.map ModuleDecl.Node decls |]
    member val Header = header

type TypeFunsNode(parameters: (Type * SingleTextNode) list, returnType: Type, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! nodes (List.collect (fun (t, arrow) -> [ yield Type.Node t; yield (arrow :> Node) ]) parameters)
           yield Type.Node returnType |]

    /// Type + arrow
    member val Parameters = parameters
    member val ReturnType = returnType

type TypeTupleNode(path: Choice<Type, SingleTextNode> list, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield!
               List.map
                   (function
                   | Choice1Of2 t -> Type.Node t
                   | Choice2Of2 n -> n :> Node)
                   path |]

    member val Path = path

type TypeHashConstraintNode(hash: SingleTextNode, t: Type, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield hash; yield Type.Node t |]
    member val Hash = hash
    member val Type = t

type TypeMeasurePowerNode(baseMeasure: Type, exponent: RationalConstNode, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Type.Node baseMeasure |]
    member val BaseMeasure = baseMeasure
    member val Exponent = exponent

type TypeStaticConstantExprNode(constNode: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield constNode; yield Expr.Node expr |]
    member val Const = constNode
    member val Expr = expr

type TypeStaticConstantNamedNode(identifier: Type, value: Type, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Type.Node identifier; yield Type.Node value |]
    member val Identifier = identifier
    member val Value = value

type TypeArrayNode(t: Type, rank: int, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Type.Node t |]
    member val Type = t
    member val Rank = rank

type TypeAppPostFixNode(first: Type, last: Type, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Type.Node first; yield Type.Node last |]
    member val First = first
    member val Last = last

type TypeAppPrefixNode
    (
        identifier: Type,
        postIdentifier: IdentListNode option,
        lessThan: SingleTextNode,
        arguments: Type list,
        greaterThan: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Type.Node identifier
           yield! noa postIdentifier
           yield lessThan
           yield! (List.map Type.Node arguments)
           yield greaterThan |]

    member val Identifier = identifier
    member val PostIdentifier = postIdentifier
    member val GreaterThan = greaterThan
    member val Arguments = arguments
    member val LessThen = lessThan

type TypeStructTupleNode
    (keyword: SingleTextNode, path: Choice<Type, SingleTextNode> list, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield keyword
           yield!
               List.map
                   (function
                   | Choice1Of2 t -> Type.Node t
                   | Choice2Of2 n -> n :> Node)
                   path
           yield closingParen |]

    member val Keyword = keyword
    member val Path = path
    member val ClosingParen = closingParen

type TypeWithGlobalConstraintsNode(t: Type, constraints: TypeConstraint list, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Type.Node t; yield! List.map TypeConstraint.Node constraints |]

    member val Type = t
    member val TypeConstraints = constraints

type TypeAnonRecordNode
    (
        structNode: SingleTextNode option,
        openingToken: SingleTextNode option,
        fields: (SingleTextNode * Type) list,
        closingToken: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa structNode
           yield! noa openingToken
           yield! (fields |> List.collect (fun (i, t) -> [ yield (i :> Node); yield Type.Node t ]))
           yield closingToken |]

    member val Struct = structNode
    member val Opening = openingToken
    member val Fields = fields
    member val Closing = closingToken

type TypeParenNode(openingParen: SingleTextNode, t: Type, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield openingParen; yield Type.Node t; yield closingParen |]
    member val OpeningParen = openingParen
    member val Type = t
    member val ClosingParen = closingParen

type TypeSignatureParameterNode
    (attributes: MultipleAttributeListNode option, identifier: SingleTextNode option, t: Type, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield! noa attributes; yield! noa identifier; yield Type.Node t |]

    member val Attributes = attributes
    member val Identifier = identifier
    member val Type = t

type TypeOrNode(lhs: Type, orNode: SingleTextNode, rhs: Type, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Type.Node lhs; yield orNode; yield Type.Node rhs |]
    member val LeftHandSide = lhs
    member val Or = orNode
    member val RightHandSide = rhs

type TypeLongIdentAppNode(appType: Type, longIdent: IdentListNode, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Type.Node appType; yield longIdent |]
    member val AppType = appType
    member val LongIdent = longIdent

type TypeIntersectionNode(typesAndSeparators: Choice<Type, SingleTextNode> list, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| for t in typesAndSeparators do
               match t with
               | Choice1Of2 t -> Type.Node t
               | Choice2Of2 amp -> amp |]

    member val TypesAndSeparators = typesAndSeparators

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Type =
    | Funs of TypeFunsNode
    | Tuple of TypeTupleNode
    | HashConstraint of TypeHashConstraintNode
    | MeasurePower of TypeMeasurePowerNode
    | StaticConstant of Constant
    | StaticConstantExpr of TypeStaticConstantExprNode
    | StaticConstantNamed of TypeStaticConstantNamedNode
    | Array of TypeArrayNode
    | Anon of SingleTextNode
    | Var of SingleTextNode
    | AppPostfix of TypeAppPostFixNode
    | AppPrefix of TypeAppPrefixNode
    | StructTuple of TypeStructTupleNode
    | WithSubTypeConstraint of TypeConstraint
    | WithGlobalConstraints of TypeWithGlobalConstraintsNode
    | LongIdent of IdentListNode
    | AnonRecord of TypeAnonRecordNode
    | Paren of TypeParenNode
    | SignatureParameter of TypeSignatureParameterNode
    | Or of TypeOrNode
    | LongIdentApp of TypeLongIdentAppNode
    | Intersection of TypeIntersectionNode

    static member Node(x: Type) : Node =
        match x with
        | Funs n -> n
        | Tuple n -> n
        | HashConstraint n -> n
        | MeasurePower n -> n
        | StaticConstant c -> Constant.Node c
        | StaticConstantExpr n -> n
        | StaticConstantNamed n -> n
        | Array n -> n
        | Anon n -> n
        | Var n -> n
        | AppPostfix n -> n
        | AppPrefix n -> n
        | StructTuple n -> n
        | WithSubTypeConstraint tc -> TypeConstraint.Node tc
        | WithGlobalConstraints n -> n
        | LongIdent n -> n
        | AnonRecord n -> n
        | Paren n -> n
        | SignatureParameter n -> n
        | Or n -> n
        | LongIdentApp n -> n
        | Intersection n -> n

/// A pattern composed from a left hand-side pattern, a single text token/operator and a right hand-side pattern.
type PatLeftMiddleRight(lhs: Pattern, middle: Choice<SingleTextNode, string>, rhs: Pattern, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Pattern.Node lhs
           match middle with
           | Choice1Of2 n -> yield n
           | _ -> ()
           yield Pattern.Node rhs |]

    member val LeftHandSide = lhs
    member val Middle = middle
    member val RightHandSide = rhs

type PatAndsNode(pats: Pattern list, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield! List.map Pattern.Node pats |]
    member val Patterns = pats

type PatParameterNode(attributes: MultipleAttributeListNode option, pat: Pattern, t: Type option, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa attributes
           yield Pattern.Node pat
           yield! noa (Option.map Type.Node t) |]

    member val Attributes = attributes
    member val Pattern = pat
    member val Type = t

type PatNamedParenStarIdentNode
    (
        accessibility: SingleTextNode option,
        openingParen: SingleTextNode,
        name: SingleTextNode,
        closingParen: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa accessibility
           yield openingParen
           yield name
           yield closingParen |]

    member val Accessibility = accessibility
    member val OpeningParen = openingParen
    member val Name = name
    member val ClosingParen = closingParen

type PatNamedNode(accessibility: SingleTextNode option, name: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield name |]
    member val Name = name
    member val Accessibility = accessibility

type NamePatPair(ident: SingleTextNode, equals: SingleTextNode, pat: Pattern, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield ident; yield equals; yield Pattern.Node pat |]
    member val Ident = ident
    member val Equals = equals
    member val Pattern = pat

type PatNamePatPairsNode
    (
        identifier: IdentListNode,
        typarDecls: TyparDecls option,
        openingParen: SingleTextNode,
        pairs: NamePatPair list,
        closingParen: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield identifier
           yield! noa (Option.map TyparDecls.Node typarDecls)
           yield openingParen
           yield! nodes pairs
           yield closingParen |]

    member val Identifier = identifier
    member val TyparDecls = typarDecls
    member val OpeningParen = openingParen
    member val Pairs = pairs
    member val ClosingParen = closingParen

type PatLongIdentNode
    (
        accessibility: SingleTextNode option,
        identifier: IdentListNode,
        typarDecls: TyparDecls option,
        parameters: Pattern list,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa accessibility
           yield identifier
           yield! noa (Option.map TyparDecls.Node typarDecls)
           yield! List.map Pattern.Node parameters |]

    member val Accessibility = accessibility
    member val Identifier = identifier
    member val TyparDecls = typarDecls
    member val Parameters = parameters

type PatParenNode(openingParen: SingleTextNode, pat: Pattern, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingParen; yield Pattern.Node pat; yield closingParen |]

    member val OpeningParen = openingParen
    member val Pattern = pat
    member val ClosingParen = closingParen

type PatTupleNode(items: Choice<Pattern, SingleTextNode> list, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| for item in items do
               match item with
               | Choice1Of2 p -> Pattern.Node p
               | Choice2Of2 comma -> comma |]

    member val Items = items

type PatStructTupleNode(pats: Pattern list, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield! (List.map Pattern.Node pats) |]
    member val Patterns = pats

type PatArrayOrListNode(openToken: SingleTextNode, pats: Pattern list, closeToken: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openToken; yield! List.map Pattern.Node pats; yield closeToken |]

    member val OpenToken = openToken
    member val Patterns = pats
    member val CloseToken = closeToken

type PatRecordField
    (prefix: IdentListNode option, fieldName: SingleTextNode, equals: SingleTextNode, pat: Pattern, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield! noa prefix; yield fieldName; yield equals; yield Pattern.Node pat |]

    member val Prefix = prefix
    member val FieldName = fieldName
    member val Equals = equals
    member val Pattern = pat

type PatRecordNode(openingNode: SingleTextNode, fields: PatRecordField list, closingNode: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingNode; yield! nodes fields; yield closingNode |]
    member val OpeningNode = openingNode
    member val Fields = fields
    member val ClosingNode = closingNode

type PatIsInstNode(token: SingleTextNode, t: Type, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield token; yield Type.Node t |]
    member val Token = token
    member val Type = t

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Pattern =
    | OptionalVal of SingleTextNode
    | Or of PatLeftMiddleRight
    | Ands of PatAndsNode
    | Null of SingleTextNode
    | Wild of SingleTextNode
    | Parameter of PatParameterNode
    | NamedParenStarIdent of PatNamedParenStarIdentNode
    | Named of PatNamedNode
    | As of PatLeftMiddleRight
    | ListCons of PatLeftMiddleRight
    | NamePatPairs of PatNamePatPairsNode
    | LongIdent of PatLongIdentNode
    | Unit of UnitNode
    | Paren of PatParenNode
    | Tuple of PatTupleNode
    | StructTuple of PatStructTupleNode
    | ArrayOrList of PatArrayOrListNode
    | Record of PatRecordNode
    | Const of Constant
    | IsInst of PatIsInstNode
    | QuoteExpr of ExprQuoteNode

    static member Node(x: Pattern) : Node =
        match x with
        | OptionalVal n -> n
        | Parameter n -> n
        | Or n -> n
        | Ands n -> n
        | Null n -> n
        | Wild n -> n
        | NamedParenStarIdent n -> n
        | Named n -> n
        | As n -> n
        | ListCons n -> n
        | NamePatPairs n -> n
        | LongIdent n -> n
        | Unit n -> n
        | Paren n -> n
        | Tuple n -> n
        | StructTuple n -> n
        | ArrayOrList n -> n
        | Record n -> n
        | Const c -> Constant.Node c
        | IsInst n -> n
        | QuoteExpr n -> n

type ExprLazyNode(lazyWord: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield lazyWord; yield Expr.Node expr |]

    member val LazyWord = lazyWord
    member val Expr = expr

    member val ExprIsInfix = false
// match Expr.Node expr with
// | :? InfixApp -> true
// | _ -> false

type ExprSingleNode(leading: SingleTextNode, addSpace: bool, supportsStroustrup: bool, expr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield leading; yield Expr.Node expr |]

    member val Leading = leading
    member val AddSpace = addSpace
    member val SupportsStroustrup = supportsStroustrup
    member val Expr = expr

type ExprConstantNode(range) =
    inherit NodeBase(range)

    override val Children: Node array = failwith "todo"

type ExprQuoteNode(openToken: SingleTextNode, expr, closeToken: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openToken; yield Expr.Node expr; yield closeToken |]
    member val OpenToken = openToken
    member val Expr = expr
    member val CloseToken = closeToken

type ExprTypedNode(expr: Expr, operator: string, t: Type, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Expr.Node expr; yield Type.Node t |]
    member val Expr = expr
    member val Operator = operator
    member val Type = t

type ExprNewNode(newKeyword: SingleTextNode, t: Type, arguments: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield newKeyword; yield Type.Node t; yield Expr.Node arguments |]

    member val NewKeyword = newKeyword
    member val Type = t
    member val Arguments = arguments

type ExprTupleNode(items: Choice<Expr, SingleTextNode> list, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        items
        |> Seq.map (function
            | Choice1Of2 e -> Expr.Node e
            | Choice2Of2 comma -> comma :> Node)
        |> Seq.toArray

    member val Items = items

type ExprStructTupleNode(structNode: SingleTextNode, tuple: ExprTupleNode, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield structNode; yield tuple; yield closingParen |]
    member val Struct = structNode
    member val Tuple = tuple
    member val ClosingParen = closingParen

type ExprArrayOrListNode(openingToken: SingleTextNode, elements: Expr list, closingToken: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingToken; yield! List.map Expr.Node elements; yield closingToken |]

    member val Opening = openingToken
    member val Elements = elements
    member val Closing = closingToken

type InheritConstructorTypeOnlyNode(inheritKeyword: SingleTextNode, t: Type, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield inheritKeyword; yield Type.Node t |]
    member val InheritKeyword = inheritKeyword
    member val Type = t

type InheritConstructorUnitNode
    (inheritKeyword: SingleTextNode, t: Type, openingParen: SingleTextNode, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield inheritKeyword
           yield Type.Node t
           yield openingParen
           yield closingParen |]

    member val InheritKeyword = inheritKeyword
    member val Type = t
    member val OpeningParen = openingParen
    member val ClosingParen = closingParen

type InheritConstructorParenNode(inheritKeyword: SingleTextNode, t: Type, expr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield inheritKeyword; yield Type.Node t; yield Expr.Node expr |]

    member val InheritKeyword = inheritKeyword
    member val Type = t
    member val Expr = expr

type InheritConstructorOtherNode(inheritKeyword: SingleTextNode, t: Type, expr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield inheritKeyword; yield Type.Node t; yield Expr.Node expr |]

    member val InheritKeyword = inheritKeyword
    member val Type = t
    member val Expr = expr

[<RequireQualifiedAccess; NoComparison>]
type InheritConstructor =
    | TypeOnly of InheritConstructorTypeOnlyNode
    | Unit of InheritConstructorUnitNode
    | Paren of InheritConstructorParenNode
    | Other of InheritConstructorOtherNode

    static member Node(ic: InheritConstructor) : Node =
        match ic with
        | TypeOnly n -> n
        | Unit n -> n
        | Paren n -> n
        | Other n -> n

    member x.InheritKeyword =
        match x with
        | TypeOnly n -> n.InheritKeyword
        | Unit n -> n.InheritKeyword
        | Paren n -> n.InheritKeyword
        | Other n -> n.InheritKeyword

type RecordFieldNode(fieldName: IdentListNode, equals: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield fieldName; yield equals; yield Expr.Node expr |]
    member val FieldName = fieldName
    member val Equals = equals
    member val Expr = expr

[<AbstractClass>]
type ExprRecordBaseNode(openingBrace: SingleTextNode, fields: RecordFieldNode list, closingBrace: SingleTextNode, range)
    =
    inherit NodeBase(range)

    member val OpeningBrace = openingBrace
    member val Fields = fields
    member val ClosingBrace = closingBrace
    member x.HasFields = List.isNotEmpty x.Fields

/// <summary>
/// Represents a record instance, parsed from both `SynExpr.Record` and `SynExpr.AnonRecd`.
/// </summary>
type ExprRecordNode
    (
        openingBrace: SingleTextNode,
        copyInfo: Expr option,
        fields: RecordFieldNode list,
        closingBrace: SingleTextNode,
        range
    ) =
    inherit ExprRecordBaseNode(openingBrace, fields, closingBrace, range)

    member val CopyInfo = copyInfo

    override val Children: Node array =
        [| yield openingBrace
           yield! copyInfo |> Option.map Expr.Node |> noa
           yield! nodes fields
           yield closingBrace |]

    member x.HasFields = List.isNotEmpty x.Fields

type ExprAnonStructRecordNode
    (
        structNode: SingleTextNode,
        openingBrace: SingleTextNode,
        copyInfo: Expr option,
        fields: RecordFieldNode list,
        closingBrace: SingleTextNode,
        range
    ) =
    inherit ExprRecordNode(openingBrace, copyInfo, fields, closingBrace, range)
    member val Struct = structNode

    override val Children: Node array =
        [| yield structNode
           yield openingBrace
           yield! copyInfo |> Option.map Expr.Node |> noa
           yield! nodes fields
           yield closingBrace |]

type ExprInheritRecordNode
    (
        openingBrace: SingleTextNode,
        inheritConstructor: InheritConstructor,
        fields: RecordFieldNode list,
        closingBrace: SingleTextNode,
        range
    ) =
    inherit ExprRecordBaseNode(openingBrace, fields, closingBrace, range)

    member val InheritConstructor = inheritConstructor

    override val Children: Node array =
        [| yield openingBrace
           yield InheritConstructor.Node inheritConstructor
           yield! nodes fields
           yield closingBrace |]

type InterfaceImplNode
    (
        interfaceNode: SingleTextNode,
        t: Type,
        withNode: SingleTextNode option,
        bindings: BindingNode list,
        members: MemberDefn list,
        range
    ) =

    inherit NodeBase(range)

    override val Children: Node array =
        [| yield interfaceNode
           yield Type.Node t
           yield! noa withNode
           yield! nodes bindings
           yield! List.map MemberDefn.Node members |]

    member val Interface = interfaceNode
    member val Type = t
    member val With = withNode
    member val Bindings = bindings
    member val Members = members

type ExprObjExprNode
    (
        openingBrace: SingleTextNode,
        newNode: SingleTextNode,
        t: Type,
        e: Expr option,
        withNode: SingleTextNode option,
        bindings: BindingNode list,
        members: MemberDefn list,
        interfaces: InterfaceImplNode list,
        closingBrace: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield openingBrace
           yield newNode
           yield Type.Node t
           yield! noa (Option.map Expr.Node e)
           yield! noa withNode
           yield! nodes bindings
           yield! List.map MemberDefn.Node members
           yield! nodes interfaces
           yield closingBrace |]

    member val OpeningBrace = openingBrace
    member val New = newNode
    member val Type = t
    member val Expr = e
    member val With = withNode
    member val Bindings = bindings
    member val Members = members
    member val Interfaces = interfaces
    member val ClosingBrace = closingBrace

type ExprWhileNode(whileNode: SingleTextNode, whileExpr: Expr, doExpr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield whileNode; yield Expr.Node whileExpr; yield Expr.Node doExpr |]

    member val While = whileNode
    member val WhileExpr = whileExpr
    member val DoExpr = doExpr

type ExprForNode
    (
        forNode: SingleTextNode,
        ident: SingleTextNode,
        equals: SingleTextNode,
        identBody: Expr,
        direction: bool,
        toBody: Expr,
        doBody: Expr,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield forNode
           yield ident
           yield equals
           yield Expr.Node identBody
           yield Expr.Node toBody
           yield Expr.Node doBody |]

    member val For = forNode
    member val Ident = ident
    member val Equals = equals
    member val IdentBody = identBody
    member val Direction = direction
    member val ToBody = toBody
    member val DoBody = doBody

type ExprForEachNode(forNode: SingleTextNode, pat: Pattern, enumExpr: Expr, isArrow: bool, bodyExpr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield forNode
           yield Pattern.Node pat
           yield Expr.Node enumExpr
           yield Expr.Node bodyExpr |]

    member val For = forNode
    member val Pattern = pat
    member val EnumExpr = enumExpr
    member val IsArrow = isArrow
    member val BodyExpr = bodyExpr

type ExprNamedComputationNode
    (nameExpr: Expr, openingBrace: SingleTextNode, bodyExpr: Expr, closingBrace: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Expr.Node nameExpr
           yield openingBrace
           yield Expr.Node bodyExpr
           yield closingBrace |]

    member val Name = nameExpr
    member val OpeningBrace = openingBrace
    member val Body = bodyExpr
    member val ClosingBrace = closingBrace

type ExprComputationNode(openingBrace: SingleTextNode, bodyExpr: Expr, closingBrace: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingBrace; yield Expr.Node bodyExpr; yield closingBrace |]

    member val OpeningBrace = openingBrace
    member val Body = bodyExpr
    member val ClosingBrace = closingBrace

type ExprLetOrUseNode(binding: BindingNode, inKeyword: SingleTextNode option, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield binding; yield! noa inKeyword |]
    member val Binding = binding
    member val In = inKeyword

type ExprLetOrUseBangNode(leadingKeyword: SingleTextNode, pat: Pattern, equals: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield leadingKeyword
           yield Pattern.Node pat
           yield equals
           yield Expr.Node expr |]

    member val LeadingKeyword = leadingKeyword
    member val Pattern = pat
    member val Equals = equals
    member val Expression = expr

type ExprAndBang(leadingKeyword: SingleTextNode, pat: Pattern, equals: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield leadingKeyword
           yield Pattern.Node pat
           yield equals
           yield Expr.Node expr |]

    member val LeadingKeyword = leadingKeyword
    member val Pattern = pat
    member val Equals = equals
    member val Expression = expr

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ComputationExpressionStatement =
    | LetOrUseStatement of ExprLetOrUseNode
    | LetOrUseBangStatement of ExprLetOrUseBangNode
    | AndBangStatement of ExprAndBang
    | OtherStatement of Expr

    static member Node(ces: ComputationExpressionStatement) : Node =
        match ces with
        | LetOrUseStatement n -> n
        | LetOrUseBangStatement n -> n
        | AndBangStatement n -> n
        | OtherStatement o -> Expr.Node o

type ExprCompExprBodyNode(statements: ComputationExpressionStatement list, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield! List.map ComputationExpressionStatement.Node statements |]

    member val Statements = statements

type ExprJoinInNode(lhs: Expr, inNode: SingleTextNode, rhs: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Expr.Node lhs; yield inNode; yield Expr.Node rhs |]
    member val LeftHandSide = lhs
    member val In = inNode
    member val RightHandSide = rhs

type ExprParenLambdaNode(openingParen: SingleTextNode, lambda: ExprLambdaNode, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield openingParen; yield lambda; yield closingParen |]
    member val OpeningParen = openingParen
    member val Lambda = lambda
    member val ClosingParen = closingParen

type ExprLambdaNode(funNode: SingleTextNode, parameters: Pattern list, arrow: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield funNode
           yield! List.map Pattern.Node parameters
           yield arrow
           yield Expr.Node expr |]

    member val Fun = funNode
    member val Parameters = parameters
    member val Arrow = arrow
    member val Expr = expr

type MatchClauseNode
    (bar: SingleTextNode option, pattern: Pattern, whenExpr: Expr option, arrow: SingleTextNode, bodyExpr: Expr, range)
    =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa bar
           yield Pattern.Node pattern
           yield! noa (Option.map Expr.Node whenExpr)
           yield arrow
           yield Expr.Node bodyExpr |]

    member val Bar = bar
    member val Pattern = pattern
    member val WhenExpr = whenExpr
    member val Arrow = arrow
    member val BodyExpr = bodyExpr

type ExprMatchLambdaNode(functionNode: SingleTextNode, clauses: MatchClauseNode list, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield functionNode; yield! nodes clauses |]
    member val Function = functionNode
    member val Clauses = clauses

type ExprMatchNode
    (matchNode: SingleTextNode, matchExpr: Expr, withNode: SingleTextNode, clauses: MatchClauseNode list, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield matchNode
           yield Expr.Node matchExpr
           yield withNode
           yield! nodes clauses |]

    member val Match = matchNode
    member val MatchExpr = matchExpr
    member val With = withNode
    member val Clauses = clauses

type ExprTraitCallNode(t: Type, md: MemberDefn, expr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Type.Node t; yield MemberDefn.Node md; yield Expr.Node expr |]

    member val Type = t
    member val MemberDefn = md
    member val Expr = expr

type ExprParenFunctionNameWithStarNode
    (openingParen: SingleTextNode, functionName: SingleTextNode, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingParen; yield functionName; yield closingParen |]
    member val OpeningParen = openingParen
    member val FunctionName = functionName
    member val ClosingParen = closingParen

type ExprParenNode(openingParen: SingleTextNode, expr: Expr, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingParen; yield Expr.Node expr; yield closingParen |]

    member val OpeningParen = openingParen
    member val Expr = expr
    member val ClosingParen = closingParen

type ExprDynamicNode(funcExpr: Expr, argExpr: Expr, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Expr.Node funcExpr; yield Expr.Node argExpr |]
    member val FuncExpr = funcExpr
    member val ArgExpr = argExpr

type ExprPrefixAppNode(operator: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield operator; yield Expr.Node expr |]
    member val Operator = operator
    member val Expr = expr

// type InfixApp = interface end
//
type ExprSameInfixAppsNode(leadingExpr: Expr, subsequentExpressions: (SingleTextNode * Expr) list, range) =
    inherit NodeBase(range)
    // interface InfixApp

    override val Children: Node array =
        let xs =
            List.collect (fun (operator, expr) -> [ (operator :> Node); Expr.Node expr ]) subsequentExpressions

        [| yield Expr.Node leadingExpr; yield! xs |]

    member val LeadingExpr = leadingExpr
    member val SubsequentExpressions = subsequentExpressions

type ExprInfixAppNode(lhs: Expr, operator: SingleTextNode, rhs: Expr, range) =
    inherit NodeBase(range)
    // interface InfixApp

    override val Children: Node array = [| yield Expr.Node lhs; yield operator; yield Expr.Node rhs |]
    member val LeftHandSide = lhs
    member val RightHandSide: Expr = rhs
    member val Operator = operator

type ExprIndexWithoutDotNode(identifierExpr: Expr, indexExpr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Expr.Node identifierExpr; yield Expr.Node indexExpr |]
    member val Identifier = identifierExpr
    member val Index = indexExpr

type LinkSingleAppParen(functionName: Expr, parenExpr: ExprParenNode, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Expr.Node functionName; yield parenExpr |]
    member val FunctionName = functionName
    member val Paren = parenExpr

type LinkSingleAppUnit(functionName: Expr, unit: UnitNode, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Expr.Node functionName; yield unit |]
    member val FunctionName = functionName
    member val Unit = unit

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ChainLink =
    | Identifier of Expr
    | Dot of SingleTextNode
    | Expr of Expr
    | AppParen of
        // There should only be one argument
        LinkSingleAppParen
    | AppUnit of LinkSingleAppUnit
    // [ expr ] from DotIndexedGet
    | IndexExpr of Expr // e.[f]

    static member Node(link: ChainLink) : Node =
        match link with
        | Identifier e -> Expr.Node e
        | Dot n -> n
        | Expr e -> Expr.Node e
        | AppParen n -> n
        | AppUnit n -> n
        | IndexExpr e -> Expr.Node e

type ExprChain(links: ChainLink list, range) =
    inherit NodeBase(range)
    override val Children: Node array = List.map ChainLink.Node links |> List.toArray
    member val Links = links

type ExprAppLongIdentAndSingleParenArgNode(functionName: IdentListNode, argExpr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield functionName; yield Expr.Node argExpr |]
    member val FunctionName = functionName
    member val ArgExpr = argExpr

type ExprAppSingleParenArgNode(functionExpr: Expr, argExpr: Expr, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Expr.Node functionExpr; yield Expr.Node argExpr |]
    member val FunctionExpr = functionExpr
    member val ArgExpr = argExpr

type ExprAppWithLambdaNode
    (
        functionName: Expr,
        arguments: Expr list,
        openingParen: SingleTextNode,
        lambda: Choice<ExprLambdaNode, ExprMatchLambdaNode>,
        closingParen: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        let lambdaNode =
            match lambda with
            | Choice1Of2 n -> n :> Node
            | Choice2Of2 n -> n

        [| yield Expr.Node functionName
           yield! List.map Expr.Node arguments
           yield openingParen
           yield lambdaNode
           yield closingParen |]

    member val FunctionName = functionName
    member val Arguments = arguments
    member val OpeningParen = openingParen
    member val Lambda = lambda
    member val ClosingParen = closingParen

type ExprNestedIndexWithoutDotNode(identifierExpr: Expr, indexExpr: Expr, argumentExpr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Expr.Node identifierExpr
           yield Expr.Node indexExpr
           yield Expr.Node argumentExpr |]

    member val Identifier = identifierExpr
    member val Index = indexExpr
    member val Argument = argumentExpr

type ExprAppNode(functionExpr: Expr, arguments: Expr list, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Expr.Node functionExpr; yield! List.map Expr.Node arguments |]

    member val FunctionExpr: Expr = functionExpr
    member val Arguments: Expr list = arguments

type ExprTypeAppNode
    (identifierExpr: Expr, lessThan: SingleTextNode, typeParameters: Type list, greaterThan: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Expr.Node identifierExpr
           yield lessThan
           yield! List.map Type.Node typeParameters
           yield greaterThan |]

    member val Identifier = identifierExpr
    member val LessThan = lessThan
    member val TypeParameters = typeParameters
    member val GreaterThan = greaterThan

type ExprTryWithSingleClauseNode
    (tryNode: SingleTextNode, tryExpr: Expr, withNode: SingleTextNode, clause: MatchClauseNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield tryNode; yield Expr.Node tryExpr; yield withNode; yield clause |]

    member val Try = tryNode
    member val TryExpr = tryExpr
    member val With = withNode
    member val Clause = clause

type ExprTryWithNode
    (tryNode: SingleTextNode, tryExpr: Expr, withNode: SingleTextNode, clauses: MatchClauseNode list, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield tryNode
           yield Expr.Node tryExpr
           yield withNode
           yield! nodes clauses |]

    member val Try = tryNode
    member val TryExpr = tryExpr
    member val With = withNode
    member val Clauses = clauses

type ExprTryFinallyNode(tryNode: SingleTextNode, tryExpr: Expr, finallyNode: SingleTextNode, finallyExpr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield tryNode
           yield Expr.Node tryExpr
           yield finallyNode
           yield Expr.Node finallyExpr |]

    member val Try = tryNode
    member val TryExpr = tryExpr
    member val Finally = finallyNode
    member val FinallyExpr = finallyExpr

type ElseIfNode(mElse: range, mIf: range, condition: Node, range) as elseIfNode =
    let mutable elseCursor = None
    let mutable ifCursor = None
    let nodesBefore = Queue<TriviaNode>(0)
    let nodesAfter = Queue<TriviaNode>(0)
    let mutable lastNodeAfterIsLineCommentAfterSource = false

    let elseNode =
        { new Node with
            member _.ContentBefore: TriviaNode seq = Seq.empty
            member _.HasContentBefore: bool = false
            member _.ContentAfter: TriviaNode seq = Seq.empty
            member _.HasContentAfter: bool = false
            member _.Range = mElse

            member _.AddBefore(triviaNode: TriviaNode) =
                (elseIfNode :> Node).AddBefore triviaNode

            member _.AddAfter(triviaNode: TriviaNode) =
                (elseIfNode :> Node).AddAfter triviaNode

            member _.Children = Array.empty
            member _.AddCursor cursor = elseCursor <- Some cursor
            member _.TryGetCursor = elseCursor }

    let ifNode =
        { new Node with
            member _.ContentBefore: TriviaNode seq = Seq.empty
            member _.HasContentBefore: bool = false
            member _.ContentAfter: TriviaNode seq = Seq.empty
            member _.HasContentAfter: bool = false
            member _.Range = mIf

            member _.AddBefore(triviaNode: TriviaNode) =
                match triviaNode.Content with
                | CommentOnSingleLine _
                | Newline -> condition.AddBefore triviaNode
                | _ -> (elseIfNode :> Node).AddAfter triviaNode

            member _.AddAfter(triviaNode: TriviaNode) =
                (elseIfNode :> Node).AddAfter triviaNode

            member _.Children = Array.empty
            member _.AddCursor cursor = ifCursor <- Some cursor
            member _.TryGetCursor = ifCursor }

    interface Node with
        member _.ContentBefore: TriviaNode seq = nodesBefore
        member _.HasContentBefore: bool = not (Seq.isEmpty nodesBefore)
        member _.ContentAfter: TriviaNode seq = nodesAfter
        member _.HasContentAfter: bool = not (Seq.isEmpty nodesAfter)
        member _.Range = range
        member _.AddBefore(triviaNode: TriviaNode) = nodesBefore.Enqueue triviaNode

        member _.AddAfter(triviaNode: TriviaNode) =
            match triviaNode.Content with
            | TriviaContent.LineCommentAfterSourceCode comment when lastNodeAfterIsLineCommentAfterSource ->
                // If we already have a line comment after the `else if`, we cannot add another one.
                // The next best thing would be to add it on the next line as content before of the condition.
                let triviaNode =
                    TriviaNode(TriviaContent.CommentOnSingleLine comment, triviaNode.Range)

                condition.AddBefore triviaNode
            | _ ->
                lastNodeAfterIsLineCommentAfterSource <-
                    match triviaNode.Content with
                    | LineCommentAfterSourceCode _ -> true
                    | _ -> false

                nodesAfter.Enqueue triviaNode

        member val Children = [| elseNode; ifNode |]
        member _.AddCursor _ = ()
        member _.TryGetCursor = None

[<RequireQualifiedAccess; NoComparison>]
type IfKeywordNode =
    | SingleWord of SingleTextNode
    | ElseIf of ElseIfNode

    member x.Node =
        match x with
        | SingleWord n -> n :> Node
        | ElseIf n -> n :> Node

    member x.Range = x.Node.Range

type ExprIfThenNode(ifNode: IfKeywordNode, ifExpr: Expr, thenNode: SingleTextNode, thenExpr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield ifNode.Node
           yield Expr.Node ifExpr
           yield thenNode
           yield Expr.Node thenExpr |]

    member val If = ifNode
    member val IfExpr = ifExpr
    member val Then = thenNode
    member val ThenExpr = thenExpr

type ExprIfThenElseNode
    (
        ifNode: IfKeywordNode,
        ifExpr: Expr,
        thenNode: SingleTextNode,
        thenExpr: Expr,
        elseNode: SingleTextNode,
        elseExpr: Expr,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield ifNode.Node
           yield Expr.Node ifExpr
           yield thenNode
           yield Expr.Node thenExpr
           yield elseNode
           yield Expr.Node elseExpr |]

    member val If = ifNode
    member val IfExpr = ifExpr
    member val Then = thenNode
    member val ThenExpr = thenExpr
    member val Else = elseNode
    member val ElseExpr = elseExpr

type ExprIfThenElifNode(branches: ExprIfThenNode list, elseBranch: (SingleTextNode * Expr) option, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        let elseNodes =
            match elseBranch with
            | None -> []
            | Some(elseNode, elseExpr) -> [ yield (elseNode :> Node); yield Expr.Node elseExpr ]

        [| yield! nodes branches; yield! elseNodes |]

    member val Branches = branches
    member val Else = elseBranch

type ExprOptVarNode(isOptional: bool, identifier: IdentListNode, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield identifier |]
    member val IsOptional = isOptional
    member val Identifier = identifier

type ExprLongIdentSetNode(identifier: IdentListNode, rhs: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield identifier; yield Expr.Node rhs |]
    member val Identifier = identifier
    member val Expr = rhs

type ExprDotIndexedGetNode(objectExpr: Expr, indexExpr: Expr, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Expr.Node objectExpr; yield Expr.Node indexExpr |]
    member val ObjectExpr = objectExpr
    member val IndexExpr = indexExpr

type ExprDotIndexedSetNode(objectExpr: Expr, indexExpr: Expr, valueExpr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Expr.Node objectExpr
           yield Expr.Node indexExpr
           yield Expr.Node valueExpr |]

    member val ObjectExpr = objectExpr
    member val Index = indexExpr
    member val Value = valueExpr

type ExprNamedIndexedPropertySetNode(identifier: IdentListNode, indexExpr: Expr, valueExpr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield identifier; yield Expr.Node indexExpr; yield Expr.Node valueExpr |]

    member val Identifier = identifier
    member val Index = indexExpr
    member val Value = valueExpr

type ExprDotNamedIndexedPropertySetNode
    (identifierExpr: Expr, name: IdentListNode, propertyExpr: Expr, setExpr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Expr.Node identifierExpr
           yield name
           yield Expr.Node propertyExpr
           yield Expr.Node setExpr |]

    member val Identifier = identifierExpr
    member val Name = name
    member val Property = propertyExpr
    member val Set = setExpr

type ExprSetNode(identifier: Expr, setExpr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Expr.Node identifier; yield Expr.Node setExpr |]
    member val Identifier = identifier
    member val Set = setExpr

type StaticOptimizationConstraintWhenTyparTyconEqualsTyconNode(typar: SingleTextNode, t: Type, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield typar; yield Type.Node t |]
    member val TypeParameter = typar
    member val Type = t

[<NoComparison>]
type StaticOptimizationConstraint =
    | WhenTyparTyconEqualsTycon of StaticOptimizationConstraintWhenTyparTyconEqualsTyconNode
    | WhenTyparIsStruct of SingleTextNode

    static member Node(c: StaticOptimizationConstraint) : Node =
        match c with
        | WhenTyparTyconEqualsTycon n -> n
        | WhenTyparIsStruct n -> n

type ExprLibraryOnlyStaticOptimizationNode
    (optimizedExpr: Expr, constraints: StaticOptimizationConstraint list, expr: Expr, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Expr.Node optimizedExpr
           yield! List.map StaticOptimizationConstraint.Node constraints
           yield Expr.Node expr |]

    member val OptimizedExpr = optimizedExpr
    member val Constraints = constraints
    member val Expr = expr

type FillExprNode(expr: Expr, ident: SingleTextNode option, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Expr.Node expr; yield! noa ident |]
    member val Expr = expr
    member val Ident = ident

type ExprInterpolatedStringExprNode(parts: Choice<SingleTextNode, FillExprNode> list, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield!
               List.map
                   (function
                   | Choice1Of2 n -> (n :> Node)
                   | Choice2Of2 n -> (n :> Node))
                   parts |]

    member val Parts = parts

type ExprTripleNumberIndexRangeNode
    (
        startNode: SingleTextNode,
        startDots: SingleTextNode,
        centerNode: SingleTextNode,
        endDots: SingleTextNode,
        endNode: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield startNode
           yield startDots
           yield centerNode
           yield endDots
           yield endNode |]

    member val Start = startNode
    member val StartDots = startDots
    member val Center = centerNode
    member val EndDots = endDots
    member val End = endNode

type ExprIndexRangeNode(fromExpr: Expr option, dots: SingleTextNode, toExpr: Expr option, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa (Option.map Expr.Node fromExpr)
           yield dots
           yield! noa (Option.map Expr.Node toExpr) |]

    member val From = fromExpr
    member val Dots = dots
    member val To = toExpr

type ExprIndexFromEndNode(expr: Expr, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| Expr.Node expr |]
    member val Expr = expr

type ExprDotLambda(underscore: SingleTextNode, dot: SingleTextNode, expr: Expr, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| underscore; dot; Expr.Node expr |]
    member val Underscore = underscore
    member val Dot = dot
    member val Expr = expr

type ExprBeginEndNode(beginNode: SingleTextNode, expr: Expr, endNode: SingleTextNode, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield beginNode; yield Expr.Node expr; yield endNode |]

    member val Begin = beginNode
    member val Expr = expr
    member val End = endNode

/// then <expr>
/// Only valid in secondary constructors, original coming from SynExpr.Sequential(trivia = { SeparatorRange = Some mThen })
type ExprExplicitConstructorThenExpr(thenNode: SingleTextNode, expr: Expr, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield thenNode; yield Expr.Node expr |]
    member val Then = thenNode
    member val Expr = expr

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Expr =
    | Lazy of ExprLazyNode
    | Single of ExprSingleNode
    | Constant of Constant
    | Null of SingleTextNode
    | Quote of ExprQuoteNode
    | Typed of ExprTypedNode
    | New of ExprNewNode
    | Tuple of ExprTupleNode
    | StructTuple of ExprStructTupleNode
    | ArrayOrList of ExprArrayOrListNode
    | Record of ExprRecordNode
    | InheritRecord of ExprInheritRecordNode
    | AnonStructRecord of ExprAnonStructRecordNode
    | ObjExpr of ExprObjExprNode
    | While of ExprWhileNode
    | For of ExprForNode
    | ForEach of ExprForEachNode
    | NamedComputation of ExprNamedComputationNode
    | Computation of ExprComputationNode
    | CompExprBody of ExprCompExprBodyNode
    | JoinIn of ExprJoinInNode
    | ParenLambda of ExprParenLambdaNode
    | Lambda of ExprLambdaNode
    | MatchLambda of ExprMatchLambdaNode
    | Match of ExprMatchNode
    | TraitCall of ExprTraitCallNode
    | ParenILEmbedded of SingleTextNode
    | ParenFunctionNameWithStar of ExprParenFunctionNameWithStarNode
    | Paren of ExprParenNode
    | Dynamic of ExprDynamicNode
    | PrefixApp of ExprPrefixAppNode
    | SameInfixApps of ExprSameInfixAppsNode
    | InfixApp of ExprInfixAppNode
    | IndexWithoutDot of ExprIndexWithoutDotNode
    | AppLongIdentAndSingleParenArg of ExprAppLongIdentAndSingleParenArgNode
    | AppSingleParenArg of ExprAppSingleParenArgNode
    | AppWithLambda of ExprAppWithLambdaNode
    | NestedIndexWithoutDot of ExprNestedIndexWithoutDotNode
    | App of ExprAppNode
    | TypeApp of ExprTypeAppNode
    | TryWithSingleClause of ExprTryWithSingleClauseNode
    | TryWith of ExprTryWithNode
    | TryFinally of ExprTryFinallyNode
    | IfThen of ExprIfThenNode
    | IfThenElse of ExprIfThenElseNode
    | IfThenElif of ExprIfThenElifNode
    | Ident of SingleTextNode
    | OptVar of ExprOptVarNode
    | LongIdentSet of ExprLongIdentSetNode
    | DotIndexedGet of ExprDotIndexedGetNode
    | DotIndexedSet of ExprDotIndexedSetNode
    | NamedIndexedPropertySet of ExprNamedIndexedPropertySetNode
    | DotNamedIndexedPropertySet of ExprDotNamedIndexedPropertySetNode
    | Set of ExprSetNode
    | LibraryOnlyStaticOptimization of ExprLibraryOnlyStaticOptimizationNode
    | InterpolatedStringExpr of ExprInterpolatedStringExprNode
    | IndexRangeWildcard of SingleTextNode
    | TripleNumberIndexRange of ExprTripleNumberIndexRangeNode
    | IndexRange of ExprIndexRangeNode
    | IndexFromEnd of ExprIndexFromEndNode
    | Typar of SingleTextNode
    | Chain of ExprChain
    | DotLambda of ExprDotLambda
    | BeginEnd of ExprBeginEndNode
    | ExplicitConstructorThenExpr of ExprExplicitConstructorThenExpr

    static member Node(x: Expr) : Node =
        match x with
        | Lazy n -> n
        | Single n -> n
        | Constant n -> Constant.Node n
        | Null n -> n
        | Quote n -> n
        | Typed n -> n
        | New n -> n
        | Tuple n -> n
        | StructTuple n -> n
        | ArrayOrList n -> n
        | Record n -> n
        | InheritRecord n -> n
        | AnonStructRecord n -> n
        | ObjExpr n -> n
        | While n -> n
        | For n -> n
        | ForEach n -> n
        | NamedComputation n -> n
        | Computation n -> n
        | CompExprBody n -> n
        | JoinIn n -> n
        | ParenLambda n -> n
        | Lambda n -> n
        | MatchLambda n -> n
        | Match n -> n
        | TraitCall n -> n
        | ParenILEmbedded n -> n
        | ParenFunctionNameWithStar n -> n
        | Paren n -> n
        | Dynamic n -> n
        | PrefixApp n -> n
        | SameInfixApps n -> n
        | InfixApp n -> n
        | IndexWithoutDot n -> n
        | AppLongIdentAndSingleParenArg n -> n
        | AppSingleParenArg n -> n
        | AppWithLambda n -> n
        | NestedIndexWithoutDot n -> n
        | App n -> n
        | TypeApp n -> n
        | TryWithSingleClause n -> n
        | TryWith n -> n
        | TryFinally n -> n
        | IfThen n -> n
        | IfThenElse n -> n
        | IfThenElif n -> n
        | Ident n -> n
        | OptVar n -> n
        | LongIdentSet n -> n
        | DotIndexedGet n -> n
        | DotIndexedSet n -> n
        | NamedIndexedPropertySet n -> n
        | DotNamedIndexedPropertySet n -> n
        | Set n -> n
        | LibraryOnlyStaticOptimization n -> n
        | InterpolatedStringExpr n -> n
        | IndexRangeWildcard n -> n
        | TripleNumberIndexRange n -> n
        | IndexRange n -> n
        | IndexFromEnd n -> n
        | Typar n -> n
        | Chain n -> n
        | DotLambda n -> n
        | BeginEnd n -> n
        | ExplicitConstructorThenExpr n -> n

    member e.HasParentheses: bool =
        match e with
        | Expr.Paren _ -> true
        | _ -> false

type OpenModuleOrNamespaceNode(identListNode: IdentListNode, range) =
    inherit NodeBase(range)

    override val Children = Array.empty
    member val Name = identListNode

type OpenTargetNode(target: Type, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Type.Node target |]
    member val Target = target

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Open =
    | ModuleOrNamespace of OpenModuleOrNamespaceNode
    | Target of OpenTargetNode

    static member Node(x: Open) : Node =
        match x with
        | ModuleOrNamespace n -> n
        | Target n -> n

type OpenListNode(opens: Open list) =
    inherit NodeBase(List.map (Open.Node >> nodeRange) opens |> combineRanges)

    override val Children: Node array = [| yield! (List.map Open.Node opens) |]
    member val Opens = opens

type HashDirectiveListNode(hashDirectives: ParsedHashDirectiveNode list) =
    inherit NodeBase(hashDirectives |> List.map (fun n -> n.Range) |> combineRanges)

    override val Children: Node array = [| yield! nodes hashDirectives |]
    member val HashDirectives = hashDirectives

type AttributeNode(typeName: IdentListNode, expr: Expr option, target: SingleTextNode option, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield typeName; yield! noa (Option.map Expr.Node expr); yield! noa target |]

    member val TypeName = typeName
    member val Expr = expr
    member val Target = target

/// The content from [< to >]
type AttributeListNode
    (openingToken: SingleTextNode, attributesNodes: AttributeNode list, closingToken: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingToken; yield! nodes attributesNodes; yield closingToken |]

    member val Opening = openingToken
    member val Attributes = attributesNodes
    member val Closing = closingToken

type MultipleAttributeListNode(attributeLists: AttributeListNode list, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield! nodes attributeLists |]
    member val AttributeLists = attributeLists
    member val IsEmpty = attributeLists.IsEmpty

type ModuleDeclAttributesNode(attributes: MultipleAttributeListNode option, doExpr: Expr, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield! noa attributes; yield Expr.Node doExpr |]
    member val Attributes = attributes
    member val Expr = doExpr

type ExceptionDefnNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        accessibility: SingleTextNode option,
        unionCase: UnionCaseNode,
        withKeyword: SingleTextNode option,
        ms: MemberDefn list,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield! noa accessibility
           yield unionCase
           yield! noa withKeyword
           yield! nodes (List.map MemberDefn.Node ms) |]

    member val XmlDoc = xmlDoc
    member val Attributes = attributes
    member val Accessibility = accessibility
    member val UnionCase = unionCase
    member val WithKeyword = withKeyword
    member val Members = ms

type ExternBindingPatternNode
    (attributes: MultipleAttributeListNode option, t: Type option, pat: Pattern option, range: range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa attributes
           yield! noa (Option.map Type.Node t)
           yield! noa (Option.map Pattern.Node pat) |]

    member val Attributes = attributes
    member val Type = t
    member val Pattern = pat

type ExternBindingNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        externNode: SingleTextNode,
        attributesOfType: MultipleAttributeListNode option,
        t: Type,
        accessibility: SingleTextNode option,
        identifier: IdentListNode,
        openingParen: SingleTextNode,
        parameters: ExternBindingPatternNode list,
        closingParen: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield externNode
           yield! noa attributesOfType
           yield Type.Node t
           yield! noa accessibility
           yield identifier
           yield openingParen
           yield! nodes parameters
           yield closingParen |]

    member val XmlDoc = xmlDoc
    member val Attributes = attributes
    member val Extern = externNode
    member val AttributesOfType = attributesOfType
    member val Type = t
    member val Accessibility = accessibility
    member val Identifier = identifier
    member val OpeningParen = openingParen
    member val Parameters = parameters
    member val ClosingParen = closingParen

type ModuleAbbrevNode(moduleNode: SingleTextNode, name: SingleTextNode, alias: IdentListNode, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield moduleNode; yield name; yield alias |]
    member val Module = moduleNode
    member val Name = name
    member val Alias = alias

type NestedModuleNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        moduleKeyword: SingleTextNode,
        accessibility: SingleTextNode option,
        isRecursive: bool,
        identifier: IdentListNode,
        equalsNode: SingleTextNode,
        decls: ModuleDecl list,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield moduleKeyword
           yield! noa accessibility
           yield identifier
           yield equalsNode
           yield! List.map ModuleDecl.Node decls |]

    member val XmlDoc = xmlDoc
    member val Attributes = attributes
    member val Module = moduleKeyword
    member val Accessibility = accessibility
    member val IsRecursive = isRecursive
    member val Identifier = identifier
    member val Equals = equalsNode
    member val Declarations = decls

/// Each case in this DU should have a container node
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ModuleDecl =
    | OpenList of OpenListNode
    | HashDirectiveList of HashDirectiveListNode
    | Attributes of ModuleDeclAttributesNode
    | DeclExpr of Expr
    | Exception of ExceptionDefnNode
    | ExternBinding of ExternBindingNode
    | TopLevelBinding of BindingNode
    | ModuleAbbrev of ModuleAbbrevNode
    | NestedModule of NestedModuleNode
    | TypeDefn of TypeDefn
    | Val of ValNode

    static member Node(x: ModuleDecl) : Node =
        match x with
        | OpenList n -> n
        | HashDirectiveList n -> n
        | Attributes n -> n
        | DeclExpr e -> Expr.Node e
        | Exception n -> n
        | ExternBinding n -> n
        | TopLevelBinding n -> n
        | ModuleAbbrev n -> n
        | NestedModule n -> n
        | TypeDefn t -> TypeDefn.Node t
        | Val n -> n

type BindingReturnInfoNode(colon: SingleTextNode, t: Type, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield colon; yield Type.Node t |]
    member val Colon = colon
    member val Type = t

type BindingNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: MultipleTextsNode,
        isMutable: bool,
        inlineNode: SingleTextNode option,
        accessibility: SingleTextNode option,
        functionName: Choice<IdentListNode, Pattern>,
        genericTypeParameters: TyparDecls option,
        parameters: Pattern list,
        returnType: BindingReturnInfoNode option,
        equals: SingleTextNode,
        expr: Expr,
        range
    ) =
    inherit NodeBase(range)
    member val XmlDoc = xmlDoc
    member val Attributes = attributes
    member val LeadingKeyword = leadingKeyword
    member val IsMutable = isMutable
    member val Inline = inlineNode
    member val Accessibility = accessibility
    member val FunctionName = functionName
    member val GenericTypeParameters = genericTypeParameters
    member val Parameters = parameters
    member val ReturnType = returnType
    member val Equals = equals
    member val Expr = expr

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield leadingKeyword
           yield! noa inlineNode
           yield! noa accessibility
           yield
               match functionName with
               | Choice1Of2 n -> (n :> Node)
               | Choice2Of2 p -> Pattern.Node p
           yield! noa (Option.map TyparDecls.Node genericTypeParameters)
           yield! nodes (List.map Pattern.Node parameters)
           yield! noa returnType
           yield equals
           yield Expr.Node expr |]

type BindingListNode(bindings: BindingNode list, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield! nodes bindings |]
    member val Bindings = bindings

type FieldNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: MultipleTextsNode option,
        mutableKeyword: SingleTextNode option,
        accessibility: SingleTextNode option,
        name: SingleTextNode option,
        t: Type,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield! noa leadingKeyword
           yield! noa mutableKeyword
           yield! noa accessibility
           yield! noa name
           yield Type.Node t |]

    member val XmlDoc = xmlDoc
    member val Attributes = attributes
    member val LeadingKeyword = leadingKeyword
    member val MutableKeyword = mutableKeyword
    member val Accessibility = accessibility
    member val Name = name
    member val Type = t

type UnionCaseNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        bar: SingleTextNode option,
        identifier: SingleTextNode,
        fields: FieldNode list,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield! noa bar
           yield identifier
           yield! nodes fields |]

    member val XmlDoc = xmlDoc
    member val Attributes = attributes
    member val Bar = bar
    member val Identifier = identifier
    member val Fields = fields

type TypeNameNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: SingleTextNode,
        ao: SingleTextNode option,
        identifier: IdentListNode,
        typeParams: TyparDecls option,
        constraints: TypeConstraint list,
        implicitConstructor: ImplicitConstructorNode option,
        equalsToken: SingleTextNode option,
        withKeyword: SingleTextNode option,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield leadingKeyword
           yield! noa ao
           yield identifier
           yield! noa (Option.map TyparDecls.Node typeParams)
           yield! List.map TypeConstraint.Node constraints
           yield! noa implicitConstructor
           yield! noa equalsToken
           yield! noa withKeyword |]

    member val XmlDoc = xmlDoc
    member val Attributes = attributes
    member val IsFirstType = leadingKeyword.Text = "type"
    member val LeadingKeyword = leadingKeyword
    member val Accessibility = ao
    member val Identifier = identifier
    member val TypeParameters = typeParams
    member val Constraints = constraints
    member val ImplicitConstructor = implicitConstructor
    member val EqualsToken = equalsToken
    member val WithKeyword = withKeyword

type ITypeDefn =
    abstract member TypeName: TypeNameNode
    abstract member Members: MemberDefn list

type EnumCaseNode
    (
        xmlDoc: XmlDocNode option,
        bar: SingleTextNode option,
        attributes: MultipleAttributeListNode option,
        identifier: SingleTextNode,
        equals: SingleTextNode,
        constant: Expr,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa bar
           yield identifier
           yield equals
           yield Expr.Node constant |]

    member val XmlDoc = xmlDoc
    member val Bar = bar
    member val Attributes = attributes
    member val Identifier = identifier
    member val Equals = equals
    member val Constant = constant

type TypeDefnEnumNode(typeNameNode, enumCases: EnumCaseNode list, members: MemberDefn list, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield typeNameNode
           yield! nodes enumCases
           yield! nodes (List.map MemberDefn.Node members) |]

    member val EnumCases = enumCases

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = members

type TypeDefnUnionNode
    (typeNameNode, accessibility: SingleTextNode option, unionCases: UnionCaseNode list, members: MemberDefn list, range)
    =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield typeNameNode
           yield! noa accessibility
           yield! nodes unionCases
           yield! nodes (List.map MemberDefn.Node members) |]

    member val Accessibility = accessibility
    member val UnionCases = unionCases

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = members

type TypeDefnRecordNode
    (
        typeNameNode,
        accessibility: SingleTextNode option,
        openingBrace: SingleTextNode,
        fields: FieldNode list,
        closingBrace: SingleTextNode,
        members,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield typeNameNode
           yield! noa accessibility
           yield openingBrace
           yield! nodes fields
           yield closingBrace
           yield! nodes (List.map MemberDefn.Node members) |]

    member val Accessibility = accessibility
    member val OpeningBrace = openingBrace
    member val Fields = fields
    member val ClosingBrace = closingBrace

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = members

type TypeDefnAbbrevNode(typeNameNode, t: Type, members, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield typeNameNode
           yield Type.Node t
           yield! nodes (List.map MemberDefn.Node members) |]

    member val Type = t

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = members

type AsSelfIdentifierNode(asNode: SingleTextNode, self: SingleTextNode, range) =
    inherit NodeBase(range)
    override val Children = [| yield (asNode :> Node); yield self |]
    member val As = asNode
    member val Self = self

type ImplicitConstructorNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        accessibility: SingleTextNode option,
        pat: Pattern,
        self: AsSelfIdentifierNode option,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield! noa accessibility
           yield Pattern.Node pat
           yield! noa self |]

    member val XmlDoc = xmlDoc
    member val Attributes = attributes
    member val Accessibility = accessibility
    member val Pattern = pat
    member val Self = self

type TypeDefnExplicitBodyNode(kind: SingleTextNode, members: MemberDefn list, endNode: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield kind; yield! nodes (List.map MemberDefn.Node members); yield endNode |]

    member val Kind = kind
    member val Members = members
    member val End = endNode

type TypeDefnExplicitNode(typeNameNode, body: TypeDefnExplicitBodyNode, members, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield typeNameNode
           yield body
           yield! nodes (List.map MemberDefn.Node members) |]

    member val Body = body

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = members

type TypeDefnAugmentationNode(typeNameNode, members, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield typeNameNode; yield! (List.map MemberDefn.Node members) |]

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = members

type TypeDefnDelegateNode(typeNameNode, delegateNode: SingleTextNode, typeList: TypeFunsNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield typeNameNode; yield delegateNode; yield typeList |]

    member val DelegateNode = delegateNode
    member val TypeList = typeList

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = List.empty

type TypeDefnRegularNode(typeNameNode, members, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield typeNameNode; yield! List.map MemberDefn.Node members |]

    interface ITypeDefn with
        member val TypeName = typeNameNode
        member val Members = members

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TypeDefn =
    | Enum of TypeDefnEnumNode
    | Union of TypeDefnUnionNode
    | Record of TypeDefnRecordNode
    | None of TypeNameNode
    | Abbrev of TypeDefnAbbrevNode
    | Explicit of TypeDefnExplicitNode
    | Augmentation of TypeDefnAugmentationNode
    | Delegate of TypeDefnDelegateNode
    | Regular of TypeDefnRegularNode

    static member Node(x: TypeDefn) : Node =
        match x with
        | Enum n -> n
        | Union n -> n
        | Record n -> n
        | None n -> n
        | Abbrev n -> n
        | Explicit n -> n
        | Augmentation n -> n
        | Delegate n -> n
        | Regular n -> n

    static member TypeDefnNode(x: TypeDefn) : ITypeDefn =
        match x with
        | Enum n -> n
        | Union n -> n
        | Record n -> n
        | None n ->
            { new ITypeDefn with
                member _.TypeName = n
                member _.Members = [] }
        | Abbrev n -> n
        | Explicit n -> n
        | Augmentation n -> n
        | Delegate n -> n
        | Regular n -> n

type MemberDefnInheritNode(inheritKeyword: SingleTextNode, baseType: Type, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield inheritKeyword; yield Type.Node baseType |]

    member val Inherit = inheritKeyword
    member val BaseType = baseType

/// Secondary constructor
/// new (pat: type) = expr
type MemberDefnExplicitCtorNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        accessibility: SingleTextNode option,
        newKeyword: SingleTextNode,
        pat: Pattern,
        alias: SingleTextNode option,
        equals: SingleTextNode,
        expr: Expr,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield! noa accessibility
           yield newKeyword
           yield Pattern.Node pat
           yield! noa alias
           yield equals
           yield Expr.Node expr |]

    member val XmlDoc = xmlDoc
    member val Attributes = attributes
    member val Accessibility = accessibility
    member val New = newKeyword
    member val Pattern = pat
    member val Alias = alias
    member val Equals = equals
    member val Expr = expr

type MemberDefnInterfaceNode
    (interfaceNode: SingleTextNode, t: Type, withNode: SingleTextNode option, members: MemberDefn list, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield interfaceNode
           yield Type.Node t
           yield! noa withNode
           yield! List.map MemberDefn.Node members |]

    member val Interface = interfaceNode
    member val Type = t
    member val With = withNode
    member val Members = members

type MemberDefnAutoPropertyNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: MultipleTextsNode,
        accessibility: SingleTextNode option,
        identifier: SingleTextNode,
        t: Type option,
        equals: SingleTextNode,
        expr: Expr,
        withGetSet: MultipleTextsNode option,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield leadingKeyword
           yield! noa accessibility
           yield identifier
           yield! noa (Option.map Type.Node t)
           yield equals
           yield Expr.Node expr
           yield! noa withGetSet |]

    member val XmlDoc = xmlDoc
    member val Attributes = attributes
    member val LeadingKeyword = leadingKeyword
    member val Accessibility = accessibility
    member val Identifier = identifier
    member val Type = t
    member val Equals = equals
    member val Expr = expr
    member val WithGetSet = withGetSet

type MemberDefnAbstractSlotNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: MultipleTextsNode,
        identifier: SingleTextNode,
        typeParams: TyparDecls option,
        t: Type,
        withGetSet: MultipleTextsNode option,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield leadingKeyword
           yield identifier
           yield! noa (Option.map TyparDecls.Node typeParams)
           yield Type.Node t
           yield! noa withGetSet |]

    member val XmlDoc = xmlDoc
    member val Attributes = attributes
    member val LeadingKeyword = leadingKeyword
    member val Identifier = identifier
    member val TypeParams = typeParams
    member val Type = t
    member val WithGetSet = withGetSet

type PropertyGetSetBindingNode
    (
        inlineNode: SingleTextNode option,
        attributes: MultipleAttributeListNode option,
        accessibility: SingleTextNode option,
        leadingKeyword: SingleTextNode,
        parameters: Pattern list,
        returnType: BindingReturnInfoNode option,
        equals: SingleTextNode,
        expr: Expr,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa inlineNode
           yield! noa attributes
           yield! noa accessibility
           yield leadingKeyword
           yield! List.map Pattern.Node parameters
           yield! noa returnType
           yield equals
           yield Expr.Node expr |]

    member val Inline = inlineNode
    member val Attributes = attributes
    member val Accessibility = accessibility
    member val LeadingKeyword = leadingKeyword
    member val Parameters = parameters
    member val ReturnType = returnType
    member val Equals = equals
    member val Expr = expr

type MemberDefnPropertyGetSetNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: MultipleTextsNode,
        inlineNode: SingleTextNode option,
        accessibility: SingleTextNode option,
        memberName: IdentListNode,
        withKeyword: SingleTextNode,
        firstBinding: PropertyGetSetBindingNode,
        andKeyword: SingleTextNode option,
        lastBinding: PropertyGetSetBindingNode option,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield leadingKeyword
           yield! noa accessibility
           yield memberName
           yield withKeyword
           yield firstBinding
           yield! noa andKeyword
           yield! noa lastBinding |]

    member val XmlDoc = xmlDoc
    member val Attributes = attributes
    member val LeadingKeyword = leadingKeyword
    member val Inline = inlineNode
    member val Accessibility = accessibility
    member val MemberName = memberName
    member val WithKeyword = withKeyword
    member val FirstBinding = firstBinding
    member val AndKeyword = andKeyword
    member val LastBinding = lastBinding

type ValNode
    (
        xmlDoc: XmlDocNode option,
        attributes: MultipleAttributeListNode option,
        leadingKeyword: MultipleTextsNode option,
        inlineNode: SingleTextNode option,
        isMutable: bool,
        accessibility: SingleTextNode option,
        identifier: SingleTextNode,
        typeParams: TyparDecls option,
        t: Type,
        equals: SingleTextNode option,
        eo: Expr option,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa xmlDoc
           yield! noa attributes
           yield! noa leadingKeyword
           yield! noa accessibility
           yield identifier
           yield! noa (Option.map TyparDecls.Node typeParams)
           yield Type.Node t
           yield! noa equals
           yield! noa (Option.map Expr.Node eo) |]

    member val XmlDoc = xmlDoc
    member val Attributes = attributes
    member val LeadingKeyword = leadingKeyword
    member val Inline = inlineNode
    member val IsMutable = isMutable
    member val Accessibility = accessibility
    member val Identifier = identifier
    member val TypeParams = typeParams
    member val Type = t
    member val Equals = equals
    member val Expr = eo

type MemberDefnSigMemberNode(valNode: ValNode, withGetSet: MultipleTextsNode option, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield valNode; yield! noa withGetSet |]
    member val Val = valNode
    member val WithGetSet = withGetSet

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type MemberDefn =
    | ImplicitInherit of InheritConstructor
    | Inherit of MemberDefnInheritNode
    | ValField of FieldNode
    | Member of BindingNode
    | ExternBinding of ExternBindingNode
    | DoExpr of ExprSingleNode
    | LetBinding of BindingListNode
    | ExplicitCtor of MemberDefnExplicitCtorNode
    | Interface of MemberDefnInterfaceNode
    | AutoProperty of MemberDefnAutoPropertyNode
    | AbstractSlot of MemberDefnAbstractSlotNode
    | PropertyGetSet of MemberDefnPropertyGetSetNode
    | SigMember of MemberDefnSigMemberNode

    static member Node(md: MemberDefn) : Node =
        match md with
        | ImplicitInherit n -> InheritConstructor.Node n
        | Inherit n -> n
        | ValField n -> n
        | Member n -> n
        | ExternBinding n -> n
        | DoExpr n -> n
        | LetBinding n -> n
        | ExplicitCtor n -> n
        | Interface n -> n
        | AutoProperty n -> n
        | AbstractSlot n -> n
        | PropertyGetSet n -> n
        | SigMember n -> n

type UnitNode(openingParen: SingleTextNode, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield openingParen; yield closingParen |]
    member val OpeningParen = openingParen
    member val ClosingParen = closingParen

type ConstantMeasureNode(constant: Constant, measure: UnitOfMeasureNode, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Constant.Node constant; yield measure |]
    member val Constant = constant
    member val Measure = measure

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Constant =
    | FromText of SingleTextNode
    | Unit of UnitNode
    | Measure of ConstantMeasureNode

    static member Node(c: Constant) : NodeBase =
        match c with
        | FromText n -> n
        | Unit n -> n
        | Measure n -> n

type TyparDeclNode
    (
        attributes: MultipleAttributeListNode option,
        typar: SingleTextNode,
        intersectionConstraints: Choice<Type, SingleTextNode> list,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield! noa attributes
           yield typar
           yield!
               List.map
                   (function
                   | Choice1Of2 t -> Type.Node t
                   | Choice2Of2 amp -> amp :> Node)
                   intersectionConstraints |]

    member val Attributes = attributes
    member val TypeParameter = typar
    member val IntersectionConstraints = intersectionConstraints

type TyparDeclsPostfixListNode
    (
        lessThan: SingleTextNode,
        decls: TyparDeclNode list,
        constraints: TypeConstraint list,
        greaterThan: SingleTextNode,
        range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield lessThan
           yield! nodes decls
           yield! List.map TypeConstraint.Node constraints
           yield greaterThan |]

    member val LessThan = lessThan
    member val Decls = decls
    member val Constraints = constraints
    member val GreaterThan = greaterThan

type TyparDeclsPrefixListNode
    (openingParen: SingleTextNode, decls: TyparDeclNode list, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield openingParen; yield! nodes decls; yield closingParen |]
    member val OpeningParen = openingParen
    member val Decls = decls
    member val ClosingParen = closingParen

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TyparDecls =
    | PostfixList of TyparDeclsPostfixListNode
    | PrefixList of TyparDeclsPrefixListNode
    | SinglePrefix of TyparDeclNode

    static member Node(td: TyparDecls) : Node =
        match td with
        | PostfixList n -> n
        | PrefixList n -> n
        | SinglePrefix n -> n

type TypeConstraintSingleNode(typar: SingleTextNode, kind: SingleTextNode, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield typar; yield kind |]
    member val Typar = typar
    member val Kind = kind

type TypeConstraintDefaultsToTypeNode(defaultNode: SingleTextNode, typar: SingleTextNode, t: Type, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield defaultNode; yield typar; yield Type.Node t |]
    member val Default = defaultNode
    member val Typar = typar
    member val Type = t

type TypeConstraintSubtypeOfTypeNode(typar: SingleTextNode, t: Type, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield typar; yield Type.Node t |]
    member val Typar = typar
    member val Type = t

type TypeConstraintSupportsMemberNode(t: Type, memberSig: MemberDefn, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield Type.Node t |]
    member val Type = t
    member val MemberSig = memberSig

type TypeConstraintEnumOrDelegateNode(typar: SingleTextNode, verb: string, ts: Type list, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield typar; yield! List.map Type.Node ts |]
    member val Typar = typar
    member val Verb = verb
    member val Types = ts

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type TypeConstraint =
    | Single of TypeConstraintSingleNode
    | DefaultsToType of TypeConstraintDefaultsToTypeNode
    | SubtypeOfType of TypeConstraintSubtypeOfTypeNode
    | SupportsMember of TypeConstraintSupportsMemberNode
    | EnumOrDelegate of TypeConstraintEnumOrDelegateNode
    | WhereSelfConstrained of Type

    static member Node(tc: TypeConstraint) : Node =
        match tc with
        | Single n -> n
        | DefaultsToType n -> n
        | SubtypeOfType n -> n
        | SupportsMember n -> n
        | EnumOrDelegate n -> n
        | WhereSelfConstrained t -> Type.Node t

type UnitOfMeasureNode(lessThan: SingleTextNode, measure: Measure, greaterThan: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield lessThan; yield Measure.Node measure; yield greaterThan |]

    member val LessThan = lessThan
    member val Measure = measure
    member val GreaterThan = greaterThan

type MeasureOperatorNode(lhs: Measure, operator: SingleTextNode, rhs: Measure, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield Measure.Node lhs; yield operator; yield Measure.Node rhs |]

    member val LeftHandSide = lhs
    member val Operator = operator
    member val RightHandSide = rhs

type MeasureDivideNode(lhs: Measure option, operator: SingleTextNode, rhs: Measure, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| match lhs with
           | Some n -> yield Measure.Node n
           | None -> ()
           yield operator
           yield Measure.Node rhs |]

    member val LeftHandSide = lhs
    member val Operator = operator
    member val RightHandSide = rhs

type MeasurePowerNode(measure: Measure, caret: SingleTextNode, exponent: RationalConstNode, range) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield Measure.Node measure
           yield caret
           yield RationalConstNode.Node exponent |]

    member val Measure = measure
    member val Caret = caret
    member val Exponent = exponent

type MeasureSequenceNode(measures: Measure list, range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield! List.map Measure.Node measures |]
    member val Measures = measures

type MeasureParenNode(openingParen: SingleTextNode, measure: Measure, closingParen: SingleTextNode, range) =
    inherit NodeBase(range)

    override val Children: Node array = [| yield openingParen; yield Measure.Node measure; yield closingParen |]

    member val OpeningParen = openingParen
    member val Measure = measure
    member val ClosingParen = closingParen

type RationalNode
    (
        openingParen: SingleTextNode,
        numerator: SingleTextNode,
        divOp: SingleTextNode,
        denominator: SingleTextNode,
        closingParen: SingleTextNode,
        range: range
    ) =
    inherit NodeBase(range)

    override val Children: Node array =
        [| yield openingParen
           yield numerator
           yield divOp
           yield denominator
           yield closingParen |]

    member val OpeningParen = openingParen
    member val Numerator = numerator
    member val DivOp = divOp
    member val Denominator = denominator
    member val ClosingParen = closingParen

type NegateRationalNode(minus: SingleTextNode, rationalConst: RationalConstNode, range: range) =
    inherit NodeBase(range)
    override val Children: Node array = [| yield minus; yield RationalConstNode.Node rationalConst |]

    member val Minus = minus
    member val Rational = rationalConst

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type RationalConstNode =
    | Integer of SingleTextNode
    | Rational of RationalNode
    | Negate of NegateRationalNode

    static member Node(r: RationalConstNode) : Node =
        match r with
        | Integer n -> n
        | Rational n -> n
        | Negate n -> n

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type Measure =
    | Single of SingleTextNode
    | Operator of MeasureOperatorNode
    | Divide of MeasureDivideNode
    | Power of MeasurePowerNode
    | Multiple of IdentListNode
    | Seq of MeasureSequenceNode
    | Paren of MeasureParenNode

    static member Node(m: Measure) : Node =
        match m with
        | Single n -> n
        | Operator n -> n
        | Divide n -> n
        | Power n -> n
        | Multiple n -> n
        | Seq n -> n
        | Paren n -> n
