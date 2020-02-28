namespace Fantomas

open FSharp.Compiler.Range
open Fantomas.TriviaTypes

[<RequireQualifiedAccess>]
module internal TriviaHelpers =
    let findByRange (trivia: TriviaNode list) (range: range) =
        trivia
        |> List.tryFind (fun t -> t.Range = range)

    let findFirstContentBeforeByRange (trivia: TriviaNode list) (range: range) =
        findByRange trivia range
        |> Option.bind (fun t -> t.ContentBefore |> List.tryHead)

    let ``has content after that matches``
        (findTrivia: TriviaNode -> bool)
        (contentAfter: TriviaContent -> bool)
        (trivia: TriviaNode list)
        =
        List.tryFind findTrivia trivia
        |> Option.map (fun t -> t.ContentAfter |> List.exists contentAfter)
        |> Option.defaultValue false

    let ``has content before that matches``
        (findTrivia: TriviaNode -> bool)
        (contentBefore: TriviaContent -> bool)
        (trivia: TriviaNode list)
        =
        List.tryFind findTrivia trivia
        |> Option.map (fun t -> t.ContentBefore |> List.exists contentBefore)
        |> Option.defaultValue false

    let ``has content after that ends with`` (findTrivia: TriviaNode -> bool) (contentAfterEnd: TriviaContent -> bool) (trivia: TriviaNode list) =
        List.tryFind findTrivia trivia
        |> Option.bind (fun t -> t.ContentAfter |> List.tryLast |> Option.map contentAfterEnd)
        |> Option.defaultValue false

    let ``is token of type`` tokenName (triviaNode: TriviaNode) =
        match triviaNode.Type with
        | Token({ TokenInfo = ti }) -> ti.TokenName = tokenName
        | _ -> false

    let ``keyword tokens inside range`` keywords range (trivia: TriviaNode list) =
        trivia
        |> List.choose(fun t ->
            match t.Type with
            | TriviaNodeType.Token({ TokenInfo = { TokenName = tn } as tok })
                when ( RangeHelpers.``range contains`` range t.Range && List.contains tn keywords) ->
                Some (tok, t)
            | _ -> None
        )

    let ``has line comment after`` triviaNode =
        triviaNode.ContentAfter
        |> List.filter(fun tn ->
            match tn with
            | Comment(LineCommentAfterSourceCode(_)) -> true
            | _ -> false
        )
        |> (List.isEmpty >> not)

    let ``has line comment before`` range triviaNodes =
        triviaNodes
        |> List.tryFind (fun tv -> tv.Range = range)
        |> Option.map (fun tv -> tv.ContentBefore |> List.exists (function | Comment(LineCommentOnSingleLine(_)) -> true | _ -> false))
        |> Option.defaultValue false

    let ``get CharContent`` range triviaNodes =
        triviaNodes
        |> List.tryFind (fun tv -> tv.Range = range)
        |> Option.bind (fun tv ->
            match tv.ContentItself with
            | Some(CharContent c) -> Some c
            | _ -> None)

    let ``has content itself is ident between ticks`` range (triviaNodes: TriviaNode list) =
        triviaNodes
        |> List.choose (fun tn ->
            match tn.Range = range, tn.ContentItself with
            | true, Some(IdentBetweenTicks(ident)) -> Some ident
            | _ -> None)
        |> List.tryHead