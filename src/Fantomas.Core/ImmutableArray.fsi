﻿module Fantomas.Core.ImmutableArray

open System.Collections.Generic
open System.Collections.Immutable

type ImmutableArrayBuilderCode<'T> = delegate of byref<ImmutableArray<'T>.Builder> -> unit

type ImmutableArrayViaBuilder<'T> =
    new: builder: ImmutableArray<'T>.Builder -> ImmutableArrayViaBuilder<'T>
    member inline Delay: (unit -> ImmutableArrayBuilderCode<'T>) -> ImmutableArrayBuilderCode<'T>
    member inline Zero: unit -> ImmutableArrayBuilderCode<'T>

    member inline Combine:
        ImmutableArrayBuilderCode<'T> * ImmutableArrayBuilderCode<'T> -> ImmutableArrayBuilderCode<'T>

    member inline While: (unit -> bool) * ImmutableArrayBuilderCode<'T> -> ImmutableArrayBuilderCode<'T>

    member inline TryWith:
        ImmutableArrayBuilderCode<'T> * (exn -> ImmutableArrayBuilderCode<'T>) -> ImmutableArrayBuilderCode<'T>

    member inline TryFinally: ImmutableArrayBuilderCode<'T> * (unit -> unit) -> ImmutableArrayBuilderCode<'T>
    member inline Using: 'a * ('a -> ImmutableArrayBuilderCode<'T>) -> ImmutableArrayBuilderCode<'T>
    member inline For: seq<'TElement> * ('TElement -> ImmutableArrayBuilderCode<'T>) -> ImmutableArrayBuilderCode<'T>
    member inline Yield: 'T -> ImmutableArrayBuilderCode<'T>
    member inline YieldFrom: IEnumerable<'T> -> ImmutableArrayBuilderCode<'T>
    member inline Run: ImmutableArrayBuilderCode<'T> -> ImmutableArray<'T>

val immarray<'T> : ImmutableArrayViaBuilder<'T>

val fixedImmarray<'T> : capacity: int -> ImmutableArrayViaBuilder<'T>

type immarray<'T> = ImmutableArray<'T>

[<RequireQualifiedAccess>]
module ImmutableArray =
    val empty<'T> : 'T immarray
    val singleton: item: 'T -> 'T immarray
    val ofSeq: xs: 'T seq -> 'T immarray
    val map: mapper: ('T -> 'U) -> 'T immarray -> 'U immarray
