﻿module Fantomas.Tests.Ragnarok.SynBindingFunctionLongPatternExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config =
    { config with
          MultilineBlockBracketsOnSameColumn = true
          Ragnarok = true }

// TODO: conclude on what should happen here
// This one feels very weird to have `= {` because the pattern is already multiline

[<Test>]
let ``synbinding function with record`` () =
    formatSourceString
        false
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    {
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    }
"""

[<Test>]
let ``synbinding function with anonymous record`` () =
    formatSourceString
        false
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    {| A = longTypeName
       B = someOtherVariable
       C = ziggyBarX |}
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
"""

[<Test>]
let ``synbinding function with computation expression`` () =
    formatSourceString
        false
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    task {
        // some computation here
        ()
    }
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    task {
        // some computation here
        ()
    }
"""