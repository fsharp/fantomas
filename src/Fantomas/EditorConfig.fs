module Fantomas.EditorConfig

open System.Collections.Generic
open System.ComponentModel
open EditorConfig.Core
open Fantomas.Core

module Reflection =
    open System
    open System.Reflection
    open FSharp.Reflection

    type FSharpRecordField =
        { PropertyName: string
          Category: string option
          DisplayName: string option
          Description: string option }

    let inline getCustomAttribute<'t, 'v when 't :> Attribute and 't: null>
        (projection: 't -> 'v)
        (property: PropertyInfo)
        : 'v option =
        property.GetCustomAttribute<'t>() |> Option.ofObj |> Option.map projection

    let inline getRecordFields x =
        let names =
            FSharpType.GetRecordFields(x.GetType())
            |> Seq.map (fun x ->
                { PropertyName = x.Name
                  Category = getCustomAttribute<CategoryAttribute, string> (fun a -> a.Category) x
                  DisplayName = getCustomAttribute<DisplayNameAttribute, string> (fun a -> a.DisplayName) x
                  Description = getCustomAttribute<DescriptionAttribute, string> (fun a -> a.Description) x })

        let values = FSharpValue.GetRecordFields x
        Seq.zip names values |> Seq.toArray

let supportedProperties =
    [ "max_line_length"; "indent_size"; "end_of_line"; "insert_final_newline" ]

let toEditorConfigName value =
    value
    |> Seq.map (fun c ->
        if System.Char.IsUpper(c) then
            $"_%s{c.ToString().ToLower()}"
        else
            c.ToString())
    |> String.concat ""
    |> fun s -> s.TrimStart([| '_' |])
    |> fun name ->
        if List.contains name supportedProperties then
            name
        else
            $"fsharp_%s{name}"

let getFantomasFields (fallbackConfig: FormatConfig) =
    Reflection.getRecordFields fallbackConfig
    |> Array.map (fun (recordField, defaultValue) ->
        let editorConfigName = toEditorConfigName recordField.PropertyName

        (editorConfigName, defaultValue))

let (|Number|_|) (d: string) =
    match System.Int32.TryParse(d) with
    | true, d -> Some(box d)
    | _ -> None

let (|PatternMatchStyle|_|) pms = PatternMatchStyle.OfConfigString pms

let (|MultilineFormatterType|_|) mft =
    MultilineFormatterType.OfConfigString mft

let (|BracketStyle|_|) bs = MultilineBracketStyle.OfConfigString bs

let (|EndOfLineStyle|_|) eol = EndOfLineStyle.OfConfigString eol

let (|Boolean|_|) b =
    if b = "true" then Some(box true)
    elif b = "false" then Some(box false)
    else None

let parseOptionsFromEditorConfig
    (fallbackConfig: FormatConfig)
    (editorConfigProperties: IReadOnlyDictionary<string, string>)
    : FormatConfig =
    getFantomasFields fallbackConfig
    |> Array.map (fun (editorConfigName, defaultValue) ->
        match editorConfigProperties.TryGetValue(editorConfigName) with
        | true, Number n -> n
        | true, Boolean b -> b
        | true, MultilineFormatterType mft -> box mft
        | true, PatternMatchStyle pms -> box pms
        | true, EndOfLineStyle eol -> box eol
        | true, BracketStyle bs -> box bs
        | _ -> defaultValue)
    |> fun newValues ->

        let formatConfigType = FormatConfig.Default.GetType()
        Microsoft.FSharp.Reflection.FSharpValue.MakeRecord(formatConfigType, newValues) :?> FormatConfig

let configToEditorConfig (config: FormatConfig) : string =
    Reflection.getRecordFields config
    |> Array.choose (fun (recordField, v) ->
        match v with
        | :? System.Boolean as b ->
            sprintf "%s=%s" (toEditorConfigName recordField.PropertyName) (if b then "true " else "false")
            |> Some
        | :? System.Int32 as i -> $"%s{toEditorConfigName recordField.PropertyName}=%d{i}" |> Some
        | :? MultilineFormatterType as mft ->
            $"%s{toEditorConfigName recordField.PropertyName}=%s{MultilineFormatterType.ToConfigString mft}"
            |> Some
        | :? PatternMatchStyle as pms ->
            $"%s{toEditorConfigName recordField.PropertyName}=%s{PatternMatchStyle.ToConfigString pms}"
            |> Some
        | :? EndOfLineStyle as eols ->
            $"%s{toEditorConfigName recordField.PropertyName}=%s{EndOfLineStyle.ToConfigString eols}"
            |> Some
        | _ -> None)
    |> String.concat "\n"

let editorConfigParser = EditorConfigParser(EditorConfigFileCache.GetOrCreate)

let tryReadConfiguration (fsharpFile: string) : FormatConfig option =
    let editorConfigSettings: FileConfiguration =
        editorConfigParser.Parse(fileName = fsharpFile)

    if editorConfigSettings.Properties.Count = 0 then
        None
    else
        Some(parseOptionsFromEditorConfig FormatConfig.Default editorConfigSettings.Properties)

let readConfiguration (fsharpFile: string) : FormatConfig =
    tryReadConfiguration fsharpFile |> Option.defaultValue FormatConfig.Default
