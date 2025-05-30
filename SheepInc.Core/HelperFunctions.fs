module SheepInc.Core.HelperFunctions

open System
open System.Collections.Generic
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Microsoft.FSharp.Core.LanguagePrimitives
open Microsoft.Xna.Framework

open FSharp.TypedNumerics
open FSharp.TypedNumerics.Operators

open SheepInc.Core
open SheepInc.Core.Units

module AdhocNullable =
    let iter f  x : unit = match x with | null -> () | x -> f x
    let toOption (x: 'a) : 'a option = match x with | null -> None | x -> Some x
    let toNullable (x: 'a) : Nullable<'a> = match x with | null -> Nullable() | x -> Nullable(x)

/// Operator for AdhocNullable.iter
let (|>?) x f = AdhocNullable.iter f x
