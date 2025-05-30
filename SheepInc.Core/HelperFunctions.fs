module SheepInc.Core.HelperFunctions

open System
open System.Collections.Generic
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Microsoft.FSharp.Core.LanguagePrimitives
open Microsoft.Xna.Framework

open SheepInc.Core
open SheepInc.Core.Units

module AdhocNullable =
    let iter f  x : unit = match x with | null -> () | x -> f x
    let toOption (x: 'a) : 'a option = match x with | null -> None | x -> Some x
    let toNullable (x: 'a) : Nullable<'a> = match x with | null -> Nullable() | x -> Nullable(x)

/// Operator for AdhocNullable.iter
let (|>?) x f = AdhocNullable.iter f x


module internal PreludeOperatorConstants =
    [<Literal>]
    let degPerRad = 180.<deg> / (Math.PI * 1.<rad>)
    
    [<Literal>]
    let degPerRadf32 = 180.f<deg> / (MathF.PI * 1.f<rad>)
    
    [<Literal>]
    let radPerDeg = (Math.PI * 1.<rad>) / 180.<deg>
    
    [<Literal>]
    let radPerDegf32 = (MathF.PI * 1.f<rad>) / 180.f<deg>

open PreludeOperatorConstants

type PreOps =
    static member RadiansToDegrees (r: float<rad>) : float<deg> = r * degPerRad
    static member RadiansToDegrees (r: float32<rad>) : float32<deg> = r * degPerRadf32
    
    static member DegreesToRadians (r: float<deg>) : float<rad> = r * radPerDeg
    static member DegreesToRadians (r: float32<deg>) : float32<rad> = r * radPerDegf32

[<AutoOpen>]
module PreludeOperators =
    
    let inline radToDeg (radians: ^radians) : ^degrees =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^radians) : (static member RadiansToDegrees : ^radians -> ^degrees) radians)

    let inline degToRad (degrees: ^degrees) : ^radians =
        let _lemma: ^M->_ = id<PreOps>
        ((^M or ^degrees) : (static member DegreesToRadians : ^degrees -> ^radians) degrees)
