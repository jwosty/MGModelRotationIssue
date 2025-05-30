namespace SheepInc.Core.Units

open FSharp.TypedNumerics.Units
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open System

/// Pixels
type [<Measure>] px
type [<Measure>] frame
type [<Measure>] ms
type [<Measure>] rad = FSharp.TypedNumerics.Units.rad
type [<Measure>] deg = FSharp.TypedNumerics.Units.deg
/// World units (cartesian)
type [<Measure>] wu
/// World units (hexagonal)
type [<Measure>] hexWu

/// Incremental-height-unit
type [<Measure>] ihu

module UnitConversions =
    let msPerS = 1000.<ms/s>
    let msPerSD = 1000.M<ms/s>
    let radPerDeg = ((float32 Math.PI) / 180.f) * 1.f<rad/deg>
