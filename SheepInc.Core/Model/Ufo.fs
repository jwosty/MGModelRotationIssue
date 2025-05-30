namespace SheepInc.Core.Model

open System
open System.Collections.Generic
open FSharp.Data.UnitSystems.SI.UnitSymbols
open FSharp.TypedNumerics
open FSharp.TypedNumerics.Easing
open FSharp.TypedNumerics.Swizzling
open FSharp.TypedNumerics.Operators
open Microsoft.Xna.Framework.Input
open SheepInc.Core
open SheepInc.Core.HelperFunctions
open SheepInc.Core.Units

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Ufo =
    let init (rand: Random) (config: GameConfig) originalSheepPositions = { Position = %(0.f<wu>, 0.f<wu>, 3.f<wu>) }
        
    let update (rand: Random) (deltaTime: float<s>) (simDeltaTime: float<s>) (config: GameConfig) (camera: Camera) (ufo: Ufo) : Ufo = ufo
