namespace SheepInc.Core.Model

open System
open System.Collections.Generic
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open SheepInc.Core
open SheepInc.Core.HelperFunctions
open SheepInc.Core.Units

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Ufo =
    let init (rand: Random) (config: GameConfig) originalSheepPositions = { Position = Vector3(0.f, 0.f, 3.f) }
        
    let update (rand: Random) (deltaTime: float<s>) (simDeltaTime: float<s>) (config: GameConfig) (camera: Camera) (ufo: Ufo) : Ufo = ufo
