namespace SheepInc.Core.Model

open System
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Microsoft.Xna.Framework
open SheepInc.Core
open SheepInc.Core.Units
open SheepInc.Core.HelperFunctions

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module World =
    let init (rand: Random) (config: GameConfig) =
        {
            Camera = Camera.defaultValue
            Ufo = Ufo.init rand config Map.empty
        }
