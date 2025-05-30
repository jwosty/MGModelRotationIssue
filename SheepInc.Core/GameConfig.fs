namespace SheepInc.Core

open FSharp.Data.UnitSystems.SI.UnitSymbols
open Microsoft.Xna.Framework
open SheepInc.Core
open SheepInc.Core.Units
open SheepInc.Core.Model

type GameConfig = {
    /// Defines render scale of the world (how many screen pixels per virtual pixel)
    WorldRenderScale: int
    /// Defines render scale of the UI (how many screen pixels per virtual pixel)
    HudRenderScale: int
}

module GameConfig =
    let heightLevelsPerTile = 5<ihu/hexWu>
    let invHeightLevelsPerTile = 1.f / float32 heightLevelsPerTile
    
    let defaultValue =
        {
            WorldRenderScale = 3
            HudRenderScale = 2
        }
