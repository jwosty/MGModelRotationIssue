namespace SheepInc.Core

open Microsoft.Xna.Framework.Graphics

type Models = {
    Spaceship: Model
}

[<Struct>]
type SheepIncContent = {
    DefaultShader: BasicEffect
    PostProcessingShader: Effect
    Models: Models
}
