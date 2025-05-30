namespace SheepInc.Core
open System
open System.Collections.Immutable
open System.Linq
open FSharp.Core.LanguagePrimitives
open SheepInc.Core.Units

[<AutoOpen>]
module Extensions =
    type Random with
        member this.NextBool () = this.NextSingle () < 0.5f
    
    type IImmutableDictionary<'TKey, 'TValue> with
        member this.Change (key: 'TKey, f: 'TValue option -> 'TValue option) =
            let x =
                match this.TryGetValue key with
                | true, x -> Some x
                | false, _ -> None
            match f x with
            | Some x' -> this.SetItem (key, x')
            | None -> this.Remove key
    
    type Microsoft.Xna.Framework.Graphics.Texture2D with
        member this.WidthM = Int32WithMeasure<px> this.Width
        member this.HeightM = Int32WithMeasure<px> this.Height
    
    type Microsoft.Xna.Framework.Graphics.Viewport with
        member this.XM = Int32WithMeasure<px> this.X
        member this.YM = Int32WithMeasure<px> this.Y
        member this.WidthM = Int32WithMeasure<px> this.Width
        member this.HeightM = Int32WithMeasure<px> this.Height
    
module ImmutableDictionary =
    let map (mapping: 'Key -> 'T -> 'U) (source: IImmutableDictionary<'Key, 'T>) : IImmutableDictionary<'Key, 'U> =
        let builder = ImmutableDictionary.CreateBuilder()
        for KeyValue(key, value) in source do
            builder.Add (key, mapping key value)
        builder.ToImmutable ()
