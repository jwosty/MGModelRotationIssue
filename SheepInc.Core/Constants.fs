namespace SheepInc.Core

open FSharp.TypedNumerics
open FSharp.TypedNumerics.Operators

/// Intrinsic values that could never conceivably vary.
module Constants =
    let forward = %(1.f, 0.f, 0.f)
    let backward = -forward
    let up = %(0.f, 0.f, 1.f)
    let down = -up
    let right = %(0.f, -1.f, 0.f)
    let left = -right

