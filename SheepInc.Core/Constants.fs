namespace SheepInc.Core

open System.Numerics


/// Intrinsic values that could never conceivably vary.
module Constants =
    let forward = Vector3(1.f, 0.f, 0.f)
    let backward = -forward
    let up = Vector3(0.f, 0.f, 1.f)
    let down = -up
    let right = Vector3(0.f, -1.f, 0.f)
    let left = -right

