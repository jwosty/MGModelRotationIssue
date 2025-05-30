namespace SheepInc.Core.Model

open System
open System.Collections.Generic
open FSharp.Data.UnitSystems.SI.UnitSymbols
open FSharp.Core.LanguagePrimitives
open FSharp.TypedNumerics
open FSharp.TypedNumerics.Operators
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open SheepInc.Core
open SheepInc.Core.Units

type CameraMode =
    /// Use an isometric/orthographic projection.
    | Isometric
    /// Use a perspective projection. Really just for debugging purposes.
    | Perspective

type Camera =
    {
        Mode: CameraMode
        NearPlane: float32<wu>
        FarPlane: float32<wu>
        Pitch: float32<deg>
        Yaw: float32<deg>
        /// Specifies how zoomed in the camera is. Bigger values = more zoomed in.
        /// In isometric view, 1 = true scale (i.e. 2d sprites should appear their true size)
        ZoomLevel: float32
        MinZoomLevel: float32
        MaxZoomLevel: float32
        LookPosition: Vector3f32<wu>
    }
    with
        member this.ZoomAsIsometricScaleFactor = 2.f ** (this.ZoomLevel - 1.f)
        member this.ZoomAsPerspectiveDistance = 2.f ** -(this.ZoomLevel - 4.f) |> Float32WithMeasure<wu>

type CameraMatrices private(cameraModel: XnaMatrix, view: XnaMatrix, projection: XnaMatrix, viewProjection: XnaMatrix, invView: XnaMatrix, invProjection: XnaMatrix, invViewProjection: XnaMatrix) =
    member this.CameraModel = cameraModel
    member this.View = view
    member this.Projection = projection
    member this.ViewProjection = viewProjection
    member this.InvView = invView
    member this.InvProjection = invProjection
    member this.InvViewProjection = invViewProjection
    
    new(cameraModel: XnaMatrix, view: XnaMatrix, projection: XnaMatrix) =
        let vp = view * projection
        CameraMatrices(cameraModel, view, projection, vp, XnaMatrix.Invert view, XnaMatrix.Invert projection, XnaMatrix.Invert vp)


type Ufo = { Position: Vector3f32<wu> }

type World = { Camera: Camera; Ufo: Ufo }
