namespace SheepInc.Core.Model

open System
open FSharp.Core.LanguagePrimitives
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open SheepInc.Core
open SheepInc.Core.HelperFunctions
open SheepInc.Core.Units

// NOTE: We are using a right-handed camera/world coordinate system, where X and Y lie on the horizontal ground plane.
// In other words: +X is right, +Y is forward, and +Z is up.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Camera =
    let defaultValue =
        {
            NearPlane = 1.f<wu>
            FarPlane = 50.f<wu>
            ZoomLevel = 0.f
            Pitch = -90.f<deg> + 60.f<deg>
            Yaw = 45.f<deg>
            MinZoomLevel = -1.f
            MaxZoomLevel =  3.f
            LookPosition = Vector3(0.f, 0.f, 2.0f)
            Mode = Isometric
        }
    
    let forward (camera: Camera) (cameraMatrices: CameraMatrices) =
        cameraMatrices.CameraModel.Forward
    
    let backward (camera: Camera) (cameraMatrices: CameraMatrices) =
        cameraMatrices.CameraModel.Backward
    
    let up (camera: Camera) (cameraMatrices: CameraMatrices) =
        cameraMatrices.CameraModel.Up
    
    let down (camera: Camera) (cameraMatrices: CameraMatrices) =
        cameraMatrices.CameraModel.Down
    
    let right (camera: Camera) (cameraMatrices: CameraMatrices) =
        cameraMatrices.CameraModel.Right
    
    let left (camera: Camera) (cameraMatrices: CameraMatrices) =
        cameraMatrices.CameraModel.Left
    
    let buildCameraMatrix (camera: Camera) =
        // distance from the camera to the look-at point
        let cameraDistance =
            match camera.Mode with
            | Isometric -> -20.f<wu>
            | Perspective -> -camera.ZoomAsPerspectiveDistance
        
        Matrix.CreateTranslation (0.f, 0.f, -cameraDistance * 1.f</wu>) *
        Matrix.CreateRotationX (degToRad (camera.Pitch + 90.f<deg>) * 1.f</rad>) *
        Matrix.CreateRotationZ (degToRad camera.Yaw * 1.f</rad>) *
        Matrix.CreateTranslation camera.LookPosition
    
    let update (deltaTime: float<s>) isDebugView (camera: Camera) : Camera = camera
    
    let buildMatrices (viewport: Viewport) (camera: Camera) =
        // TODO: this code scales based on square isometric tiles. We should figure out how to scale based on hexagonal
        // tiles instead.
        // -- old note --
        // We want our world scale to specify how wide, in screen pixels, one of our isometric tiles are. We need to
        // calculate the scale in terms of a square's side length (given the diagonal length). Here we simplified the
        // Pythagorean theorem for this case and plug it in
        // -- end old note --
        let worldScale = 128.f<wu> * camera.ZoomAsIsometricScaleFactor
        let sqDiag = worldScale / sqrt(2.f)
        
        let cameraMatrix = buildCameraMatrix camera
        
        // TODO: figure out why the lines aren't quite pixel-perfect (e.g. draw a square wireframe and observe that, at
        // least on Windows/DX, a corner pixel is often missing from one or more of the vertices)
        let viewMatrix = Matrix.Invert cameraMatrix
        
        let projectionMatrix =
            match camera.Mode with
            | Perspective ->
                Matrix.CreatePerspectiveFieldOfView(MathHelper.ToRadians(45.f), viewport.AspectRatio, camera.NearPlane * 1.f</wu>, camera.FarPlane * 1.f</wu>)
            | Isometric ->
                Matrix.CreateScale (sqDiag * 1.f</wu>, sqDiag * 1.f</wu>, 1.f) *
                Matrix.CreateOrthographic(float32 viewport.Width, float32 viewport.Height, camera.NearPlane * 1.f</wu>, camera.FarPlane * 1.f</wu>)
        
        CameraMatrices(cameraMatrix, viewMatrix, projectionMatrix)
