namespace SheepInc.Core
open Microsoft.Xna.Framework

[<AutoOpen>]
module MatrixExtensions =
    type Matrix with
        /// Creates a matrix which swaps Y and Z coordinates
        static member CreateSwapYZ () =
            Matrix(
                M11 = 1.f,
                M23 = 1.f,
                M32 = 1.f,
                M44 = 1.f)
        
        /// Creates a matrix that transforms from a Z-up coordinate system to a Y-up coordinate system
        static member CreateZUpToYUp () = Matrix.CreateSwapYZ ()
        
        /// Creates a matrix that transforms from a Y-up coordinate system to a Z-up coordinate system
        static member CreateYUpToZUp () = Matrix.CreateSwapYZ ()
        
        /// Creates a matrix which performs scaling relative to a given origin, aka fixed point, as opposed to (0,0,0)
        static member CreateScaleOffCenter (origin: Vector3, scale: Vector3) =
            Matrix.CreateTranslation -origin *
            Matrix.CreateScale scale *
            Matrix.CreateTranslation origin
        
        
        /// Creates a matrix which performs scaling relative to a given origin, aka fixed point, as opposed to (0,0,0)
        static member CreateScaleOffCenter (origin: Vector3, scale: float32) =
            Matrix.CreateScaleOffCenter (origin, XnaVec3(scale, scale, scale))
        
        /// Creates a matrix which contains the rotation moment around the specific axis, around a given fixed point.
        static member CreateFromAxisAngleOffCenter (origin: Vector3, axis: XnaVec3, angle: float32) =
            Matrix.CreateTranslation -origin *
            Matrix.CreateFromAxisAngle (axis, angle) *
            Matrix.CreateTranslation origin
        
        static member CreateShearX (yShear: float32, zShear: float32) =
            Matrix(
                1.f,    yShear, zShear, 0.f,
                0.f,    1.f,    0.f,    0.f,
                0.f,    0.f,    1.f,    0.f,
                0.f,    0.f,    0.f,    1.f
            )
        
        static member CreateShearY (xShear: float32, zShear: float32) =
            Matrix(
                1.f,    0.f,    0.f,    0.f,
                xShear, 1.f,    zShear, 0.f,
                0.f,    0.f,    1.f,    0.f,
                0.f,    0.f,    0.f,    1.f
            )

        static member CreateShearZ (xShear: float32, yShear: float32) =
            Matrix(
                1.f,    0.f,    0.f,    0.f,
                0.f,    1.f,    0.f,    0.f,
                xShear, yShear, 1.f,    0.f,
                0.f,    0.f,    0.f,    1.f
            )
        