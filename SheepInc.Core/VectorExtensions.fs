namespace SheepInc.Core

open System.ComponentModel
open System.Runtime.CompilerServices

open FSharp.TypedNumerics
open FSharp.TypedNumerics.Operators
open Microsoft.Xna.Framework

type XnaVec2 = Microsoft.Xna.Framework.Vector2
type XnaVec3 = Microsoft.Xna.Framework.Vector3
type XnaVec4 = Microsoft.Xna.Framework.Vector4
type XnaPoint = Microsoft.Xna.Framework.Point
type XnaMatrix = Microsoft.Xna.Framework.Matrix
type XnaQuaternion = Microsoft.Xna.Framework.Quaternion

type VectorExtensions =
    [<Extension>]
    static member ToXnaVec<[<Measure>] 'u> (vec: Vector2i32<'u>) = XnaVec2(float32 vec.X, float32 vec.Y)
    
    [<Extension>]
    static member ToXnaVec<[<Measure>] 'u> (vec: Vector2f32<'u>) = XnaVec2(float32 vec.X, float32 vec.Y)
    
    [<Extension>]
    static member ToXnaVec<[<Measure>] 'u> (vec: Vector2f<'u>) = XnaVec2(float32 vec.X, float32 vec.Y)
    
    [<Extension>]
    static member ToXnaVec<[<Measure>] 'u> (vec: Vector3i32<'u>) = XnaVec3(float32 vec.X, float32 vec.Y, float32 vec.Z)
    
    [<Extension>]
    static member ToXnaVec<[<Measure>] 'u> (vec: Vector3f32<'u>) = XnaVec3(float32 vec.X, float32 vec.Y, float32 vec.Z)
    
    [<Extension>]
    static member ToXnaVec<[<Measure>] 'u> (vec: Vector3f<'u>) = XnaVec3(float32 vec.X, float32 vec.Y, float32 vec.Z)

    [<Extension>]
    static member ToXnaVec<[<Measure>] 'u> (vec: Vector4i32<'u>) = XnaVec4(float32 vec.X, float32 vec.Y, float32 vec.Z, float32 vec.W)
    
    [<Extension>]
    static member ToXnaVec<[<Measure>] 'u> (vec: Vector4f32<'u>) = XnaVec4(float32 vec.X, float32 vec.Y, float32 vec.Z, float32 vec.W)
    
    [<Extension>]
    static member ToXnaVec<[<Measure>] 'u> (vec: Vector4f<'u>) = XnaVec4(float32 vec.X, float32 vec.Y, float32 vec.Z, float32 vec.W)
    
    [<Extension>]
    static member ToXnaPoint<[<Measure>] 'u> (vec: Vector2i32<'u>) = XnaPoint(int vec.X, int vec.Y)
    
    [<Extension>]
    static member ToXnaColor (vec: Vector3f32<1>) = Color (vec.X, vec.Y, vec.Z)
    
    [<Extension>]
    static member ToXnaColor (vec: Vector3f<1>) = Color (float32 vec.X, float32 vec.Y, float32 vec.Z)
    
    [<Extension>]
    static member ToXnaColor (vec: Vector4f32<1>) = Color (vec.X, vec.Y, vec.Z, vec.W)
    
    [<Extension>]
    static member ToXnaColor (vec: Vector4f<1>) = Color (float32 vec.X, float32 vec.Y, float32 vec.Z, float32 vec.W)
    
    [<Extension>]
    static member ToSize<[<Measure>] 'u> (vec: Vector2i32<'u>) = System.Drawing.Size(int32 vec.X, int32 vec.Y)
    
    [<Extension>]
    static member ToSize<[<Measure>] 'u> (vec: Vector2f32<'u>) = System.Drawing.Size(int32 vec.X, int32 vec.Y)
    
    [<Extension>]
    static member ToSize<[<Measure>] 'u> (vec: Vector2f<'u>) = System.Drawing.Size(int32 vec.X, int32 vec.Y)
    
    [<Extension>]
    static member ToPoint<[<Measure>] 'u> (vec: Vector2i32<'u>) = Point(int32 vec.X, int32 vec.Y)
    
    [<Extension>]
    static member ToPoint<[<Measure>] 'u> (vec: Vector2f32<'u>) = Point(int32 vec.X, int32 vec.Y)

    [<Extension>]
    static member ToPoint<[<Measure>] 'u> (vec: Vector2f<'u>) = Point(int32 vec.X, int32 vec.Y)


[<Sealed; AbstractClass; EditorBrowsable(EditorBrowsableState.Never)>]
type VecOps =
    static member OfXnaVec (vec: XnaVec2) = Vector2f32(vec.X, vec.Y)
    static member OfXnaVec (vec: XnaVec3) = Vector3f32(vec.X, vec.Y, vec.Z)
    static member OfXnaVec (vec: XnaVec4) = Vector4f32(vec.X, vec.Y, vec.Z, vec.W)
    
    static member OfXnaPoint (vec: XnaPoint) = Vector2i32(vec.X, vec.Y)
    
    static member Transform (vec: Vector2f32<1>, matrix: XnaMatrix) =
        let vec' = (XnaVec2.Transform (vec.ToXnaVec(), matrix))
        Vector2f32<1>(vec'.X, vec'.Y)
    static member Transform (vec: Vector2f<1>, matrix: XnaMatrix) =
        let vec' = XnaVec2.Transform (vec.ToXnaVec(), matrix)
        Vector2f<1>(float vec'.X, float vec'.Y)
    static member Transform (vec: Vector3f32<1>, matrix: XnaMatrix) =
        let vec' = (XnaVec3.Transform (vec.ToXnaVec(), matrix))
        Vector3f32<1>(vec'.X, vec'.Y, vec'.Z)
    static member Transform (vec: Vector3f<1>, matrix: XnaMatrix) =
        let vec' = XnaVec3.Transform (vec.ToXnaVec(), matrix)
        Vector3f<1>(float vec'.X, float vec'.Y, float vec'.Z)
    static member Transform (vec: Vector4f32<1>, matrix: XnaMatrix) =
        let vec' = (XnaVec4.Transform (vec.ToXnaVec(), matrix))
        Vector4f32<1>(vec'.X, vec'.Y, vec'.Z, vec'.W)
    static member Transform (vec: Vector4f<1>, matrix: XnaMatrix) =
        let vec' = XnaVec4.Transform (vec.ToXnaVec(), matrix)
        Vector4f<1>(float vec'.X, float vec'.Y, float vec'.Z, float vec'.W)
    
    static member Transform (vec: Vector2f32<1>, quaternion: XnaQuaternion) =
        let vec' = (XnaVec2.Transform (vec.ToXnaVec(), quaternion))
        Vector2f32<1>(vec'.X, vec'.Y)
    static member Transform (vec: Vector2f<1>, quaternion: XnaQuaternion) =
        let vec' = XnaVec2.Transform (vec.ToXnaVec(), quaternion)
        Vector2f<1>(float vec'.X, float vec'.Y)
    static member Transform (vec: Vector3f32<1>, quaternion: XnaQuaternion) =
        let vec' = (XnaVec3.Transform (vec.ToXnaVec(), quaternion))
        Vector3f32<1>(vec'.X, vec'.Y, vec'.Z)
    static member Transform (vec: Vector3f<1>, quaternion: XnaQuaternion) =
        let vec' = XnaVec3.Transform (vec.ToXnaVec(), quaternion)
        Vector3f<1>(float vec'.X, float vec'.Y, float vec'.Z)
    static member Transform (vec: Vector4f32<1>, quaternion: XnaQuaternion) =
        let vec' = (XnaVec4.Transform (vec.ToXnaVec(), quaternion))
        Vector4f32<1>(vec'.X, vec'.Y, vec'.Z, vec'.W)
    static member Transform (vec: Vector4f<1>, quaternion: XnaQuaternion) =
        let vec' = XnaVec4.Transform (vec.ToXnaVec(), quaternion)
        Vector4f<1>(float vec'.X, float vec'.Y, float vec'.Z, float vec'.W)


// [FS0064] This construct causes code to be less generic than indicated by its type annotations.
#nowarn "64"

module Vector =
    let inline ofXnaVec (x: ^a) : #IIntVector<_, _> =
        let _lemma: ^M->_ = id<VecOps>
        ((^M or ^a) : (static member OfXnaVec : _ -> _) x)
    
    let inline toXnaVec (x: ^a) : ^XnaVector =
        let _lemma: ^M->_ = id<VectorExtensions>
        ((^M or ^a) : (static member ToXnaVec : _ -> _) x)
    
    let inline ofXnaPoint (x: ^a) : #IIntVector<_, _> =
        let _lemma: ^M->_ = id<VecOps>
        ((^M or ^a) : (static member OfXnaPoint : _ -> _) x)
    
    let inline toXnaPoint (x: ^a) : ^XnaVector =
        let _lemma: ^M->_ = id<VectorExtensions>
        ((^M or ^a) : (static member ToXnaPoint : _ -> _) x)
    
    let ofXnaColor (x: Color) = x.ToVector4 () |> ofXnaVec
    
    let inline toXnaColor (x: ^a) : ^XnaVector =
        let _lemma: ^M->_ = id<VectorExtensions>
        ((^M or ^a) : (static member ToXnaColor : _ -> _) x)
    
    let inline toSize (x: ^a) : System.Drawing.Size =
        let _lemma: ^M->_ = id<VectorExtensions>
        ((^M or ^a) : (static member ToSize : _ -> _) x)
    
    let inline toPoint (x: ^a) : Point =
        let _lemma: ^M->_ = id<VectorExtensions>
        ((^M or ^a) : (static member ToPoint : _ -> _) x)
    
    /// <summary>
    ///     Returns a new vector containing the transformation of a given vector by a given vector transformer.
    /// </summary>
    /// <param name="vectorTransformer">
    ///     Something for which a Vector Transform operation is defined; namely: Matrices and Quaternions.
    /// </param>
    /// <param name="vec">A vector to which to apply the transformation.</param>
    let inline transform (vectorTransformer: ^VectorTransformer) (vec: ^Vector) : ^Vector =
        let _lemma: ^M->_ = id<VecOps>
        ((^M or ^VectorTransformer or ^Vector) : (static member Transform : _ * _ -> _) (vec, vectorTransformer))

// module Vector3 =
//     let inline ofXnaColor (c: Color) = c.ToVector3 () |> Vector.toXnaVec
//     // let inline toXnaColor (v: Color) = 
//
// module Vector4 =
//     let inline ofXnaColor (c: Color) = c.ToVector4 () |> Vector.toXnaVec
