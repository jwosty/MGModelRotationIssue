namespace SheepInc.Core

open System
open System.IO
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

open SheepInc.Core
open SheepInc.Core.Model
open SheepInc.Core.Units
open SheepInc.Core.HelperFunctions

type GraphicsHelpers =
    static member DrawModel (model: Model, modelMatrix: Matrix, viewMatrix: Matrix, projectionMatrix: Matrix) =
        let boneMatrices = Array.zeroCreate model.Bones.Count
        model.CopyAbsoluteBoneTransformsTo boneMatrices
        
        for mesh in model.Meshes do
            for effect in mesh.Effects do
                let effect = effect :?> BasicEffect
                effect.EnableDefaultLighting ()
                effect.World <- mesh.ParentBone.ModelTransform * modelMatrix
                effect.View <- viewMatrix
                effect.Projection <- projectionMatrix
                // this is the actually important bit and why just using model.Draw doesn't work -- everything else
                // is just mimicking it as-is
                effect.Alpha <- 1.f
            
            mesh.Draw ()
        ()
    
[<Struct>]
type GraphicsResources = {
    Content: SheepIncContent option
    RenderTarget: RenderTarget2D
    HudRenderTarget: RenderTarget2D
}

type SheepInc() as this =
    inherit Game()
    let gdm = new GraphicsDeviceManager(this)
    let mutable sb: SpriteBatch = null
    let mutable graphicsResources: GraphicsResources option = None
    
    let rand = Random()
    let config = GameConfig.defaultValue
    let mutable world : World = Unchecked.defaultof<_>
    
    let disposables = new CompositeDisposable()
    
    do
        gdm.GraphicsProfile <- GraphicsProfile.HiDef
        this.Content.RootDirectory <- "Content"
        this.IsMouseVisible <- true
        this.Window.AllowUserResizing <- true
        gdm.PreferredBackBufferWidth <- 1280
        gdm.PreferredBackBufferHeight <- 720
        // gdm.GraphicsDevice.PresentationParameters.RenderTargetUsage <- RenderTargetUsage.PreserveContents
        // this.GraphicsDevice.PresentationParameters.RenderTargetUsage <- RenderTargetUsage.PreserveContents
        gdm.PreparingDeviceSettings |> Observable.add (fun e ->
            e.GraphicsDeviceInformation.PresentationParameters.RenderTargetUsage <- RenderTargetUsage.PreserveContents)
    
    override this.Initialize () =
        world <- World.init rand config
        
        base.Initialize ()
    
    member this.CreateRenderTarget (width, height) =
        new RenderTarget2D(
            this.GraphicsDevice,
            width,
            height,
            false,
            this.GraphicsDevice.PresentationParameters.BackBufferFormat,
            DepthFormat.Depth24
        )
    
    member this.CreateWorldRenderTarget () =
        this.CreateRenderTarget (this.GraphicsDevice.PresentationParameters.BackBufferWidth / config.WorldRenderScale,
                                 this.GraphicsDevice.PresentationParameters.BackBufferHeight / config.WorldRenderScale)
    
    member this.CreateHudRenderTarget () =
        this.CreateRenderTarget (this.GraphicsDevice.PresentationParameters.BackBufferWidth / config.HudRenderScale,
                                 this.GraphicsDevice.PresentationParameters.BackBufferHeight / config.HudRenderScale)
    
    override this.LoadContent() =
        sb <- new SpriteBatch(this.GraphicsDevice)
        
        base.LoadContent()
        
        let shadersRoot = "Shaders"
        let modelRoot = "Models"
        
        let content = {
            DefaultShader = new BasicEffect(this.GraphicsDevice)
            PostProcessingShader = this.Content.Load<Effect> (Path.Combine (shadersRoot, "PostProcessing"))
            Models = {
                Spaceship = this.Content.Load<Model> (Path.Combine (modelRoot, "Spaceship"))
            }
        }
        
        this.Window.ClientSizeChanged.Subscribe (fun _ ->
            let rt = this.CreateWorldRenderTarget ()
            let hrt = this.CreateHudRenderTarget ()
            
            match graphicsResources with
            | None -> graphicsResources <- Some { Content = None; RenderTarget = rt; HudRenderTarget = hrt }
            | Some gr ->
                let oldRt, oldHrt = gr.RenderTarget, gr.HudRenderTarget
                graphicsResources <- Some { gr with RenderTarget = rt; HudRenderTarget = hrt }
                oldRt.Dispose ()
                oldHrt.Dispose ()
        )
        |> disposables.Add
        
        graphicsResources <- Some {
            Content = Some content
            RenderTarget = this.CreateWorldRenderTarget ()
            HudRenderTarget = this.CreateHudRenderTarget ()
        }
    
    override this.Update gameTime =
        let deltaTime = gameTime.ElapsedGameTime.TotalSeconds * 1.<s>
        
        let graphicsResources = match graphicsResources with | Some gr -> gr | None -> failwith "Failed to update game. Graphics resources not yet initialized."
        let content = match graphicsResources.Content with | Some t -> t | None -> failwith "Failed to update1 game. Content not yet loaded."
        
        // update world
        
        base.Update gameTime
        
    member this.DrawUfo cullMode (content: SheepIncContent) (cameraMatrices: CameraMatrices) (camera: Camera) (ufo: Ufo) =
        // Draw the UFO
        // ooh spooky
        do
            let visModel = content.Models.Spaceship
            let modelMatrix = Matrix.CreateTranslation ufo.Position
            GraphicsHelpers.DrawModel (visModel, modelMatrix, cameraMatrices.View, cameraMatrices.Projection)
    
    member this.DrawWorld virtualViewport (content: SheepIncContent) (world: World) =
        let cameraMatrices = Camera.buildMatrices virtualViewport world.Camera
        
        let cullMode = CullMode.CullCounterClockwiseFace
        
        this.DrawUfo cullMode content cameraMatrices world.Camera world.Ufo
    
    member this.DrawWorldToRenderTarget (renderTarget: RenderTarget2D) (content: SheepIncContent) (world: World) =
        // Set up pipeline to render to a texture instead of directly to the screen (to make post-processing possible)
        this.GraphicsDevice.SetRenderTarget renderTarget
        
        let virtualViewport = Viewport(renderTarget.Bounds)
        
        this.GraphicsDevice.DepthStencilState <- DepthStencilState.Default
        this.GraphicsDevice.Clear Color.White
        
        this.DrawWorld virtualViewport content world
    
    member this.DrawScreen trueViewport graphicsResources content world =
        this.DrawWorldToRenderTarget graphicsResources.RenderTarget content world
        
        // -- Final scene rendering steps --
        
        // Drop the render target (i.e. render to the screen now)
        this.GraphicsDevice.SetRenderTarget null
#if DEBUG
        this.GraphicsDevice.Clear Color.CornflowerBlue
#endif
        
        do
            // Finally render the scene texture to the screen for real
            let effect = content.PostProcessingShader
            let wvpMatrix = Matrix.Identity
            effect.Parameters["WorldViewProj"] |>? (_.SetValue(wvpMatrix))
            effect.Parameters["Texture2"] |>? (_.SetValue(Unchecked.defaultof<Texture2D>))
            this.GraphicsDevice.SamplerStates[1] <- SamplerState.PointWrap
            effect.Parameters["HudTexture"] |>? (_.SetValue(graphicsResources.HudRenderTarget))
            this.GraphicsDevice.SamplerStates[2] <- SamplerState.PointWrap
            effect.Parameters["BlendFactor"] |>? (_.SetValue(float32 0.0f))
            let c = Color(1.0f, 1.0f, 1.0f, 1.0f)
            effect.Parameters["DiffuseColor"] |>? (_.SetValue(c.ToVector4()))
            sb.Begin (SpriteSortMode.Deferred, BlendState.NonPremultiplied, SamplerState.PointClamp, DepthStencilState.DepthRead, RasterizerState.CullNone, effect)
            sb.Draw (graphicsResources.RenderTarget, Rectangle(0, 0, this.GraphicsDevice.PresentationParameters.BackBufferWidth, this.GraphicsDevice.PresentationParameters.BackBufferHeight), Color.White)
            sb.End ()
            
    override this.Draw(gameTime) =
        let graphicsResources = match graphicsResources with | Some gr -> gr | None -> failwith "Failed to render game. Graphics resources not yet initialized."
        let content = match graphicsResources.Content with | Some t -> t | None -> failwith "Failed to render game. Content not yet loaded."
        
        let trueViewport = this.GraphicsDevice.Viewport
        
        this.DrawScreen trueViewport graphicsResources content world
        
        base.Draw gameTime
    
    override this.Dispose disposing =
        disposables.Dispose ()
        base.Dispose disposing
