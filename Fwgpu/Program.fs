module Program

open System
open System.Runtime.InteropServices
open System.Text
open System.Threading.Tasks
// open Dootverse.WebGPU.Shaders
open Wgsl
// open Raycast.Compute.ComputeShaders.Shaders
open Silk.NET.Input
open Microsoft.FSharp.NativeInterop
open Silk.NET.Core.Native
open Silk.NET.Maths
open Silk.NET.WebGPU
open Silk.NET.Windowing
open System.Collections.Generic

// let mutable shaderModule = Unchecked.defaultof<nativeptr<ShaderModule>>
let mutable posX = 4f
let mutable posZ = 0f
let mutable posY = -11f
let keys = Dictionary()

keys[int Key.Space] <- false
keys[int Key.ShiftLeft] <- false
for i in 1..100 do
    keys[i] <- false

let binding0Size = 20uL

// [<AbstractClass>]
type WebGpuWin(window: IWindow, bindings: ShaderWithBindings) as this =
    let mutable windowWidth = 1920
    let mutable windowHeight = 1080
    let wgpu = bindings.Info.wgpu
    // let wgpu = wgpu
    let surface = wgpu.Surface

    let surfaceCapabilities =
        let mutable result = Operators.Unchecked.defaultof<_>
        wgpu.SurfaceGetCapabilities(surface, wgpu.Adapter, &result)
        result

    let shaderModule =
        wgpu.CreateShader(wgpu.Device.Device, bindings.Info.code)

    let groups =
        wgpu.InitBindings
            ShaderStage.Fragment
            wgpu.Device.Device
            bindings.Buffers

    let swap () =
        // let mutable surfaceConfig = Operators.Unchecked.defaultof<_>
        // Create swap
        let mutable surfaceConfig =
            SurfaceConfiguration(
                Usage = TextureUsage.RenderAttachment,
                Format = NativePtr.read surfaceCapabilities.Formats,
                PresentMode = PresentMode.Fifo,
                Device = wgpu.Device.Device,
                Width = uint window.FramebufferSize.X,
                Height = uint window.FramebufferSize.Y
            )

        wgpu.SurfaceConfigure(surface, &surfaceConfig)

    let renderPipeline =
        let layout =
            wgpu.CreatePipelineLayout(
                wgpu.Device.Device,
                [| groups.layout |]
            )

        let result =
            Init.createRender
                wgpu
                wgpu.Device.Device
                surfaceCapabilities
                layout
                shaderModule

        swap()
        result

    do
        window.add_Closing(fun () ->
            wgpu.ShaderModuleRelease(shaderModule)
            wgpu.RenderPipelineRelease(renderPipeline)
            wgpu.DeviceRelease(wgpu.Device.Device)
            wgpu.AdapterRelease(wgpu.Adapter)
            wgpu.SurfaceRelease(surface)
            wgpu.InstanceRelease(wgpu.Instance)
            wgpu.Dispose()
        )

    let onFramebufferResize (size: Vector2D<int>) =
        windowWidth <- size.X
        windowHeight <- size.Y
        swap()

    do window.add_FramebufferResize onFramebufferResize

    do
        let input = window.CreateInput()

        let onKeyDown (keyboard: IKeyboard) (key: Key) (code: int) =
            keys[int key] <- true

        let onKeyUp keyboard key code = keys[int key] <- false
        let onMouseMove mouse movement = ()
        let onMouseDown mouse button = ()
        let onMouseUp mouse button = ()
        input.Keyboards |> Seq.iter(fun k -> k.add_KeyDown onKeyDown)
        input.Keyboards |> Seq.iter(fun k -> k.add_KeyUp onKeyUp)
        input.Mice |> Seq.iter(fun m -> m.add_MouseMove onMouseMove)
        input.Mice |> Seq.iter(fun m -> m.add_MouseDown onMouseDown)
        input.Mice |> Seq.iter(fun m -> m.add_MouseUp onMouseDown)

    member this.SurfaceCapabilities = surfaceCapabilities
    member this.Surface = surface
    member this.RenderPipeline = renderPipeline
    member this.Window = window

    member this.onRender value =
        window.add_Render(fun t ->
            let mutable texture = SurfaceTexture()
            wgpu.SurfaceGetCurrentTexture(surface, &&texture)

            match texture.Status with
            | SurfaceGetCurrentTextureStatus.Timeout
            | SurfaceGetCurrentTextureStatus.Lost
            | SurfaceGetCurrentTextureStatus.Outdated ->
                wgpu.TextureRelease(texture.Texture)
                swap()
            | SurfaceGetCurrentTextureStatus.OutOfMemory
            | SurfaceGetCurrentTextureStatus.DeviceLost
            | SurfaceGetCurrentTextureStatus.Force32 -> failwith "Error"
            | SurfaceGetCurrentTextureStatus.Success -> ()
            | _ -> ()

            let view =
                wgpu.TextureCreateView(
                    texture.Texture,
                    Unchecked.defaultof<nativeptr<_>>
                )

            value view t
            wgpu.TextureViewRelease(view)
            wgpu.TextureRelease(texture.Texture)
        )


    member this.Groups = groups
    member this.BindGroup = groups.bindGroup
    // let renderPipeline = this.Init
    member this.Wgpu = wgpu
// abstract member Init : nativeptr<RenderPipeline>
// let vbLayout = VertexBufferLayout(
//
// )
// type yo' (window: IWindow, wgpu: WebGPU', shaderCode, shader) as this =
open type Wgsl
// open Raycast.Compute.ComputeShaders
open System.Diagnostics
type Config = { t: float32; gridSize: int }
    
type Result =
    {
        nextPos: vec3f
        voxel: vec3f
        offsetVoxel: vec3f
        mask: vec3f
    }
type Output = {
    [<Location(0)>]
    xy: vec2<float32>
    [<BuiltIn(Builtin'.position)>]
    position: vec4<float32>
}
type Id = { id: int; Ptr: int }
[<ReflectedDefinition>]
type Shader(config: Config, camera: vec3f, voxelGrid: uint[], hashes: int[], voxelMap: Id[]) =
    let toIndex (v: vec3f) =
        let q = floor v
        let x = int q.x
        let y = int q.y
        let z = int q.z
        x + (y * config.gridSize) + (z * config.gridSize * config.gridSize)
        
    let containsVoxel pos =
        let i = toIndex pos
        let arrayIndex = uint i / 32u
        let arrayOffset = uint (i % 32)
        let n = voxelGrid[int arrayIndex]
        
        // (n <<< int arrayOffset) >>> 31
        (n &&& (1u <<< (31 - int arrayOffset))) >>> 31 - int arrayOffset
        
        // let index = floor(float32 i / 32f)
        // let offset = i - (int index * 32)
        // let n = voxelGrid[int index]
        // (n <<< offset) >>> 31
        // extractBits(n, arrayOffset, 1u)
        // let dbg = vec3(0f, 11f, 8f)
        // let p = toIndex dbg
        // if pos.x < 1f && pos.y >= 11f && pos.y <= 12f then
            // 1u
        // if i = p then
        //     1u
        // else
    let containsVoxel2 pos =
        let i = toIndex pos
        voxelMap[i]
        
    let fastDda (rayPos: vec3f) (rayDir: vec3f) =
        let mapPos = floor rayPos
        let quantized = sign(rayDir)
        let bGreater = greaterThan(quantized, vec3(0f))
        let bLess = lessThan(quantized, vec3(0f))

        let greater =
            vec3(
                float32(bGreater.x),
                float32(bGreater.y),
                float32(bGreater.z)
            )

        let lesser =
            vec3(float32(bLess.x), float32(bLess.y), float32(bLess.z))

        let nextVoxel =
            (floor(quantized + rayPos) * greater)
            + (ceil(quantized + rayPos) * lesser)

        let mutable distancePerSide = abs(nextVoxel - rayPos)
        let maxVal = 1000000000f
        let mutable rayLengthPerSide = length(rayDir) / abs(rayDir)

        if rayLengthPerSide.x = 1f / 0f then
            rayLengthPerSide.x <- maxVal
            distancePerSide.x <- maxVal

        if rayLengthPerSide.y = 1f / 0f then
            rayLengthPerSide.y <- maxVal
            distancePerSide.y <- maxVal

        if rayLengthPerSide.z = 1f / 0f then
            rayLengthPerSide.z <- maxVal
            distancePerSide.z <- maxVal

        let sidePerLength = abs(rayDir) / length(rayDir)
        let scaledPerSide = distancePerSide * rayLengthPerSide

        let mask =
            lessThanEqual(
                scaledPerSide,
                min(scaledPerSide.yzx, scaledPerSide.zxy)
            )

        let maskf = vec3(float32(mask.x), float32(mask.y), float32(mask.z))

        let intermediateResult = scaledPerSide * maskf
        // todo : max
        let toTravel =
            max(
                intermediateResult.x,
                max(intermediateResult.y, intermediateResult.z)
            )

        let offset = toTravel * sidePerLength * sign(rayDir)
        let result = rayPos + offset
        // let offsetVoxel = floor result - mapPos
        // let offsetVoxel = maskf * quantized
        // todo why does this work but mapPos + maskf * quantized doesn't
        let tinyOffset = maskf * rayDir * 0.001f
        let offsetVoxel = floor (result + tinyOffset)
        // let offsetVoxel = mapPos + maskf * quantized

        {
            nextPos = result
            voxel = floor result
            offsetVoxel = offsetVoxel
            mask = maskf
        }
        
    let Modf (f: float32) =
        f - float32 (int32 f)
        // let mutable remainder = f 
    [<Fragment; Location 0>]
    member this.fragment(vertexOutput: Output) =
        // let rgb = (vec3(0f, vertexOutput.xy.x, vertexOutput.xy.y) + vec3(2f)) * 0.5f - 1f
        // vec4(rgb, 1f)
        // vec4(0.05f, vertexOutput.xy.x, vertexOutput.xy.y, 1f)

        // let rgb = vec3(0f, vertexOutput.xy.x, vertexOutput.xy.y) + vec3(0f, 1f, 1f)
        // let rgbS = rgb * 0.5f
        // vec4(rgbS, 1f)

        let scale = 1f
        let pixel = scale * vec3(vertexOutput.xy.x * 4f / 3f, vertexOutput.xy.y, 2f)
        // let pos = (pixel * 32f) + camera + vec3(0f, 0f, -80f) // + vec3(0f, 0f, -64f)
        let dir = normalize(pixel) // + vec3(0f, 0f, scale))
        let pos = pixel + camera
        // let dir = vec3(vertexOutput.xy.x, vertexOutput.xy.y, 0.5f)
        // let dir = normalize(pixel + vec3(0f, 0f, 1f)) * 0.04f
        // let dir = normalize(pos - camera + vec3(0f, 0f, 40f))
        let mutable p = pos // - vec3(0f, 0f, 1f)
        let normalDir = normalize(dir)
        let mutable voxel = floor p
        let mutable foundVoxel = containsVoxel voxel
        let mutable i = 0
        let mutable mask = vec3(0f, 0f, 0f)
        while i < 700 && foundVoxel = 0u do
            let result = fastDda p dir
            p <- result.nextPos
            voxel <- result.offsetVoxel // * 0.125f
            mask <- result.mask
            foundVoxel <- containsVoxel voxel
            i <- i + 1
        if foundVoxel = 1u then
            let index = toIndex voxel
            let hash = index % 1000
            let Ptr = hashes[hash]
            let mutable offset = 1
            let maxOffset = voxelMap[Ptr].Ptr
            let mutable id = voxelMap[Ptr + offset].id
            while offset <= maxOffset && id <> index do
                offset <- offset + 1
                id <- voxelMap[Ptr + offset].id
            if id = index then
                // vec4(0f, float32 offset / 20f, float32 offset / 40f, 1f)
                let normal = mask * sign(dir) * vec3(-1f)
                let result = normalize(cross(normal, normalDir) + p)
                let sunlight = 1f //dot(normal, vec3(1f, 1f, -2f))
                let rgb = vec3((cos(3.14f * result.x + config.t) + 2f) * 0.5f, (cos(3.14f * 3f * result.y + config.t) + 2f) * 0.5f, (cos(3.14f * 8.2f * result.z + config.t) + 2f) * 0.5f)
                vec4(sunlight * rgb, 1f)
            else
                let normal = mask * sign(dir) * vec3(-1f)
                let result = normalize(cross(normal, normalDir) + p)
                // vec4(abs(result), 1f)
                vec4(0f)
            // vec4(0f, dir.x + 0.5f, dir.y + 0.5f, 1f)
        else
            vec4(0f)
        
    [<Wgsl.Vertex>]
    member this.vertex([<BuiltIn(Builtin'.vertex_index)>] index: uint) =
        let pos = [|
            vec2(-1f, 1f)
            vec2(-1f, -1f)
            vec2(1f, -1f)

            vec2(1f, 1f)
            vec2(-1f, 1f)
            vec2(1f, -1f)
        |]

        let n = int index

        {
            xy = vec2(pos[n].x, pos[n].y)
            position = vec4(pos[n], 0f, 1f)
        }

let sw = Stopwatch()
let mutable frame = 0
sw.Start()

let init (window: IWindow) =
    // let gridSize = 200
    // let r = Random()
    // for i in 1..10000 do
    //     mapVoxels[r.NextInt64 mapVoxels.Length |> int] <-
    //         Wgsl.vec4(r.NextSingle(), r.NextSingle(), r.NextSingle(), 1f)
    let wgpu = new WebGPU'(window)
    let state = wgpu.CreateBinder Shader
    let mutable cfg = { gridSize = 400; t = 0f }
    let mapVoxels = Array.zeroCreate(cfg.gridSize * cfg.gridSize * cfg.gridSize)
    mapVoxels[0 + (11 * cfg.gridSize) + (8 * cfg.gridSize * cfg.gridSize)] <- Wgsl.vec4(0f, 1f, 1f, 1f)
    mapVoxels[8 + (11 * cfg.gridSize) + (8 * cfg.gridSize * cfg.gridSize)] <- Wgsl.vec4(0f, 1f, 1f, 1f)
    mapVoxels[4 + (15 * cfg.gridSize) + (8 * cfg.gridSize * cfg.gridSize)] <- Wgsl.vec4(0f, 1f, 1f, 1f)
    mapVoxels[4 + (7 * cfg.gridSize) + (8 * cfg.gridSize * cfg.gridSize)] <- Wgsl.vec4(0f, 1f, 1f, 1f)
    let temp = [||]
    let dimensions = if mapVoxels.Length % 32 = 0 then mapVoxels.Length / 32 else mapVoxels.Length / 32 + 1
    let compressedMap: uint[] =
        // Array.zeroCreate (dimensions * dimensions * dimensions)
        Array.zeroCreate dimensions
    let containsVoxel i =
        let arrayIndex = i / 32
        let arrayOffset = i % 32
        let n = compressedMap[arrayIndex]
        (n <<< arrayOffset) >>> 31
    let makeData (voxels: ((int * int * int) * int) seq) =
        let compressedMap: uint[] =
            Array.zeroCreate dimensions
        let hashVoxels = Dictionary()
        let hashes = Array.zeroCreate 1000
        for ((x, y, z), data) in voxels do
            let index = x + (y * cfg.gridSize) + (z * cfg.gridSize * cfg.gridSize)
            let i = index / 32
            let o = index % 32
            let value = compressedMap[int i]
            let updated = value ||| (1u <<< (31 - o))
            compressedMap[int i] <- updated
            if not (hashVoxels.ContainsKey (index % 1000)) then
                hashVoxels[index % 1000] <- ResizeArray()
            hashVoxels[index % 1000].Add { id = index; Ptr = data }
        let voxelMap = ResizeArray()
        for i in 0..1000 - 1 do
            let voxels = if hashVoxels.ContainsKey i then hashVoxels[i] else ResizeArray()
            hashes[i] <- voxelMap.Count
            voxelMap.Add { id = i; Ptr = voxels.Count }
            if hashVoxels.ContainsKey i then
                for voxel in hashVoxels[i] do
                    voxelMap.Add voxel
        {| compressed = compressedMap; hashes = hashes; voxelMap = voxelMap.ToArray() |}
        
    let r = Random()
    let mapData = makeData [
        (0, 0, 0), 420
        for i in 1..10_000_00 do
            (r.NextInt64 (int64 cfg.gridSize) |> int, r.NextInt64 (int64 cfg.gridSize) |> int, r.NextInt64 (int64 cfg.gridSize) |> int), 1
    ]
        
        
    for i in 0..mapVoxels.Length - 1 do
        let index = i / 32
        let offset = i % 32
        let value = compressedMap[index]
        let bit = if mapVoxels[i].w = 0f then 0u else 1u
        let bit = if r.NextSingle() < 0.07f then 1u else 0u
        let updated = value ||| (bit <<< (31 - offset))
        compressedMap[index] <- updated
        let result = containsVoxel i
        if result <> bit then
            printfn $"error"
    let (config, state) = Wgpu.Bind state
    let (camera, state) = Wgpu.Bind state
    let (voxelGrid, state) = Wgpu.Bind state mapData.compressed.Length
    let (hashesBuffer, state) = Wgpu.Bind state mapData.hashes.Length
    // let (voxelMap, state) = Wgpu.Bind state mapVoxels.Length
    let (voxelMap, state) = Wgpu.Bind state mapData.voxelMap.Length
    
    // let cpuShader = Shader(cfg, compressedMap, [||])
    // let result = cpuShader.fragment({ xy = { x = -0.7f; y = 0f }; position = Operators.Unchecked.defaultof<_> })
        
    let mutable wroteMap = false
    let win = WebGpuWin(window, state)
    let wgpu = win.Wgpu
    printfn $"{state.Info.code}"

    win.onRender (fun view t ->
        // sw.Stop()
        sw.Start()
        // time <- time + t
        cfg <- { cfg with t = cfg.t + float32 t }

        let speed = 20f
        if keys[int Key.A] then
            posX <- posX - speed * float32 t

        if keys[int Key.D] then
            posX <- posX + speed * float32 t

        if keys[int Key.W] then
            posZ <- posZ + speed * float32 t

        if keys[int Key.S] then
            posZ <- posZ - speed * float32 t
            
        if keys[int Key.Space] then
            posY <- posY + speed * float32 t
            
        if keys[int Key.ShiftLeft] then
            posY <- posY - speed * float32 t

        let colorAttachment =
            RenderPassColorAttachment(
                View = view,
                ResolveTarget = Unchecked.defaultof<_>,
                LoadOp = LoadOp.Clear,
                StoreOp = StoreOp.Store,
                ClearValue = Color(0, 1, 0, 1)
            )

        let encoder = wgpu.Device.CreateCommandEncoder()
        let queue = wgpu.Device.GetQueue()
        let renderPass = encoder.StartRenderPass'(colorAttachment)
        renderPass.SetPipeline win.RenderPipeline
        renderPass.SetBindGroup win.BindGroup 0u

        config.Write(wgpu, queue, cfg)
        camera.Write(wgpu, queue, Wgsl.vec3(posX, posY, posZ))
        // cfg.Write (wgpu, queue, { config with cameraX = posX; cameraY = posY; cameraZ = posZ })
        if not wroteMap then
            voxelGrid.Write(wgpu, queue, 0uL, mapData.compressed)
            hashesBuffer.Write(wgpu, queue, 0uL, mapData.hashes)
            voxelMap.Write(wgpu, queue, 0uL, mapData.voxelMap)
            // voxelMap.Write(wgpu, queue, 0uL, mapVoxels |> Array.map (fun v -> if v.w = 0f then 0u else 1u))
            wroteMap <- true
            
            // hashes.Write(wgpu, queue, 0uL, map.hashes)
            // ids.Write(wgpu, queue, 0uL, map.ids)
            // objects.Write(wgpu, queue, 0uL, map.objects)
            // shapes.Write(wgpu, queue, 0uL, map.shapes)
        // voxels.Write(wgpu, queue, 0uL, [| 1; 2; 3; 4; 5; 6; 7; 8; 0; 11 |])

        // Vertex indicies for fragment shader
        renderPass.Draw 6u 2u 0u 0u
        renderPass.End()
        let buffer = Init.Ptr.arrayPtr [| encoder.Finish() |]
        wgpu.QueueSubmit(queue, unativeint 1, buffer)
        wgpu.SurfacePresent(win.Surface)
        wgpu.CommandBufferRelease(NativePtr.read buffer)
        encoder.Release()
        frame <- frame + 1
        let _ = wgpu.DevicePoll(wgpu.Device.Device, true)
        sw.Stop()
        if frame % 10 = 0 then
            printfn $"{float32 sw.ElapsedMilliseconds * 0.1f}"
            sw.Reset()
    )

let mutable options = WindowOptions.Default
options.API <- GraphicsAPI.None
options.Size <- Vector2D(400, 300)
// options.Size <- Vector2D(1920, 1080)
// options.Size <- Vector2D(960, 720)
// options.Size <- Vector2D(480, 360)
options.FramesPerSecond <- 60
options.UpdatesPerSecond <- 60
options.Position <- Vector2D(400, 400)
options.Title <- "WebGPU Demo"
options.IsVisible <- true
options.ShouldSwapAutomatically <- true
options.IsContextControlDisabled <- false
// let window = Window.Create options
let window = Window.Create options
window.add_Load(fun () -> init window)
window.Run()
// yo' window
