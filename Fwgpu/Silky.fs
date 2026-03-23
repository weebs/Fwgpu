module Silky

open Microsoft.FSharp.NativeInterop
open Silk.NET.Input
open Silk.NET.Maths
open Silk.NET.WebGPU
open Silk.NET.Windowing
open System.Collections.Generic

let keys = Dictionary()

keys[int Key.Space] <- false
keys[int Key.ShiftLeft] <- false
for i in 1..100 do
    keys[i] <- false

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
