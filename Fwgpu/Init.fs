module Init

open Microsoft.FSharp.NativeInterop
open Silk.NET.Core.Native
open Silk.NET.Maths
open Silk.NET.WebGPU
open Silk.NET.Windowing
open System.Collections.Generic

type Ptr =
    static member inline arrayPtr(values: 't[]) =
        use ptr = fixed values
        ptr

    static member inline ptr<'t when 't: unmanaged>(value: 't) =
        use ptr = fixed [| value |]
        ptr
        
[<AutoOpen>]
module AutoImport =
    let arrayPtr = Ptr.arrayPtr
    let ptr = Ptr.ptr


let createRender
    (wgpu: WebGPU)
    device
    (surfaceCapabilities: SurfaceCapabilities)
    layout
    shaderModule
    =
    let blendState =
        BlendState(
            Color =
                BlendComponent(
                    SrcFactor = BlendFactor.One,
                    DstFactor = BlendFactor.Zero,
                    Operation = BlendOperation.Add
                ),
            Alpha =
                BlendComponent(
                    SrcFactor = BlendFactor.One,
                    DstFactor = BlendFactor.Zero,
                    Operation = BlendOperation.Add
                )
        )

    let colorTargetState =
        ColorTargetState(
            Format = NativePtr.read surfaceCapabilities.Formats,
            Blend = arrayPtr [| blendState |],
            WriteMask = ColorWriteMask.All
        )

    let fragmentState =
        FragmentState(
            Module = shaderModule,
            TargetCount = unativeint 1,
            Targets = arrayPtr [| colorTargetState |],
            EntryPoint = C.string "fragment"
        )

    let renderPipelineDescriptor =
        RenderPipelineDescriptor(
            Vertex =
                VertexState(
                    Module = shaderModule,
                    EntryPoint = C.string "vertex"
                // BufferCount = unativeint 1
                ),
            Primitive =
                PrimitiveState(
                    Topology = PrimitiveTopology.TriangleList,
                    StripIndexFormat = IndexFormat.Undefined,
                    FrontFace = FrontFace.Ccw,
                    CullMode = CullMode.None
                ),
            Multisample =
                MultisampleState(
                    Count = 1u,
                    Mask = ~~~ 0u,
                    AlphaToCoverageEnabled = false
                ),
            // Fragment = &&fragmentState,
            Fragment = ptr fragmentState,
            DepthStencil = Unchecked.defaultof<_>,
            // Layout = wgpu.CreatePipelineLayout(device, [| group.bindGroupLayout |])
            Layout = layout
        )

    wgpu.CreateRenderPipeline(device, renderPipelineDescriptor)
