namespace global

// type WgslShader = interface end

open Microsoft.FSharp.Reflection
open System
open Dootverse.WebGPU
open System.Reflection
open Microsoft.FSharp.Quotations
open System.Threading.Tasks
open Silk.NET.WebGPU
open Silk.NET.Core.Native
open Microsoft.FSharp.NativeInterop
// module Foo =    
//     let foo<'a, 'b> (a: 'a) (b: 'b) : (Impl<'a> & Impl<'b>)=
//         { new Impl<'a> with member this.Get = a
//           interface Impl2<'b> with member this.Get = b }

type C =
    static member string value = NativePtr.ofNativeInt<byte> (SilkMarshal.StringToPtr value)

type BufferInfo =
    {
        isUniform: bool
        usage: BufferUsage
        // size: uint64
        size: int
    }
    
type DotnetBuffer =
    {
        ptr: nativeptr<Buffer> ref
        info: BufferInfo
    }
module Wgpu =
    let getAttributes (customAttributes: CustomAttributeData seq) =
        customAttributes
        |> Seq.filter (fun c ->
            c.Constructor.DeclaringType.BaseType = typeof<Wgsl.WgslAttribute>)
        |> Seq.choose (fun data ->
            let c = data.Constructor.Invoke(data.ConstructorArguments |> Seq.map _.Value |> Seq.toArray)
            match c with
            | :? Wgsl.WgslAttribute as attr -> Some attr.Serialize
            | _ -> None
        ) |> Seq.toList
    let rec getMethodBody e =
        match e with
        | Patterns.Let (var, Patterns.TupleGet (Patterns.Var tpl, index), in_) when tpl.Name = "tupledArg" ->
            getMethodBody in_
        | Patterns.Lambda (_, e2) -> getMethodBody e2
        | _ -> e
        
// type Wgpu =
    // static member BufferForType (t: Type) =
    //     

type Setup =
    static let window (wgpu, nativeInstance, surface) = null
    // static member inline compileModule<'a, 'b when 'b: (new: 'a -> 'b)> (constructor: 'a -> 'b) : Compiler.Module =
    static let compileMethod (method: MethodInfo) e =
        let fn =
            if method.ReturnType <> typeof<System.Void> then
                Compiler.translateStatement (Wgpu.getMethodBody e)
                |> Compiler.addReturn
            else
                Compiler.translateStatement (Wgpu.getMethodBody e)
        let result : Compiler.WgslFunc = {
            name = method.Name
            args =
                method.GetParameters()
                |> Array.map (fun p ->
                    let attrs = p.CustomAttributes |> Wgpu.getAttributes
                    p.Name, Compiler.toType p.ParameterType, attrs)
                |> Array.toList
            attrs =
                Wgpu.getAttributes method.CustomAttributes
                |> fun items ->
                    if items |> List.exists (fun i -> i.StartsWith "@vertex" || i.StartsWith "@fragment") then
                        items |> List.filter (fun i -> i.StartsWith "@location" = false)
                    else
                        items
            returnAttr =
                method.CustomAttributes |> Seq.tryPick (fun p ->
                    if p.Constructor.DeclaringType = typeof<Wgsl.LocationAttribute> then
                        match p.Constructor.Invoke (p.ConstructorArguments |> Seq.map _.Value |> Seq.toArray) with
                        | :? Wgsl.LocationAttribute as location -> Some location.Serialize
                        | _ -> None
                    else
                        None
                )
            returnType =
                if method.ReturnType = typeof<System.Void> then None
                else Some (Compiler.toType method.ReturnType)
            fn = fn
        } in result
    static member calculateBuffers (t: Type) =
        let ctor = t.GetConstructors() |> Seq.head
        let moduleParams = ctor.GetParameters()
        let moduleParamAttrs =
            moduleParams
            |> Array.map (_.CustomAttributes >> Seq.toArray)
            |> Array.zip moduleParams
        Map.ofList [
            let mutable count = 0
            for (info, attrs) in moduleParamAttrs do
                info.Name, ({
                    name = info.Name
                    binding = Some (0, count)
                    varTypes =
                        if attrs.Length <> 0 then
                            attrs |> Array.map string |> Array.toList
                        else
                            [ "storage"; "read_write" ]
                    varType = Compiler.toType info.ParameterType
                } : Compiler.WgslModuleVar)
                
                count <- count + 1
        ]
        // [|
        //     for i in 0..moduleParams.Length - 1 do
        //         let p = moduleParams[i]
        //         {|
        //             Label = $"{t.Name} Parameter {p.Name}"
        //             Attributes = moduleParamAttrs
        //             Binding = 0u
        //         |}
        // |]
    static member compileModule (constructor: 'a -> 'b) : Compiler.Module =
        let t = typeof<'b>
        // let a = new 'a()
        let ctors = typeof<'b>.GetConstructors()
        let buffers = Setup.calculateBuffers t
        let ctor = ctors[0]
        let structs = ResizeArray()
        do
            ctor.GetParameters()
            |> Array.filter (_.ParameterType >> Compiler.requiresDecl)
            |> Array.map (fun p ->
                Compiler.structsUsedByType p.ParameterType
            )
            |> Array.collect id
            |> structs.AddRange
            // |> Array.map (fun p -> p.ParameterType.Name, Compiler.translateStruct p.ParameterType)
        let moduleArgTypes =
            if FSharpType.IsTuple t then
                FSharpType.GetTupleElements t
            else
                [| t |]
        let methods = typeof<'b>.GetMethods(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        let methodQuotations =
            methods
            |> Array.choose (function
                | method & DerivedPatterns.MethodWithReflectedDefinition e ->
                    Some (method.Name, (method, e))
                | method -> None)
            |> Map.ofArray
        let methodTypes =
            methods |> Array.map (fun m -> m.GetParameters() |> Array.map _.ParameterType |> Array.append [| m.ReturnType |]) |> Array.collect id |> Array.distinct
        do
            methodTypes
            |> Array.map Compiler.structsUsedByType
            |> Array.collect id
            |> structs.AddRange
        let compiled = ResizeArray()
        try
        for (method, e) in methodQuotations.Values do
            let result = compileMethod method e
            compiled.Add result
        with error ->
            printfn $"{error}"
        {
            unfinishedBindings = Map.empty
            bindings = Setup.calculateBuffers t
            structs = Map.ofSeq (structs |> Seq.map (fun s -> fst s, s))
            fns = compiled |> Seq.map (fun fn -> fn.name, fn) |> Map.ofSeq
        }
    
[<AutoOpen>]
module PollExtensions =
    type PollDelegate = delegate of nativeptr<Device> * bool * nativeint -> bool
    let mutable poll = None
    type WebGPU with
        member inline this.DevicePoll(device, wait, userData) =
            if poll.IsNone then
                let found, ptr = this.Context.TryGetProcAddress("wgpuDevicePoll")
                poll <- Some (System.Runtime.InteropServices.Marshal.GetDelegateForFunctionPointer<PollDelegate> ptr)
            poll.Value.Invoke(device, wait, userData)
        member inline this.DevicePoll(device, wait) =
            this.DevicePoll(device, wait, Unchecked.defaultof<_>)
        member inline this.CreateInstance(?desc) =
            let descriptor = desc |> Option.defaultValue (InstanceDescriptor())
            this.CreateInstance(&descriptor)
        member inline this.RequestDeviceAsync(adapter, ?request) =
            let request = request |> Option.defaultValue (DeviceDescriptor())
            let promise = TaskCompletionSource<_>()
            let callback = new PfnRequestDeviceCallback(RequestDeviceCallback(fun _ device _ _ ->
                promise.TrySetResult device |> ignore))
            task {
                this.AdapterRequestDevice(adapter, &request, callback, Unchecked.defaultof<_>)
                let! result = promise.Task
                callback.Dispose()
                return result
            }
        member inline this.RequestAdapterAsync(instance, ?request: RequestAdapterOptions) =
            let request = request |> Option.defaultValue (RequestAdapterOptions())
            let promise = TaskCompletionSource<_>()
            let callback = new PfnRequestAdapterCallback(RequestAdapterCallback(fun _ a _ _ ->
                promise.TrySetResult a |> ignore))
            task {
                this.InstanceRequestAdapter(instance, &request, callback, Unchecked.defaultof<_>)
                let! result = promise.Task
                callback.Dispose()
                return result
            }
        member inline this.EncoderBeginComputePass(encoder, ?desc: ComputePassDescriptor) =
            let value = desc |> Option.defaultValue (ComputePassDescriptor())
            this.CommandEncoderBeginComputePass(encoder, &value)
        member inline this.CreateCommandEncoder(device, ?desc) =
            let mutable value = desc |> Option.defaultValue (CommandEncoderDescriptor())
            this.DeviceCreateCommandEncoder(device, &&value)
    
// type Foo =
//     class
//         val mutable NextInChain : int
//         new () = Operators.Unchecked.defaultof<Foo>
//     end
// module A =
//     let f = new Foo(y = 1)
[<AutoOpen>]
module rec Wrappers =
    type WebGPU'(wgpu: WebGPU, ?instance, ?requestAdapterOptions, ?surface) as this =
        inherit WebGPU(wgpu.Context)
        let instance = instance |> Option.defaultWith (fun () -> wgpu.CreateInstance())
        // let instance = wgpu.CreateInstance()
        let adapter = wgpu.RequestAdapterAsync(instance, ?request=requestAdapterOptions).Result
        let device = Device'(this, wgpu.RequestDeviceAsync(adapter).Result)
        new (window: Silk.NET.Windowing.IWindow) =
            let api = WebGPU.GetApi()
            let inst = api.CreateInstance()
            let surface = window.CreateWebGPUSurface(api, inst)
            new WebGPU'(api, inst, RequestAdapterOptions(CompatibleSurface=surface), surface)
        member this.Device = device
        member this.Instance = instance
        member this.Adapter = adapter
        member this.Surface = surface.Value
        // member this.CreateBinder (shader: Quotations.Expr<'a * 'b -> _>) =
        //     // let infoForType (t: System.Type) : BufferInfo =
        //         // { isUniform = false; size = 0uL }
        //     // let group = this.CreateBuffers device [| infoForType typeof<'a>; infoForType typeof<'b> |]
        //     // let a = ref Unchecked.defaultof<_>
        //     // let b = ref Unchecked.defaultof<_>
        //     ShaderBinder<'a, 'b>(device.Device, [])
        // member this.CreateBinder (shader: Quotations.Expr<'a * 'b -> _>) =
        //     let info = { device = device.Device }
        //     let m = Setup.compileModule shader
        //     let code = Compiler.Print.module' m
        //     ShaderBinder<'a, 'b>(info, [])
        // member this.CreateBinder (shader: Quotations.Expr<'a * 'b * 'c -> _>) =
        //     let info = { device = device.Device }
        //     let m = Setup.compileModule shader
        //     let code = Compiler.Print.module' m
        //     ShaderBinder<'a, 'b, 'c>(info, [])
        // member this.CreateBinder (shader: Quotations.Expr<'a * 'b * 'c * 'd -> _>) =
        //     let info = { device = device.Device }
        //     let m = Setup.compileModule shader
        //     let code = Compiler.Print.module' m
        //     ShaderBinder<'a, 'b, 'c, 'd>(info, [])
        interface System.IDisposable with
            member this.Dispose() =
                wgpu.DeviceRelease device.Device
                wgpu.AdapterRelease adapter
                wgpu.InstanceRelease instance
    type Queue'(wgpu: WebGPU') = class end
    type CommandEncoder'(wgpu: WebGPU', encoder) =
        member this.BeginComputePass (?d: ComputePassDescriptor) =
            ComputePassEncoder'(wgpu, wgpu.EncoderBeginComputePass(encoder, ?desc=d))
        member this.StartRenderPass colorAttachment =
            wgpu.StartRenderPass(encoder, [| colorAttachment |])
        member this.StartRenderPass' colorAttachment =
            RenderPass'(wgpu, wgpu.StartRenderPass(encoder, [| colorAttachment |]))
        member this.Finish() =
            let cbd = CommandBufferDescriptor()
            wgpu.CommandEncoderFinish(encoder, &cbd)
        member this.Release () =
            wgpu.CommandEncoderRelease encoder
        member this.Encoder = encoder
    type RenderPass'(wgpu: WebGPU', pass) =
        member this.SetPipeline pipeline = wgpu.RenderPassEncoderSetPipeline(pass, pipeline)
        member this.SetBindGroup group index =
            wgpu.RenderPassEncoderSetBindGroup(pass, index, group, unativeint 0, Unchecked.defaultof<nativeptr<_>>)
        member this.Draw a b c d = wgpu.RenderPassEncoderDraw(pass, a, b, c, d)
        member this.End() = wgpu.RenderPassEncoderEnd pass
    type ComputePassEncoder'(wgpu: WebGPU', encoder) =
        member this.SetPipeline pipeline = wgpu.ComputePassEncoderSetPipeline (encoder, pipeline)
        member this.SetBindGroup group index =
            wgpu.ComputePassEncoderSetBindGroup(encoder, index, group, unativeint 0, Unchecked.defaultof<nativeptr<_>>)
        member this.DispatchWorkgroups x y z =
            wgpu.ComputePassEncoderDispatchWorkgroups(encoder, x, y, z)
        member this.End () = wgpu.ComputePassEncoderEnd encoder
        member this.Encoder = encoder
    type Device'(wgpu: WebGPU', device: nativeptr<Device>) =
        member this.GetQueue () = wgpu.DeviceGetQueue device
        member this.CreateCommandEncoder () = CommandEncoder'(wgpu, wgpu.CreateCommandEncoder device)
        member this.Device = device
        member this.Wgpu = wgpu
    type ShaderBindingData = { wgpu: WebGPU'; code: string }
    type WebGPU with
        member this.StartRenderPass (encoder, descriptors: _ []) =
            let ptr = fixed descriptors
            let renderPass = RenderPassDescriptor(
                ColorAttachments = ptr,
                ColorAttachmentCount = unativeint descriptors.Length
            )
            this.CommandEncoderBeginRenderPass(encoder, &renderPass)
type ShaderWithBindings(info: ShaderBindingData, buffers: DotnetBuffer[]) =
    member this.Buffers = buffers
    member this.Info = info
    // type ShaderBinder<'t>(buffer: nativeptr<Buffer> ref) =
    // todo : create a RenderInstance type that will hold onto all the buffers so you don't need to save variables?
type ShaderBinder<'t>(data: ShaderBindingData, acc) = // todo : linear types would guarantee that this only gets used once so you don't accidentally unbind another shader
    let buffer = ref Unchecked.defaultof<nativeptr<Buffer>>
    member this.Buffer = buffer
    member this.Info = data
    member this.BufferRefs info =
        let variables = List.toArray <| List.rev ({ ptr = buffer; info = info } :: acc)
        ShaderWithBindings (data, variables)
// type ShaderBinder<'t2, 't1>(buffer: nativeptr<Buffer> ref, cons: ShaderBinder<'t1>) =
type ShaderBinder<'t2, 't1>(data, acc: _ list) =
    // inherit ShaderBinding<'t1>(cons.Buffer1)
    let buffer = ref Unchecked.defaultof<nativeptr<Buffer>>
    // member this.Bindings = bindings
    member this.Info = data
    member this.Buffer = buffer
    member this.Rest info = ShaderBinder<'t1>(data, { ptr = buffer; info = info } :: acc)
type ShaderBinder<'t3, 't2, 't1>(data, acc: _ list) =
    // inherit ShaderBinding<'t1>(cons.Buffer1)
    let buffer = ref Unchecked.defaultof<nativeptr<Buffer>>
    // member this.Bindings = bindings
    member this.Info = data
    member this.Buffer = buffer
    member this.Rest info = ShaderBinder<'t2, 't1>(data, { ptr = buffer; info = info } :: acc)
type ShaderBinder<'t4, 't3, 't2, 't1>(data, acc: _ list) =
    // inherit ShaderBinding<'t1>(cons.Buffer1)
    let buffer = ref Unchecked.defaultof<nativeptr<Buffer>>
    // member this.Bindings = bindings
    member this.Info = data
    member this.Buffer = buffer
    member this.Rest info = ShaderBinder<'t3, 't2, 't1>(data, { ptr = buffer; info = info } :: acc)
type ShaderBinder<'t5, 't4, 't3, 't2, 't1>(data, acc: _ list) =
    // inherit ShaderBinding<'t1>(cons.Buffer1)
    let buffer = ref Unchecked.defaultof<nativeptr<Buffer>>
    // member this.Bindings = bindings
    member this.Info = data
    member this.Buffer = buffer
    member this.Rest info = ShaderBinder<'t4, 't3, 't2, 't1>(data, { ptr = buffer; info = info } :: acc)
type ShaderBinder<'t6, 't5, 't4, 't3, 't2, 't1>(data, acc: _ list) =
    // inherit ShaderBinding<'t1>(cons.Buffer1)
    let buffer = ref Unchecked.defaultof<nativeptr<Buffer>>
    // member this.Bindings = bindings
    member this.Info = data
    member this.Buffer = buffer
    member this.Rest info = ShaderBinder<'t5, 't4, 't3, 't2, 't1>(data, { ptr = buffer; info = info } :: acc)
type ShaderBinder<'t7, 't6, 't5, 't4, 't3, 't2, 't1>(data, acc: _ list) =
    // inherit ShaderBinding<'t1>(cons.Buffer1)
    let buffer = ref Unchecked.defaultof<nativeptr<Buffer>>
    // member this.Bindings = bindings
    member this.Info = data
    member this.Buffer = buffer
    member this.Rest info = ShaderBinder<'t6, 't5, 't4, 't3, 't2, 't1>(data, { ptr = buffer; info = info } :: acc)
type ShaderBinder<'t8, 't7, 't6, 't5, 't4, 't3, 't2, 't1>(data, acc: _ list) =
    // inherit ShaderBinding<'t1>(cons.Buffer1)
    let buffer = ref Unchecked.defaultof<nativeptr<Buffer>>
    // member this.Bindings = bindings
    member this.Info = data
    member this.Buffer = buffer
    member this.Rest info = ShaderBinder<'t7, 't6, 't5, 't4, 't3, 't2, 't1>(data, { ptr = buffer; info = info } :: acc)
type ShaderVariable<'t>(buffer: _ ref, serializer: 't -> byte[]) =
    member this.Write(wgpu: WebGPU, queue, data: 't) =
        let serialized = serializer data
        let ptr = fixed serialized
        wgpu.QueueWriteBuffer(queue, buffer.Value, 0uL, NativePtr.toVoidPtr ptr, unativeint serialized.Length)
    member this.Buffer = buffer.Value
type ShaderMapVar<'t>(buffer: _ ref, serializer: 't -> byte[]) =
    member this.Write(wgpu: WebGPU, queue, data: 't) =
        let serialized = serializer data
        let ptr = fixed serialized
        wgpu.QueueWriteBuffer(queue, buffer.Value, 0uL, NativePtr.toVoidPtr ptr, unativeint serialized.Length)
    member this.Buffer = buffer.Value
type ShaderBuffer<'t>(buffer: _ ref, size: int, serializer: 't -> byte[]) =
    member this.Write(wgpu: WebGPU, queue, offset, data: 't seq) =
        let bufferData = [| for item in data do yield! serializer item |]
        let (* todo: use ? *) ptr = fixed bufferData
        let writeSize = bufferData.Length
        wgpu.QueueWriteBuffer(queue, buffer.Value, offset, NativePtr.toVoidPtr ptr, unativeint writeSize)
    member this.Buffer = buffer.Value
    
// type 't with todo Extensions to types with a reference to themselves, ie: serializeWith
//     member this.Foo = ()

// [<AutoOpen>]
// module WebGPUBind =
type ShaderMap<'t when 't: unmanaged>
    (wgpu: WebGPU, device: nativeptr<Device>, buffer: _ ref, size: int, serializer: 't -> byte[]) =
    // todo
    let stagingDesc = BufferDescriptor(
        Usage = (BufferUsage.MapRead ||| BufferUsage.CopyDst),
        Size = uint64 size,
        MappedAtCreation = false
    )
    let stagingBuffer = wgpu.DeviceCreateBuffer(device, &stagingDesc)
    member this.Write(wgpu: WebGPU, queue, offset, data: 't seq) =
        let bufferData = [| for item in data do yield! serializer item |]
        let (* todo: use ? *) ptr = fixed bufferData
        let writeSize = bufferData.Length
        wgpu.QueueWriteBuffer(queue, buffer.Value, offset, NativePtr.toVoidPtr ptr, unativeint writeSize)
    member this.AddCopy(wgpu: WebGPU, encoder) =
        wgpu.CommandEncoderCopyBufferToBuffer(encoder, buffer.Value, 0uL, stagingBuffer, 0uL, uint64 size)
    member this.MapAsync (wgpu: WebGPU) =
        task {
            let promise = TaskCompletionSource<_>()
            let callback = new PfnBufferMapCallback(fun _ _ ->
                ignore <| promise.TrySetResult ()
            )
            wgpu.BufferMapAsync(stagingBuffer, MapMode.Read, unativeint 0, unativeint size, callback, Unchecked.defaultof<_>)
            do! promise.Task
            callback.Dispose()
            let result = wgpu.BufferGetMappedRange(stagingBuffer, unativeint 0, unativeint size)
            return result |> NativePtr.ofVoidPtr<'t>
        }
    member this.ReadBufferRange (wgpu: WebGPU', offset, readCount) =
        task {
            let promise = TaskCompletionSource<_>()
            let callback = new PfnBufferMapCallback(fun _ _ ->
                ignore <| promise.TrySetResult ()
            )
            let readSize = readCount * 4
            wgpu.BufferMapAsync(stagingBuffer, MapMode.Read, unativeint offset, unativeint readSize, callback, Unchecked.defaultof<_>)
            wgpu.DevicePoll(wgpu.Device.Device, true) |> ignore
            do! promise.Task
            callback.Dispose()
            let result =
                wgpu.BufferGetMappedRange(
                    stagingBuffer,
                    unativeint offset,
                    unativeint readSize)
                |> NativePtr.ofVoidPtr<'t>
            let item0 = NativePtr.read result
            let array = Array.zeroCreate readCount
            let ptr = fixed array // todo use doesn't work here
            // todo : use deserialization instead of copyBlock ?
            NativePtr.copyBlock ptr result readCount
            wgpu.BufferUnmap(stagingBuffer)
            return array
        }
    member this.Buffer = buffer.Value
type W = { wgpu: WebGPU } // todo
type Dev = { device: Device; wgpu: W } // todo
type CompPipeline = { p: nativeptr<ComputePipeline>; device: Dev }
type Wgpu =
    static let bufferUsage = BufferUsage.CopySrc ||| BufferUsage.CopyDst ||| BufferUsage.Storage
    static member BindI(binding: ShaderBinder<'t[]>, info, ?serializer) =
        let serializer = serializer |> Option.defaultWith (fun () ->
            Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
        ShaderBuffer<'t>(binding.Buffer, int info.size, serializer), binding.BufferRefs info
    static member Bind'(binding: ShaderBinder<'t[]>, ?serializer) = fun info ->
        let serializer = serializer |> Option.defaultWith (fun () ->
            Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
        ShaderBuffer<'t>(binding.Buffer, int info.size, serializer), binding.BufferRefs info
    static member Bind(binding: ShaderBinder<'t[]>, ?serializer) = fun size ->
        let serializer = serializer |> Option.defaultWith (fun () ->
            Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
        let size = size * 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t>
        let info = { size = size; isUniform = false; usage = bufferUsage }
        ShaderBuffer<'t>(binding.Buffer, int info.size, serializer), binding.BufferRefs info
    static member BindI(bindings: ShaderBinder<'t2[], 't1>, info, ?serializer) =
        let serializer = serializer |> Option.defaultWith (fun () ->
            Dootverse.WebGPU.Compiler.makeSerialize<'t2> ())
        // let size = size * 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t2>
        // let info = { size = size; isUniform = false; usage = bufferUsage }
        ShaderBuffer<'t2>(bindings.Buffer, int info.size, serializer), bindings.Rest info
    static member Bind(bindings: ShaderBinder<'t2[], 't1>, ?serializer) = fun size ->
        let serializer = serializer |> Option.defaultWith (fun () ->
            Dootverse.WebGPU.Compiler.makeSerialize<'t2> ())
        let size = size * 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t2>
        let info = { size = size; isUniform = false; usage = bufferUsage }
        ShaderBuffer<'t2>(bindings.Buffer, int info.size, serializer), bindings.Rest info

    static member Bind(bindings: ShaderBinder<'t[], 't1, 't2, 't3, 't4, 't5, 't6, 't7>, ?serializer) = fun size ->
        let serializer = serializer |> Option.defaultWith (fun () ->
            Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
        let size = size * 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t>
        let info = { size = size; isUniform = false; usage = bufferUsage }
        ShaderBuffer<'t>(bindings.Buffer, int info.size, serializer), bindings.Rest info
    static member Bind(bindings: ShaderBinder<'t[], 't1, 't2, 't3, 't4, 't5, 't6>, ?serializer) = fun size ->
        let serializer = serializer |> Option.defaultWith (fun () ->
            Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
        let size = size * 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t>
        let info = { size = size; isUniform = false; usage = bufferUsage }
        ShaderBuffer<'t>(bindings.Buffer, int info.size, serializer), bindings.Rest info

    static member Bind(bindings: ShaderBinder<'t[], 't1, 't2, 't3, 't4, 't5>, ?serializer) = fun size ->
        let serializer = serializer |> Option.defaultWith (fun () ->
            Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
        let size = size * 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t>
        let info = { size = size; isUniform = false; usage = bufferUsage }
        ShaderBuffer<'t>(bindings.Buffer, int info.size, serializer), bindings.Rest info

    static member Bind(bindings: ShaderBinder<'t[], 't1, 't2, 't3, 't4>, ?serializer) = fun size ->
        let serializer = serializer |> Option.defaultWith (fun () ->
            Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
        let size = size * 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t>
        let info = { size = size; isUniform = false; usage = bufferUsage }
        ShaderBuffer<'t>(bindings.Buffer, int info.size, serializer), bindings.Rest info

    static member Bind(bindings: ShaderBinder<'t[], 't1, 't2, 't3>, ?serializer) = fun size ->
        let serializer = serializer |> Option.defaultWith (fun () ->
            Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
        let size = size * 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t>
        let info = { size = size; isUniform = false; usage = bufferUsage }
        ShaderBuffer<'t>(bindings.Buffer, int info.size, serializer), bindings.Rest info

    static member Bind(bindings: ShaderBinder<'t[], 't1, 't2>, ?serializer) = fun size ->
        let serializer = serializer |> Option.defaultWith (fun () ->
            Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
        let size = size * 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t>
        let info = { size = size; isUniform = false; usage = bufferUsage }
        ShaderBuffer<'t>(bindings.Buffer, int info.size, serializer), bindings.Rest info
    static member Bind(bindings: ShaderBinder<'t3[], 't2, 't1>, info, ?serializer) =
        let serializer = serializer |> Option.defaultWith (fun () ->
            Dootverse.WebGPU.Compiler.makeSerialize<'t3> ())
        ShaderBuffer<'t3>(bindings.Buffer, int info.size, serializer), bindings.Rest info
    static member BindInfo(bindings: ShaderBinder<'t3[], 't2, 't1>, ?serializer) = fun info ->
        let serializer = serializer |> Option.defaultWith (fun () ->
            Dootverse.WebGPU.Compiler.makeSerialize<'t3> ())
        ShaderBuffer<'t3>(bindings.Buffer, int info.size, serializer), bindings.Rest info
    static member BindI(bindings: ShaderBinder<'t3[], 't2, 't1>, info, ?serializer) =
        let serializer = serializer |> Option.defaultWith (fun () ->
            Dootverse.WebGPU.Compiler.makeSerialize<'t3> ())
        ShaderBuffer<'t3>(bindings.Buffer, int info.size, serializer), bindings.Rest info
    static member BindI(bindings: ShaderBinder<'t4[], 't3, 't2, 't1>, info, serializer) =
        // let serializer = serializer |> Option.defaultWith (fun () ->
            // Dootverse.WebGPU.Compiler.makeSerialize<'t4> ())
        ShaderBuffer<'t4>(bindings.Buffer, int info.size, serializer), bindings.Rest info
    static member Bind(bindings: ShaderBinder<'t4[], 't3, 't2, 't1>, serializer) = fun info ->
        // let serializer = serializer |> Option.defaultWith (fun () ->
            // Dootverse.WebGPU.Compiler.makeSerialize<'t4> ())
        ShaderBuffer<'t4>(bindings.Buffer, int info.size, serializer), bindings.Rest info
    static member Map(binding: ShaderBinder<'t[]>, ?serializer) = fun size -> 
        let serializer = serializer |> Option.defaultWith (fun () ->
            Compiler.makeSerialize<'t> ())
        let sizeofType = 4 * Compiler.sizeofType typeof<'t>
        let bufferSize = size * sizeofType
        let info = { size = bufferSize; isUniform = false; usage = bufferUsage }
        ShaderMap<'t>(binding.Info.wgpu, binding.Info.wgpu.Device.Device, binding.Buffer, int info.size, serializer), binding.BufferRefs info
    static member Map(binding: ShaderBinder<'t[], 't1>, ?serializer) = fun size -> 
        let serializer = serializer |> Option.defaultWith (fun () ->
            Compiler.makeSerialize<'t> ())
        let sizeofType = 4 * Compiler.sizeofType typeof<'t>
        let bufferSize = size * sizeofType
        let info = { size = bufferSize; isUniform = false; usage = bufferUsage }
        ShaderMap<'t>(binding.Info.wgpu, binding.Info.wgpu.Device.Device, binding.Buffer, int info.size, serializer), binding.Rest info
    static member Map(binding: ShaderBinder<'t[], 't1, 't2>, ?serializer) = fun size -> 
        let serializer = serializer |> Option.defaultWith (fun () ->
            Compiler.makeSerialize<'t> ())
        let sizeofType = 4 * Compiler.sizeofType typeof<'t>
        let bufferSize = size * sizeofType
        let info = { size = bufferSize; isUniform = false; usage = bufferUsage }
        ShaderMap<'t>(binding.Info.wgpu, binding.Info.wgpu.Device.Device, binding.Buffer, int info.size, serializer), binding.Rest info
    static member MapS(bindings: ShaderBinder<'t2[], 't1>) = fun wgpu device info -> fun serializer ->
        ShaderMap<'t2>(wgpu, device, bindings.Buffer, int info.size, serializer), bindings.Rest info
    static member MapS(bindings: ShaderBinder<'t3[], 't2, 't1>) = fun wgpu device info -> fun serializer ->
        ShaderMap<'t3>(wgpu, device, bindings.Buffer, int info.size, serializer), bindings.Rest info
    static member MapS(bindings: ShaderBinder<'t4[], 't3, 't2, 't1>) = fun wgpu device info -> fun serializer ->
        ShaderMap<'t4>(wgpu, device, bindings.Buffer, int info.size, serializer), bindings.Rest info
[<AutoOpen>]
module WebGPUBindExtensions =
    // let inline takesList<'t, 'a when 'a: (member value: 't option)> (value: {| value: 't option; cons: 'a |}) =
    //     ()
    let bufferUsage = BufferUsage.CopySrc ||| BufferUsage.CopyDst ||| BufferUsage.Storage
    type Wgpu with
        static member Bind(binding: ShaderBinder<'t, 't1, 't2, 't3, 't4, 't5, 't6, 't7>, ?serializer) =
            let serializer = serializer |> Option.defaultWith (fun () ->
                Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
            let size = 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t>
            let info = { size = size; isUniform = false; usage = bufferUsage }
            ShaderVariable<'t>(binding.Buffer, serializer), binding.Rest info
        static member Bind(binding: ShaderBinder<'t, 't1, 't2, 't3, 't4, 't5, 't6>, ?serializer) =
            let serializer = serializer |> Option.defaultWith (fun () ->
                Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
            let size = 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t>
            let info = { size = size; isUniform = false; usage = bufferUsage }
            ShaderVariable<'t>(binding.Buffer, serializer), binding.Rest info
        static member Bind(binding: ShaderBinder<'t, 't1, 't2, 't3, 't4, 't5>, ?serializer) =
            let serializer = serializer |> Option.defaultWith (fun () ->
                Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
            let size = 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t>
            let info = { size = size; isUniform = false; usage = bufferUsage }
            ShaderVariable<'t>(binding.Buffer, serializer), binding.Rest info
        static member Bind(binding: ShaderBinder<'t, 't1, 't2, 't3, 't4>, ?serializer) =
            let serializer = serializer |> Option.defaultWith (fun () ->
                Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
            let size = 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t>
            let info = { size = size; isUniform = false; usage = bufferUsage }
            ShaderVariable<'t>(binding.Buffer, serializer), binding.Rest info

        static member Bind(binding: ShaderBinder<'t>, ?serializer) = fun info ->
            let serializer = serializer |> Option.defaultWith (fun () ->
                Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
            ShaderVariable<'t>(binding.Buffer, serializer), binding.BufferRefs info
        static member Bind(binding: ShaderBinder<'t2, 't1>, ?serializer) = fun info ->
            let serializer = serializer |> Option.defaultWith (fun () ->
                Dootverse.WebGPU.Compiler.makeSerialize<'t2> ())
            ShaderVariable<'t2>(binding.Buffer, serializer), binding.Rest info
        static member Bind(binding: ShaderBinder<'t3, 't2, 't1>, ?serializer) =
            let serializer = serializer |> Option.defaultWith (fun () ->
                Dootverse.WebGPU.Compiler.makeSerialize<'t3> ())
            let size = 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t3>
            let info = { size = size; isUniform = false; usage = bufferUsage }
            ShaderVariable<'t3>(binding.Buffer, serializer), binding.Rest info

        static member BindI(binding: ShaderBinder<'t3, 't2, 't1>, info, ?serializer) =
            let serializer = serializer |> Option.defaultWith (fun () ->
                Dootverse.WebGPU.Compiler.makeSerialize<'t3> ())
            // let size = 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t3>
            // let info = { size = size; isUniform = false; usage = bufferUsage }
            ShaderVariable<'t3>(binding.Buffer, serializer), binding.Rest info
        static member Bind(binding: ShaderBinder<'t, 't3, 't2, 't1>, ?serializer) = 
            let serializer = serializer |> Option.defaultWith (fun () ->
                Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
            let size = 4 * Dootverse.WebGPU.Compiler.sizeofType typeof<'t>
            let info = { size = size; isUniform = false; usage = bufferUsage }
            ShaderVariable<'t>(binding.Buffer, serializer), binding.Rest info
        static member BindInfo(binding: ShaderBinder<'t4, 't3, 't2, 't1>, ?serializer) = fun info ->
            let serializer = serializer |> Option.defaultWith (fun () ->
                Dootverse.WebGPU.Compiler.makeSerialize<'t4> ())
            ShaderVariable<'t4>(binding.Buffer, serializer), binding.Rest info
        static member BindI(binding: ShaderBinder<'t4, 't3, 't2, 't1>, info, serializer) =
            // let serializer = serializer |> Option.defaultWith (fun () ->
                // Dootverse.WebGPU.Compiler.makeSerialize<'t4> ())
            ShaderVariable<'t4>(binding.Buffer, serializer), binding.Rest info
        static member MapI(binding: ShaderBinder<'t>, ?serializer) = fun info ->
            let serializer = serializer |> Option.defaultWith (fun () ->
                Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
            ShaderMapVar<'t>(binding.Buffer, serializer), binding.BufferRefs info
        // static member Map(bindings: ShaderBinder<'t2, 't1>, ?serializer) = fun info ->
        //     let serializer = serializer |> Option.defaultWith (fun () ->
        //         Dootverse.WebGPU.Compiler.makeSerialize<'t2> ())
        //     ShaderMapVar<'t2>(bindings.Buffer, serializer), bindings.Rest info
        // static member Map(bindings: ShaderBinder<'t3, 't2, 't1>, ?serializer) = fun info ->
        //     let serializer = serializer |> Option.defaultWith (fun () ->
        //         Dootverse.WebGPU.Compiler.makeSerialize<'t3> ())
        //     ShaderMapVar<'t3>(bindings.Buffer, serializer), bindings.Rest info
        // static member Map(bindings: ShaderBinder<'t4, 't3, 't2, 't1>, ?serializer) = fun info ->
        //     let serializer = serializer |> Option.defaultWith (fun () ->
        //         Dootverse.WebGPU.Compiler.makeSerialize<'t4> ())
        //     ShaderMapVar<'t4>(bindings.Buffer, serializer), bindings.Rest info
[<AutoOpen>]
module WebGPUBindExtensions2 =
    // let inline takesList<'t, 'a when 'a: (member value: 't option)> (value: {| value: 't option; cons: 'a |}) =
    //     ()
    type Wgpu with
        static member Bind(binding: ShaderBinder<'t>, ?serializer) = fun info ->
            let serializer = serializer |> Option.defaultWith (fun () ->
                Dootverse.WebGPU.Compiler.makeSerialize<'t> ())
            ShaderVariable<'t>(binding.Buffer, serializer), binding.BufferRefs info
            

[<AutoOpen>]
module Extensions =
    type WebGPU with
        member inline this.DevicePoll(device, wait) =
            this.DevicePoll(device, wait, Unchecked.defaultof<_>)
        member inline this.CreateBuffer(device, desc) =
            let mutable value = desc
            this.DeviceCreateBuffer(device, &&value)
        member inline this.DeviceCreateShaderModule(device, descriptor) =
            this.DeviceCreateShaderModule(device, &descriptor)
        member inline this.CreateShader(device, shader) =
            let ptr = SilkMarshal.StringToPtr shader
            let mutable descriptor = ShaderModuleWGSLDescriptor(
                ChainedStruct(SType = SType.ShaderModuleWgsldescriptor),
                NativePtr.ofNativeInt ptr)
            let module_ = ShaderModuleDescriptor(NativePtr.ofNativeInt (NativePtr.toNativeInt &&descriptor))
            this.DeviceCreateShaderModule(device, module_)
        member inline this.CreateBindGroupLayout(device, bindings: _ []) =
            use entries = fixed bindings
            let mutable descriptor = BindGroupLayoutDescriptor(
                EntryCount = unativeint bindings.Length,
                Entries = entries)
            this.DeviceCreateBindGroupLayout(device, &&descriptor)
        member this.CreateBindGroup(device, layout, entries: _ []) =
            use ptr = fixed entries
            let mutable descriptor = BindGroupDescriptor(
                Layout = layout,
                EntryCount = unativeint entries.Length,
                Entries = ptr)
            this.DeviceCreateBindGroup(device, &&descriptor)
        member inline this.GetCurrentTexture(surface) =
            let mutable texture = SurfaceTexture()
            this.SurfaceGetCurrentTexture(surface, &&texture)
            texture
        member inline this.CreateRenderPipeline(device, descriptor) =
            this.DeviceCreateRenderPipeline(device, &descriptor)
        member inline this.CreatePipelineLayout(device, entries: _ []) =
            use ptr = fixed entries
            let mutable descriptor = PipelineLayoutDescriptor(
                BindGroupLayoutCount = unativeint entries.Length,
                // BindGroupLayouts = &&bindGroupLayout
                BindGroupLayouts = ptr
            )
            this.DeviceCreatePipelineLayout(device, &&descriptor)
        member inline this.CreateBuffers (device, entries: _ []) =
            [| for descriptor in entries do
                this.CreateBuffer(device, descriptor) |]
            
        member inline this.CreateBindGroupEntries(entries: BufferDescriptor[], buffers: _ []) =
            [|
                for i in 0..buffers.Length - 1 do
                    let buffer = buffers[i]
                    BindGroupEntry(
                        Binding = uint i,
                        Buffer = buffer,
                        Offset = 0uL,
                        Size = entries[i].Size
                    )
            |]
        member inline this.EncoderFinish(encoder, ?desc) =
            let desc = desc |> Option.defaultValue(CommandBufferDescriptor())
            this.CommandEncoderFinish(encoder, &desc)
        member this.CreateBindGroup(device, bindGroupLayout, entries) =
            let buffers = this.CreateBuffers(device, entries)
            let bindings = this.CreateBindGroupEntries(entries, buffers)
            let bindGroup = this.CreateBindGroup(device, bindGroupLayout, bindings)
            {| buffers = buffers; bindings = bindings; bindGroup = bindGroup |}
            
        member this.CreateBuffers visibility = fun device (infos: BufferInfo[]) ->
            let layouts = [|
                for i in 0..infos.Length - 1 do
                    let info = infos[i]
                    let minBindingSize = uint64 info.size
                    // let visibility =
                        // if info.isUniform
                        // then ShaderStage.Compute
                        // else ShaderStage.Compute
                    BindGroupLayoutEntry(
                        Binding = uint i,
                        Visibility = visibility,
                        Buffer = BufferBindingLayout(
                            Type = (
                                if info.isUniform
                                then BufferBindingType.Uniform
                                else BufferBindingType.Storage),
                            MinBindingSize = minBindingSize
                        )
                    )
            |]
            let descriptors = [|
                for info in infos do
                    // let usage =
                        // if info.isUniform
                        // then BufferUsage.Uniform
                        // else BufferUsage.Storage
                    BufferDescriptor(
                        Size = uint64 info.size,
                        // Usage = (usage ||| BufferUsage.CopyDst)
                        // Usage = (usage ||| info.usage)
                        Usage = info.usage,
                        Label = C.string (Guid.NewGuid().ToString())
                    )
            |]
            let layout = this.CreateBindGroupLayout(device, layouts)
            {| this.CreateBindGroup(device, layout, descriptors) with
                bindGroupLayout = layout |}
        member this.CreateCompute shaderCode entryPoint device (vars: DotnetBuffer array) =
            // let layout: nativeptr<BindGroupLayout> = this.ComputePipelineGetBindGroupLayout(pipeline, 0u)
            // let layout = this.CreateBindGroupLayout()
            let infos = vars |> Array.map _.info
            let result = (this.CreateBuffers ShaderStage.Compute device infos)
            let computeLayout = this.CreatePipelineLayout(device, [| result.bindGroupLayout |])
            let shaderModule =
                let mutable wgslDesc = ShaderModuleWGSLDescriptor(
                     Chain = ChainedStruct(SType = SType.ShaderModuleWgslDescriptor),
                     Code = C.string shaderCode
                    // NextInChain = &&wgslDesc
                )
                let mutable desc = ShaderModuleDescriptor(NativePtr.ofNativeInt (NativePtr.toNativeInt &&wgslDesc))
                this.DeviceCreateShaderModule(device, &&desc)
            let desc = ComputePipelineDescriptor(
                Compute = ProgrammableStageDescriptor(
                    Module = shaderModule,
                    EntryPoint = C.string entryPoint
                ),
                Layout = computeLayout
            )
            let pipeline = this.DeviceCreateComputePipeline(device, &desc)
            for i in 0..result.buffers.Length - 1 do
                vars[i].ptr.Value <- result.buffers[i]
            {| result with
                pipeline = pipeline
                shaderModule = shaderModule |}
        member this.InitBindings stage (device: nativeptr<Device>) (vars: DotnetBuffer array) =
            let infos = vars |> Array.map _.info
            let group = this.CreateBuffers stage device infos
            for i in 0..group.buffers.Length - 1 do
                vars[i].ptr.Value <- group.buffers[i]
            {| bindGroup = group.bindGroup; layout = group.bindGroupLayout |}
        member this.RunComputeModule (compute_pipeline, bindGroup, device, x, y, z) =
            let command_encoder = 
                this.CreateCommandEncoder(device, CommandEncoderDescriptor())
            let compute_pass_encoder = 
                this.EncoderBeginComputePass(command_encoder, ComputePassDescriptor())
            let queue = this.DeviceGetQueue(device)
            this.ComputePassEncoderSetPipeline(compute_pass_encoder, compute_pipeline);
            this.ComputePassEncoderSetBindGroup(compute_pass_encoder, 0u, bindGroup, unativeint 0, Unchecked.defaultof<nativeptr<_>>)
            this.ComputePassEncoderDispatchWorkgroups(compute_pass_encoder, uint x, uint y, uint z)
            this.ComputePassEncoderEnd(compute_pass_encoder)
            
            let mutable command_buffer = 
                this.EncoderFinish(command_encoder, CommandBufferDescriptor())
            // this.QueueWriteBuffer // todo
            // this.QueueSubmit // todo
            // this.BufferMapAsync // todo (use the ShaderMapVar / ShaderMap type)
            ()
    type Device' with
        member this.CreateCompute shader entryPoint binds =
            this.Wgpu.CreateCompute shader entryPoint this.Device binds
    type WebGPU' with
        member this.CreateBinderS (shader: 'a -> _) =
            let m = Setup.compileModule shader
            let code = Compiler.Print.module' m
            let info = { wgpu = this; code = code }
            ShaderBinder<'a>(info, [])
        member this.CreateBinder (shader: 'a * 'b -> _) =
            let m = Setup.compileModule shader
            let code = Compiler.Print.module' m
            let info = { wgpu = this; code = code }
            ShaderBinder<'a, 'b>(info, [])
        member this.CreateBinder (shader: 'a * 'b * 'c -> _) =
            let m = Setup.compileModule shader
            let code = Compiler.Print.module' m
            let info = { wgpu = this; code = code }
            ShaderBinder<'a, 'b, 'c>(info, [])
        member this.CreateBinder (shader: 'a * 'b * 'c * 'd -> _) =
            let m = Setup.compileModule shader
            let code = Compiler.Print.module' m
            let info = { wgpu = this; code = code }
            ShaderBinder<'a, 'b, 'c, 'd>(info, [])
        member this.CreateBinder (shader: 'a * 'b * 'c * 'd * 'e -> _) =
            let m = Setup.compileModule shader
            let code = Compiler.Print.module' m
            printfn $"{code}"
            let info = { wgpu = this; code = code }
            ShaderBinder<'a, 'b, 'c, 'd, 'e>(info, [])
        member this.CreateBinder (shader: 'a * 'b * 'c * 'd * 'e * 'f -> _) =
            let m = Setup.compileModule shader
            let code = Compiler.Print.module' m
            printfn $"{code}"
            let info = { wgpu = this; code = code }
            ShaderBinder<'a, 'b, 'c, 'd, 'e, 'f>(info, [])
        member this.CreateBinder (shader: 'a * 'b * 'c * 'd * 'e * 'f * 'g -> _) =
            let m = Setup.compileModule shader
            let code = Compiler.Print.module' m
            printfn $"{code}"
            let info = { wgpu = this; code = code }
            ShaderBinder<'a, 'b, 'c, 'd, 'e, 'f, 'g>(info, [])
        member this.CreateBinder (shader: 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h -> _) =
            let m = Setup.compileModule shader
            let code = Compiler.Print.module' m
            printfn $"{code}"
            let info = { wgpu = this; code = code }
            ShaderBinder<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h>(info, [])
        member this.CreateBinder (shader: Quotations.Expr<'a * 'b * 'c * 'd -> _>) =
            // failwith ""
            // Unchecked.defaultof<ShaderBinder<'a, 'b, 'c, 'd>>
            let m = Compiler.translateModule shader Compiler.Module.empty
            // let m = Setup.compileModule shader
            let code = Compiler.Print.module' m
            let info = { wgpu = this; code = code }
            ShaderBinder<'a, 'b, 'c, 'd>(info, [])
    type ComputePipeline(name, bindings: ShaderWithBindings) =
        let device = bindings.Info.wgpu.Device
        let group = device.CreateCompute bindings.Info.code name bindings.Buffers
        member this.Begin(x, y, z) = fun fnEncoder fn ->
            let encoder = device.CreateCommandEncoder()
            let computePassEncoder = encoder.BeginComputePass()
            computePassEncoder.SetPipeline group.pipeline
            computePassEncoder.SetBindGroup group.bindGroup 0u
            computePassEncoder.DispatchWorkgroups x y z
            computePassEncoder.End ()
            // todo
            // output.AddCopy (wgpu, encoder.Encoder)
            fnEncoder encoder.Encoder
            let mutable commandBuffer = device.Wgpu.EncoderFinish(encoder.Encoder) // todo
            let queue = device.Wgpu.DeviceGetQueue(device.Device)
            fn queue
            device.Wgpu.QueueSubmit(queue, unativeint 1, &&commandBuffer)
            device.Wgpu.QueueRelease(queue)
            device.Wgpu.CommandBufferRelease(commandBuffer)
            device.Wgpu.ComputePassEncoderRelease(computePassEncoder.Encoder)
            device.Wgpu.CommandEncoderRelease(encoder.Encoder);
        interface System.IDisposable with
            member this.Dispose() =
                device.Wgpu.BindGroupRelease(group.bindGroup);
                device.Wgpu.BindGroupLayoutRelease(group.bindGroupLayout);
                device.Wgpu.ComputePipelineRelease(group.pipeline)
                device.Wgpu.ShaderModuleRelease(group.shaderModule)
//     
// [<AutoOpen>]
// module Extensions' =
//     type ShaderWithBindings with
//         member this.ComputePipeline(device, code, functionName) =
//             new ComputePipeline(device, code, functionName, this.Buffers)