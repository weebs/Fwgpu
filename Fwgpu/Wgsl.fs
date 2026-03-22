module rec Wgsl

open System
open System.Diagnostics
open System.Numerics
open Microsoft.FSharp.Core
open Microsoft.FSharp.Reflection
open type Quotations.Expr
open System.Runtime.InteropServices
module Patterns = Quotations.Patterns


[<AbstractClass>]
type WgslAttribute() =
    inherit Attribute()
    abstract member Serialize : string
type LocationAttribute(index: int) =
    inherit WgslAttribute()
    override this.Serialize = $"@location({index})"
type VarType =
    | Uniform
    | Storage
    | ReadWrite
type Builtin =
    | Position 
    | VertexIndex
type Builtin' =
    | position = 0
    | vertex_index = 1
    | global_invocation_id = 2
// let Position = { new Builtin with member _.Foo = () }
// type BuiltInAttribute<'t when 't :> Builtin>(name: string) = inherit Attribute()
type ComputeAttribute() =
    inherit WgslAttribute()
    override this.Serialize = "@compute"
type VertexAttribute() =
    inherit WgslAttribute()
    override this.Serialize = "@vertex"
type FragmentAttribute() =
    inherit WgslAttribute()
    override this.Serialize = "@fragment"
type WorkgroupSizeAttribute(a, b, c) =
    inherit WgslAttribute()
    new a = WorkgroupSizeAttribute(a, a, a)
    override this.Serialize = $"@workgroup_size({a}, {b}, {c})"
    
type BuiltInAttribute(value: Builtin') =
    inherit WgslAttribute()
    override this.Serialize = $"@builtin({value})"
type vec2<'t when
    't :> IAdditionOperators<'t, 't, 't> and
    't :> IMultiplyOperators<'t, 't, 't> and
    't :> IDivisionOperators<'t, 't, 't> and
    't :> ISubtractionOperators<'t, 't, 't>>
    = { x: 't; y: 't }
    with
    member this.xyy = Operators.Unchecked.defaultof<vec3<'t>>
    member this.xxx = Operators.Unchecked.defaultof<vec3<'t>>
    member this.yxy = Operators.Unchecked.defaultof<vec3<'t>>
    member this.yyx = Operators.Unchecked.defaultof<vec3<'t>>
    member this.xxy = Operators.Unchecked.defaultof<vec3<'t>>
    member this.yxx = Operators.Unchecked.defaultof<vec3<'t>>
    member this.xyx = Operators.Unchecked.defaultof<vec3<'t>>
and [<Struct; StructLayout(LayoutKind.Sequential)>] vec4<'t when 't :> IAdditionOperators<'t, 't, 't> and 't : (static member (-) : 't * 't -> 't)> = 
    { mutable x: 't; mutable y: 't; mutable z: 't; mutable w: 't }
// type vec4<'t>(x: 't, y: 't, z: 't, a: 't) =
//     member this.X with get () = x and set value = ()
//     member this.Y with get () = y and set value = ()
//     member this.Z with get () = z and set value = ()
//     member this.A with get () = a and set value = ()
and Numeric<'t when
    't :> IAdditionOperators<'t, 't, 't> and
    't :> IMultiplyOperators<'t, 't, 't> and
    't :> ISubtractionOperators<'t, 't, 't> and
    't :> IDivisionOperators<'t, 't, 't>> = 't
// and Vec3<'t when 't :> Numeric<'t>> = unit
// and Vec(value: Numeric<'t>) = class end
and vec3<'t when
    't :> IAdditionOperators<'t, 't, 't> and
    't :> IMultiplyOperators<'t, 't, 't> and
    't :> ISubtractionOperators<'t, 't, 't> and
    't :> IDivisionOperators<'t, 't, 't>
    // 't : (static member (-) : 't * 't -> 't)
    // (x: 't, y: 't, z: 't) =
    > = { mutable x: 't; mutable y: 't; mutable z: 't } with
    member this.xxyy = Unchecked.defaultof<vec4<'t>>
    member this.yzzx = Unchecked.defaultof<vec4<'t>>
    static member (+)
        (v3: vec3<'t>, v3': vec3<'t>) = { x = v3.x + v3'.x; y = v3.y + v3'.y; z = v3.z + v3'.z }
    static member (*)
        (v3: vec3<'t>, scale: 't) = { x = v3.x * scale; y = v3.y * scale; z = v3.z * scale }
    static member (*)
        (scale: 't, v3: vec3<'t>) = { x = v3.x * scale; y = v3.y * scale; z = v3.z * scale }
    static member (/)
        (v3: vec3<'t>, scale: 't) = { x = v3.x / scale; y = v3.y / scale; z = v3.z / scale }
    static member (/)
        // (scale: 't, v3: vec3<'t>) = { x = v3.x / scale; y = v3.y / scale; z = v3.z / scale }
        (scale: 't, v3: vec3<'t>) = { x = scale / v3.x; y = scale / v3.y; z = scale / v3.z }
    static member (/)
        (v3: vec3<'t>, v3': vec3<'t>) = { x = v3.x / v3'.x; y = v3.y / v3'.y; z = v3.z / v3'.z }
    static member op_Multiplication
        (v3: vec3<'t>, scale: 't) = { x = v3.x * scale; y = v3.y * scale; z = v3.z * scale }
    static member op_Addition
        (v3: vec3<'t>, scale: 't) = { x = v3.x + scale; y = v3.y + scale; z = v3.z + scale }
    static member op_Multiplication
        (scale: 't, v3: vec3<'t>) = { x = v3.x * scale; y = v3.y * scale; z = v3.z * scale }
    static member op_Multiply
        (v3: vec3<'t>, v3': vec3<'t>) =
        { x = v3.x * v3'.x; y = v3.y * v3'.y; z = v3.z * v3'.z }
    static member op_Subtraction
        // <^a when
        // ^a :> IAdditionOperators<^a,^a,^a> and
        // ^a : (static member (-) : ^a * ^a -> ^a)>
        // (v3: vec3<^a>, v3': vec3<^a>) =
        (v3: vec3<'t>, v3': vec3<'t>) =
        { x = v3.x - v3'.x; y = v3.y - v3'.y; z = v3.z - v3'.z }
    static member op_Subtraction
        (v3: vec3<'t>, f: 't) =
        { x = v3.x - f; y = v3.y - f; z = v3.z - f }
    member this.yzx = { x = this.y; y = this.z; z = this.x }
    member this.zxy = { x = this.z; y = this.x; z = this.y }
    // member this.x with get () = x and set value = ()
    // member this.y with get () = y and set value = ()
    // member this.z with get () = z and set value = ()
type vec3f = vec3<float32>
type mat2x2<'t>(mx: 't, my: 't, ma: 't, mb: 't) =
    static member (*) (m: mat2x2<float32>, b: float32) = m
type Foo<'t> = 't
type Wgsl =
    static member abs(p: vec4<float32>) = failwith ""
    static member abs(p: vec3f) = failwith ""
    static member abs(f: float32) = MathF.Abs(f)
    static member abs(f: vec3f) : vec3f = Wgsl.vec3(abs(f.x), abs(f.y), abs(f.z))
    static member round(f: vec3f) : vec3f = failwith ""
    static member arrayLength(values: 't[]) = values.Length
    static member step (a: vec4<int>, b: vec4<int>) : vec4<int> = failwith ""
    static member step (a: vec4<float32>, b: vec4<float32>) : vec4<float32> = failwith ""
    static member step (a: vec4<uint>, b: vec4<uint>) : vec4<uint> = failwith ""
    static member step (a: float32, b: vec3<float32>) : vec3<float32> = failwith ""
    static member step (a: vec3<float32>, b: vec3<float32>) : vec3<float32> = failwith ""
    static member min(a: float32, b: float32) = MathF.Min(a, b)
    static member max(a: vec3f, b: vec3f) = failwith ""
    static member max(a: vec4<float32>, b: vec4<float32>) : vec4<float32> = failwith ""
    static member max(a: vec4<float32>, b: float32) : vec4<float32> = failwith ""
    static member max(a: vec3<float32>, b: vec3<float32>) : vec3<float32> =
        Wgsl.vec3(max a.x b.x, max a.y b.y, max a.z b.z)
    static member min(a: vec3<float32>, b: vec3<float32>) : vec3<float32> =
        let x = min a.x b.x
        let y = min a.y b.y
        let z = min a.z b.z
        Wgsl.vec3(x, y, z)
    // static member max(a: vec3f, b: float32) = failwith ""
    static member max(a: float32, b: float32) = MathF.Max(a, b)
    static member inline vec2(a, b) = { x = a; y = b; }
    static member inline vec3(a) = { x = a; y = a; z = a }
    static member vec3(a: int, b, c) : vec3<int> = { x = a; y = b; z = c }
    static member vec3(a: float32, b, c) : vec3<float32> = { x = a; y = b; z = c }
    static member vec3(a: uint, b, c) : vec3<uint> = { x = a; y = b; z = c }
    static member inline lessThanEqual (a: vec3<'t>, b: vec3<'t>) =
        {
            x = if a.x <= b.x then 1f else 0f
            y = if a.y <= b.y then 1f else 0f
            z = if a.z <= b.z then 1f else 0f
        }
    static member inline greaterThan (a: vec3<'t>, b: vec3<'t>) =
        {
            x = if a.x > b.x then 1f else 0f
            y = if a.y > b.y then 1f else 0f
            z = if a.z > b.z then 1f else 0f
        }
    static member inline greaterThanEqual (a: vec3<'t>, b: vec3<'t>) =
        {
            x = if a.x >= b.x then 1f else 0f
            y = if a.y >= b.y then 1f else 0f
            z = if a.z >= b.z then 1f else 0f
        }
    static member inline lessThan (a: vec3<'t>, b: vec3<'t>) =
        {
            x = if a.x < b.x then 1f else 0f
            y = if a.y < b.y then 1f else 0f
            z = if a.z < b.z then 1f else 0f
        }
    static member inline vec4(a, b, c, d) = { x = a; y = b; z = c; w = d; }
    static member cross(a: vec3f, b: vec3f) : vec3f = failwith "TODO : Wgsl cross(v1, v2)"
    static member dot(a: vec3f, b: vec3f) : vec3f = failwith "TODO : Wgsl dot(v1, v2)"
    static member vec4(value: float32) = Wgsl.vec4(value, value, value, value)
    static member vec4(value: int32) = Wgsl.vec4(value, value, value, value)
    static member vec4(value: vec2<float32>, a, b) = Wgsl.vec4(value.x, value.y, a, b)
    static member vec4(value: vec2<int32>, a, b) = Wgsl.vec4(value.x, value.y, a, b)
    static member vec4(value: uint32) = Wgsl.vec4(value, value, value, value)
    static member vec4(value: vec3<int32>, a) = Wgsl.vec4(value.x, value.y, value.z, a)
    static member vec4(value: vec3<float32>, a) = Wgsl.vec4(value.x, value.y, value.z, a)
    static member length (v3: vec3f) = MathF.Sqrt((v3.x * v3.x) + (v3.y * v3.y) + (v3.z * v3.z))
    static member normalize (v3: vec3f) = v3 / Wgsl.length(v3)
    static member sqrt f = MathF.Sqrt f
    static member floor(a: float32) = MathF.Floor a
    static member ceil (a: float32) : float32 = MathF.Ceiling a
    static member ceil (a: vec3<float32>) : vec3<float32> = Wgsl.vec3(ceil(a.x), ceil(a.y), ceil(a.z))
    static member floor (a: vec3<float32>) : vec3<float32> = Wgsl.vec3(floor(a.x), floor(a.y), floor(a.z))
    static member extractBits (e: uint, offset: uint, count: uint) = (e <<< (31 - int offset)) >>> (31 - int count)
    static member sign (a: vec3<float32>) : vec3<float32> =
        {
            x = if a.x > 0f then 1f elif a.x < 0f then -1f else 0f
            y = if a.y > 0f then 1f elif a.y < 0f then -1f else 0f
            z = if a.z > 0f then 1f elif a.z < 0f then -1f else 0f
        }
