module Shader

open Wgsl
open type Wgsl

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
