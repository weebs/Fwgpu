@group(0) @binding(0) var<uniform> config: Config;
// 3 floats per voxel (rgb) for color
@group(0) @binding(1) var<storage, read> map: array<f32>;

struct Config {
    width: u32,
    height: u32,
    cameraX: f32,
    cameraY: f32,
    cameraZ: f32,
    cameraRotX: f32,
    cameraRotY: f32,
    focalLength: f32,
};

struct Output {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
};

@vertex
fn vertex(@builtin(vertex_index) index: u32) -> Output {
    var output: Output;
    // Rectangle
    let positions = array<vec2<f32>, 6>(
        vec2(-1, 1),
        vec2(-1, -1),
        vec2(1, -1),
        vec2(1, 1),
        vec2(-1, 1),
        vec2(1, -1),
    );
    let position = positions[index];
    output.position = vec4<f32>(position, 0.0, 1.0);
    // output.uv = position * vec2<f32>(0.5, -0.5) + vec2<f32>(0.5, 0.5);
    output.uv = position;
    return output;
}

@fragment
fn fragment(input: Output) -> @location(0) vec4<f32> {
    let pixelsPerMeter = 100.0;
    let rayDir = vec3<f32>(
        input.uv.x * f32(config.width) / pixelsPerMeter * 0.5,
        input.uv.y * f32(config.height) / pixelsPerMeter * 0.5,
        config.focalLength
    );
    let rotatedRay = vec3<f32>(
        rayDir.x * cos(config.cameraRotY) + rayDir.z * sin(config.cameraRotY),
        rayDir.y,
        -rayDir.x * sin(config.cameraRotY) + rayDir.z * cos(config.cameraRotY)
    );
    return vec4f(0.0, (input.uv.x + 1) * 0.5, (input.uv.y + 1) * 0.5, 1.0);
}