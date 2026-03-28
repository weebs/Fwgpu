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
    return vec4f(0.0, (input.uv.x + 1) * 0.5, (input.uv.y + 1) * 0.5, 1.0);
}