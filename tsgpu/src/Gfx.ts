interface GfxInstance {
  device: GPUDevice
  context: GPUCanvasContext
  format: GPUTextureFormat
  // pipeline: GPURenderPipeline
}

interface BindingInfo {
  label: string
  type: GPUBufferBindingType
  visibility: GPUShaderStageFlags
  size: number
  usage: GPUBufferUsageFlags
}

const READ = {
  usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST,
  type: "read-only-storage" as GPUBufferBindingType,
  // todo : not sure if this should be vertex | fragment or just fragment?
  visibility: GPUShaderStage.FRAGMENT | GPUShaderStage.VERTEX,
}
const READ_WRITE = {
  usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC,
  type: "storage" as GPUBufferBindingType,
  visibility: GPUShaderStage.FRAGMENT,
}
const MAPPED = {
  usage: GPUBufferUsage.MAP_READ | GPUBufferUsage.COPY_DST,
  type: "storage" as GPUBufferBindingType,
  visibility: GPUShaderStage.FRAGMENT,
}

async function setupWgpu(canvas: HTMLCanvasElement): Promise<GfxInstance> {
  const adapter =
    (await navigator.gpu.requestAdapter()) ||
    (() => {
      throw new Error("WebGPU not supported")
    })()
  const device = await adapter.requestDevice()
  const context = canvas.getContext("webgpu") as GPUCanvasContext
  const format = navigator.gpu.getPreferredCanvasFormat()
  context.configure({ device, format })
  return { device, context, format }
}

function createBindings(device: GPUDevice, descriptors: BindingInfo[]) {
  const buffers: GPUBuffer[] = []
  for (const desc of descriptors) {
    const buffer = device.createBuffer({
      size: desc.size,
      label: desc.label,
      usage: desc.usage,
    })
    buffers.push(buffer)
  }
  const layout = device.createBindGroupLayout({
    entries: descriptors.map((desc, i) => ({
      binding: i,
      visibility: desc.visibility,
      buffer: { type: desc.type },
    })),
  })
  const group = device.createBindGroup({
    layout,
    entries: buffers.map((buffer, i) => ({
      binding: i,
      resource: { buffer },
    })),
  })
  return { buffers, group, layout }
}

function crateRenderPipeline(
  g: GfxInstance,
  shaderSource: string,
  bgl: GPUBindGroupLayout,
  // bindings: ReturnType<typeof createBindings>,
) {
  return g.device.createRenderPipeline({
    layout: g.device.createPipelineLayout({
      bindGroupLayouts: [bgl],
    }),
    vertex: {
      module: g.device.createShaderModule({ code: shaderSource }),
      entryPoint: "vertex",
    },
    fragment: {
      module: g.device.createShaderModule({ code: shaderSource }),
      entryPoint: "fragment",
      targets: [{ format: g.format }],
    },
    primitive: {
      topology: "triangle-list",
    },
  })
}

function createRenderPassDescriptor(g: GfxInstance): GPURenderPassDescriptor {
  return {
    colorAttachments: [
      {
        view: g.context.getCurrentTexture().createView(),
        loadOp: "clear",
        storeOp: "store",
      },
    ],
  }
}

export {
  setupWgpu,
  createBindings,
  crateRenderPipeline,
  createRenderPassDescriptor,
  READ,
  READ_WRITE,
  MAPPED,
}
export type { GfxInstance, BindingInfo }
