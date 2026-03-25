import React from "react"
import * as Gfx from "./Gfx.ts"
import shaderSource from "./shader.wgsl?raw"

const l = 32

const info: Gfx.BindingDetails[] = [
  { label: "Config", size: 4 * 4 * 8, ...Gfx.READ },
  { label: "Voxels", size: l * l * 3 * 4, ...Gfx.READ },
  { label: "Output", size: 4 * 800 * 600, ...Gfx.READ_WRITE },
  { label: "Atomic Output", size: 10000, ...Gfx.READ_WRITE },
]

class Engine {
  g: Gfx.GfxInstance
  canvas: HTMLCanvasElement
  rpl: GPURenderPipeline
  bindings: ReturnType<typeof Gfx.createBindings>

  constructor(g: Gfx.GfxInstance, canvas: HTMLCanvasElement, shaderSource: string) {
    this.g = g
    this.canvas = canvas
    this.bindings = Gfx.createBindings(g.device, info)
    this.rpl = Gfx.crateRenderPipeline(g, shaderSource, this.bindings.layout)
  }

  renderFrame() {
    var commandEncoder = this.g.device.createCommandEncoder()
    var passEncoder = commandEncoder.beginRenderPass(Gfx.createRenderPassDescriptor(this.g))
    passEncoder.setPipeline(this.rpl)
    passEncoder.setBindGroup(0, this.bindings.group)
    passEncoder.draw(6, 1, 0, 0)
    passEncoder.end()
    this.g.device.queue.submit([commandEncoder.finish()])
  }
  // todo : read atomic output buffer by creating a buffer and mapping it or w/e
  readOutput(cb: (data: ArrayBuffer) => void | Promise<void>) {
    var readOutputBuffer = this.g.device.createBuffer({
      size: this.bindings.buffers[2].size,
      ...Gfx.MAPPED,
    })

    var encoder = this.g.device.createCommandEncoder()
    encoder.copyBufferToBuffer(this.bindings.buffers[2], 0, readOutputBuffer, 0, readOutputBuffer.size)
    this.g.device.queue.submit([encoder.finish()])

    readOutputBuffer.mapAsync(GPUMapMode.READ).then(() => {
      const copyArrayBuffer = readOutputBuffer.getMappedRange()
      var possiblePromise = cb(copyArrayBuffer)
      if (possiblePromise instanceof Promise) {
        possiblePromise.then(() => readOutputBuffer.unmap())
      } else {
        readOutputBuffer.unmap()
      }
    })
  }
}

export function Game() {
  const canvasRef = React.useRef<HTMLCanvasElement>(null)
  const wasInit = React.useRef(false)
  const engine = React.useRef<Engine>(null)

  React.useEffect(() => {
    if (!canvasRef.current || wasInit.current) return
    wasInit.current = true
    Gfx.setupWgpu(canvasRef.current)
      .then(async (g) => {
        console.log("engine setup complete", g)
        console.log("shader source", shaderSource)
        engine.current = new Engine(g, canvasRef.current!, shaderSource)
        engine.current.renderFrame()
      })
      .catch((e) => console.log(e))
  })
  return (
    <div>
      <h1>Game</h1>
      <canvas ref={canvasRef} />
    </div>
  )
}
