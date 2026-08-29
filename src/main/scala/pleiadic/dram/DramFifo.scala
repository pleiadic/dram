package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/** Ring-buffer occupancy and address controller used by the DRAM FIFO. */
class LiteDramFifoControl(config: DramConfig, baseAddress: BigInt, depth: Int) extends Module {
  require(depth >= 1)
  private val addressWidth = config.addressBits - config.byteOffsetBits
  require(baseAddress >= 0 && baseAddress + depth <= (BigInt(1) << addressWidth))
  private val pointerWidth = log2Ceil(depth.max(2))
  private val levelWidth = log2Ceil(depth + 1).max(1)

  val io = IO(new Bundle {
    val write = Input(Bool())
    val read = Input(Bool())
    val writable = Output(Bool())
    val readable = Output(Bool())
    val writeAddress = Output(UInt(addressWidth.W))
    val readAddress = Output(UInt(addressWidth.W))
    val level = Output(UInt(levelWidth.W))
  })

  private val writePointer = RegInit(0.U(pointerWidth.W))
  private val readPointer = RegInit(0.U(pointerWidth.W))
  private val level = RegInit(0.U(levelWidth.W))

  io.writable := level < depth.U
  io.readable := level =/= 0.U
  io.writeAddress := baseAddress.U(addressWidth.W) + writePointer
  io.readAddress := baseAddress.U(addressWidth.W) + readPointer
  io.level := level

  when(io.write) {
    assert(io.writable, "DRAM FIFO overflow")
    writePointer := Mux(writePointer === (depth - 1).U, 0.U, writePointer + 1.U)
  }
  when(io.read) {
    assert(io.readable, "DRAM FIFO underflow")
    readPointer := Mux(readPointer === (depth - 1).U, 0.U, readPointer + 1.U)
  }
  switch(Cat(io.write, io.read)) {
    is("b10".U) { level := level + 1.U }
    is("b01".U) { level := level - 1.U }
  }
}

/**
  * Equal-width DRAM-backed FIFO using independent Native write/read ports.
  * `depth` and `baseAddress` are expressed in Native words.
  */
class LiteDramFifo(config: DramConfig, baseAddress: BigInt, depth: Int,
    writerFifoDepth: Int = 16, readerFifoDepth: Int = 16) extends Module {
  require(depth >= 1)

  val io = IO(new Bundle {
    val input = Flipped(Decoupled(UInt(config.dataBits.W)))
    val output = Decoupled(UInt(config.dataBits.W))
    val level = Output(UInt(log2Ceil(depth + 1).max(1).W))

    val nativeWriteCommand = Decoupled(new NativeCommand(config))
    val nativeWriteData = Decoupled(new NativeWriteData(config))
    val nativeReadCommand = Decoupled(new NativeCommand(config))
    val nativeReadData = Flipped(Decoupled(new NativeReadData(config)))
  })

  private val control = Module(new LiteDramFifoControl(config, baseAddress, depth))
  private val writer = Module(new LiteDramDmaWriter(config, writerFifoDepth))
  private val reader = Module(new LiteDramDmaReader(config, readerFifoDepth))

  writer.io.request.valid := io.input.valid && control.io.writable
  writer.io.request.bits.address := control.io.writeAddress
  writer.io.request.bits.data := io.input.bits
  writer.io.request.bits.byteEnable := Fill(config.dataBits / 8, 1.U(1.W))
  writer.io.request.bits.last := true.B
  io.input.ready := writer.io.request.ready && control.io.writable
  control.io.write := writer.io.request.fire

  reader.io.enable := true.B
  reader.io.request.valid := control.io.readable
  reader.io.request.bits.address := control.io.readAddress
  reader.io.request.bits.last := true.B
  control.io.read := reader.io.request.fire
  io.output.valid := reader.io.data.valid
  io.output.bits := reader.io.data.bits.data
  reader.io.data.ready := io.output.ready
  io.level := control.io.level

  io.nativeWriteCommand <> writer.io.nativeCommand
  io.nativeWriteData <> writer.io.nativeWriteData
  io.nativeReadCommand <> reader.io.nativeCommand
  reader.io.nativeReadData <> io.nativeReadData
}

private class StreamWidthPacker(inputBits: Int, outputBits: Int) extends Module {
  require(inputBits >= 1 && outputBits >= inputBits && outputBits % inputBits == 0)
  private val ratio = outputBits / inputBits
  private val indexWidth = log2Ceil(ratio.max(2))

  val io = IO(new Bundle {
    val input = Flipped(Decoupled(UInt(inputBits.W)))
    val output = Decoupled(UInt(outputBits.W))
    val collecting = Output(Bool())
    val busy = Output(Bool())
  })

  private val lanes = Reg(Vec(ratio, UInt(inputBits.W)))
  private val index = RegInit(0.U(indexWidth.W))
  private val full = RegInit(false.B)
  private val last = index === (ratio - 1).U

  io.input.ready := !full
  io.output.valid := full
  io.output.bits := lanes.asUInt
  io.collecting := index =/= 0.U
  io.busy := full || io.collecting

  when(io.output.fire) { full := false.B }
  when(io.input.fire) {
    lanes(index) := io.input.bits
    when(last) {
      index := 0.U
      full := true.B
    }.otherwise {
      index := index + 1.U
    }
  }
}

private class StreamWidthUnpacker(inputBits: Int, outputBits: Int) extends Module {
  require(outputBits >= 1 && inputBits >= outputBits && inputBits % outputBits == 0)
  private val ratio = inputBits / outputBits
  private val indexWidth = log2Ceil(ratio.max(2))

  val io = IO(new Bundle {
    val input = Flipped(Decoupled(UInt(inputBits.W)))
    val output = Decoupled(UInt(outputBits.W))
    val last = Output(Bool())
  })

  private val data = Reg(UInt(inputBits.W))
  private val index = RegInit(0.U(indexWidth.W))
  private val full = RegInit(false.B)
  private val lanes = data.asTypeOf(Vec(ratio, UInt(outputBits.W)))

  io.input.ready := !full
  io.output.valid := full
  io.output.bits := lanes(index)
  io.last := index === (ratio - 1).U

  when(io.input.fire) {
    data := io.input.bits
    index := 0.U
    full := true.B
  }
  when(io.output.fire) {
    when(io.last) {
      full := false.B
    }.otherwise {
      index := index + 1.U
    }
  }
}

/**
  * Stream-width DRAM FIFO with LiteDRAM-style automatic bypass. Narrow words
  * initially cross the pre/post queues without touching DRAM. When output
  * backpressure leaves a complete Native word queued, the module packs words
  * low-lane first, stores them through the equal-width DRAM FIFO, and unpacks
  * them in order before returning to bypass mode.
  */
class LiteDramStreamFifo(config: DramConfig, streamDataBits: Int,
    baseAddress: BigInt, depth: Int, withBypass: Boolean = true,
    preFifoDepth: Int = 16, postFifoDepth: Int = 16,
    writerFifoDepth: Int = 16, readerFifoDepth: Int = 16) extends Module {
  require(streamDataBits >= 1 && config.dataBits % streamDataBits == 0)
  require(withBypass || streamDataBits == config.dataBits,
    "narrow stream conversion requires automatic bypass for partial-word draining")
  private val ratio = config.dataBits / streamDataBits
  private val preDepth = preFifoDepth.max(2 * ratio)
  private val postDepth = postFifoDepth.max(2 * ratio)
  private val outstandingLimit = depth + readerFifoDepth + 2
  private val outstandingWidth = log2Ceil(outstandingLimit + 1).max(1)

  val io = IO(new Bundle {
    val input = Flipped(Decoupled(UInt(streamDataBits.W)))
    val output = Decoupled(UInt(streamDataBits.W))
    val bypass = Output(Bool())
    val dramLevel = Output(UInt(log2Ceil(depth + 1).max(1).W))

    val nativeWriteCommand = Decoupled(new NativeCommand(config))
    val nativeWriteData = Decoupled(new NativeWriteData(config))
    val nativeReadCommand = Decoupled(new NativeCommand(config))
    val nativeReadData = Flipped(Decoupled(new NativeReadData(config)))
  })

  private val preFifo = Module(new Queue(UInt(streamDataBits.W), preDepth))
  private val postFifo = Module(new Queue(UInt(streamDataBits.W), postDepth))
  private val packer = Module(new StreamWidthPacker(streamDataBits, config.dataBits))
  private val unpacker = Module(new StreamWidthUnpacker(config.dataBits, streamDataBits))
  private val dramFifo = Module(new LiteDramFifo(config, baseAddress, depth,
    writerFifoDepth, readerFifoDepth))
  private val usingDram = RegInit((!withBypass).B)
  private val launched = RegInit(false.B)
  private val outstanding = RegInit(0.U(outstandingWidth.W))

  preFifo.io.enq <> io.input
  io.output <> postFifo.io.deq
  packer.io.output <> dramFifo.io.input
  dramFifo.io.output <> unpacker.io.input
  io.nativeWriteCommand <> dramFifo.io.nativeWriteCommand
  io.nativeWriteData <> dramFifo.io.nativeWriteData
  io.nativeReadCommand <> dramFifo.io.nativeReadCommand
  dramFifo.io.nativeReadData <> io.nativeReadData
  io.bypass := !usingDram
  io.dramLevel := dramFifo.io.level

  preFifo.io.deq.ready := false.B
  packer.io.input.valid := false.B
  packer.io.input.bits := preFifo.io.deq.bits
  postFifo.io.enq.valid := false.B
  postFifo.io.enq.bits := 0.U
  unpacker.io.output.ready := false.B

  when(!usingDram) {
    postFifo.io.enq.valid := preFifo.io.deq.valid
    postFifo.io.enq.bits := preFifo.io.deq.bits
    preFifo.io.deq.ready := postFifo.io.enq.ready
  }.otherwise {
    val canPack = packer.io.collecting || preFifo.io.count >= ratio.U
    packer.io.input.valid := preFifo.io.deq.valid && canPack
    preFifo.io.deq.ready := packer.io.input.ready && canPack
    postFifo.io.enq.valid := unpacker.io.output.valid
    postFifo.io.enq.bits := unpacker.io.output.bits
    unpacker.io.output.ready := postFifo.io.enq.ready
  }

  private val groupStored = dramFifo.io.input.fire
  private val groupReturned = unpacker.io.output.fire && unpacker.io.last
  switch(Cat(groupStored, groupReturned)) {
    is("b10".U) { outstanding := outstanding + 1.U }
    is("b01".U) {
      assert(outstanding =/= 0.U, "DRAM stream FIFO returned an untracked word")
      outstanding := outstanding - 1.U
    }
  }

  if (withBypass) {
    when(!usingDram && preFifo.io.count >= ratio.U && !postFifo.io.enq.ready) {
      usingDram := true.B
      launched := false.B
    }
    when(usingDram && groupStored) { launched := true.B }
    when(usingDram && launched && outstanding === 0.U && !packer.io.busy &&
        preFifo.io.count < ratio.U) {
      usingDram := false.B
      launched := false.B
    }
  }
}
