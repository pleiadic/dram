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
