package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

class DmaReadRequest(config: DramConfig) extends Bundle {
  val address = UInt((config.addressBits - config.byteOffsetBits).W)
  val last = Bool()
}

class DmaReadData(config: DramConfig) extends Bundle {
  val data = UInt(config.dataBits.W)
  val last = Bool()
}

class DmaWriteRequest(config: DramConfig) extends Bundle {
  val address = UInt((config.addressBits - config.byteOffsetBits).W)
  val data = UInt(config.dataBits.W)
  val byteEnable = UInt((config.dataBits / 8).W)
  val last = Bool()
}

/** Native DMA reader with an ordered outstanding-request reservation FIFO. */
class LiteDramDmaReader(config: DramConfig, fifoDepth: Int = 16) extends Module {
  require(fifoDepth >= 1)

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val request = Flipped(Decoupled(new DmaReadRequest(config)))
    val data = Decoupled(new DmaReadData(config))
    val nativeCommand = Decoupled(new NativeCommand(config))
    val nativeReadData = Flipped(Decoupled(new NativeReadData(config)))
  })

  private val reservations = Module(new Queue(Bool(), fifoDepth))
  private val returnedData = Module(new Queue(new NativeReadData(config), fifoDepth))

  io.nativeCommand.valid := io.enable && io.request.valid && reservations.io.enq.ready
  io.nativeCommand.bits.write := false.B
  io.nativeCommand.bits.address := io.request.bits.address
  io.request.ready := io.enable && io.nativeCommand.ready && reservations.io.enq.ready

  reservations.io.enq.valid := io.nativeCommand.fire
  reservations.io.enq.bits := io.request.bits.last
  returnedData.io.enq <> io.nativeReadData

  io.data.valid := io.enable && reservations.io.deq.valid && returnedData.io.deq.valid
  io.data.bits.data := returnedData.io.deq.bits.data
  io.data.bits.last := reservations.io.deq.bits

  private val consume = returnedData.io.deq.valid && reservations.io.deq.valid &&
    (io.data.ready || !io.enable)
  returnedData.io.deq.ready := consume
  reservations.io.deq.ready := consume
}

private class DmaWritePayload(config: DramConfig) extends Bundle {
  val data = UInt(config.dataBits.W)
  val byteEnable = UInt((config.dataBits / 8).W)
}

/** Native DMA writer with independently buffered write data. */
class LiteDramDmaWriter(config: DramConfig, fifoDepth: Int = 16) extends Module {
  require(fifoDepth >= 1)

  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new DmaWriteRequest(config)))
    val nativeCommand = Decoupled(new NativeCommand(config))
    val nativeWriteData = Decoupled(new NativeWriteData(config))
    val busy = Output(Bool())
  })

  private val dataFifo = Module(new Queue(new DmaWritePayload(config), fifoDepth))

  // A request is accepted only when both the Native command and its future
  // write-data FIFO entry can be reserved in the same cycle.
  io.nativeCommand.valid := io.request.valid && dataFifo.io.enq.ready
  io.nativeCommand.bits.write := true.B
  io.nativeCommand.bits.address := io.request.bits.address
  io.request.ready := io.nativeCommand.ready && dataFifo.io.enq.ready
  dataFifo.io.enq.valid := io.request.valid && io.nativeCommand.ready
  dataFifo.io.enq.bits.data := io.request.bits.data
  dataFifo.io.enq.bits.byteEnable := io.request.bits.byteEnable
  when(dataFifo.io.enq.fire) { assert(io.nativeCommand.fire) }

  io.nativeWriteData.valid := dataFifo.io.deq.valid
  io.nativeWriteData.bits.data := dataFifo.io.deq.bits.data
  io.nativeWriteData.bits.byteEnable := dataFifo.io.deq.bits.byteEnable
  dataFifo.io.deq.ready := io.nativeWriteData.ready
  io.busy := dataFifo.io.deq.valid

  // Native has no explicit burst-last signal; last remains a stream-side
  // descriptor used by higher-level address generators.
  dontTouch(io.request.bits.last)
}
