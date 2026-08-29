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

/** CSR-style contiguous word-address sequencer shared by controlled DMAs. */
private class DmaAddressSequencer(addressWidth: Int) extends Module {
  require(addressWidth >= 1)
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val base = Input(UInt(addressWidth.W))
    val length = Input(UInt(addressWidth.W))
    val loop = Input(Bool())
    val issue = Input(Bool())
    val ready = Input(Bool())
    val valid = Output(Bool())
    val address = Output(UInt(addressWidth.W))
    val last = Output(Bool())
    val done = Output(Bool())
    val offset = Output(UInt(addressWidth.W))
  })

  private val active = RegInit(false.B)
  private val done = RegInit(false.B)
  private val offset = RegInit(0.U(addressWidth.W))
  io.valid := active && io.issue
  io.address := io.base + offset
  io.last := offset === io.length - 1.U
  io.done := done
  io.offset := offset

  when(!io.enable) {
    active := false.B
    done := false.B
    offset := 0.U
  }.elsewhen(!active && !done) {
    assert(io.length =/= 0.U, "DMA control length must be non-zero")
    active := true.B
    offset := 0.U
  }.elsewhen(io.valid && io.ready) {
    when(io.last) {
      when(io.loop) {
        offset := 0.U
      }.otherwise {
        active := false.B
        done := true.B
      }
    }.otherwise {
      offset := offset + 1.U
    }
  }
}

/** Native DMA reader with a LiteX CSR-style base/length/enable/loop frontend. */
class LiteDramDmaReaderControl(config: DramConfig, fifoDepth: Int = 16) extends Module {
  private val addressWidth = config.addressBits - config.byteOffsetBits
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val base = Input(UInt(addressWidth.W))
    val length = Input(UInt(addressWidth.W))
    val loop = Input(Bool())
    val done = Output(Bool())
    val offset = Output(UInt(addressWidth.W))
    val data = Decoupled(new DmaReadData(config))
    val nativeCommand = Decoupled(new NativeCommand(config))
    val nativeReadData = Flipped(Decoupled(new NativeReadData(config)))
  })

  private val sequencer = Module(new DmaAddressSequencer(addressWidth))
  private val reader = Module(new LiteDramDmaReader(config, fifoDepth))
  sequencer.io.enable := io.enable
  sequencer.io.base := io.base
  sequencer.io.length := io.length
  sequencer.io.loop := io.loop
  sequencer.io.issue := true.B
  sequencer.io.ready := reader.io.request.ready
  reader.io.enable := io.enable
  reader.io.request.valid := sequencer.io.valid
  reader.io.request.bits.address := sequencer.io.address
  reader.io.request.bits.last := sequencer.io.last
  io.data <> reader.io.data
  io.nativeCommand <> reader.io.nativeCommand
  reader.io.nativeReadData <> io.nativeReadData
  io.done := sequencer.io.done
  io.offset := sequencer.io.offset
}

/** Native DMA writer with a LiteX CSR-style contiguous address frontend. */
class LiteDramDmaWriterControl(config: DramConfig, fifoDepth: Int = 16) extends Module {
  private val addressWidth = config.addressBits - config.byteOffsetBits
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val base = Input(UInt(addressWidth.W))
    val length = Input(UInt(addressWidth.W))
    val loop = Input(Bool())
    val done = Output(Bool())
    val offset = Output(UInt(addressWidth.W))
    val input = Flipped(Decoupled(UInt(config.dataBits.W)))
    val nativeCommand = Decoupled(new NativeCommand(config))
    val nativeWriteData = Decoupled(new NativeWriteData(config))
    val busy = Output(Bool())
  })

  private val sequencer = Module(new DmaAddressSequencer(addressWidth))
  private val writer = Module(new LiteDramDmaWriter(config, fifoDepth))
  sequencer.io.enable := io.enable
  sequencer.io.base := io.base
  sequencer.io.length := io.length
  sequencer.io.loop := io.loop
  sequencer.io.issue := io.input.valid
  sequencer.io.ready := writer.io.request.ready
  writer.io.request.valid := sequencer.io.valid
  writer.io.request.bits.address := sequencer.io.address
  writer.io.request.bits.data := io.input.bits
  writer.io.request.bits.byteEnable := Fill(config.dataBits / 8, 1.U(1.W))
  writer.io.request.bits.last := sequencer.io.last
  io.input.ready := sequencer.io.valid && writer.io.request.ready
  io.nativeCommand <> writer.io.nativeCommand
  io.nativeWriteData <> writer.io.nativeWriteData
  io.done := sequencer.io.done
  io.offset := sequencer.io.offset
  io.busy := io.enable && (writer.io.busy || !sequencer.io.done)
}
