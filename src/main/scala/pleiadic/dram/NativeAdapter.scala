package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

class NativeAdapterCommand(addressWidth: Int) extends Bundle {
  val write = Bool()
  val address = UInt(addressWidth.W)
}

class NativeAdapterWriteData(dataWidth: Int) extends Bundle {
  val data = UInt(dataWidth.W)
  val byteEnable = UInt((dataWidth / 8).W)
}

class NativeAdapterReadData(dataWidth: Int) extends Bundle {
  val data = UInt(dataWidth.W)
}

/**
  * Optional single-transaction Native read-modify-write stage. Full writes and
  * reads pass through unchanged. A partial write is completed as an internal
  * read followed by a full-mask write, and its upstream write-data handshake
  * is delayed until that final write has reached the downstream Native port.
  */
class NativeReadModifyWrite(config: DramConfig) extends Module {
  private val nativeAddressWidth = config.addressBits - config.byteOffsetBits
  private val byteCount = config.dataBits / 8

  val io = IO(new Bundle {
    val inputCommand = Flipped(Decoupled(new NativeCommand(config)))
    val inputWriteData = Flipped(Decoupled(new NativeWriteData(config)))
    val outputReadData = Decoupled(new NativeReadData(config))
    val outputCommand = Decoupled(new NativeCommand(config))
    val outputWriteData = Decoupled(new NativeWriteData(config))
    val inputReadData = Flipped(Decoupled(new NativeReadData(config)))
  })

  private val Seq(sIdle, sClassifyWrite, sPassWriteCommand, sPassWriteData,
    sRmwReadCommand, sRmwReadData, sRmwWriteCommand, sRmwWriteData,
    sPassReadCommand, sPassReadData) = Enum(10)
  private val state = RegInit(sIdle)
  private val address = Reg(UInt(nativeAddressWidth.W))
  private val partialData = Reg(UInt(config.dataBits.W))
  private val partialByteEnable = Reg(UInt(byteCount.W))
  private val mergedData = Reg(UInt(config.dataBits.W))

  io.inputCommand.ready := false.B
  io.inputWriteData.ready := false.B
  io.outputReadData.valid := false.B
  io.outputReadData.bits.data := io.inputReadData.bits.data
  io.outputCommand.valid := false.B
  io.outputCommand.bits.write := false.B
  io.outputCommand.bits.address := address
  io.outputWriteData.valid := false.B
  io.outputWriteData.bits.data := 0.U
  io.outputWriteData.bits.byteEnable := 0.U
  io.inputReadData.ready := false.B

  switch(state) {
    is(sIdle) {
      io.inputCommand.ready := true.B
      when(io.inputCommand.fire) {
        address := io.inputCommand.bits.address
        state := Mux(io.inputCommand.bits.write, sClassifyWrite, sPassReadCommand)
      }
    }
    is(sClassifyWrite) {
      when(io.inputWriteData.valid) {
        when(io.inputWriteData.bits.byteEnable.andR) {
          state := sPassWriteCommand
        }.otherwise {
          partialData := io.inputWriteData.bits.data
          partialByteEnable := io.inputWriteData.bits.byteEnable
          state := sRmwReadCommand
        }
      }
    }
    is(sPassWriteCommand) {
      io.outputCommand.valid := true.B
      io.outputCommand.bits.write := true.B
      when(io.outputCommand.fire) { state := sPassWriteData }
    }
    is(sPassWriteData) {
      io.outputWriteData.valid := io.inputWriteData.valid
      io.outputWriteData.bits.data := io.inputWriteData.bits.data
      io.outputWriteData.bits.byteEnable := io.inputWriteData.bits.byteEnable
      io.inputWriteData.ready := io.outputWriteData.ready
      when(io.outputWriteData.fire) { state := sIdle }
    }
    is(sRmwReadCommand) {
      io.outputCommand.valid := true.B
      io.outputCommand.bits.write := false.B
      when(io.outputCommand.fire) { state := sRmwReadData }
    }
    is(sRmwReadData) {
      io.inputReadData.ready := true.B
      when(io.inputReadData.fire) {
        val byteMask = Cat((0 until byteCount).reverse.map { byte =>
          Fill(8, partialByteEnable(byte))
        })
        mergedData := (io.inputReadData.bits.data & ~byteMask) |
          (partialData & byteMask)
        state := sRmwWriteCommand
      }
    }
    is(sRmwWriteCommand) {
      io.outputCommand.valid := true.B
      io.outputCommand.bits.write := true.B
      when(io.outputCommand.fire) { state := sRmwWriteData }
    }
    is(sRmwWriteData) {
      io.outputWriteData.valid := io.inputWriteData.valid
      io.outputWriteData.bits.data := mergedData
      io.outputWriteData.bits.byteEnable := Fill(byteCount, 1.U(1.W))
      io.inputWriteData.ready := io.outputWriteData.ready
      when(io.inputWriteData.valid) {
        assert(io.inputWriteData.bits.data === partialData &&
          io.inputWriteData.bits.byteEnable === partialByteEnable,
          "Native partial write changed before RMW completion")
      }
      when(io.outputWriteData.fire) { state := sIdle }
    }
    is(sPassReadCommand) {
      io.outputCommand.valid := true.B
      io.outputCommand.bits.write := false.B
      when(io.outputCommand.fire) { state := sPassReadData }
    }
    is(sPassReadData) {
      io.outputReadData.valid := io.inputReadData.valid
      io.inputReadData.ready := io.outputReadData.ready
      when(io.outputReadData.fire) { state := sIdle }
    }
  }
}

/**
  * Splits each wide native transaction into `inputWidth / outputWidth`
  * consecutive narrow transactions and joins narrow read responses.
  */
class NativeDownConverter(inputAddressWidth: Int, inputDataWidth: Int, outputDataWidth: Int,
    reverse: Boolean = false) extends Module {
  require(inputAddressWidth >= 1)
  require(inputDataWidth > outputDataWidth)
  require(inputDataWidth % outputDataWidth == 0)
  require(inputDataWidth % 8 == 0 && outputDataWidth % 8 == 0)
  private val ratio = inputDataWidth / outputDataWidth
  require((ratio & (ratio - 1)) == 0, "width ratio must be a power of two")
  private val ratioBits = log2Ceil(ratio)
  private val outputAddressWidth = inputAddressWidth + ratioBits
  private val indexWidth = log2Ceil(ratio max 2)

  val io = IO(new Bundle {
    val inputCommand = Flipped(Decoupled(new NativeAdapterCommand(inputAddressWidth)))
    val outputCommand = Decoupled(new NativeAdapterCommand(outputAddressWidth))
    val inputWriteData = Flipped(Decoupled(new NativeAdapterWriteData(inputDataWidth)))
    val outputWriteData = Decoupled(new NativeAdapterWriteData(outputDataWidth))
    val inputReadData = Flipped(Decoupled(new NativeAdapterReadData(outputDataWidth)))
    val outputReadData = Decoupled(new NativeAdapterReadData(inputDataWidth))
  })

  private val commandActive = RegInit(false.B)
  private val command = Reg(new NativeAdapterCommand(inputAddressWidth))
  private val commandIndex = RegInit(0.U(indexWidth.W))
  io.inputCommand.ready := !commandActive
  when(io.inputCommand.fire) {
    command := io.inputCommand.bits
    commandIndex := 0.U
    commandActive := true.B
  }
  io.outputCommand.valid := commandActive
  io.outputCommand.bits.write := command.write
  io.outputCommand.bits.address := Cat(command.address, 0.U(ratioBits.W)) + commandIndex
  when(io.outputCommand.fire) {
    when(commandIndex === (ratio - 1).U) { commandActive := false.B }
      .otherwise { commandIndex := commandIndex + 1.U }
  }

  private val writeActive = RegInit(false.B)
  private val writeData = Reg(new NativeAdapterWriteData(inputDataWidth))
  private val writeIndex = RegInit(0.U(indexWidth.W))
  private val writeLane = Mux(reverse.B, (ratio - 1).U - writeIndex, writeIndex)
  io.inputWriteData.ready := !writeActive
  when(io.inputWriteData.fire) {
    writeData := io.inputWriteData.bits
    writeIndex := 0.U
    writeActive := true.B
  }
  io.outputWriteData.valid := writeActive
  io.outputWriteData.bits.data :=
    (writeData.data >> (writeLane * outputDataWidth.U))(outputDataWidth - 1, 0)
  io.outputWriteData.bits.byteEnable :=
    (writeData.byteEnable >> (writeLane * (outputDataWidth / 8).U))(outputDataWidth / 8 - 1, 0)
  when(io.outputWriteData.fire) {
    when(writeIndex === (ratio - 1).U) { writeActive := false.B }
      .otherwise { writeIndex := writeIndex + 1.U }
  }

  private val readLanes = Reg(Vec(ratio, UInt(outputDataWidth.W)))
  private val readIndex = RegInit(0.U(indexWidth.W))
  private val readOutputValid = RegInit(false.B)
  private val readOutput = Reg(UInt(inputDataWidth.W))
  private val readLane = Mux(reverse.B, (ratio - 1).U - readIndex, readIndex)
  io.inputReadData.ready := !readOutputValid
  when(io.inputReadData.fire) {
    readLanes(readLane) := io.inputReadData.bits.data
    when(readIndex === (ratio - 1).U) {
      val assembled = Wire(Vec(ratio, UInt(outputDataWidth.W)))
      assembled := readLanes
      assembled(readLane) := io.inputReadData.bits.data
      readOutput := assembled.asUInt
      readOutputValid := true.B
      readIndex := 0.U
    }.otherwise { readIndex := readIndex + 1.U }
  }
  when(io.outputReadData.fire) { readOutputValid := false.B }
  io.outputReadData.valid := readOutputValid
  io.outputReadData.bits.data := readOutput
}

/**
  * Maps narrow transactions into lanes of a wider native port. Partial writes
  * use byte enables, so functional behavior does not require read-modify-write.
  * Commands are not coalesced; this preserves ordering under arbitrary gaps.
  */
class NativeUpConverter(inputAddressWidth: Int, inputDataWidth: Int, outputDataWidth: Int,
    reverse: Boolean = false, tagDepth: Int = 16) extends Module {
  require(inputAddressWidth >= 2 && tagDepth >= 1)
  require(outputDataWidth > inputDataWidth)
  require(outputDataWidth % inputDataWidth == 0)
  require(inputDataWidth % 8 == 0 && outputDataWidth % 8 == 0)
  private val ratio = outputDataWidth / inputDataWidth
  require((ratio & (ratio - 1)) == 0, "width ratio must be a power of two")
  private val ratioBits = log2Ceil(ratio)
  require(inputAddressWidth > ratioBits)
  private val outputAddressWidth = inputAddressWidth - ratioBits

  val io = IO(new Bundle {
    val inputCommand = Flipped(Decoupled(new NativeAdapterCommand(inputAddressWidth)))
    val outputCommand = Decoupled(new NativeAdapterCommand(outputAddressWidth))
    val inputWriteData = Flipped(Decoupled(new NativeAdapterWriteData(inputDataWidth)))
    val outputWriteData = Decoupled(new NativeAdapterWriteData(outputDataWidth))
    val inputReadData = Flipped(Decoupled(new NativeAdapterReadData(outputDataWidth)))
    val outputReadData = Decoupled(new NativeAdapterReadData(inputDataWidth))
    val flush = Input(Bool())
  })

  private val writeTags = Module(new Queue(UInt(ratioBits.W), tagDepth))
  private val readTags = Module(new Queue(UInt(ratioBits.W), tagDepth))
  private val rawLane = io.inputCommand.bits.address(ratioBits - 1, 0)
  private val commandLane = Mux(reverse.B, (ratio - 1).U - rawLane, rawLane)
  private val selectedTagReady = Mux(io.inputCommand.bits.write,
    writeTags.io.enq.ready, readTags.io.enq.ready)

  io.outputCommand.valid := io.inputCommand.valid && selectedTagReady
  io.outputCommand.bits.write := io.inputCommand.bits.write
  io.outputCommand.bits.address := io.inputCommand.bits.address(inputAddressWidth - 1, ratioBits)
  io.inputCommand.ready := io.outputCommand.ready && selectedTagReady

  writeTags.io.enq.valid := io.inputCommand.fire && io.inputCommand.bits.write
  writeTags.io.enq.bits := commandLane
  readTags.io.enq.valid := io.inputCommand.fire && !io.inputCommand.bits.write
  readTags.io.enq.bits := commandLane

  private val writeShiftBits = writeTags.io.deq.bits * inputDataWidth.U
  private val maskShiftBits = writeTags.io.deq.bits * (inputDataWidth / 8).U
  io.outputWriteData.valid := io.inputWriteData.valid && writeTags.io.deq.valid
  io.outputWriteData.bits.data := (io.inputWriteData.bits.data << writeShiftBits)(outputDataWidth - 1, 0)
  io.outputWriteData.bits.byteEnable :=
    (io.inputWriteData.bits.byteEnable << maskShiftBits)(outputDataWidth / 8 - 1, 0)
  io.inputWriteData.ready := io.outputWriteData.ready && writeTags.io.deq.valid
  writeTags.io.deq.ready := io.outputWriteData.fire

  private val readShiftBits = readTags.io.deq.bits * inputDataWidth.U
  io.outputReadData.valid := io.inputReadData.valid && readTags.io.deq.valid
  io.outputReadData.bits.data :=
    (io.inputReadData.bits.data >> readShiftBits)(inputDataWidth - 1, 0)
  io.inputReadData.ready := io.outputReadData.ready && readTags.io.deq.valid
  readTags.io.deq.ready := io.outputReadData.fire

  // No partial aggregate is retained in this functional converter, so flush
  // has no state to commit. It is present for Native-port API compatibility.
  dontTouch(io.flush)
}
