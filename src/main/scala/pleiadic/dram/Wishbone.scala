package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

object WishboneCycleType {
  val classic = 0.U(3.W)
  val constantAddressBurst = 1.U(3.W)
  val incrementingBurst = 2.U(3.W)
  val endOfBurst = 7.U(3.W)
}

class WishbonePort(addressWidth: Int, dataWidth: Int) extends Bundle {
  val cyc = Input(Bool())
  val stb = Input(Bool())
  val writeEnable = Input(Bool())
  val address = Input(UInt(addressWidth.W))
  val writeData = Input(UInt(dataWidth.W))
  val select = Input(UInt((dataWidth / 8).W))
  val cycleType = Input(UInt(3.W))
  val burstType = Input(UInt(2.W))
  val readData = Output(UInt(dataWidth.W))
  val acknowledge = Output(Bool())
  val error = Output(Bool())
  val stall = Output(Bool())
}

private class WishboneNativeBridge(addressWidth: Int, dataWidth: Int,
    baseWordAddress: BigInt) extends Module {
  val io = IO(new Bundle {
    val wishbone = new WishbonePort(addressWidth, dataWidth)
    val nativeCommand = Decoupled(new NativeAdapterCommand(addressWidth))
    val nativeWriteData = Decoupled(new NativeAdapterWriteData(dataWidth))
    val nativeReadData = Flipped(Decoupled(new NativeAdapterReadData(dataWidth)))
  })

  private val sIdle :: sWriteCommand :: sWriteData :: sWriteAck :: sReadCommand :: sReadData :: sReadAck :: Nil = Enum(7)
  private val state = RegInit(sIdle)
  private val address = Reg(UInt(addressWidth.W))
  private val writeData = Reg(UInt(dataWidth.W))
  private val select = Reg(UInt((dataWidth / 8).W))
  private val readData = RegInit(0.U(dataWidth.W))
  private val aborted = RegInit(false.B)

  io.nativeCommand.valid := false.B
  io.nativeCommand.bits.write := false.B
  io.nativeCommand.bits.address := address
  io.nativeWriteData.valid := false.B
  io.nativeWriteData.bits.data := writeData
  io.nativeWriteData.bits.byteEnable := select
  io.nativeReadData.ready := false.B
  io.wishbone.readData := readData
  io.wishbone.acknowledge := false.B
  io.wishbone.error := false.B
  io.wishbone.stall := state =/= sIdle

  // CTI/BTE describe the sequence between beats. Each beat is nevertheless a
  // normal request/acknowledge transfer, so no inferred address state is needed.
  dontTouch(io.wishbone.cycleType)
  dontTouch(io.wishbone.burstType)

  switch(state) {
    is(sIdle) {
      io.wishbone.stall := false.B
      when(io.wishbone.cyc && io.wishbone.stb) {
        assert(io.wishbone.address >= baseWordAddress.U,
          "Wishbone address is below baseAddress")
        address := io.wishbone.address - baseWordAddress.U
        writeData := io.wishbone.writeData
        select := io.wishbone.select
        aborted := false.B
        state := Mux(io.wishbone.writeEnable, sWriteCommand, sReadCommand)
      }
    }
    is(sWriteCommand) {
      io.nativeCommand.valid := true.B
      io.nativeCommand.bits.write := true.B
      when(!io.wishbone.cyc) { aborted := true.B }
      when(io.nativeCommand.fire) { state := sWriteData }
    }
    is(sWriteData) {
      io.nativeWriteData.valid := true.B
      when(!io.wishbone.cyc) { aborted := true.B }
      when(io.nativeWriteData.fire) {
        state := Mux(io.wishbone.cyc && !aborted, sWriteAck, sIdle)
      }
    }
    is(sWriteAck) {
      io.wishbone.acknowledge := io.wishbone.cyc && !aborted
      state := sIdle
    }
    is(sReadCommand) {
      io.nativeCommand.valid := io.wishbone.cyc
      io.nativeCommand.bits.write := false.B
      when(!io.wishbone.cyc) { state := sIdle }
      when(io.nativeCommand.fire) { state := sReadData }
    }
    is(sReadData) {
      io.nativeReadData.ready := true.B
      when(!io.wishbone.cyc) { aborted := true.B }
      when(io.nativeReadData.fire) {
        readData := io.nativeReadData.bits.data
        state := Mux(io.wishbone.cyc && !aborted, sReadAck, sIdle)
      }
    }
    is(sReadAck) {
      io.wishbone.acknowledge := io.wishbone.cyc && !aborted
      state := sIdle
    }
  }
}

/** Wishbone B4 slave to an equal-width Native port. Addresses are word addressed. */
class WishboneToNative(config: DramConfig, baseAddress: BigInt = 0) extends Module {
  require(baseAddress >= 0 && baseAddress % (config.dataBits / 8) == 0)
  private val nativeAddressWidth = config.addressBits - config.byteOffsetBits
  private val baseWordAddress = baseAddress / (config.dataBits / 8)

  val io = IO(new Bundle {
    val wishbone = new WishbonePort(nativeAddressWidth, config.dataBits)
    val nativeCommand = Decoupled(new NativeCommand(config))
    val nativeWriteData = Decoupled(new NativeWriteData(config))
    val nativeReadData = Flipped(Decoupled(new NativeReadData(config)))
  })

  private val bridge = Module(new WishboneNativeBridge(
    nativeAddressWidth, config.dataBits, baseWordAddress))
  bridge.io.wishbone <> io.wishbone

  io.nativeCommand.valid := bridge.io.nativeCommand.valid
  io.nativeCommand.bits.write := bridge.io.nativeCommand.bits.write
  io.nativeCommand.bits.address := bridge.io.nativeCommand.bits.address
  bridge.io.nativeCommand.ready := io.nativeCommand.ready
  io.nativeWriteData.valid := bridge.io.nativeWriteData.valid
  io.nativeWriteData.bits.data := bridge.io.nativeWriteData.bits.data
  io.nativeWriteData.bits.byteEnable := bridge.io.nativeWriteData.bits.byteEnable
  bridge.io.nativeWriteData.ready := io.nativeWriteData.ready
  bridge.io.nativeReadData.valid := io.nativeReadData.valid
  bridge.io.nativeReadData.bits.data := io.nativeReadData.bits.data
  io.nativeReadData.ready := bridge.io.nativeReadData.ready
}

/**
  * Wishbone B4 slave with power-of-two width conversion to the Native width.
  * Wide accesses split into consecutive Native words; narrow accesses use the
  * selected Native byte lane. Ordering and byte enables are preserved.
  */
class WishboneToNativeWidthAdapter(config: DramConfig, wishboneDataBits: Int,
    baseAddress: BigInt = 0, reverse: Boolean = false) extends Module {
  require(wishboneDataBits >= 8 && wishboneDataBits % 8 == 0)
  require(baseAddress >= 0 && baseAddress % (wishboneDataBits / 8) == 0)
  private val ratio = if (wishboneDataBits >= config.dataBits)
    wishboneDataBits / config.dataBits else config.dataBits / wishboneDataBits
  require((wishboneDataBits >= config.dataBits && wishboneDataBits % config.dataBits == 0) ||
    (config.dataBits > wishboneDataBits && config.dataBits % wishboneDataBits == 0))
  require((ratio & (ratio - 1)) == 0, "width ratio must be a power of two")
  private val wishboneAddressWidth = config.addressBits - log2Ceil(wishboneDataBits / 8)
  private val baseWordAddress = baseAddress / (wishboneDataBits / 8)

  val io = IO(new Bundle {
    val wishbone = new WishbonePort(wishboneAddressWidth, wishboneDataBits)
    val nativeCommand = Decoupled(new NativeCommand(config))
    val nativeWriteData = Decoupled(new NativeWriteData(config))
    val nativeReadData = Flipped(Decoupled(new NativeReadData(config)))
  })

  private val bridge = Module(new WishboneNativeBridge(
    wishboneAddressWidth, wishboneDataBits, baseWordAddress))
  bridge.io.wishbone <> io.wishbone

  if (wishboneDataBits == config.dataBits) {
    io.nativeCommand.valid := bridge.io.nativeCommand.valid
    io.nativeCommand.bits.write := bridge.io.nativeCommand.bits.write
    io.nativeCommand.bits.address := bridge.io.nativeCommand.bits.address
    bridge.io.nativeCommand.ready := io.nativeCommand.ready
    io.nativeWriteData.valid := bridge.io.nativeWriteData.valid
    io.nativeWriteData.bits.data := bridge.io.nativeWriteData.bits.data
    io.nativeWriteData.bits.byteEnable := bridge.io.nativeWriteData.bits.byteEnable
    bridge.io.nativeWriteData.ready := io.nativeWriteData.ready
    bridge.io.nativeReadData.valid := io.nativeReadData.valid
    bridge.io.nativeReadData.bits.data := io.nativeReadData.bits.data
    io.nativeReadData.ready := bridge.io.nativeReadData.ready
  } else if (wishboneDataBits > config.dataBits) {
    val converter = Module(new NativeDownConverter(
      wishboneAddressWidth, wishboneDataBits, config.dataBits, reverse))
    converter.io.inputCommand <> bridge.io.nativeCommand
    converter.io.inputWriteData <> bridge.io.nativeWriteData
    bridge.io.nativeReadData <> converter.io.outputReadData

    io.nativeCommand.valid := converter.io.outputCommand.valid
    io.nativeCommand.bits.write := converter.io.outputCommand.bits.write
    io.nativeCommand.bits.address := converter.io.outputCommand.bits.address
    converter.io.outputCommand.ready := io.nativeCommand.ready
    io.nativeWriteData.valid := converter.io.outputWriteData.valid
    io.nativeWriteData.bits.data := converter.io.outputWriteData.bits.data
    io.nativeWriteData.bits.byteEnable := converter.io.outputWriteData.bits.byteEnable
    converter.io.outputWriteData.ready := io.nativeWriteData.ready
    converter.io.inputReadData.valid := io.nativeReadData.valid
    converter.io.inputReadData.bits.data := io.nativeReadData.bits.data
    io.nativeReadData.ready := converter.io.inputReadData.ready
  } else {
    val converter = Module(new NativeUpConverter(
      wishboneAddressWidth, wishboneDataBits, config.dataBits, reverse))
    converter.io.flush := !io.wishbone.cyc
    converter.io.inputCommand <> bridge.io.nativeCommand
    converter.io.inputWriteData <> bridge.io.nativeWriteData
    bridge.io.nativeReadData <> converter.io.outputReadData

    io.nativeCommand.valid := converter.io.outputCommand.valid
    io.nativeCommand.bits.write := converter.io.outputCommand.bits.write
    io.nativeCommand.bits.address := converter.io.outputCommand.bits.address
    converter.io.outputCommand.ready := io.nativeCommand.ready
    io.nativeWriteData.valid := converter.io.outputWriteData.valid
    io.nativeWriteData.bits.data := converter.io.outputWriteData.bits.data
    io.nativeWriteData.bits.byteEnable := converter.io.outputWriteData.bits.byteEnable
    converter.io.outputWriteData.ready := io.nativeWriteData.ready
    converter.io.inputReadData.valid := io.nativeReadData.valid
    converter.io.inputReadData.bits.data := io.nativeReadData.bits.data
    io.nativeReadData.ready := converter.io.inputReadData.ready
  }
}
