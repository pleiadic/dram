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

/**
  * LiteDRAM-compatible narrow Wishbone burst bridge. Consecutive writes to
  * lanes of one Native word are merged and committed once; consecutive reads
  * from one Native word reuse the returned wide word. A pending partial write
  * is committed before a read, a repeated lane, an address change, or CYC
  * deassertion, preserving Wishbone ordering.
  */
private class WishboneNarrowBurstBridge(config: DramConfig, wishboneDataBits: Int,
    baseWordAddress: BigInt, reverse: Boolean) extends Module {
  require(config.dataBits > wishboneDataBits)
  require(config.dataBits % wishboneDataBits == 0)
  private val ratio = config.dataBits / wishboneDataBits
  require((ratio & (ratio - 1)) == 0)
  private val ratioBits = log2Ceil(ratio)
  private val wishboneBytes = wishboneDataBits / 8
  private val nativeBytes = config.dataBits / 8
  private val wishboneAddressWidth = config.addressBits - log2Ceil(wishboneBytes)
  private val nativeAddressWidth = config.addressBits - config.byteOffsetBits

  val io = IO(new Bundle {
    val wishbone = new WishbonePort(wishboneAddressWidth, wishboneDataBits)
    val nativeCommand = Decoupled(new NativeCommand(config))
    val nativeWriteData = Decoupled(new NativeWriteData(config))
    val nativeReadData = Flipped(Decoupled(new NativeReadData(config)))
  })

  private val Seq(sAccept, sWriteAck, sWriteCommand, sWriteData,
    sReadCommand, sReadData, sReadAck) = Enum(7)
  private val state = RegInit(sAccept)

  private val writePending = RegInit(false.B)
  private val writeAddress = Reg(UInt(nativeAddressWidth.W))
  private val writeData = RegInit(0.U(config.dataBits.W))
  private val writeMask = RegInit(0.U(nativeBytes.W))
  private val writeLanes = RegInit(0.U(ratio.W))
  private val flushAfterAck = RegInit(false.B)

  private val readAddress = Reg(UInt(nativeAddressWidth.W))
  private val readLane = Reg(UInt(ratioBits.W))
  private val readLast = RegInit(true.B)
  private val readResult = RegInit(0.U(wishboneDataBits.W))
  private val readCacheValid = RegInit(false.B)
  private val readCacheAddress = Reg(UInt(nativeAddressWidth.W))
  private val readCacheData = Reg(UInt(config.dataBits.W))

  private val relativeAddress = io.wishbone.address - baseWordAddress.U
  private val rawLane = relativeAddress(ratioBits - 1, 0)
  private val mappedLane = Mux(reverse.B, (ratio - 1).U - rawLane, rawLane)
  private val nativeAddress = relativeAddress(wishboneAddressWidth - 1, ratioBits)
  private val laneDataShift = mappedLane * wishboneDataBits.U
  private val laneMaskShift = mappedLane * wishboneBytes.U
  private val shiftedWriteData =
    (io.wishbone.writeData << laneDataShift)(config.dataBits - 1, 0)
  private val shiftedWriteMask =
    (io.wishbone.select << laneMaskShift)(nativeBytes - 1, 0)
  private val laneBit = (1.U(ratio.W) << mappedLane)(ratio - 1, 0)
  private val request = io.wishbone.cyc && io.wishbone.stb
  private val lastBeat = io.wishbone.cycleType =/= WishboneCycleType.incrementingBurst

  io.nativeCommand.valid := false.B
  io.nativeCommand.bits.write := false.B
  io.nativeCommand.bits.address := 0.U
  io.nativeWriteData.valid := false.B
  io.nativeWriteData.bits.data := writeData
  io.nativeWriteData.bits.byteEnable := writeMask
  io.nativeReadData.ready := false.B
  io.wishbone.readData := readResult
  io.wishbone.acknowledge := false.B
  io.wishbone.error := false.B
  io.wishbone.stall := state =/= sAccept

  // BTE controls the master's address sequence. The slave consumes the
  // explicit address on every beat and therefore needs no inferred counter.
  dontTouch(io.wishbone.burstType)

  switch(state) {
    is(sAccept) {
      io.wishbone.stall := false.B
      when(!io.wishbone.cyc) {
        readCacheValid := false.B
        when(writePending) { state := sWriteCommand }
      }.elsewhen(io.wishbone.stb) {
        assert(io.wishbone.address >= baseWordAddress.U,
          "Wishbone address is below baseAddress")
        when(io.wishbone.writeEnable) {
          readCacheValid := false.B
          val canMerge = !writePending ||
            (writeAddress === nativeAddress && (writeLanes & laneBit) === 0.U)
          when(canMerge) {
            when(!writePending) {
              writeAddress := nativeAddress
              writeData := shiftedWriteData
              writeMask := shiftedWriteMask
              writeLanes := laneBit
            }.otherwise {
              writeData := writeData | shiftedWriteData
              writeMask := writeMask | shiftedWriteMask
              writeLanes := writeLanes | laneBit
            }
            writePending := true.B
            flushAfterAck := lastBeat || ((writeLanes | laneBit).andR)
            state := sWriteAck
          }.otherwise {
            // Do not acknowledge the current beat until the previous aggregate
            // has reached Native; the master keeps its request stable.
            state := sWriteCommand
          }
        }.otherwise {
          when(writePending) {
            state := sWriteCommand
          }.elsewhen(readCacheValid && readCacheAddress === nativeAddress) {
            readResult :=
              (readCacheData >> laneDataShift)(wishboneDataBits - 1, 0)
            when(lastBeat) { readCacheValid := false.B }
            state := sReadAck
          }.otherwise {
            readAddress := nativeAddress
            readLane := mappedLane
            readLast := lastBeat
            state := sReadCommand
          }
        }
      }
    }
    is(sWriteAck) {
      io.wishbone.acknowledge := request
      when(flushAfterAck || !io.wishbone.cyc) { state := sWriteCommand }
        .otherwise { state := sAccept }
    }
    is(sWriteCommand) {
      io.nativeCommand.valid := writePending
      io.nativeCommand.bits.write := true.B
      io.nativeCommand.bits.address := writeAddress
      when(!writePending) { state := sAccept }
      when(io.nativeCommand.fire) { state := sWriteData }
    }
    is(sWriteData) {
      io.nativeWriteData.valid := true.B
      when(io.nativeWriteData.fire) {
        writePending := false.B
        writeData := 0.U
        writeMask := 0.U
        writeLanes := 0.U
        state := sAccept
      }
    }
    is(sReadCommand) {
      io.nativeCommand.valid := io.wishbone.cyc
      io.nativeCommand.bits.write := false.B
      io.nativeCommand.bits.address := readAddress
      when(!io.wishbone.cyc) { state := sAccept }
      when(io.nativeCommand.fire) { state := sReadData }
    }
    is(sReadData) {
      io.nativeReadData.ready := true.B
      when(io.nativeReadData.fire) {
        readResult :=
          (io.nativeReadData.bits.data >> (readLane * wishboneDataBits.U))(
            wishboneDataBits - 1, 0)
        readCacheData := io.nativeReadData.bits.data
        readCacheAddress := readAddress
        readCacheValid := io.wishbone.cyc && !readLast
        state := Mux(io.wishbone.cyc, sReadAck, sAccept)
      }
    }
    is(sReadAck) {
      io.wishbone.acknowledge := request
      state := sAccept
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

  if (wishboneDataBits < config.dataBits) {
    val bridge = Module(new WishboneNarrowBurstBridge(
      config, wishboneDataBits, baseWordAddress, reverse))
    bridge.io.wishbone <> io.wishbone
    io.nativeCommand <> bridge.io.nativeCommand
    io.nativeWriteData <> bridge.io.nativeWriteData
    bridge.io.nativeReadData <> io.nativeReadData
  } else {
    val bridge = Module(new WishboneNativeBridge(
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
    } else {
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
    }
  }
}
