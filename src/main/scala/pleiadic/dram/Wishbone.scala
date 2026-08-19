package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

class WishbonePort(addressWidth: Int, dataWidth: Int) extends Bundle {
  val cyc = Input(Bool())
  val stb = Input(Bool())
  val writeEnable = Input(Bool())
  val address = Input(UInt(addressWidth.W))
  val writeData = Input(UInt(dataWidth.W))
  val select = Input(UInt((dataWidth / 8).W))
  val readData = Output(UInt(dataWidth.W))
  val acknowledge = Output(Bool())
  val error = Output(Bool())
  val stall = Output(Bool())
}

/** Wishbone classic-cycle frontend for an equal-width Native port. */
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

  private val sIdle :: sWrite :: sWriteAck :: sReadCommand :: sReadData :: sReadAck :: Nil = Enum(6)
  private val state = RegInit(sIdle)
  private val address = Reg(UInt(nativeAddressWidth.W))
  private val writeData = Reg(UInt(config.dataBits.W))
  private val select = Reg(UInt((config.dataBits / 8).W))
  private val readData = RegInit(0.U(config.dataBits.W))
  private val commandSent = RegInit(false.B)
  private val dataSent = RegInit(false.B)

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

  switch(state) {
    is(sIdle) {
      io.wishbone.stall := false.B
      when(io.wishbone.cyc && io.wishbone.stb) {
        address := io.wishbone.address + baseWordAddress.U
        writeData := io.wishbone.writeData
        select := io.wishbone.select
        commandSent := false.B
        dataSent := false.B
        state := Mux(io.wishbone.writeEnable, sWrite, sReadCommand)
      }
    }
    is(sWrite) {
      io.nativeCommand.valid := !commandSent
      io.nativeCommand.bits.write := true.B
      io.nativeWriteData.valid := !dataSent
      when(io.nativeCommand.fire) { commandSent := true.B }
      when(io.nativeWriteData.fire) { dataSent := true.B }
      when((commandSent || io.nativeCommand.fire) && (dataSent || io.nativeWriteData.fire)) {
        state := sWriteAck
      }
      when(!io.wishbone.cyc) { state := sIdle }
    }
    is(sWriteAck) {
      io.wishbone.acknowledge := io.wishbone.cyc
      state := sIdle
    }
    is(sReadCommand) {
      io.nativeCommand.valid := true.B
      io.nativeCommand.bits.write := false.B
      when(io.nativeCommand.fire) { state := sReadData }
      when(!io.wishbone.cyc) { state := sIdle }
    }
    is(sReadData) {
      io.nativeReadData.ready := true.B
      when(io.nativeReadData.fire) {
        readData := io.nativeReadData.bits.data
        state := sReadAck
      }
      when(!io.wishbone.cyc) { state := sIdle }
    }
    is(sReadAck) {
      io.wishbone.acknowledge := io.wishbone.cyc
      state := sIdle
    }
  }
}
