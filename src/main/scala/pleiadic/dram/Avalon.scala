package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

class AvalonMmPort(addressWidth: Int, dataWidth: Int, burstCountWidth: Int = 9) extends Bundle {
  val address = Input(UInt(addressWidth.W))
  val read = Input(Bool())
  val write = Input(Bool())
  val burstCount = Input(UInt(burstCountWidth.W))
  val writeData = Input(UInt(dataWidth.W))
  val byteEnable = Input(UInt((dataWidth / 8).W))
  val waitRequest = Output(Bool())
  val readData = Output(UInt(dataWidth.W))
  val readDataValid = Output(Bool())
}

/** Equal-width Avalon-MM to Native bridge with one outstanding burst. */
class AvalonMmToNative(config: DramConfig, maxBurstLength: Int = 16,
    baseAddress: BigInt = 0, burstIncrement: Int = 1) extends Module {
  require(maxBurstLength >= 1 && maxBurstLength <= 256)
  require(baseAddress >= 0 && baseAddress % (config.dataBits / 8) == 0)
  require(burstIncrement >= 1)
  private val addressWidth = config.addressBits - config.byteOffsetBits
  private val baseWordAddress = baseAddress / (config.dataBits / 8)
  private val countWidth = log2Ceil((maxBurstLength + 1) max 2)

  val io = IO(new Bundle {
    val avalon = new AvalonMmPort(addressWidth, config.dataBits)
    val nativeCommand = Decoupled(new NativeCommand(config))
    val nativeWriteData = Decoupled(new NativeWriteData(config))
    val nativeReadData = Flipped(Decoupled(new NativeReadData(config)))
  })

  private val sIdle :: sBurstWrite :: sBurstRead :: Nil = Enum(3)
  private val state = RegInit(sIdle)
  private val address = Reg(UInt(addressWidth.W))
  private val commandsRemaining = RegInit(0.U(countWidth.W))
  private val responsesRemaining = RegInit(0.U(countWidth.W))

  io.nativeCommand.valid := false.B
  io.nativeCommand.bits.write := false.B
  io.nativeCommand.bits.address := address
  io.nativeWriteData.valid := false.B
  io.nativeWriteData.bits.data := io.avalon.writeData
  io.nativeWriteData.bits.byteEnable := io.avalon.byteEnable
  io.nativeReadData.ready := false.B
  io.avalon.waitRequest := true.B
  io.avalon.readData := io.nativeReadData.bits.data
  io.avalon.readDataValid := false.B

  private def checkedBurstCount: UInt = Mux(io.avalon.burstCount === 0.U, 1.U, io.avalon.burstCount)

  switch(state) {
    is(sIdle) {
      when(io.avalon.write) {
        // Couple the two Native channels so Avalon sees a beat accepted only
        // when both payloads have transferred.
        io.nativeCommand.valid := io.nativeWriteData.ready
        io.nativeCommand.bits.write := true.B
        io.nativeCommand.bits.address := io.avalon.address - baseWordAddress.U
        io.nativeWriteData.valid := io.nativeCommand.ready
        io.avalon.waitRequest := !(io.nativeCommand.ready && io.nativeWriteData.ready)
        when(io.nativeCommand.fire && io.nativeWriteData.fire) {
          assert(checkedBurstCount <= maxBurstLength.U)
          when(checkedBurstCount > 1.U) {
            address := io.avalon.address - baseWordAddress.U + burstIncrement.U
            commandsRemaining := checkedBurstCount - 1.U
            state := sBurstWrite
          }
        }
      }.elsewhen(io.avalon.read) {
        io.nativeCommand.valid := true.B
        io.nativeCommand.bits.write := false.B
        io.nativeCommand.bits.address := io.avalon.address - baseWordAddress.U
        io.avalon.waitRequest := !io.nativeCommand.ready
        when(io.nativeCommand.fire) {
          assert(checkedBurstCount <= maxBurstLength.U)
          address := io.avalon.address - baseWordAddress.U + burstIncrement.U
          commandsRemaining := checkedBurstCount - 1.U
          responsesRemaining := checkedBurstCount
          state := sBurstRead
        }
      }
    }
    is(sBurstWrite) {
      when(io.avalon.write) {
        io.nativeCommand.valid := io.nativeWriteData.ready
        io.nativeCommand.bits.write := true.B
        io.nativeWriteData.valid := io.nativeCommand.ready
        io.avalon.waitRequest := !(io.nativeCommand.ready && io.nativeWriteData.ready)
        when(io.nativeCommand.fire && io.nativeWriteData.fire) {
          address := address + burstIncrement.U
          commandsRemaining := commandsRemaining - 1.U
          when(commandsRemaining === 1.U) { state := sIdle }
        }
      }
    }
    is(sBurstRead) {
      io.nativeCommand.valid := commandsRemaining =/= 0.U
      io.nativeCommand.bits.write := false.B
      when(io.nativeCommand.fire) {
        address := address + burstIncrement.U
        commandsRemaining := commandsRemaining - 1.U
      }

      io.nativeReadData.ready := true.B
      io.avalon.readDataValid := io.nativeReadData.valid
      when(io.nativeReadData.fire) {
        responsesRemaining := responsesRemaining - 1.U
        when(responsesRemaining === 1.U) { state := sIdle }
      }
    }
  }
}
