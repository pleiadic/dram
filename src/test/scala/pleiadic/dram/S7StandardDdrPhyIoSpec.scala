package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags, VerilatorFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class S7StandardDdrPhyIoHarness(memType: String, withOutputDelay: Boolean = false)
    extends Module {
  private val config = DramConfig(addressBits = 40, dataBits = 64,
    bankBits = (if (memType == "DDR4") 4 else 3),
    rowBits = (if (memType == "DDR4") 17 else 15), columnBits = 10,
    memType = memType, nPhases = 4, padDataBits = 8)
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  val io = IO(new Bundle {
    val address0 = Input(UInt(8.W))
    val chipSelectN = Input(UInt(8.W))
    val activateN = Input(UInt(8.W))
    val address0Pad = Output(Bool())
    val chipSelectNPad = Output(Bool())
    val activateNPad = Output(Bool())
    val dqDelayValue = Output(UInt(5.W))
    val dqOutputDelayValue = Output(UInt(5.W))
    val dqsOutputDelayValue = Output(UInt(5.W))
    val clockPositive = Analog(1.W)
    val clockNegative = Analog(1.W)
    val dq = Vec(8, Analog(1.W))
    val dqsPositive = Analog(1.W)
    val dqsNegative = Analog(1.W)
  })

  private val phy = Module(new S7StandardDdrPhyIo(config,
    withOutputDelay = withOutputDelay, dqsOutputDelayInitialValue = 7))
  phy.io.reset := reset.asBool
  phy.io.dividedClock := clock
  phy.io.serialClock := clock
  phy.io.invertedSerialClock := (!clock.asBool).asClock
  phy.io.shiftedDqsOutputClock := clock
  phy.io.delayClock := clock
  phy.io.delaySelect := 0.U
  phy.io.dqInputDelayReset := false.B
  phy.io.dqInputDelayIncrement := false.B
  phy.io.commandOutputDelayReset := false.B
  phy.io.commandOutputDelayIncrement := false.B
  phy.io.dqOutputDelayReset := false.B
  phy.io.dqOutputDelayIncrement := false.B
  phy.io.dqsOutputDelayReset := false.B
  phy.io.dqsOutputDelayIncrement := false.B
  phy.io.parallel.clock := "haa".U
  for (bit <- 0 until addressBits) {
    phy.io.parallel.address(bit) := (if (bit == 0) io.address0 else 0.U)
  }
  phy.io.parallel.bank.foreach(_ := 0.U)
  phy.io.parallel.chipSelectN.foreach(_ := io.chipSelectN)
  phy.io.parallel.rowStrobeN := "hff".U
  phy.io.parallel.columnStrobeN := "hff".U
  phy.io.parallel.writeEnableN := "hff".U
  phy.io.parallel.activateN := io.activateN
  phy.io.parallel.clockEnable.foreach(_ := 0.U)
  phy.io.parallel.onDieTermination.foreach(_ := 0.U)
  phy.io.parallel.resetN := "hff".U
  phy.io.parallel.dq.foreach(_ := 0.U)
  phy.io.parallel.dqOutputEnable := false.B
  phy.io.parallel.dqs.foreach(_ := 0.U)
  phy.io.parallel.dqsOutputEnable := false.B
  phy.io.parallel.dataMask.foreach(_ := 0.U)

  io.address0Pad := phy.io.pads.address(0)
  io.chipSelectNPad := phy.io.pads.chipSelectN(0)
  io.activateNPad := phy.io.pads.activateN
  io.dqDelayValue := phy.io.dqDelayValue(0)
  io.dqOutputDelayValue := phy.io.dqOutputDelayValue(0)
  io.dqsOutputDelayValue := phy.io.dqsOutputDelayValue(0)
  attach(io.clockPositive, phy.io.pads.clockPositive)
  attach(io.clockNegative, phy.io.pads.clockNegative)
  for (bit <- 0 until 8) attach(io.dq(bit), phy.io.pads.dq(bit))
  attach(io.dqsPositive, phy.io.pads.dqsPositive(0))
  attach(io.dqsNegative, phy.io.pads.dqsNegative(0))
}

class S7StandardDdrPhyIoSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "S7 standard DDR PHY I/O"

  it should "assemble the complete DDR3 Artix-7 pad boundary" in {
    test(new S7StandardDdrPhyIoHarness("DDR3")).withAnnotations(Seq(
      VerilatorBackendAnnotation, VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.address0.poke("h01".U)
      dut.io.chipSelectN.poke("hfe".U)
      dut.io.activateN.poke("hff".U)
      dut.io.address0Pad.expect(true.B)
      dut.io.chipSelectNPad.expect(false.B)
      dut.io.activateNPad.expect(true.B)
      dut.io.dqDelayValue.expect(0.U)
      dut.io.dqOutputDelayValue.expect(0.U)
      dut.io.dqsOutputDelayValue.expect(0.U)
      dut.clock.step(2)
    }
  }

  it should "model the Kintex/Virtex output-delay path" in {
    test(new S7StandardDdrPhyIoHarness("DDR3", withOutputDelay = true))
      .withAnnotations(Seq(VerilatorBackendAnnotation,
        VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.address0.poke("h01".U)
      dut.io.chipSelectN.poke("hfe".U)
      dut.io.activateN.poke("hff".U)
      dut.io.address0Pad.expect(true.B)
      dut.io.dqDelayValue.expect(0.U)
      dut.io.dqOutputDelayValue.expect(0.U)
      dut.io.dqsOutputDelayValue.expect(7.U)
      dut.clock.step(2)
    }
  }

  it should "parse the DDR4 pad assembly with real Xilinx primitive branches" in {
    test(new S7StandardDdrPhyIoHarness("DDR4", withOutputDelay = true)).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq("-DSYNTHESIS", "-Wno-MODMISSING")),
      VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.address0.poke("h3c".U)
      dut.io.chipSelectN.poke("hff".U)
      dut.io.activateN.poke("hfc".U)
      dut.clock.step()
    }
  }
}
