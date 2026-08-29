package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags, VerilatorFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class UltraScaleStandardDdrPhyIoHarness(memType: String, plus: Boolean,
    dqsPerByte: Int = 1)
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
    val commandOutputDelayValue = Output(UInt(9.W))
    val dqInputDelayValue = Output(UInt(9.W))
    val dqOutputDelayValue = Output(UInt(9.W))
    val dataMaskOutputDelayValue = Output(UInt(9.W))
    val dqsOutputDelayValue = Output(Vec(dqsPerByte, UInt(9.W)))
    val clockPositive = Analog(1.W)
    val clockNegative = Analog(1.W)
    val dq = Vec(8, Analog(1.W))
    val dqsPositive = Vec(dqsPerByte, Analog(1.W))
    val dqsNegative = Vec(dqsPerByte, Analog(1.W))
  })

  private val phy: UltraScaleStandardDdrPhyIo = if (plus) {
    Module(new UltraScalePlusDdrPhyIo(config,
      dqsOutputDelayInitialValuePs = 375, dqsPerByte = dqsPerByte))
  } else {
    Module(new UltraScaleDdrPhyIo(config,
      dqsOutputDelayInitialValuePs = 375, dqsPerByte = dqsPerByte))
  }
  phy.io.reset := reset.asBool
  phy.io.dividedClock := clock
  phy.io.serialClock := clock
  phy.io.delayClock := clock
  phy.io.enableVtc := true.B
  phy.io.delaySelect := 0.U
  phy.io.commandOutputDelayReset := false.B
  phy.io.commandOutputDelayIncrement := false.B
  phy.io.dqInputDelayReset := false.B
  phy.io.dqInputDelayIncrement := false.B
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
  io.commandOutputDelayValue := phy.io.commandOutputDelayValue
  io.dqInputDelayValue := phy.io.dqInputDelayValue(0)
  io.dqOutputDelayValue := phy.io.dqOutputDelayValue(0)
  io.dataMaskOutputDelayValue := phy.io.dataMaskOutputDelayValue(0)
  io.dqsOutputDelayValue := phy.io.dqsOutputDelayValue
  attach(io.clockPositive, phy.io.pads.clockPositive)
  attach(io.clockNegative, phy.io.pads.clockNegative)
  for (bit <- 0 until 8) attach(io.dq(bit), phy.io.pads.dq(bit))
  for (strobe <- 0 until dqsPerByte) {
    attach(io.dqsPositive(strobe), phy.io.pads.dqsPositive(strobe))
    attach(io.dqsNegative(strobe), phy.io.pads.dqsNegative(strobe))
  }
}

class UltraScaleStandardDdrPhyIoSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val verilator = Seq(VerilatorBackendAnnotation,
    VerilatorCFlags(Seq("-DWData=IData")))

  behavior of "UltraScale standard DDR PHY I/O"

  it should "assemble DDR3 through the UltraScale E3 pad boundary" in {
    test(new UltraScaleStandardDdrPhyIoHarness("DDR3", plus = false))
      .withAnnotations(verilator) { dut =>
        dut.io.address0.poke("h01".U)
        dut.io.chipSelectN.poke("hfe".U)
        dut.io.activateN.poke("hff".U)
        dut.io.address0Pad.expect(true.B)
        dut.io.chipSelectNPad.expect(false.B)
        dut.io.activateNPad.expect(true.B)
        dut.io.commandOutputDelayValue.expect(0.U)
        dut.io.dqInputDelayValue.expect(0.U)
        dut.io.dqOutputDelayValue.expect(0.U)
        dut.io.dataMaskOutputDelayValue.expect(0.U)
        dut.io.dqsOutputDelayValue(0).expect(375.U)
        dut.clock.step(2)
      }
  }

  it should "parse DDR4 with the UltraScale Plus primitive branches" in {
    test(new UltraScaleStandardDdrPhyIoHarness("DDR4", plus = true))
      .withAnnotations(Seq(
        VerilatorBackendAnnotation,
        VerilatorFlags(Seq("-DSYNTHESIS", "-Wno-MODMISSING")),
        VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
        dut.io.address0.poke("h3c".U)
        dut.io.chipSelectN.poke("hff".U)
        dut.io.activateN.poke("hfc".U)
        dut.clock.step()
      }
  }

  it should "replicate each byte strobe across both x4 DIMM DQS pairs" in {
    test(new UltraScaleStandardDdrPhyIoHarness("DDR4", plus = false,
      dqsPerByte = 2)).withAnnotations(verilator) { dut =>
        dut.io.address0.poke("h01".U)
        dut.io.chipSelectN.poke("hfe".U)
        dut.io.activateN.poke("hff".U)
        dut.io.dqsOutputDelayValue(0).expect(375.U)
        dut.io.dqsOutputDelayValue(1).expect(375.U)
        dut.clock.step(2)
      }
  }
}
