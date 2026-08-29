package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags, VerilatorFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class UltraScaleStandardDdrPhyIoHarness(memType: String, plus: Boolean,
    dqsPerByte: Int = 1, padGroups: Int = 1)
    extends Module {
  private val config = DramConfig(addressBits = 40, dataBits = 64,
    bankBits = (if (memType == "DDR4") 4 else 3),
    rowBits = (if (memType == "DDR4") 17 else 15), columnBits = 10,
    memType = memType, nPhases = 4, padDataBits = 8 * padGroups)
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  val io = IO(new Bundle {
    val address0 = Input(UInt(8.W))
    val chipSelectN = Input(UInt(8.W))
    val activateN = Input(UInt(8.W))
    val address0Pad = Output(Vec(padGroups, Bool()))
    val chipSelectNPad = Output(Vec(padGroups, Bool()))
    val activateNPad = Output(Vec(padGroups, Bool()))
    val commandOutputDelayValue = Output(UInt(9.W))
    val dqInputDelayValue = Output(UInt(9.W))
    val dqOutputDelayValue = Output(UInt(9.W))
    val dataMaskOutputDelayValue = Output(UInt(9.W))
    val dqsOutputDelayValue = Output(Vec(padGroups * dqsPerByte, UInt(9.W)))
    val clockPositive = Vec(padGroups, Analog(1.W))
    val clockNegative = Vec(padGroups, Analog(1.W))
    val dq = Vec(8 * padGroups, Analog(1.W))
    val dqsPositive = Vec(padGroups * dqsPerByte, Analog(1.W))
    val dqsNegative = Vec(padGroups * dqsPerByte, Analog(1.W))
  })

  private val phy: UltraScaleStandardDdrPhyIo = if (plus) {
    Module(new UltraScalePlusDdrPhyIo(config,
      dqsOutputDelayInitialValuePs = 375, dqsPerByte = dqsPerByte,
      padGroups = padGroups))
  } else {
    Module(new UltraScaleDdrPhyIo(config,
      dqsOutputDelayInitialValuePs = 375, dqsPerByte = dqsPerByte,
      padGroups = padGroups))
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

  for (group <- 0 until padGroups) {
    io.address0Pad(group) := phy.io.pads.commandGroups(group).address(0)
    io.chipSelectNPad(group) := phy.io.pads.commandGroups(group).chipSelectN(0)
    io.activateNPad(group) := phy.io.pads.commandGroups(group).activateN
    attach(io.clockPositive(group),
      phy.io.pads.commandGroups(group).clockPositive)
    attach(io.clockNegative(group),
      phy.io.pads.commandGroups(group).clockNegative)
  }
  io.commandOutputDelayValue := phy.io.commandOutputDelayValue
  io.dqInputDelayValue := phy.io.dqInputDelayValue(0)
  io.dqOutputDelayValue := phy.io.dqOutputDelayValue(0)
  io.dataMaskOutputDelayValue := phy.io.dataMaskOutputDelayValue(0)
  io.dqsOutputDelayValue := phy.io.dqsOutputDelayValue
  for (bit <- 0 until 8 * padGroups) attach(io.dq(bit), phy.io.pads.dq(bit))
  for (strobe <- 0 until padGroups * dqsPerByte) {
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
        dut.io.address0Pad(0).expect(true.B)
        dut.io.chipSelectNPad(0).expect(false.B)
        dut.io.activateNPad(0).expect(true.B)
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

  it should "replicate clock and command primitives across separate pad groups" in {
    test(new UltraScaleStandardDdrPhyIoHarness("DDR3", plus = false,
      padGroups = 2)).withAnnotations(verilator) { dut =>
        dut.io.address0.poke("ha5".U)
        dut.io.chipSelectN.poke("h3c".U)
        dut.io.activateN.poke("h81".U)
        for (group <- 0 until 2) {
          dut.io.address0Pad(group).expect(true.B)
          dut.io.chipSelectNPad(group).expect(false.B)
          dut.io.activateNPad(group).expect(true.B)
        }
        dut.io.dqsOutputDelayValue.foreach(_.expect(375.U))
        dut.clock.step(2)
      }
  }
}
