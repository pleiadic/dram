package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import chisel3.util.Cat
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags, VerilatorFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class GowinDdrPhyIoHarness(family: GowinFamily) extends Module {
  private val config = DramConfig(addressBits = 40, dataBits = 64,
    bankBits = 3, rowBits = 15, columnBits = 10, memType = "DDR3",
    nPhases = 2, phyDataBits = 64, padDataBits = 8)
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  val io = IO(new Bundle {
    val address0 = Input(UInt(4.W))
    val chipSelectN = Input(UInt(4.W))
    val dataMask = Input(UInt(4.W))
    val delaySelect = Input(Bool())
    val delayReset = Input(Bool())
    val delayIncrement = Input(Bool())
    val address0Pad = Output(Bool())
    val chipSelectNPad = Output(Bool())
    val clockPositive = Output(Bool())
    val clockNegative = Output(Bool())
    val dataMaskPad = Output(Bool())
    val inputDelayValue = Output(UInt(3.W))
    val dataValid = Output(Bool())
    val initBusy = Output(Bool())
    val dq = Vec(8, Analog(1.W))
    val dqsPositive = Analog(1.W)
    val dqsNegative = Analog(1.W)
  })

  private val phy = Module(new GowinDdrPhyIo(config, family,
    commandDelayInitialValue = 6))
  phy.io.reset := reset.asBool
  phy.io.initClock := clock
  phy.io.systemClock := clock
  phy.io.edgeClock := clock
  phy.io.dllReset := reset.asBool
  phy.io.delaySelect := io.delaySelect
  phy.io.inputDelayReset := io.delayReset
  phy.io.inputDelayIncrement := io.delayIncrement
  phy.io.burstDetectClear := false.B
  phy.io.parallel.clock := "ha".U
  for (bit <- 0 until addressBits) {
    phy.io.parallel.address(bit) := (if (bit == 0) io.address0 else 0.U)
  }
  phy.io.parallel.bank.foreach(_ := 0.U)
  phy.io.parallel.chipSelectN.foreach(_ := io.chipSelectN)
  phy.io.parallel.rowStrobeN := "hf".U
  phy.io.parallel.columnStrobeN := "hf".U
  phy.io.parallel.writeEnableN := "hf".U
  phy.io.parallel.clockEnable.foreach(_ := 0.U)
  phy.io.parallel.onDieTermination.foreach(_ := 0.U)
  phy.io.parallel.resetN := "hf".U
  phy.io.parallel.dq.foreach(_ := 0.U)
  phy.io.parallel.dataMask.foreach(_ := io.dataMask)
  phy.io.parallel.dqOutputEnable := false.B
  phy.io.parallel.dqs.foreach(_ := "ha".U)
  phy.io.parallel.dqsOutputEnable := true.B
  phy.io.parallel.dqsPreamble := false.B
  phy.io.parallel.dqsPostamble := false.B
  phy.io.parallel.dqsReadEnable := true.B

  io.address0Pad := phy.io.pads.address(0)
  io.chipSelectNPad := phy.io.pads.chipSelectN(0)
  io.clockPositive := phy.io.pads.clockPositive
  io.clockNegative := phy.io.pads.clockNegative
  io.dataMaskPad := phy.io.pads.dataMask(0)
  io.inputDelayValue := phy.io.inputDelayValue(0)
  io.dataValid := phy.io.dataValid(0)
  io.initBusy := phy.io.initBusy
  for (bit <- 0 until 8) attach(io.dq(bit), phy.io.pads.dq(bit))
  attach(io.dqsPositive, phy.io.pads.dqsPositive(0))
  attach(io.dqsNegative, phy.io.pads.dqsNegative(0))
}

class GowinDdrPhyIntegrationHarness extends Module {
  private val config = DramConfig(addressBits = 40, dataBits = 64,
    bankBits = 3, rowBits = 15, columnBits = 10, memType = "DDR3",
    nPhases = 2, phyDataBits = 64, padDataBits = 8)
  val io = IO(new Bundle {
    val phase0Address = Input(Bool())
    val phase1Address = Input(Bool())
    val phase0ChipSelectN = Input(Bool())
    val phase1ChipSelectN = Input(Bool())
    val addressPad = Output(Bool())
    val chipSelectNPad = Output(Bool())
    val clockPositive = Output(Bool())
    val clockNegative = Output(Bool())
    val dq = Vec(8, Analog(1.W))
    val dqsPositive = Analog(1.W)
    val dqsNegative = Analog(1.W)
  })
  private val core = Module(new Ecp5DdrPhy(config, readLatency = 4,
    readCommandTap = 1, writeLatency = 2))
  private val pads = Module(new GowinDdrPhyIo(config, GowinFamily.GW2A))

  for ((phase, index) <- core.io.dfi.phases.zipWithIndex) {
    val addressBit = if (index == 0) io.phase0Address else io.phase1Address
    phase.address := Cat(0.U((phase.address.getWidth - 1).W), addressBit)
    phase.bank := 0.U
    phase.csN(0) := (if (index == 0) io.phase0ChipSelectN else io.phase1ChipSelectN)
    phase.rasN := true.B
    phase.casN := true.B
    phase.weN := true.B
    phase.actN := true.B
    phase.cke.foreach(_ := false.B)
    phase.odt.foreach(_ := false.B)
    phase.resetN := true.B
    phase.rddataEn := false.B
    phase.wrdataEn := false.B
    phase.wrdata := 0.U
    phase.wrdataMask := 0.U
    phase.rddata := 0.U
    phase.rddataValid := false.B
  }
  core.io.delaySelect := 0.U
  core.io.readBitslipReset := false.B
  core.io.readBitslip := false.B
  core.io.dqHalfIn := pads.io.dqHalfIn

  pads.io.reset := reset.asBool
  pads.io.initClock := clock
  pads.io.systemClock := clock
  pads.io.edgeClock := clock
  pads.io.dllReset := reset.asBool
  pads.io.delaySelect := 0.U
  pads.io.inputDelayReset := false.B
  pads.io.inputDelayIncrement := false.B
  pads.io.burstDetectClear := false.B
  pads.io.parallel := core.io.output
  io.addressPad := pads.io.pads.address(0)
  io.chipSelectNPad := pads.io.pads.chipSelectN(0)
  io.clockPositive := pads.io.pads.clockPositive
  io.clockNegative := pads.io.pads.clockNegative
  for (bit <- 0 until 8) attach(io.dq(bit), pads.io.pads.dq(bit))
  attach(io.dqsPositive, pads.io.pads.dqsPositive(0))
  attach(io.dqsNegative, pads.io.pads.dqsNegative(0))
}

class GowinDdrPhyIoSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val verilator = Seq(VerilatorBackendAnnotation,
    VerilatorCFlags(Seq("-DWData=IData")))
  private val synthesis = Seq(VerilatorBackendAnnotation,
    VerilatorFlags(Seq("-DSYNTHESIS", "-Wno-MODMISSING")),
    VerilatorCFlags(Seq("-DWData=IData")))

  behavior of "Gowin GW2A/GW5A DDR3 PHY I/O"

  it should "assemble differential pads and trained lanes for both families" in {
    Seq(GowinFamily.GW2A, GowinFamily.GW5A).foreach { family =>
      test(new GowinDdrPhyIoHarness(family)).withAnnotations(verilator) { dut =>
        dut.io.address0.poke(1.U)
        dut.io.chipSelectN.poke(0.U)
        dut.io.dataMask.poke(1.U)
        dut.io.delaySelect.poke(false.B)
        dut.io.delayReset.poke(false.B)
        dut.io.delayIncrement.poke(false.B)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        dut.io.address0Pad.expect(true.B)
        dut.io.chipSelectNPad.expect(false.B)
        dut.io.clockPositive.expect(false.B)
        dut.io.clockNegative.expect(true.B)
        dut.io.dataMaskPad.expect(true.B)
        dut.io.dataValid.expect(true.B)

        dut.io.delaySelect.poke(true.B)
        dut.io.delayIncrement.poke(true.B)
        dut.clock.step()
        dut.io.inputDelayValue.expect(1.U)
        dut.io.delayIncrement.poke(false.B)
        dut.io.delayReset.poke(true.B)
        dut.clock.step()
        dut.io.inputDelayValue.expect(0.U)
      }
    }
  }

  it should "connect the portable half-rate core through the Gowin pads" in {
    test(new GowinDdrPhyIntegrationHarness).withAnnotations(verilator) { dut =>
      dut.io.phase0Address.poke(true.B)
      dut.io.phase1Address.poke(false.B)
      dut.io.phase0ChipSelectN.poke(false.B)
      dut.io.phase1ChipSelectN.poke(true.B)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      dut.io.addressPad.expect(true.B)
      dut.io.chipSelectNPad.expect(false.B)
      dut.io.clockPositive.expect(false.B)
      dut.io.clockNegative.expect(true.B)
    }
  }

  it should "parse each complete Gowin memory primitive assembly" in {
    Seq(GowinFamily.GW2A, GowinFamily.GW5A).foreach { family =>
      test(new GowinDdrPhyIoHarness(family)).withAnnotations(synthesis) { dut =>
        dut.io.address0.poke(0.U)
        dut.io.chipSelectN.poke("hf".U)
        dut.io.dataMask.poke(0.U)
        dut.io.delaySelect.poke(false.B)
        dut.io.delayReset.poke(false.B)
        dut.io.delayIncrement.poke(false.B)
        dut.clock.step()
      }
    }
  }
}
