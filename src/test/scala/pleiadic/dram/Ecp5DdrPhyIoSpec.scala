package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags, VerilatorFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class Ecp5DdrPhyIoHarness extends Module {
  private val config = DramConfig(addressBits = 40, dataBits = 64,
    bankBits = 3, rowBits = 15, columnBits = 10, memType = "DDR3",
    nPhases = 2, phyDataBits = 64, padDataBits = 8)
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  val io = IO(new Bundle {
    val address0 = Input(UInt(4.W))
    val chipSelectN = Input(UInt(4.W))
    val delaySelect = Input(Bool())
    val delayReset = Input(Bool())
    val delayIncrement = Input(Bool())
    val address0Pad = Output(Bool())
    val chipSelectNPad = Output(Bool())
    val clockPad = Output(Bool())
    val inputDelayValue = Output(UInt(3.W))
    val dataValid = Output(Bool())
    val burstSeen = Output(Bool())
    val initBusy = Output(Bool())
    val dq = Vec(8, Analog(1.W))
    val dqs = Analog(1.W)
  })

  private val phy = Module(new Ecp5DdrPhyIo(config,
    commandDelayInitialValue = 5))
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
  phy.io.parallel.dataMask.foreach(_ := 0.U)
  phy.io.parallel.dqOutputEnable := false.B
  phy.io.parallel.dqs.foreach(_ := "hf".U)
  phy.io.parallel.dqsOutputEnable := true.B
  phy.io.parallel.dqsPreamble := false.B
  phy.io.parallel.dqsPostamble := false.B
  phy.io.parallel.dqsReadEnable := true.B

  io.address0Pad := phy.io.pads.address(0)
  io.chipSelectNPad := phy.io.pads.chipSelectN(0)
  io.clockPad := phy.io.pads.clockPositive
  io.inputDelayValue := phy.io.inputDelayValue(0)
  io.dataValid := phy.io.dataValid(0)
  io.burstSeen := phy.io.burstSeen(0)
  io.initBusy := phy.io.initBusy
  for (bit <- 0 until 8) attach(io.dq(bit), phy.io.pads.dq(bit))
  attach(io.dqs, phy.io.pads.dqsPositive(0))
}

class Ecp5DdrPhyIoSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val verilator = Seq(VerilatorBackendAnnotation,
    VerilatorCFlags(Seq("-DWData=IData")))

  behavior of "ECP5 DDR3 PHY I/O"

  it should "assemble command DQS and trained data lanes" in {
    test(new Ecp5DdrPhyIoHarness).withAnnotations(verilator) { dut =>
      dut.io.address0.poke(1.U)
      dut.io.chipSelectN.poke(0.U)
      dut.io.delaySelect.poke(false.B)
      dut.io.delayReset.poke(false.B)
      dut.io.delayIncrement.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      dut.io.address0Pad.expect(true.B)
      dut.io.chipSelectNPad.expect(false.B)
      dut.io.clockPad.expect(false.B)
      dut.io.dataValid.expect(true.B)
      dut.io.inputDelayValue.expect(0.U)
      dut.clock.step()
      dut.io.burstSeen.expect(true.B)

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

  it should "parse the complete ECP5 primitive assembly" in {
    test(new Ecp5DdrPhyIoHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq("-DSYNTHESIS", "-Wno-MODMISSING")),
      VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.address0.poke(0.U)
      dut.io.chipSelectN.poke("hf".U)
      dut.io.delaySelect.poke(false.B)
      dut.io.delayReset.poke(false.B)
      dut.io.delayIncrement.poke(false.B)
      dut.clock.step()
    }
  }
}
