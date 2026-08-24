package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags, VerilatorFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class S7Lpddr4GearboxHarness extends Module {
  private val config = DramConfig(addressBits = 40, dataBits = 128, bankBits = 6,
    rowBits = 17, columnBits = 10, memType = "LPDDR4", nPhases = 8,
    padDataBits = 8)
  val io = IO(new Bundle {
    val word = Input(UInt(16.W))
    val control = Input(UInt(8.W))
    val outputEnable = Input(Bool())
    val halfInput = Input(UInt(8.W))
    val halfWord = Output(UInt(8.W))
    val halfControl = Output(UInt(4.W))
    val halfOutputEnable = Output(Bool())
    val fullInput = Output(UInt(16.W))
  })

  private val controllerClockLevel = RegInit(false.B)
  controllerClockLevel := !controllerClockLevel
  private val gearbox = Module(new S7Lpddr4Gearbox(config))
  gearbox.io.reset := reset.asBool
  gearbox.io.controllerClock := controllerClockLevel.asClock
  gearbox.io.doubleRateClock := clock
  gearbox.io.parallel.clock := io.word
  gearbox.io.parallel.clockEnable := io.control
  gearbox.io.parallel.onDieTermination := 0.U
  gearbox.io.parallel.resetN := 0.U
  gearbox.io.parallel.cs := 0.U
  gearbox.io.parallel.ca.foreach(_ := 0.U)
  gearbox.io.parallel.dq.foreach(_ := io.word)
  gearbox.io.parallel.dqOutputEnable := io.outputEnable
  gearbox.io.parallel.dqs.foreach(_ := io.word)
  gearbox.io.parallel.dqsOutputEnable := io.outputEnable
  gearbox.io.parallel.dmi.foreach(_ := io.word)
  gearbox.io.parallel.dmiOutputEnable := io.outputEnable
  gearbox.io.halfDqIn.foreach(_ := io.halfInput)
  gearbox.io.halfDqsIn.foreach(_ := io.halfInput)
  io.halfWord := gearbox.io.half.dq(0)
  io.halfControl := gearbox.io.half.clockEnable
  io.halfOutputEnable := gearbox.io.half.dqOutputEnable
  io.fullInput := gearbox.io.fullDqIn(0)
}

class S7OutputEnableDelayHarness extends Module {
  val io = IO(new Bundle {
    val input = Input(Bool())
    val extended = Output(Bool())
    val exact = Output(Bool())
    val cdc = Output(Bool())
  })
  private val extended = Module(new S7OutputEnableDelay(depth = 3, extend = true))
  private val exact = Module(new S7OutputEnableDelay(depth = 2, extend = false))
  for (delay <- Seq(extended, exact)) {
    delay.io.clock := clock
    delay.io.reset := reset.asBool
    delay.io.input := io.input
  }
  io.extended := extended.io.output
  io.exact := exact.io.output
  private val cdc = Module(new S7OutputEnableCdc)
  cdc.io.reset := reset.asBool
  cdc.io.dividedClock := clock
  cdc.io.outputClock := clock
  cdc.io.input := io.input
  io.cdc := cdc.io.output
}

class S7Lpddr4PhyIoHarness extends Module {
  private val config = DramConfig(addressBits = 40, dataBits = 128, bankBits = 6,
    rowBits = 17, columnBits = 10, memType = "LPDDR4", nPhases = 8,
    padDataBits = 8)
  val io = IO(new Bundle {
    val resetN = Input(UInt(8.W))
    val cs = Input(UInt(8.W))
    val ca0 = Input(UInt(8.W))
    val resetNPad = Output(Bool())
    val csPad = Output(Bool())
    val ca0Pad = Output(Bool())
    val dqDelayValue = Output(UInt(5.W))
    val dqsDelayValue = Output(UInt(5.W))
    val clockPositive = Analog(1.W)
    val clockNegative = Analog(1.W)
    val dq = Vec(8, Analog(1.W))
    val dqsPositive = Analog(1.W)
    val dqsNegative = Analog(1.W)
    val dmi = Analog(1.W)
  })

  private val controllerClockLevel = RegInit(false.B)
  controllerClockLevel := !controllerClockLevel
  private val phy = Module(new S7Lpddr4PhyIo(config))
  phy.io.reset := reset.asBool
  phy.io.controllerClock := controllerClockLevel.asClock
  phy.io.doubleRateClock := clock
  phy.io.serialClock := clock
  phy.io.invertedSerialClock := (!clock.asBool).asClock
  phy.io.shiftedDqsOutputClock := clock
  phy.io.delayClock := clock
  phy.io.delaySelect := 0.U
  phy.io.dqInputDelayReset := false.B
  phy.io.dqInputDelayIncrement := false.B
  phy.io.dqsInputDelayReset := false.B
  phy.io.dqsInputDelayIncrement := false.B
  phy.io.parallel.clock := "h5555".U
  phy.io.parallel.clockEnable := 0.U
  phy.io.parallel.onDieTermination := 0.U
  phy.io.parallel.resetN := io.resetN
  phy.io.parallel.cs := io.cs
  for (line <- 0 until 6) phy.io.parallel.ca(line) :=
    (if (line == 0) io.ca0 else 0.U)
  phy.io.parallel.dq.foreach(_ := 0.U)
  phy.io.parallel.dqOutputEnable := false.B
  phy.io.parallel.dqs.foreach(_ := 0.U)
  phy.io.parallel.dqsOutputEnable := false.B
  phy.io.parallel.dmi.foreach(_ := 0.U)
  phy.io.parallel.dmiOutputEnable := false.B

  io.resetNPad := phy.io.pads.resetN
  io.csPad := phy.io.pads.cs
  io.ca0Pad := phy.io.pads.ca(0)
  io.dqDelayValue := phy.io.dqDelayValue(0)
  io.dqsDelayValue := phy.io.dqsDelayValue(0)
  attach(io.clockPositive, phy.io.pads.clockPositive)
  attach(io.clockNegative, phy.io.pads.clockNegative)
  for (bit <- 0 until 8) attach(io.dq(bit), phy.io.pads.dq(bit))
  attach(io.dqsPositive, phy.io.pads.dqsPositive(0))
  attach(io.dqsNegative, phy.io.pads.dqsNegative(0))
  attach(io.dmi, phy.io.pads.dmi(0))
}

class S7Lpddr4PhyIoSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val verilator = Seq(VerilatorBackendAnnotation,
    VerilatorCFlags(Seq("-DWData=IData")))

  behavior of "S7 LPDDR4 16:8 gearbox"

  it should "emit the low half first and retain output enable with the word" in {
    test(new S7Lpddr4GearboxHarness) { dut =>
      dut.io.word.poke("habcd".U)
      dut.io.control.poke("h69".U)
      dut.io.outputEnable.poke(true.B)
      dut.io.halfInput.poke(0.U)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      dut.clock.step()
      dut.io.halfWord.expect("hcd".U)
      dut.io.halfControl.expect(9.U)
      dut.io.halfOutputEnable.expect(true.B)
      dut.io.word.poke("h1234".U)
      dut.io.control.poke("ha5".U)
      dut.io.outputEnable.poke(false.B)
      dut.clock.step()
      dut.io.halfWord.expect("hab".U)
      dut.io.halfControl.expect(6.U)
      dut.io.halfOutputEnable.expect(true.B)
      dut.clock.step()
      dut.io.halfWord.expect("h34".U)
      dut.io.halfControl.expect(5.U)
      dut.io.halfOutputEnable.expect(false.B)
      dut.clock.step()
      dut.io.halfWord.expect("h12".U)
      dut.io.halfControl.expect(10.U)
    }
  }

  it should "reassemble alternating input halves with reference pipeline latency" in {
    test(new S7Lpddr4GearboxHarness) { dut =>
      dut.io.word.poke(0.U)
      dut.io.control.poke(0.U)
      dut.io.outputEnable.poke(false.B)
      dut.io.halfInput.poke(0.U)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      val samples = Seq(0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88)
      for (sample <- samples) {
        dut.io.halfInput.poke(sample.U)
        dut.clock.step()
      }
      // The newest complete word is 0x7766. Deserializer.LATENCY=2 means the
      // controller boundary intentionally still presents the 0x5544 word.
      dut.io.fullInput.expect("h5544".U)
    }
  }

  behavior of "S7 output-enable alignment"

  it should "delay DQS exactly and extend the data window across three taps" in {
    test(new S7OutputEnableDelayHarness) { dut =>
      dut.io.input.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.io.input.poke(true.B)
      dut.io.extended.expect(false.B)
      dut.io.exact.expect(false.B)
      dut.clock.step()
      dut.io.extended.expect(true.B)
      dut.io.exact.expect(false.B)
      dut.io.input.poke(false.B)
      dut.clock.step()
      dut.io.extended.expect(true.B)
      dut.io.exact.expect(true.B)
      dut.clock.step()
      dut.io.extended.expect(true.B)
      dut.io.exact.expect(false.B)
      dut.clock.step()
      dut.io.extended.expect(false.B)
    }
  }

  it should "register RPC tristate control through both clock domains" in {
    test(new S7OutputEnableDelayHarness) { dut =>
      dut.io.input.poke(true.B)
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.io.cdc.expect(true.B)
      dut.io.input.poke(false.B)
      dut.clock.step()
      dut.io.cdc.expect(true.B)
      dut.clock.step()
      dut.io.cdc.expect(false.B)
      dut.io.input.poke(true.B)
      dut.clock.step()
      dut.io.cdc.expect(false.B)
      dut.clock.step()
      dut.io.cdc.expect(true.B)
    }
  }

  behavior of "S7 LPDDR4 PHY I/O"

  it should "assemble every Artix-7 pad lane through the gearbox" in {
    test(new S7Lpddr4PhyIoHarness).withAnnotations(verilator) { dut =>
      dut.io.resetN.poke("hff".U)
      dut.io.cs.poke("hff".U)
      dut.io.ca0.poke("hff".U)
      dut.clock.step(3)
      dut.io.resetNPad.expect(true.B)
      dut.io.csPad.expect(true.B)
      dut.io.ca0Pad.expect(true.B)
      dut.io.dqDelayValue.expect(0.U)
      dut.io.dqsDelayValue.expect(0.U)
    }
  }

  it should "parse the complete LPDDR4 Xilinx primitive branch" in {
    test(new S7Lpddr4PhyIoHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq("-DSYNTHESIS", "-Wno-MODMISSING")),
      VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.resetN.poke("hff".U)
      dut.io.cs.poke(0.U)
      dut.io.ca0.poke("h3c".U)
      dut.clock.step()
    }
  }
}
