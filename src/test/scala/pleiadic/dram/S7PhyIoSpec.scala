package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags, VerilatorFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class S7ConstantBitSlipHarness extends Module {
  val io = IO(new Bundle {
    val input2 = Input(UInt(2.W))
    val input4 = Input(UInt(4.W))
    val output2 = Output(UInt(2.W))
    val output4 = Output(UInt(4.W))
  })

  private val slip2 = Module(new S7ConstantBitSlip(width = 2, slip = 1))
  private val slip4 = Module(new S7ConstantBitSlip(width = 4, slip = 3))
  for (slip <- Seq(slip2, slip4)) {
    slip.io.clock := clock
    slip.io.reset := reset.asBool
  }
  slip2.io.input := io.input2
  slip4.io.input := io.input4
  io.output2 := slip2.io.output
  io.output4 := slip4.io.output
}

/** Keeps all Verilator-visible data ports narrow while exercising every LPDDR5 pad lane. */
class S7Lpddr5PhyIoHarness(wckCkRatio: Int, withOutputDelay: Boolean = false)
    extends Module {
  private val config = DramConfig(addressBits = 40, dataBits = 128, bankBits = 7,
    rowBits = 18, columnBits = 10, memType = "LPDDR5", padDataBits = 8)
  val io = IO(new Bundle {
    val resetN = Input(Bool())
    val cs = Input(Bool())
    val ca0 = Input(UInt(2.W))
    val resetNPad = Output(Bool())
    val csPad = Output(Bool())
    val ca0Pad = Output(Bool())
    val dqDelayValue = Output(UInt(5.W))
    val dmiDelayValue = Output(UInt(5.W))
    val readDqsDelayValue = Output(UInt(5.W))
    val dqOutputDelayValue = Output(UInt(5.W))
    val dmiOutputDelayValue = Output(UInt(5.W))
    val readDqsOutputDelayValue = Output(UInt(5.W))
    val clockPositive = Analog(1.W)
    val clockNegative = Analog(1.W)
    val dq = Vec(8, Analog(1.W))
    val wckPositive = Analog(1.W)
    val wckNegative = Analog(1.W)
    val readDqsPositive = Analog(1.W)
    val readDqsNegative = Analog(1.W)
    val dmi = Analog(1.W)
  })

  private val phy = Module(new S7Lpddr5PhyIo(config, wckCkRatio,
    withOutputDelay = withOutputDelay, readDqsOutputDelayInitialValue = 9))
  phy.io.reset := reset.asBool
  phy.io.dividedClock := clock
  phy.io.serialClock := clock
  phy.io.invertedSerialClock := (!clock.asBool).asClock
  phy.io.shiftedStrobeOutputClock := clock
  phy.io.delayClock := clock
  phy.io.delaySelect := 0.U
  phy.io.delayReset := false.B
  phy.io.delayIncrement := false.B
  phy.io.commandOutputDelayReset := false.B
  phy.io.commandOutputDelayIncrement := false.B
  phy.io.dataOutputDelayReset := false.B
  phy.io.dataOutputDelayIncrement := false.B
  phy.io.readDqsOutputDelayReset := false.B
  phy.io.readDqsOutputDelayIncrement := false.B
  phy.io.parallel.resetN := io.resetN
  phy.io.parallel.clock := 1.U
  phy.io.parallel.cs := io.cs
  for (line <- 0 until 7) {
    phy.io.parallel.ca(line) := (if (line == 0) io.ca0 else 0.U)
  }
  phy.io.parallel.dq.foreach(_ := 0.U)
  phy.io.parallel.dqOutputEnable := false.B
  phy.io.parallel.wck.foreach(_ := 0.U)
  phy.io.parallel.readDqs.foreach(_ := 0.U)
  phy.io.parallel.readDqsOutputEnable := false.B
  phy.io.parallel.dmi.foreach(_ := 0.U)
  phy.io.parallel.dmiOutputEnable := false.B

  io.resetNPad := phy.io.pads.resetN
  io.csPad := phy.io.pads.cs
  io.ca0Pad := phy.io.pads.ca(0)
  io.dqDelayValue := phy.io.dqDelayValue(0)
  io.dmiDelayValue := phy.io.dmiDelayValue(0)
  io.readDqsDelayValue := phy.io.readDqsDelayValue(0)
  io.dqOutputDelayValue := phy.io.dqOutputDelayValue(0)
  io.dmiOutputDelayValue := phy.io.dmiOutputDelayValue(0)
  io.readDqsOutputDelayValue := phy.io.readDqsOutputDelayValue(0)
  attach(io.clockPositive, phy.io.pads.clockPositive)
  attach(io.clockNegative, phy.io.pads.clockNegative)
  for (bit <- 0 until 8) attach(io.dq(bit), phy.io.pads.dq(bit))
  attach(io.wckPositive, phy.io.pads.wckPositive(0))
  attach(io.wckNegative, phy.io.pads.wckNegative(0))
  attach(io.readDqsPositive, phy.io.pads.readDqsPositive(0))
  attach(io.readDqsNegative, phy.io.pads.readDqsNegative(0))
  attach(io.dmi, phy.io.pads.dmi(0))
}

/** Narrow wrapper for the fixed 16-bit RPC pad assembly. */
class S7RpcPhyIoHarness extends Module {
  private val config = DramConfig(addressBits = 32, dataBits = 256, bankBits = 2,
    rowBits = 12, columnBits = 10, memType = "RPC", nPhases = 4,
    padDataBits = 16)
  val io = IO(new Bundle {
    val chipSelectN = Input(UInt(8.W))
    val chipSelectNPad = Output(Bool())
    val dbDelayValue = Output(UInt(5.W))
    val dqsDelayValue = Output(UInt(5.W))
    val clockPositive = Analog(1.W)
    val clockNegative = Analog(1.W)
    val strobe = Analog(1.W)
    val dqsPositive = Analog(1.W)
    val dqsNegative = Analog(1.W)
    val db = Vec(16, Analog(1.W))
  })

  private val phy = Module(new S7RpcPhyIo(config))
  phy.io.reset := reset.asBool
  phy.io.dividedClock := clock
  phy.io.clockOutputSerialClock := clock
  phy.io.commandOutputSerialClock := clock
  phy.io.dataInputSerialClock := clock
  phy.io.invertedDataInputSerialClock := (!clock.asBool).asClock
  phy.io.dqsOutputSerialClock := clock
  phy.io.dqsInputSerialClock := clock
  phy.io.invertedDqsInputSerialClock := (!clock.asBool).asClock
  phy.io.delayClock := clock
  phy.io.delaySelect := 0.U
  phy.io.delayReset := false.B
  phy.io.delayIncrement := false.B
  phy.io.dbEnable := true.B
  phy.io.dqsEnable := true.B
  phy.io.parallel.clock := "h55".U
  phy.io.parallel.strobe := 0.U
  phy.io.parallel.chipSelectN := io.chipSelectN
  phy.io.parallel.dqs := 0.U
  phy.io.parallel.dqsOutputEnable := false.B
  phy.io.parallel.db.foreach(_ := 0.U)
  phy.io.parallel.dbOutputEnable := false.B

  io.chipSelectNPad := phy.io.pads.chipSelectN
  io.dbDelayValue := phy.io.dbDelayValue(0)
  io.dqsDelayValue := phy.io.dqsDelayValue
  attach(io.clockPositive, phy.io.pads.clockPositive)
  attach(io.clockNegative, phy.io.pads.clockNegative)
  attach(io.strobe, phy.io.pads.strobe)
  attach(io.dqsPositive, phy.io.pads.dqsPositive)
  attach(io.dqsNegative, phy.io.pads.dqsNegative)
  for (bit <- 0 until 16) attach(io.db(bit), phy.io.pads.db(bit))
}

class S7PhyIoSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val verilator = Seq(VerilatorBackendAnnotation,
    VerilatorCFlags(Seq("-DWData=IData")))

  behavior of "S7 fixed command bitslip"

  it should "match LiteDRAM ConstBitSlip across adjacent controller words" in {
    test(new S7ConstantBitSlipHarness) { dut =>
      def expected(width: Int, slip: Int, current: BigInt, previous: BigInt): BigInt = {
        val mask = (BigInt(1) << width) - 1
        (((current << width) | previous) >> (width - slip)) & mask
      }
      dut.io.input2.poke(0.U)
      dut.io.input4.poke(0.U)
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      var previous2 = BigInt(0)
      var previous4 = BigInt(0)
      val words = Seq((BigInt(1), BigInt(3)), (BigInt(2), BigInt(12)),
        (BigInt(3), BigInt(5)), (BigInt(0), BigInt(10)))
      for ((current2, current4) <- words) {
        dut.io.input2.poke(current2.U)
        dut.io.input4.poke(current4.U)
        dut.io.output2.expect(expected(2, 1, current2, previous2).U)
        dut.io.output4.expect(expected(4, 3, current4, previous4).U)
        dut.clock.step()
        previous2 = current2
        previous4 = current4
      }
    }
  }

  behavior of "S7 LPDDR5 PHY I/O"

  for (ratio <- Seq(2, 4)) {
    it should s"assemble every pad lane at WCK:CK $ratio:1" in {
      test(new S7Lpddr5PhyIoHarness(ratio)).withAnnotations(verilator) { dut =>
        dut.io.resetN.poke(true.B)
        dut.io.cs.poke(true.B)
        dut.io.ca0.poke(1.U)
        dut.clock.step(2)
        dut.io.resetNPad.expect(true.B)
        dut.io.csPad.expect(true.B)
        dut.io.ca0Pad.expect(true.B)
        dut.io.dqDelayValue.expect(0.U)
        dut.io.dmiDelayValue.expect(0.U)
        dut.io.readDqsDelayValue.expect(0.U)
        dut.io.dqOutputDelayValue.expect(0.U)
        dut.io.dmiOutputDelayValue.expect(0.U)
        dut.io.readDqsOutputDelayValue.expect(0.U)
      }
    }
  }

  it should "model Kintex/Virtex command data and RDQS output delays" in {
    test(new S7Lpddr5PhyIoHarness(wckCkRatio = 4, withOutputDelay = true))
      .withAnnotations(verilator) { dut =>
      dut.io.resetN.poke(true.B)
      dut.io.cs.poke(true.B)
      dut.io.ca0.poke(1.U)
      dut.clock.step(2)
      dut.io.resetNPad.expect(true.B)
      dut.io.dqOutputDelayValue.expect(0.U)
      dut.io.dmiOutputDelayValue.expect(0.U)
      dut.io.readDqsOutputDelayValue.expect(9.U)
    }
  }

  behavior of "S7 RPC PHY I/O"

  it should "assemble the phase-separated RPC pad lanes" in {
    test(new S7RpcPhyIoHarness).withAnnotations(verilator) { dut =>
      dut.io.chipSelectN.poke("ha5".U)
      dut.io.chipSelectNPad.expect(true.B)
      dut.io.dbDelayValue.expect(0.U)
      dut.io.dqsDelayValue.expect(0.U)
      dut.clock.step(2)
    }
  }

  it should "parse the real Xilinx primitive branches in the family assembly" in {
    test(new S7Lpddr5PhyIoHarness(wckCkRatio = 4, withOutputDelay = true))
      .withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq("-DSYNTHESIS", "-Wno-MODMISSING")),
      VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.resetN.poke(true.B)
      dut.io.cs.poke(false.B)
      dut.io.ca0.poke(2.U)
      dut.clock.step()
    }
  }
}
