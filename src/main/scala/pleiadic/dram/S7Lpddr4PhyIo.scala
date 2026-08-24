package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import chisel3.util._
import scala.language.reflectiveCalls

/** Half-width parallel boundary consumed by the S7 8:1 DDR primitives. */
class S7Lpddr4HalfRateOutput(config: DramConfig) extends Bundle {
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  val clock = UInt(8.W)
  val clockEnable = UInt(4.W)
  val onDieTermination = UInt(4.W)
  val resetN = UInt(4.W)
  val cs = UInt(4.W)
  val ca = Vec(6, UInt(4.W))
  val dq = Vec(padBits, UInt(8.W))
  val dqOutputEnable = Bool()
  val dqs = Vec(padBytes, UInt(8.W))
  val dqsOutputEnable = Bool()
  val dmi = Vec(padBytes, UInt(8.W))
  val dmiOutputEnable = Bool()
}

/**
  * Phase-aligned 16:8 gearbox matching LiteDRAM DoubleRateLPDDR4PHY.
  * Output words are registered on the controller clock and emitted low half
  * first on the double-rate clock. Input halves use the reference two-stage
  * controller-domain reconstruction so the coincident final sample is kept.
  */
class S7Lpddr4Gearbox(config: DramConfig) extends RawModule {
  require(config.memType == "LPDDR4" && config.nPhases == 8)
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8

  val io = IO(new Bundle {
    val reset = Input(Bool())
    val controllerClock = Input(Clock())
    val doubleRateClock = Input(Clock())
    val parallel = Input(new Lpddr4PhyOutput(config))
    val half = Output(new S7Lpddr4HalfRateOutput(config))
    val halfDqIn = Input(Vec(padBits, UInt(8.W)))
    val halfDqsIn = Input(Vec(padBytes, UInt(8.W)))
    val fullDqIn = Output(Vec(padBits, UInt(16.W)))
    val fullDqsIn = Output(Vec(padBytes, UInt(16.W)))
  })

  private val held = withClockAndReset(io.controllerClock, io.reset) {
    RegInit(0.U.asTypeOf(new Lpddr4PhyOutput(config)))
  }
  held := io.parallel

  // reset_cnt=-1 for a 2:1 LiteDRAM Serializer initializes the counter to 1;
  // the first double-rate edge advances it to the low half.
  private val phase = withClockAndReset(io.doubleRateClock, io.reset) {
    RegInit(true.B)
  }
  phase := !phase

  private def selectHalf(value: UInt, halfWidth: Int): UInt =
    Mux(phase, value(2 * halfWidth - 1, halfWidth), value(halfWidth - 1, 0))

  io.half.clock := selectHalf(held.clock, 8)
  io.half.clockEnable := selectHalf(held.clockEnable, 4)
  io.half.onDieTermination := selectHalf(held.onDieTermination, 4)
  io.half.resetN := selectHalf(held.resetN, 4)
  io.half.cs := selectHalf(held.cs, 4)
  for (line <- 0 until 6) io.half.ca(line) := selectHalf(held.ca(line), 4)
  for (bit <- 0 until padBits) io.half.dq(bit) := selectHalf(held.dq(bit), 8)
  for (byte <- 0 until padBytes) {
    io.half.dqs(byte) := selectHalf(held.dqs(byte), 8)
    io.half.dmi(byte) := selectHalf(held.dmi(byte), 8)
  }
  io.half.dqOutputEnable := held.dqOutputEnable
  io.half.dqsOutputEnable := held.dqsOutputEnable
  io.half.dmiOutputEnable := held.dmiOutputEnable

  private val dqLow = withClockAndReset(io.doubleRateClock, io.reset) {
    RegInit(VecInit(Seq.fill(padBits)(0.U(8.W))))
  }
  private val dqHigh = withClockAndReset(io.doubleRateClock, io.reset) {
    RegInit(VecInit(Seq.fill(padBits)(0.U(8.W))))
  }
  private val dqsLow = withClockAndReset(io.doubleRateClock, io.reset) {
    RegInit(VecInit(Seq.fill(padBytes)(0.U(8.W))))
  }
  private val dqsHigh = withClockAndReset(io.doubleRateClock, io.reset) {
    RegInit(VecInit(Seq.fill(padBytes)(0.U(8.W))))
  }
  withClockAndReset(io.doubleRateClock, io.reset) {
    when(phase) {
      dqHigh := io.halfDqIn
      dqsHigh := io.halfDqsIn
    }.otherwise {
      dqLow := io.halfDqIn
      dqsLow := io.halfDqsIn
    }
  }

  private val previousDq = withClockAndReset(io.controllerClock, io.reset) {
    RegInit(VecInit(Seq.fill(padBits)(0.U(16.W))))
  }
  private val resultDq = withClockAndReset(io.controllerClock, io.reset) {
    RegInit(VecInit(Seq.fill(padBits)(0.U(16.W))))
  }
  private val previousDqs = withClockAndReset(io.controllerClock, io.reset) {
    RegInit(VecInit(Seq.fill(padBytes)(0.U(16.W))))
  }
  private val resultDqs = withClockAndReset(io.controllerClock, io.reset) {
    RegInit(VecInit(Seq.fill(padBytes)(0.U(16.W))))
  }
  for (bit <- 0 until padBits) {
    previousDq(bit) := Cat(dqHigh(bit), dqLow(bit))
    resultDq(bit) := Cat(dqHigh(bit), previousDq(bit)(7, 0))
  }
  for (byte <- 0 until padBytes) {
    previousDqs(byte) := Cat(dqsHigh(byte), dqsLow(byte))
    resultDqs(byte) := Cat(dqsHigh(byte), previousDqs(byte)(7, 0))
  }
  io.fullDqIn := resultDq
  io.fullDqsIn := resultDqs
}

class S7Lpddr4Pads(config: DramConfig) extends Bundle {
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  val resetN = Output(Bool())
  val clockPositive = Analog(1.W)
  val clockNegative = Analog(1.W)
  val clockEnable = Output(Bool())
  val onDieTermination = Output(Bool())
  val cs = Output(Bool())
  val ca = Output(Vec(6, Bool()))
  val dq = Vec(padBits, Analog(1.W))
  val dqsPositive = Vec(padBytes, Analog(1.W))
  val dqsNegative = Vec(padBytes, Analog(1.W))
  val dmi = Vec(padBytes, Analog(1.W))
}

/** Artix-7 LPDDR4 pad assembly using the external sys8x_90 DQS clock. */
class S7Lpddr4PhyIo(config: DramConfig, refClockFrequencyMHz: Int = 200)
    extends RawModule {
  require(config.memType == "LPDDR4" && config.nPhases == 8)
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8

  val io = IO(new Bundle {
    val reset = Input(Bool())
    val controllerClock = Input(Clock())
    val doubleRateClock = Input(Clock())
    val serialClock = Input(Clock())
    val invertedSerialClock = Input(Clock())
    val shiftedDqsOutputClock = Input(Clock())
    val delayClock = Input(Clock())
    val delaySelect = Input(UInt(padBytes.W))
    val dqInputDelayReset = Input(Bool())
    val dqInputDelayIncrement = Input(Bool())
    val dqsInputDelayReset = Input(Bool())
    val dqsInputDelayIncrement = Input(Bool())
    val parallel = Input(new Lpddr4PhyOutput(config))
    val dqIn = Output(Vec(padBits, UInt(16.W)))
    val dqsIn = Output(Vec(padBytes, UInt(16.W)))
    val dqDelayValue = Output(Vec(padBits, UInt(5.W)))
    val dqsDelayValue = Output(Vec(padBytes, UInt(5.W)))
    val pads = new S7Lpddr4Pads(config)
  })

  private def expandSdr(value: UInt): UInt =
    VecInit((0 until 8).map(edge => value(edge / 2))).asUInt

  private def singleEndedOutput(value: UInt): Bool = {
    val serializer = Module(new S7OutputSerdes(8, "DDR"))
    serializer.io.reset := io.reset
    serializer.io.serialClock := io.serialClock
    serializer.io.dividedClock := io.doubleRateClock
    serializer.io.data := expandSdr(value)
    serializer.io.outputEnable := true.B
    serializer.io.serial
  }

  private val gearbox = Module(new S7Lpddr4Gearbox(config))
  gearbox.io.reset := io.reset
  gearbox.io.controllerClock := io.controllerClock
  gearbox.io.doubleRateClock := io.doubleRateClock
  gearbox.io.parallel := io.parallel

  private val clockLane = Module(new S7DifferentialOutputSerdesLane)
  clockLane.io.reset := io.reset
  clockLane.io.serialClock := io.serialClock
  clockLane.io.dividedClock := io.doubleRateClock
  clockLane.io.parallelOut := ~gearbox.io.half.clock
  attach(clockLane.io.padPositive, io.pads.clockPositive)
  attach(clockLane.io.padNegative, io.pads.clockNegative)

  io.pads.clockEnable := singleEndedOutput(gearbox.io.half.clockEnable)
  io.pads.onDieTermination := singleEndedOutput(gearbox.io.half.onDieTermination)
  io.pads.resetN := singleEndedOutput(gearbox.io.half.resetN)
  io.pads.cs := singleEndedOutput(gearbox.io.half.cs)
  for (line <- 0 until 6) io.pads.ca(line) := singleEndedOutput(gearbox.io.half.ca(line))

  private val dqEnable = Module(new S7OutputEnableDelay(depth = 3, extend = true))
  dqEnable.io.clock := io.doubleRateClock
  dqEnable.io.reset := io.reset
  dqEnable.io.input := gearbox.io.half.dqOutputEnable
  private val dmiEnable = Module(new S7OutputEnableDelay(depth = 3, extend = true))
  dmiEnable.io.clock := io.doubleRateClock
  dmiEnable.io.reset := io.reset
  dmiEnable.io.input := gearbox.io.half.dmiOutputEnable
  private val dqsEnable = Module(new S7OutputEnableDelay(depth = 2, extend = false))
  dqsEnable.io.clock := io.doubleRateClock
  dqsEnable.io.reset := io.reset
  dqsEnable.io.input := gearbox.io.half.dqsOutputEnable

  private val halfDqIn = Wire(Vec(padBits, UInt(8.W)))
  for (bit <- 0 until padBits) {
    val lane = Module(new S7BidirectionalSerdesLane(8, "DDR", refClockFrequencyMHz))
    lane.io.reset := io.reset
    lane.io.outputSerialClock := io.serialClock
    lane.io.inputSerialClock := io.serialClock
    lane.io.invertedSerialClock := io.invertedSerialClock
    lane.io.dividedClock := io.doubleRateClock
    lane.io.delayClock := io.delayClock
    lane.io.delayReset := io.dqInputDelayReset && io.delaySelect(bit / 8)
    lane.io.delayIncrement := io.dqInputDelayIncrement && io.delaySelect(bit / 8)
    lane.io.bitslip := false.B
    lane.io.parallelOut := gearbox.io.half.dq(bit)
    lane.io.outputEnable := dqEnable.io.output
    halfDqIn(bit) := lane.io.parallelIn
    io.dqDelayValue(bit) := lane.io.delayValue
    attach(lane.io.pad, io.pads.dq(bit))
  }
  gearbox.io.halfDqIn := halfDqIn

  private val delayedDqs = withClockAndReset(io.doubleRateClock, io.reset) {
    RegInit(VecInit(Seq.fill(padBytes)(0.U(8.W))))
  }
  delayedDqs := gearbox.io.half.dqs
  private val halfDqsIn = Wire(Vec(padBytes, UInt(8.W)))
  for (byte <- 0 until padBytes) {
    val lane = Module(new S7DifferentialBidirectionalSerdesLane(refClockFrequencyMHz))
    lane.io.reset := io.reset
    lane.io.outputSerialClock := io.shiftedDqsOutputClock
    lane.io.inputSerialClock := io.serialClock
    lane.io.invertedInputSerialClock := io.invertedSerialClock
    lane.io.dividedClock := io.doubleRateClock
    lane.io.delayClock := io.delayClock
    lane.io.delayReset := io.dqsInputDelayReset && io.delaySelect(byte)
    lane.io.delayIncrement := io.dqsInputDelayIncrement && io.delaySelect(byte)
    lane.io.bitslip := false.B
    lane.io.parallelOut := delayedDqs(byte)
    lane.io.outputEnable := dqsEnable.io.output
    halfDqsIn(byte) := lane.io.parallelIn
    io.dqsDelayValue(byte) := lane.io.delayValue
    attach(lane.io.padPositive, io.pads.dqsPositive(byte))
    attach(lane.io.padNegative, io.pads.dqsNegative(byte))

    val dmi = Module(new S7OutputSerdesIoLane)
    dmi.io.reset := io.reset
    dmi.io.serialClock := io.serialClock
    dmi.io.dividedClock := io.doubleRateClock
    dmi.io.parallelOut := gearbox.io.half.dmi(byte)
    dmi.io.outputEnable := dmiEnable.io.output
    attach(dmi.io.pad, io.pads.dmi(byte))
  }
  gearbox.io.halfDqsIn := halfDqsIn
  io.dqIn := gearbox.io.fullDqIn
  io.dqsIn := gearbox.io.fullDqsIn
}
