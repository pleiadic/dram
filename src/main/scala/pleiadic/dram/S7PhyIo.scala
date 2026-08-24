package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import chisel3.util._
import scala.language.reflectiveCalls

class S7Lpddr5Pads(config: DramConfig) extends Bundle {
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  val resetN = Output(Bool())
  val clockPositive = Analog(1.W)
  val clockNegative = Analog(1.W)
  val cs = Output(Bool())
  val ca = Output(Vec(7, Bool()))
  val dq = Vec(padBits, Analog(1.W))
  val wckPositive = Vec(padBytes, Analog(1.W))
  val wckNegative = Vec(padBytes, Analog(1.W))
  val readDqsPositive = Vec(padBytes, Analog(1.W))
  val readDqsNegative = Vec(padBytes, Analog(1.W))
  val dmi = Vec(padBytes, Analog(1.W))
}

/**
  * Artix-7-compatible LPDDR5 pad assembly. All physical streams use an 8:1
  * DDR OSERDESE2/ISERDESE2 for equal latency. Narrow logical streams are
  * repeated on output and decimated on input exactly as LiteDRAM S7Common.
  * External clock generation supplies the DQS-shifted output clock.
  */
class S7Lpddr5PhyIo(config: DramConfig, wckCkRatio: Int,
    refClockFrequencyMHz: Int = 200) extends RawModule {
  require(config.memType == "LPDDR5" && config.nPhases == 1)
  require(Set(2, 4).contains(wckCkRatio))
  private val edgeCount = 2 * wckCkRatio
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8

  val io = IO(new Bundle {
    val reset = Input(Bool())
    val dividedClock = Input(Clock())
    val serialClock = Input(Clock())
    val invertedSerialClock = Input(Clock())
    val shiftedStrobeOutputClock = Input(Clock())
    val delayClock = Input(Clock())
    val delaySelect = Input(UInt(padBytes.W))
    val delayReset = Input(Bool())
    val delayIncrement = Input(Bool())
    val parallel = Input(new Lpddr5PhyOutput(config, wckCkRatio))
    val dqIn = Output(Vec(padBits, UInt(edgeCount.W)))
    val dmiIn = Output(Vec(padBytes, UInt(edgeCount.W)))
    val readDqsIn = Output(Vec(padBytes, UInt(edgeCount.W)))
    val dqDelayValue = Output(Vec(padBits, UInt(5.W)))
    val dmiDelayValue = Output(Vec(padBytes, UInt(5.W)))
    val readDqsDelayValue = Output(Vec(padBytes, UInt(5.W)))
    val pads = new S7Lpddr5Pads(config)
  })

  private def expand(value: UInt, width: Int): UInt = {
    require(8 % width == 0)
    val repeats = 8 / width
    VecInit((0 until 8).map(edge => value(edge / repeats))).asUInt
  }

  private def decimate(value: UInt): UInt = {
    val step = 8 / edgeCount
    VecInit((0 until edgeCount).map(edge => value(edge * step))).asUInt
  }

  private def singleEndedOutput(value: UInt, clock: Clock): Bool = {
    val serializer = Module(new S7OutputSerdes(8, "DDR"))
    serializer.io.reset := io.reset
    serializer.io.serialClock := clock
    serializer.io.dividedClock := io.dividedClock
    serializer.io.data := value
    serializer.io.outputEnable := true.B
    serializer.io.serial
  }

  private val clockLane = Module(new S7DifferentialOutputSerdesLane)
  clockLane.io.reset := io.reset
  clockLane.io.serialClock := io.serialClock
  clockLane.io.dividedClock := io.dividedClock
  clockLane.io.parallelOut := expand(io.parallel.clock, 2)
  attach(clockLane.io.padPositive, io.pads.clockPositive)
  attach(clockLane.io.padNegative, io.pads.clockNegative)

  private val resetTwo = Fill(2, io.parallel.resetN)
  private val resetSlip = Module(new S7ConstantBitSlip(2, slip = 1))
  resetSlip.io.clock := io.dividedClock
  resetSlip.io.reset := io.reset
  resetSlip.io.input := resetTwo
  io.pads.resetN := singleEndedOutput(expand(resetSlip.io.output, 2), io.serialClock)

  private val csSlip = Module(new S7ConstantBitSlip(2, slip = 1))
  csSlip.io.clock := io.dividedClock
  csSlip.io.reset := io.reset
  csSlip.io.input := Fill(2, io.parallel.cs)
  io.pads.cs := singleEndedOutput(expand(csSlip.io.output, 2), io.serialClock)

  for (line <- 0 until 7) {
    val widened = VecInit((0 until 4).map(edge => io.parallel.ca(line)(edge / 2))).asUInt
    val slip = Module(new S7ConstantBitSlip(4, slip = 3))
    slip.io.clock := io.dividedClock
    slip.io.reset := io.reset
    slip.io.input := widened
    io.pads.ca(line) := singleEndedOutput(expand(slip.io.output, 4), io.serialClock)
  }

  for (byte <- 0 until padBytes) {
    val lane = Module(new S7DifferentialOutputSerdesLane)
    lane.io.reset := io.reset
    lane.io.serialClock := io.serialClock
    lane.io.dividedClock := io.dividedClock
    lane.io.parallelOut := expand(io.parallel.wck(byte), edgeCount)
    attach(lane.io.padPositive, io.pads.wckPositive(byte))
    attach(lane.io.padNegative, io.pads.wckNegative(byte))
  }

  for (bit <- 0 until padBits) {
    val lane = Module(new S7BidirectionalSerdesLane(8, "DDR", refClockFrequencyMHz))
    lane.io.reset := io.reset
    lane.io.outputSerialClock := io.serialClock
    lane.io.inputSerialClock := io.serialClock
    lane.io.invertedSerialClock := io.invertedSerialClock
    lane.io.dividedClock := io.dividedClock
    lane.io.delayClock := io.delayClock
    lane.io.delayReset := io.delayReset && io.delaySelect(bit / 8)
    lane.io.delayIncrement := io.delayIncrement && io.delaySelect(bit / 8)
    lane.io.bitslip := false.B
    lane.io.parallelOut := expand(io.parallel.dq(bit), edgeCount)
    lane.io.outputEnable := io.parallel.dqOutputEnable
    io.dqIn(bit) := decimate(lane.io.parallelIn)
    io.dqDelayValue(bit) := lane.io.delayValue
    attach(lane.io.pad, io.pads.dq(bit))
  }

  for (byte <- 0 until padBytes) {
    val lane = Module(new S7BidirectionalSerdesLane(8, "DDR", refClockFrequencyMHz))
    lane.io.reset := io.reset
    lane.io.outputSerialClock := io.serialClock
    lane.io.inputSerialClock := io.serialClock
    lane.io.invertedSerialClock := io.invertedSerialClock
    lane.io.dividedClock := io.dividedClock
    lane.io.delayClock := io.delayClock
    lane.io.delayReset := io.delayReset && io.delaySelect(byte)
    lane.io.delayIncrement := io.delayIncrement && io.delaySelect(byte)
    lane.io.bitslip := false.B
    lane.io.parallelOut := expand(io.parallel.dmi(byte), edgeCount)
    lane.io.outputEnable := io.parallel.dmiOutputEnable
    io.dmiIn(byte) := decimate(lane.io.parallelIn)
    io.dmiDelayValue(byte) := lane.io.delayValue
    attach(lane.io.pad, io.pads.dmi(byte))

    val strobe = Module(new S7DifferentialBidirectionalSerdesLane(refClockFrequencyMHz))
    strobe.io.reset := io.reset
    strobe.io.outputSerialClock := io.shiftedStrobeOutputClock
    strobe.io.inputSerialClock := io.serialClock
    strobe.io.invertedInputSerialClock := io.invertedSerialClock
    strobe.io.dividedClock := io.dividedClock
    strobe.io.delayClock := io.delayClock
    strobe.io.delayReset := io.delayReset && io.delaySelect(byte)
    strobe.io.delayIncrement := io.delayIncrement && io.delaySelect(byte)
    strobe.io.bitslip := false.B
    strobe.io.parallelOut := expand(io.parallel.readDqs(byte), edgeCount)
    strobe.io.outputEnable := io.parallel.readDqsOutputEnable
    io.readDqsIn(byte) := decimate(strobe.io.parallelIn)
    io.readDqsDelayValue(byte) := strobe.io.delayValue
    attach(strobe.io.padPositive, io.pads.readDqsPositive(byte))
    attach(strobe.io.padNegative, io.pads.readDqsNegative(byte))
  }
}

class S7RpcPads(config: DramConfig) extends Bundle {
  private val padBits = config.effectivePadDataBits
  val clockPositive = Analog(1.W)
  val clockNegative = Analog(1.W)
  val strobe = Analog(1.W)
  val chipSelectN = Output(Bool())
  val dqsPositive = Analog(1.W)
  val dqsNegative = Analog(1.W)
  val db = Vec(padBits, Analog(1.W))
}

/** Artix-7 RPC pad assembly matching the phase-separated LiteDRAM wrapper. */
class S7RpcPhyIo(config: DramConfig, refClockFrequencyMHz: Int = 200) extends RawModule {
  require(config.memType == "RPC" && config.nPhases == 4)
  require(config.effectivePadDataBits == 16)
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8

  val io = IO(new Bundle {
    val reset = Input(Bool())
    val dividedClock = Input(Clock())
    val clockOutputSerialClock = Input(Clock())
    val commandOutputSerialClock = Input(Clock())
    val dataInputSerialClock = Input(Clock())
    val invertedDataInputSerialClock = Input(Clock())
    val dqsOutputSerialClock = Input(Clock())
    val dqsInputSerialClock = Input(Clock())
    val invertedDqsInputSerialClock = Input(Clock())
    val delayClock = Input(Clock())
    val delaySelect = Input(UInt(padBytes.W))
    val delayReset = Input(Bool())
    val delayIncrement = Input(Bool())
    val dbEnable = Input(Bool())
    val dqsEnable = Input(Bool())
    val parallel = Input(new RpcPhyOutput(config))
    val dbIn = Output(Vec(padBits, UInt(8.W)))
    val dqsIn = Output(UInt(8.W))
    val strobeIn = Output(UInt(8.W))
    val dbDelayValue = Output(Vec(padBits, UInt(5.W)))
    val dqsDelayValue = Output(UInt(5.W))
    val pads = new S7RpcPads(config)
  })

  private val clockLane = Module(new S7DifferentialOutputSerdesLane)
  clockLane.io.reset := io.reset
  clockLane.io.serialClock := io.clockOutputSerialClock
  clockLane.io.dividedClock := io.dividedClock
  clockLane.io.parallelOut := io.parallel.clock
  attach(clockLane.io.padPositive, io.pads.clockPositive)
  attach(clockLane.io.padNegative, io.pads.clockNegative)

  private val chipSelect = Module(new S7OutputSerdes(8, "DDR"))
  chipSelect.io.reset := io.reset
  chipSelect.io.serialClock := io.commandOutputSerialClock
  chipSelect.io.dividedClock := io.dividedClock
  chipSelect.io.data := io.parallel.chipSelectN
  chipSelect.io.outputEnable := true.B
  io.pads.chipSelectN := chipSelect.io.serial

  private val strobe = Module(new S7BidirectionalSerdesLane(8, "DDR",
    refClockFrequencyMHz))
  strobe.io.reset := io.reset
  strobe.io.outputSerialClock := io.commandOutputSerialClock
  strobe.io.inputSerialClock := io.dataInputSerialClock
  strobe.io.invertedSerialClock := io.invertedDataInputSerialClock
  strobe.io.dividedClock := io.dividedClock
  strobe.io.delayClock := io.delayClock
  strobe.io.delayReset := io.delayReset && io.delaySelect(0)
  strobe.io.delayIncrement := io.delayIncrement && io.delaySelect(0)
  strobe.io.bitslip := false.B
  strobe.io.parallelOut := io.parallel.strobe
  strobe.io.outputEnable := true.B
  io.strobeIn := strobe.io.parallelIn
  attach(strobe.io.pad, io.pads.strobe)

  for (bit <- 0 until padBits) {
    val lane = Module(new S7BidirectionalSerdesLane(8, "DDR", refClockFrequencyMHz))
    lane.io.reset := io.reset
    lane.io.outputSerialClock := io.commandOutputSerialClock
    lane.io.inputSerialClock := io.dataInputSerialClock
    lane.io.invertedSerialClock := io.invertedDataInputSerialClock
    lane.io.dividedClock := io.dividedClock
    lane.io.delayClock := io.delayClock
    lane.io.delayReset := io.delayReset && io.delaySelect(bit / 8)
    lane.io.delayIncrement := io.delayIncrement && io.delaySelect(bit / 8)
    lane.io.bitslip := false.B
    lane.io.parallelOut := io.parallel.db(bit)
    lane.io.outputEnable := io.parallel.dbOutputEnable && io.dbEnable
    io.dbIn(bit) := lane.io.parallelIn
    io.dbDelayValue(bit) := lane.io.delayValue
    attach(lane.io.pad, io.pads.db(bit))
  }

  private val dqs = Module(new S7DifferentialBidirectionalSerdesLane(
    refClockFrequencyMHz))
  dqs.io.reset := io.reset
  dqs.io.outputSerialClock := io.dqsOutputSerialClock
  dqs.io.inputSerialClock := io.dqsInputSerialClock
  dqs.io.invertedInputSerialClock := io.invertedDqsInputSerialClock
  dqs.io.dividedClock := io.dividedClock
  dqs.io.delayClock := io.delayClock
  dqs.io.delayReset := io.delayReset && io.delaySelect(0)
  dqs.io.delayIncrement := io.delayIncrement && io.delaySelect(0)
  dqs.io.bitslip := false.B
  dqs.io.parallelOut := io.parallel.dqs
  dqs.io.outputEnable := io.parallel.dqsOutputEnable && io.dqsEnable
  io.dqsIn := dqs.io.parallelIn
  io.dqsDelayValue := dqs.io.delayValue
  attach(dqs.io.padPositive, io.pads.dqsPositive)
  attach(dqs.io.padNegative, io.pads.dqsNegative)
}
