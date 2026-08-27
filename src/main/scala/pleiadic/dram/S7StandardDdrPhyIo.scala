package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import scala.language.reflectiveCalls

class S7StandardDdrPads(config: DramConfig) extends Bundle {
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  val clockPositive = Analog(1.W)
  val clockNegative = Analog(1.W)
  val address = Output(Vec(addressBits, Bool()))
  // DDR4 board wrappers split this combined vector between BA and BG pins.
  val bank = Output(Vec(config.bankBits, Bool()))
  val chipSelectN = Output(Vec(config.nranks, Bool()))
  val rowStrobeN = Output(Bool())
  val columnStrobeN = Output(Bool())
  val writeEnableN = Output(Bool())
  val activateN = Output(Bool())
  val clockEnable = Output(Vec(config.nranks, Bool()))
  val onDieTermination = Output(Vec(config.nranks, Bool()))
  val resetN = Output(Bool())
  val dq = Vec(padBits, Analog(1.W))
  val dqsPositive = Vec(padBytes, Analog(1.W))
  val dqsNegative = Vec(padBytes, Analog(1.W))
  val dataMask = Output(Vec(padBytes, Bool()))
}

/**
  * Xilinx 7-series 1:4 DDR2/DDR3/DDR4 pad assembly. Artix-7 uses the external
  * shifted DQS clock; Kintex-7/Virtex-7 set `withOutputDelay` and use ODELAYE2.
  */
class S7StandardDdrPhyIo(config: DramConfig, refClockFrequencyMHz: Int = 200,
    withOutputDelay: Boolean = false, dqsOutputDelayInitialValue: Int = 0)
    extends RawModule {
  require(Set("DDR2", "DDR3", "DDR4").contains(config.memType))
  require(config.nPhases == 4)
  require(!withOutputDelay || (dqsOutputDelayInitialValue >= 0 &&
    dqsOutputDelayInitialValue < 32))
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8

  val io = IO(new Bundle {
    val reset = Input(Bool())
    val dividedClock = Input(Clock())
    val serialClock = Input(Clock())
    val invertedSerialClock = Input(Clock())
    val shiftedDqsOutputClock = Input(Clock())
    val delayClock = Input(Clock())
    val delaySelect = Input(UInt(padBytes.W))
    val dqInputDelayReset = Input(Bool())
    val dqInputDelayIncrement = Input(Bool())
    val commandOutputDelayReset = Input(Bool())
    val commandOutputDelayIncrement = Input(Bool())
    val dqOutputDelayReset = Input(Bool())
    val dqOutputDelayIncrement = Input(Bool())
    val dqsOutputDelayReset = Input(Bool())
    val dqsOutputDelayIncrement = Input(Bool())
    val parallel = Input(new StandardDdrPhyOutput(config))
    val dqIn = Output(Vec(padBits, UInt(8.W)))
    val dqDelayValue = Output(Vec(padBits, UInt(5.W)))
    val dqOutputDelayValue = Output(Vec(padBits, UInt(5.W)))
    val dqsOutputDelayValue = Output(Vec(padBytes, UInt(5.W)))
    val pads = new S7StandardDdrPads(config)
  })

  private def singleEndedOutput(value: UInt, delayReset: Bool,
      delayIncrement: Bool): Bool = {
    if (withOutputDelay) {
      val output = Module(new S7DelayedOutputSerdes(refClockFrequencyMHz))
      output.io.reset := io.reset
      output.io.serialClock := io.serialClock
      output.io.dividedClock := io.dividedClock
      output.io.delayClock := io.delayClock
      output.io.delayReset := io.reset || delayReset
      output.io.delayIncrement := delayIncrement
      output.io.parallelOut := value
      output.io.outputEnable := true.B
      output.io.serial
    } else {
      val serializer = Module(new S7OutputSerdes(8, "DDR"))
      serializer.io.reset := io.reset
      serializer.io.serialClock := io.serialClock
      serializer.io.dividedClock := io.dividedClock
      serializer.io.data := value
      serializer.io.outputEnable := true.B
      serializer.io.serial
    }
  }

  if (withOutputDelay) {
    val clockOutput = Module(new S7DelayedOutputSerdes(refClockFrequencyMHz))
    val clockBuffer = Module(new S7DifferentialOutputBuffer)
    clockOutput.io.reset := io.reset
    clockOutput.io.serialClock := io.serialClock
    clockOutput.io.dividedClock := io.dividedClock
    clockOutput.io.delayClock := io.delayClock
    clockOutput.io.delayReset := io.reset || io.commandOutputDelayReset
    clockOutput.io.delayIncrement := io.commandOutputDelayIncrement
    clockOutput.io.parallelOut := io.parallel.clock
    clockOutput.io.outputEnable := true.B
    clockBuffer.io.dataIn := clockOutput.io.serial
    attach(clockBuffer.io.padPositive, io.pads.clockPositive)
    attach(clockBuffer.io.padNegative, io.pads.clockNegative)
  } else {
    val clockLane = Module(new S7DifferentialOutputSerdesLane)
    clockLane.io.reset := io.reset
    clockLane.io.serialClock := io.serialClock
    clockLane.io.dividedClock := io.dividedClock
    clockLane.io.parallelOut := io.parallel.clock
    attach(clockLane.io.padPositive, io.pads.clockPositive)
    attach(clockLane.io.padNegative, io.pads.clockNegative)
  }

  for (bit <- 0 until addressBits) {
    io.pads.address(bit) := singleEndedOutput(io.parallel.address(bit),
      io.commandOutputDelayReset, io.commandOutputDelayIncrement)
  }
  for (bit <- 0 until config.bankBits) {
    io.pads.bank(bit) := singleEndedOutput(io.parallel.bank(bit),
      io.commandOutputDelayReset, io.commandOutputDelayIncrement)
  }
  for (rank <- 0 until config.nranks) {
    io.pads.chipSelectN(rank) := singleEndedOutput(io.parallel.chipSelectN(rank),
      io.commandOutputDelayReset, io.commandOutputDelayIncrement)
    io.pads.clockEnable(rank) := singleEndedOutput(io.parallel.clockEnable(rank),
      io.commandOutputDelayReset, io.commandOutputDelayIncrement)
    io.pads.onDieTermination(rank) :=
      singleEndedOutput(io.parallel.onDieTermination(rank),
        io.commandOutputDelayReset, io.commandOutputDelayIncrement)
  }
  io.pads.rowStrobeN := singleEndedOutput(io.parallel.rowStrobeN,
    io.commandOutputDelayReset, io.commandOutputDelayIncrement)
  io.pads.columnStrobeN := singleEndedOutput(io.parallel.columnStrobeN,
    io.commandOutputDelayReset, io.commandOutputDelayIncrement)
  io.pads.writeEnableN := singleEndedOutput(io.parallel.writeEnableN,
    io.commandOutputDelayReset, io.commandOutputDelayIncrement)
  io.pads.activateN := singleEndedOutput(io.parallel.activateN,
    io.commandOutputDelayReset, io.commandOutputDelayIncrement)
  io.pads.resetN := singleEndedOutput(io.parallel.resetN,
    io.commandOutputDelayReset, io.commandOutputDelayIncrement)

  for (bit <- 0 until padBits) {
    if (withOutputDelay) {
      val lane = Module(new S7DelayedBidirectionalSerdesLane(refClockFrequencyMHz))
      lane.io.reset := io.reset
      lane.io.outputSerialClock := io.serialClock
      lane.io.inputSerialClock := io.serialClock
      lane.io.invertedInputSerialClock := io.invertedSerialClock
      lane.io.dividedClock := io.dividedClock
      lane.io.delayClock := io.delayClock
      lane.io.inputDelayReset := io.dqInputDelayReset && io.delaySelect(bit / 8)
      lane.io.inputDelayIncrement := io.dqInputDelayIncrement && io.delaySelect(bit / 8)
      lane.io.outputDelayReset := io.reset ||
        (io.dqOutputDelayReset && io.delaySelect(bit / 8))
      lane.io.outputDelayIncrement := io.dqOutputDelayIncrement && io.delaySelect(bit / 8)
      lane.io.bitslip := false.B
      lane.io.parallelOut := io.parallel.dq(bit)
      lane.io.outputEnable := io.parallel.dqOutputEnable
      io.dqIn(bit) := lane.io.parallelIn
      io.dqDelayValue(bit) := lane.io.inputDelayValue
      io.dqOutputDelayValue(bit) := lane.io.outputDelayValue
      attach(lane.io.pad, io.pads.dq(bit))
    } else {
      val lane = Module(new S7BidirectionalSerdesLane(8, "DDR", refClockFrequencyMHz))
      lane.io.reset := io.reset
      lane.io.outputSerialClock := io.serialClock
      lane.io.inputSerialClock := io.serialClock
      lane.io.invertedSerialClock := io.invertedSerialClock
      lane.io.dividedClock := io.dividedClock
      lane.io.delayClock := io.delayClock
      lane.io.delayReset := io.dqInputDelayReset && io.delaySelect(bit / 8)
      lane.io.delayIncrement := io.dqInputDelayIncrement && io.delaySelect(bit / 8)
      lane.io.bitslip := false.B
      lane.io.parallelOut := io.parallel.dq(bit)
      lane.io.outputEnable := io.parallel.dqOutputEnable
      io.dqIn(bit) := lane.io.parallelIn
      io.dqDelayValue(bit) := lane.io.delayValue
      io.dqOutputDelayValue(bit) := 0.U
      attach(lane.io.pad, io.pads.dq(bit))
    }
  }

  private val delayedDqs = withClockAndReset(io.dividedClock, io.reset) {
    RegInit(VecInit(Seq.fill(padBytes)(0.U(8.W))))
  }
  delayedDqs := io.parallel.dqs
  for (byte <- 0 until padBytes) {
    if (withOutputDelay) {
      val dqs = Module(new S7DelayedDifferentialOutputSerdesIoLane(
        refClockFrequencyMHz, dqsOutputDelayInitialValue))
      dqs.io.reset := io.reset
      dqs.io.serialClock := io.serialClock
      dqs.io.dividedClock := io.dividedClock
      dqs.io.delayClock := io.delayClock
      dqs.io.delayReset := io.reset ||
        (io.dqsOutputDelayReset && io.delaySelect(byte))
      dqs.io.delayIncrement := io.dqsOutputDelayIncrement && io.delaySelect(byte)
      dqs.io.parallelOut := io.parallel.dqs(byte)
      dqs.io.outputEnable := io.parallel.dqsOutputEnable
      io.dqsOutputDelayValue(byte) := dqs.io.delayValue
      attach(dqs.io.padPositive, io.pads.dqsPositive(byte))
      attach(dqs.io.padNegative, io.pads.dqsNegative(byte))
    } else {
      val dqs = Module(new S7DifferentialOutputSerdesIoLane)
      dqs.io.reset := io.reset
      dqs.io.serialClock := io.shiftedDqsOutputClock
      dqs.io.dividedClock := io.dividedClock
      dqs.io.parallelOut := delayedDqs(byte)
      dqs.io.outputEnable := io.parallel.dqsOutputEnable
      io.dqsOutputDelayValue(byte) := 0.U
      attach(dqs.io.padPositive, io.pads.dqsPositive(byte))
      attach(dqs.io.padNegative, io.pads.dqsNegative(byte))
    }
    io.pads.dataMask(byte) := singleEndedOutput(io.parallel.dataMask(byte),
      io.dqOutputDelayReset && io.delaySelect(byte),
      io.dqOutputDelayIncrement && io.delaySelect(byte))
  }
}

class A7StandardDdrPhyIo(config: DramConfig, refClockFrequencyMHz: Int = 200)
    extends S7StandardDdrPhyIo(config, refClockFrequencyMHz, withOutputDelay = false)

class K7StandardDdrPhyIo(config: DramConfig, refClockFrequencyMHz: Int = 200,
    dqsOutputDelayInitialValue: Int = 0) extends S7StandardDdrPhyIo(config,
  refClockFrequencyMHz, withOutputDelay = true, dqsOutputDelayInitialValue)

class V7StandardDdrPhyIo(config: DramConfig, refClockFrequencyMHz: Int = 200,
    dqsOutputDelayInitialValue: Int = 0) extends S7StandardDdrPhyIo(config,
  refClockFrequencyMHz, withOutputDelay = true, dqsOutputDelayInitialValue)
