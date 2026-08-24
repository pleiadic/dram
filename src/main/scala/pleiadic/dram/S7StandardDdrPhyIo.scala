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

/** Artix-7 1:4 DDR2/DDR3/DDR4 pad assembly (without ODELAYE2). */
class S7StandardDdrPhyIo(config: DramConfig, refClockFrequencyMHz: Int = 200)
    extends RawModule {
  require(Set("DDR2", "DDR3", "DDR4").contains(config.memType))
  require(config.nPhases == 4)
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
    val parallel = Input(new StandardDdrPhyOutput(config))
    val dqIn = Output(Vec(padBits, UInt(8.W)))
    val dqDelayValue = Output(Vec(padBits, UInt(5.W)))
    val pads = new S7StandardDdrPads(config)
  })

  private def singleEndedOutput(value: UInt): Bool = {
    val serializer = Module(new S7OutputSerdes(8, "DDR"))
    serializer.io.reset := io.reset
    serializer.io.serialClock := io.serialClock
    serializer.io.dividedClock := io.dividedClock
    serializer.io.data := value
    serializer.io.outputEnable := true.B
    serializer.io.serial
  }

  private val clockLane = Module(new S7DifferentialOutputSerdesLane)
  clockLane.io.reset := io.reset
  clockLane.io.serialClock := io.serialClock
  clockLane.io.dividedClock := io.dividedClock
  clockLane.io.parallelOut := io.parallel.clock
  attach(clockLane.io.padPositive, io.pads.clockPositive)
  attach(clockLane.io.padNegative, io.pads.clockNegative)

  for (bit <- 0 until addressBits) {
    io.pads.address(bit) := singleEndedOutput(io.parallel.address(bit))
  }
  for (bit <- 0 until config.bankBits) {
    io.pads.bank(bit) := singleEndedOutput(io.parallel.bank(bit))
  }
  for (rank <- 0 until config.nranks) {
    io.pads.chipSelectN(rank) := singleEndedOutput(io.parallel.chipSelectN(rank))
    io.pads.clockEnable(rank) := singleEndedOutput(io.parallel.clockEnable(rank))
    io.pads.onDieTermination(rank) :=
      singleEndedOutput(io.parallel.onDieTermination(rank))
  }
  io.pads.rowStrobeN := singleEndedOutput(io.parallel.rowStrobeN)
  io.pads.columnStrobeN := singleEndedOutput(io.parallel.columnStrobeN)
  io.pads.writeEnableN := singleEndedOutput(io.parallel.writeEnableN)
  io.pads.activateN := singleEndedOutput(io.parallel.activateN)
  io.pads.resetN := singleEndedOutput(io.parallel.resetN)

  for (bit <- 0 until padBits) {
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
    attach(lane.io.pad, io.pads.dq(bit))
  }

  private val delayedDqs = withClockAndReset(io.dividedClock, io.reset) {
    RegInit(VecInit(Seq.fill(padBytes)(0.U(8.W))))
  }
  delayedDqs := io.parallel.dqs
  for (byte <- 0 until padBytes) {
    val dqs = Module(new S7DifferentialOutputSerdesIoLane)
    dqs.io.reset := io.reset
    dqs.io.serialClock := io.shiftedDqsOutputClock
    dqs.io.dividedClock := io.dividedClock
    dqs.io.parallelOut := delayedDqs(byte)
    dqs.io.outputEnable := io.parallel.dqsOutputEnable
    attach(dqs.io.padPositive, io.pads.dqsPositive(byte))
    attach(dqs.io.padNegative, io.pads.dqsNegative(byte))
    io.pads.dataMask(byte) := singleEndedOutput(io.parallel.dataMask(byte))
  }
}
