package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import chisel3.util.Fill
import scala.language.reflectiveCalls

class GowinDdrPads(config: DramConfig) extends Bundle {
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  val clockPositive = Output(Bool())
  val clockNegative = Output(Bool())
  val address = Output(Vec(addressBits, Bool()))
  val bank = Output(Vec(config.bankBits, Bool()))
  val chipSelectN = Output(Vec(config.nranks, Bool()))
  val rowStrobeN = Output(Bool())
  val columnStrobeN = Output(Bool())
  val writeEnableN = Output(Bool())
  val clockEnable = Output(Vec(config.nranks, Bool()))
  val onDieTermination = Output(Vec(config.nranks, Bool()))
  val resetN = Output(Bool())
  val dq = Vec(padBits, Analog(1.W))
  val dqsPositive = Vec(padBytes, Analog(1.W))
  val dqsNegative = Vec(padBytes, Analog(1.W))
  val dataMask = Output(Vec(padBytes, Bool()))
}

/** Complete Gowin GW2A/GW5A x2 DDR3 assembly around [[Ecp5DdrPhyOutput]]. */
class GowinDdrPhyIo(config: DramConfig, family: GowinFamily,
    commandDelayInitialValue: Int = 0, clockPolarity: Boolean = false)
    extends RawModule {
  require(config.memType == "DDR3" && config.nPhases == 2)
  require(config.dfiDataBits == 4 * config.effectivePadDataBits)
  require(commandDelayInitialValue >= 0 && commandDelayInitialValue < 128)
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8

  val io = IO(new Bundle {
    val reset = Input(Bool())
    val initClock = Input(Clock())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val dllReset = Input(Bool())
    val delaySelect = Input(UInt(padBytes.W))
    val inputDelayReset = Input(Bool())
    val inputDelayIncrement = Input(Bool())
    val burstDetectClear = Input(Bool())
    val parallel = Input(new Ecp5DdrPhyOutput(config))
    val dqHalfIn = Output(Vec(padBits, UInt(4.W)))
    val inputDelayValue = Output(Vec(padBytes, UInt(3.W)))
    val dataValid = Output(Vec(padBytes, Bool()))
    val burstDetected = Output(Vec(padBytes, Bool()))
    val burstSeen = Output(Vec(padBytes, Bool()))
    val edgeClockStop = Output(Bool())
    val edgeClockReset = Output(Bool())
    val initBusy = Output(Bool())
    val pads = new GowinDdrPads(config)
  })

  private val init = withClockAndReset(io.initClock, io.reset) {
    Module(new GowinDdrPhyInit(family))
  }
  init.io.sys2xClock := io.edgeClock
  init.io.dllReset := io.reset || io.dllReset
  io.edgeClockStop := init.io.stop
  io.edgeClockReset := init.io.resetDomain
  io.initBusy := init.io.busy

  private def serialized(value: UInt): Bool = {
    val serializer = Module(new GowinOutputSerdes)
    serializer.io.reset := io.reset
    serializer.io.systemClock := io.systemClock
    serializer.io.edgeClock := io.edgeClock
    serializer.io.data := value
    serializer.io.serial
  }
  private def commandOutput(value: UInt): Bool = {
    val delay = Module(new GowinCommandDelay(family, commandDelayInitialValue))
    delay.io.dataIn := serialized(value)
    delay.io.dataOut
  }

  private val clockBuffer = Module(new GowinDifferentialOutputBuffer)
  val clockPattern = if (clockPolarity) ~io.parallel.clock else io.parallel.clock
  clockBuffer.io.dataIn := commandOutput(clockPattern)
  io.pads.clockPositive := clockBuffer.io.positive
  io.pads.clockNegative := clockBuffer.io.negative
  for (bit <- 0 until addressBits) {
    io.pads.address(bit) := commandOutput(io.parallel.address(bit))
  }
  for (bit <- 0 until config.bankBits) {
    io.pads.bank(bit) := commandOutput(io.parallel.bank(bit))
  }
  for (rank <- 0 until config.nranks) {
    io.pads.chipSelectN(rank) := commandOutput(io.parallel.chipSelectN(rank))
    io.pads.clockEnable(rank) := commandOutput(io.parallel.clockEnable(rank))
    io.pads.onDieTermination(rank) :=
      commandOutput(io.parallel.onDieTermination(rank))
  }
  io.pads.rowStrobeN := commandOutput(io.parallel.rowStrobeN)
  io.pads.columnStrobeN := commandOutput(io.parallel.columnStrobeN)
  io.pads.writeEnableN := commandOutput(io.parallel.writeEnableN)
  io.pads.resetN := commandOutput(io.parallel.resetN)

  private val readDelay = withClockAndReset(io.systemClock, io.reset) {
    RegInit(VecInit(Seq.fill(padBytes)(0.U(3.W))))
  }
  private val burstPrevious = withClockAndReset(io.systemClock, io.reset) {
    RegInit(VecInit(Seq.fill(padBytes)(false.B)))
  }
  private val burstSeen = withClockAndReset(io.systemClock, io.reset) {
    RegInit(VecInit(Seq.fill(padBytes)(false.B)))
  }
  private val halfDqIn = Wire(Vec(padBits, UInt(4.W)))

  for (byte <- 0 until padBytes) {
    withClockAndReset(io.systemClock, io.reset) {
      when(io.delaySelect(byte) && io.inputDelayReset) {
        readDelay(byte) := 0.U
      }.elsewhen(io.delaySelect(byte) && io.inputDelayIncrement) {
        readDelay(byte) := readDelay(byte) + 1.U
      }
    }

    val dqsBuffer = Module(new GowinDqsBuffer)
    val dqsOutput = Module(new GowinDqsOutputSerdes)
    val dqsPad = Module(new GowinDifferentialTristateBuffer)
    dqsBuffer.io.reset := io.reset
    dqsBuffer.io.systemClock := io.systemClock
    dqsBuffer.io.edgeClock := io.edgeClock
    dqsBuffer.io.dllDelay := init.io.delayCode
    dqsBuffer.io.pause := init.io.pause || io.delaySelect(byte)
    dqsBuffer.io.readEnable := io.parallel.dqsReadEnable
    dqsBuffer.io.readDelay := readDelay(byte)
    dqsBuffer.io.dqsInput := dqsPad.io.inputData
    dqsOutput.io.reset := io.reset
    dqsOutput.io.systemClock := io.systemClock
    dqsOutput.io.edgeClock := io.edgeClock
    dqsOutput.io.writeClock := dqsBuffer.io.writeClock
    dqsOutput.io.data := io.parallel.dqs(byte)
    dqsOutput.io.tristateData := VecInit(Seq(
      !(io.parallel.dqsOutputEnable || io.parallel.dqsPostamble),
      !(io.parallel.dqsOutputEnable || io.parallel.dqsPreamble))).asUInt
    dqsPad.io.outputData := dqsOutput.io.serial
    dqsPad.io.tristate := dqsOutput.io.tristate
    attach(dqsPad.io.positive, io.pads.dqsPositive(byte))
    attach(dqsPad.io.negative, io.pads.dqsNegative(byte))

    io.inputDelayValue(byte) := readDelay(byte)
    io.dataValid(byte) := dqsBuffer.io.dataValid
    io.burstDetected(byte) := dqsBuffer.io.burstDetected
    withClockAndReset(io.systemClock, io.reset) {
      burstPrevious(byte) := dqsBuffer.io.burstDetected
      when(io.burstDetectClear) {
        burstSeen(byte) := false.B
      }.elsewhen(dqsBuffer.io.burstDetected && !burstPrevious(byte)) {
        burstSeen(byte) := true.B
      }
    }
    io.burstSeen(byte) := burstSeen(byte)

    val mask = Module(new GowinDataOutputSerdes)
    mask.io.reset := io.reset
    mask.io.systemClock := io.systemClock
    mask.io.edgeClock := io.edgeClock
    mask.io.writeClock270 := dqsBuffer.io.writeClock270
    mask.io.data := io.parallel.dataMask(byte)
    mask.io.tristateData := 0.U
    io.pads.dataMask(byte) := mask.io.serial

    val dqTristate = Fill(2, !io.parallel.dqOutputEnable)
    for (laneIndex <- 0 until 8) {
      val bit = 8 * byte + laneIndex
      val lane = Module(new GowinDataSerdesLane)
      lane.io.reset := io.reset
      lane.io.systemClock := io.systemClock
      lane.io.edgeClock := io.edgeClock
      lane.io.writeClock270 := dqsBuffer.io.writeClock270
      lane.io.readClock90 := dqsBuffer.io.readClock90
      lane.io.readPointer := dqsBuffer.io.readPointer
      lane.io.writePointer := dqsBuffer.io.writePointer
      lane.io.parallelOut := io.parallel.dq(bit)
      lane.io.tristateData := dqTristate
      halfDqIn(bit) := lane.io.parallelIn
      attach(lane.io.pad, io.pads.dq(bit))
    }
  }
  io.dqHalfIn := halfDqIn
}
