package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import scala.language.reflectiveCalls

class NexusDdrPads(config: DramConfig) extends Bundle {
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  val clockPositive = Output(Bool())
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
  val dataMask = Output(Vec(padBytes, Bool()))
}

/** Lattice Nexus x2 DDR3 pad assembly with fixed half-word write alignment. */
class NexusDdrPhyIo(config: DramConfig, commandDelayInitialValue: Int = 0)
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
    val inputDelayValue = Output(Vec(padBytes, UInt(4.W)))
    val dataValid = Output(Vec(padBytes, Bool()))
    val burstDetected = Output(Vec(padBytes, Bool()))
    val burstSeen = Output(Vec(padBytes, Bool()))
    val edgeClockStop = Output(Bool())
    val edgeClockReset = Output(Bool())
    val initBusy = Output(Bool())
    val pads = new NexusDdrPads(config)
  })

  private val init = withClockAndReset(io.initClock, io.reset) {
    Module(new NexusDdrPhyInit)
  }
  init.io.sys2xClock := io.edgeClock
  init.io.dllReset := io.reset || io.dllReset
  io.edgeClockStop := init.io.stop
  io.edgeClockReset := init.io.resetDomain
  io.initBusy := init.io.busy

  private def serialized(value: UInt): Bool = {
    val serializer = Module(new NexusOutputDdrX2)
    serializer.io.reset := io.reset
    serializer.io.systemClock := io.systemClock
    serializer.io.edgeClock := io.edgeClock
    serializer.io.data := value
    serializer.io.serial
  }
  private def commandOutput(value: UInt): Bool = {
    val delay = Module(new NexusCommandDelay(commandDelayInitialValue))
    delay.io.dataIn := serialized(value)
    delay.io.dataOut
  }

  io.pads.clockPositive := serialized(io.parallel.clock)
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
    RegInit(VecInit(Seq.fill(padBytes)(0.U(4.W))))
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

    val dqsBuffer = Module(new NexusDqsBuffer)
    val dqsPad = Module(new Ecp5TristateBuffer)
    dqsBuffer.io.reset := io.reset
    dqsBuffer.io.systemClock := io.systemClock
    dqsBuffer.io.edgeClock := io.edgeClock
    dqsBuffer.io.dllCode := init.io.delayCode
    dqsBuffer.io.pause := init.io.pause || io.delaySelect(byte)
    dqsBuffer.io.loadN := init.io.loadN
    dqsBuffer.io.move := init.io.move
    dqsBuffer.io.readEnable := io.parallel.dqsReadEnable
    dqsBuffer.io.readDelay := readDelay(byte)
    dqsBuffer.io.dqsInput := dqsPad.io.inputData

    val dqsData = VecInit(Seq(false.B, io.parallel.dqsOutputEnable,
      false.B, io.parallel.dqsOutputEnable || io.parallel.dqsPreamble)).asUInt
    val dqsTristate = VecInit(Seq(
      !(io.parallel.dqsOutputEnable || io.parallel.dqsPostamble),
      !(io.parallel.dqsOutputEnable || io.parallel.dqsPreamble))).asUInt
    val dqsDataSlip = Module(new NexusWriteBitSlip(4))
    val dqsTristateSlip = Module(new NexusWriteBitSlip(2))
    dqsDataSlip.io.clock := io.systemClock
    dqsDataSlip.io.reset := io.reset
    dqsDataSlip.io.input := dqsData
    dqsTristateSlip.io.clock := io.systemClock
    dqsTristateSlip.io.reset := io.reset
    dqsTristateSlip.io.input := dqsTristate
    val dqsOutput = Module(new NexusDqsOutputSerdes)
    dqsOutput.io.reset := io.reset
    dqsOutput.io.systemClock := io.systemClock
    dqsOutput.io.edgeClock := io.edgeClock
    dqsOutput.io.writeClock := dqsBuffer.io.writeClock
    dqsOutput.io.data := dqsDataSlip.io.output
    dqsOutput.io.tristateData := dqsTristateSlip.io.output
    dqsPad.io.outputData := dqsOutput.io.serial
    dqsPad.io.tristate := dqsOutput.io.tristate
    attach(dqsPad.io.pad, io.pads.dqsPositive(byte))

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

    val maskSlip = Module(new NexusWriteBitSlip(4))
    maskSlip.io.clock := io.systemClock
    maskSlip.io.reset := io.reset
    maskSlip.io.input := io.parallel.dataMask(byte)
    val mask = Module(new NexusDataOutputSerdes)
    mask.io.reset := io.reset
    mask.io.systemClock := io.systemClock
    mask.io.edgeClock := io.edgeClock
    mask.io.writeClock270 := dqsBuffer.io.writeClock270
    mask.io.data := maskSlip.io.output
    io.pads.dataMask(byte) := mask.io.serial

    val dqTristate = VecInit(Seq(
      !(io.parallel.dqOutputEnable || io.parallel.dqsPostamble),
      !(io.parallel.dqOutputEnable || io.parallel.dqsPreamble))).asUInt
    for (laneIndex <- 0 until 8) {
      val bit = 8 * byte + laneIndex
      val dataSlip = Module(new NexusWriteBitSlip(4))
      val tristateSlip = Module(new NexusWriteBitSlip(2))
      dataSlip.io.clock := io.systemClock
      dataSlip.io.reset := io.reset
      dataSlip.io.input := io.parallel.dq(bit)
      tristateSlip.io.clock := io.systemClock
      tristateSlip.io.reset := io.reset
      tristateSlip.io.input := dqTristate
      val lane = Module(new NexusDataSerdesLane)
      lane.io.reset := io.reset
      lane.io.systemClock := io.systemClock
      lane.io.edgeClock := io.edgeClock
      lane.io.writeClock270 := dqsBuffer.io.writeClock270
      lane.io.readClock90 := dqsBuffer.io.readClock90
      lane.io.readPointer := dqsBuffer.io.readPointer
      lane.io.writePointer := dqsBuffer.io.writePointer
      lane.io.parallelOut := dataSlip.io.output
      lane.io.tristateData := tristateSlip.io.output
      halfDqIn(bit) := lane.io.parallelIn
      attach(lane.io.pad, io.pads.dq(bit))
    }
  }
  io.dqHalfIn := halfDqIn
}
