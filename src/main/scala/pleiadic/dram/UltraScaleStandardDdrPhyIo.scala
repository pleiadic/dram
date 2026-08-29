package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import scala.language.reflectiveCalls

class UltraScaleStandardDdrPads(config: DramConfig, dqsPerByte: Int = 1)
    extends Bundle {
  require(dqsPerByte == 1 || dqsPerByte == 2,
    "UltraScale DDR pads support one (x8) or two (x4) DQS pairs per byte")
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  val clockPositive = Analog(1.W)
  val clockNegative = Analog(1.W)
  val address = Output(Vec(addressBits, Bool()))
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
  val dqsPositive = Vec(padBytes * dqsPerByte, Analog(1.W))
  val dqsNegative = Vec(padBytes * dqsPerByte, Analog(1.W))
  val dataMask = Output(Vec(padBytes, Bool()))
}

/**
  * UltraScale/UltraScale+ DDR3/DDR4 1:4 pad assembly. All outbound groups use
  * OSERDESE3 followed by ODELAYE3; DQ additionally uses IDELAYE3/ISERDESE3.
  * `dqsOutputDelayInitialValuePs` is the quarter-cycle DQS phase shift in ps.
  * Pair this with `StandardDdrPhy(outputEnableDelayCycles = 1)` to match the
  * single OE register used by LiteDRAM's E3 primitive boundary.
  */
class UltraScaleStandardDdrPhyIo(config: DramConfig, device: String,
    refClockFrequencyMHz: Int, dqsOutputDelayInitialValuePs: Int,
    dqsPerByte: Int = 1)
    extends RawModule {
  require(Set("DDR3", "DDR4").contains(config.memType))
  require(config.nPhases == 4)
  require(UltraScaleDevice.supported.contains(device))
  require(dqsOutputDelayInitialValuePs >= 0 && dqsOutputDelayInitialValuePs <= 1250)
  require(dqsPerByte == 1 || dqsPerByte == 2,
    "dqsPerByte must be one for x8 devices or two for x4 devices")
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8

  val io = IO(new Bundle {
    val reset = Input(Bool())
    val dividedClock = Input(Clock())
    val serialClock = Input(Clock())
    val delayClock = Input(Clock())
    val enableVtc = Input(Bool())
    val delaySelect = Input(UInt(padBytes.W))
    val commandOutputDelayReset = Input(Bool())
    val commandOutputDelayIncrement = Input(Bool())
    val dqInputDelayReset = Input(Bool())
    val dqInputDelayIncrement = Input(Bool())
    val dqOutputDelayReset = Input(Bool())
    val dqOutputDelayIncrement = Input(Bool())
    val dqsOutputDelayReset = Input(Bool())
    val dqsOutputDelayIncrement = Input(Bool())
    val parallel = Input(new StandardDdrPhyOutput(config))
    val dqIn = Output(Vec(padBits, UInt(8.W)))
    val commandOutputDelayValue = Output(UInt(9.W))
    val dqInputDelayValue = Output(Vec(padBits, UInt(9.W)))
    val dqOutputDelayValue = Output(Vec(padBits, UInt(9.W)))
    val dataMaskOutputDelayValue = Output(Vec(padBytes, UInt(9.W)))
    val dqsOutputDelayValue = Output(Vec(padBytes * dqsPerByte, UInt(9.W)))
    val pads = new UltraScaleStandardDdrPads(config, dqsPerByte)
  })

  private def delayedOutput(value: UInt, delayReset: Bool,
      delayIncrement: Bool): UltraScaleDelayedOutputSerdes = {
    val output = Module(new UltraScaleDelayedOutputSerdes(device,
      refClockFrequencyMHz))
    output.io.reset := io.reset
    output.io.serialClock := io.serialClock
    output.io.dividedClock := io.dividedClock
    output.io.delayClock := io.delayClock
    output.io.enableVtc := io.enableVtc
    output.io.delayReset := io.reset || delayReset
    output.io.delayIncrement := delayIncrement
    output.io.parallelOut := value
    output.io.outputEnable := true.B
    output
  }

  private val clockOutput = delayedOutput(io.parallel.clock,
    io.commandOutputDelayReset, io.commandOutputDelayIncrement)
  private val clockBuffer = Module(new S7DifferentialOutputBuffer)
  clockBuffer.io.dataIn := clockOutput.io.serial
  io.commandOutputDelayValue := clockOutput.io.delayValue
  attach(clockBuffer.io.padPositive, io.pads.clockPositive)
  attach(clockBuffer.io.padNegative, io.pads.clockNegative)

  for (bit <- 0 until addressBits) {
    io.pads.address(bit) := delayedOutput(io.parallel.address(bit),
      io.commandOutputDelayReset, io.commandOutputDelayIncrement).io.serial
  }
  for (bit <- 0 until config.bankBits) {
    io.pads.bank(bit) := delayedOutput(io.parallel.bank(bit),
      io.commandOutputDelayReset, io.commandOutputDelayIncrement).io.serial
  }
  for (rank <- 0 until config.nranks) {
    io.pads.chipSelectN(rank) := delayedOutput(io.parallel.chipSelectN(rank),
      io.commandOutputDelayReset, io.commandOutputDelayIncrement).io.serial
    io.pads.clockEnable(rank) := delayedOutput(io.parallel.clockEnable(rank),
      io.commandOutputDelayReset, io.commandOutputDelayIncrement).io.serial
    io.pads.onDieTermination(rank) := delayedOutput(
      io.parallel.onDieTermination(rank), io.commandOutputDelayReset,
      io.commandOutputDelayIncrement).io.serial
  }
  io.pads.rowStrobeN := delayedOutput(io.parallel.rowStrobeN,
    io.commandOutputDelayReset, io.commandOutputDelayIncrement).io.serial
  io.pads.columnStrobeN := delayedOutput(io.parallel.columnStrobeN,
    io.commandOutputDelayReset, io.commandOutputDelayIncrement).io.serial
  io.pads.writeEnableN := delayedOutput(io.parallel.writeEnableN,
    io.commandOutputDelayReset, io.commandOutputDelayIncrement).io.serial
  io.pads.activateN := delayedOutput(io.parallel.activateN,
    io.commandOutputDelayReset, io.commandOutputDelayIncrement).io.serial
  io.pads.resetN := delayedOutput(io.parallel.resetN,
    io.commandOutputDelayReset, io.commandOutputDelayIncrement).io.serial

  for (bit <- 0 until padBits) {
    val lane = Module(new UltraScaleBidirectionalSerdesLane(device,
      refClockFrequencyMHz))
    lane.io.reset := io.reset
    lane.io.serialClock := io.serialClock
    lane.io.dividedClock := io.dividedClock
    lane.io.delayClock := io.delayClock
    lane.io.enableVtc := io.enableVtc
    lane.io.inputDelayReset := io.reset ||
      (io.dqInputDelayReset && io.delaySelect(bit / 8))
    lane.io.inputDelayIncrement :=
      io.dqInputDelayIncrement && io.delaySelect(bit / 8)
    lane.io.outputDelayReset := io.reset ||
      (io.dqOutputDelayReset && io.delaySelect(bit / 8))
    lane.io.outputDelayIncrement :=
      io.dqOutputDelayIncrement && io.delaySelect(bit / 8)
    lane.io.parallelOut := io.parallel.dq(bit)
    lane.io.outputEnable := io.parallel.dqOutputEnable
    io.dqIn(bit) := lane.io.parallelIn
    io.dqInputDelayValue(bit) := lane.io.inputDelayValue
    io.dqOutputDelayValue(bit) := lane.io.outputDelayValue
    attach(lane.io.pad, io.pads.dq(bit))
  }

  for (byte <- 0 until padBytes) {
    for (strobe <- 0 until dqsPerByte) {
      val physicalDqs = byte * dqsPerByte + strobe
      val dqs = Module(new UltraScaleDifferentialOutputSerdesIoLane(device,
        refClockFrequencyMHz, dqsOutputDelayInitialValuePs))
      dqs.io.reset := io.reset
      dqs.io.serialClock := io.serialClock
      dqs.io.dividedClock := io.dividedClock
      dqs.io.delayClock := io.delayClock
      dqs.io.enableVtc := io.enableVtc
      dqs.io.delayReset := io.reset ||
        (io.dqsOutputDelayReset && io.delaySelect(byte))
      dqs.io.delayIncrement :=
        io.dqsOutputDelayIncrement && io.delaySelect(byte)
      dqs.io.parallelOut := io.parallel.dqs(byte)
      dqs.io.outputEnable := io.parallel.dqsOutputEnable
      io.dqsOutputDelayValue(physicalDqs) := dqs.io.delayValue
      attach(dqs.io.padPositive, io.pads.dqsPositive(physicalDqs))
      attach(dqs.io.padNegative, io.pads.dqsNegative(physicalDqs))
    }

    val dataMask = delayedOutput(io.parallel.dataMask(byte),
      io.dqOutputDelayReset && io.delaySelect(byte),
      io.dqOutputDelayIncrement && io.delaySelect(byte))
    io.pads.dataMask(byte) := dataMask.io.serial
    io.dataMaskOutputDelayValue(byte) := dataMask.io.delayValue
  }
}

class UltraScaleDdrPhyIo(config: DramConfig, refClockFrequencyMHz: Int = 200,
    dqsOutputDelayInitialValuePs: Int = 0, dqsPerByte: Int = 1)
    extends UltraScaleStandardDdrPhyIo(
  config, UltraScaleDevice.UltraScale, refClockFrequencyMHz,
  dqsOutputDelayInitialValuePs, dqsPerByte)

class UltraScalePlusDdrPhyIo(config: DramConfig, refClockFrequencyMHz: Int = 300,
    dqsOutputDelayInitialValuePs: Int = 0, dqsPerByte: Int = 1)
    extends UltraScaleStandardDdrPhyIo(
  config, UltraScaleDevice.UltraScalePlus, refClockFrequencyMHz,
  dqsOutputDelayInitialValuePs, dqsPerByte)
