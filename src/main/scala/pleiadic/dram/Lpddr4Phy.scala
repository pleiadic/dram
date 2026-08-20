package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/** Parallel, LSB-first signals to be connected to LPDDR4 I/O serializers. */
class Lpddr4PhyOutput(config: DramConfig) extends Bundle {
  private val edgeCount = 2 * config.nPhases
  private val padBytes = config.effectivePadDataBits / 8
  val clock = UInt(edgeCount.W)
  val clockEnable = UInt(config.nPhases.W)
  val onDieTermination = UInt(config.nPhases.W)
  val resetN = UInt(config.nPhases.W)
  val cs = UInt(config.nPhases.W)
  val ca = Vec(6, UInt(config.nPhases.W))
  val dq = Vec(config.effectivePadDataBits, UInt(edgeCount.W))
  val dqOutputEnable = Bool()
  val dqs = Vec(padBytes, UInt(edgeCount.W))
  val dqsOutputEnable = Bool()
  val dmi = Vec(padBytes, UInt(edgeCount.W))
  val dmiOutputEnable = Bool()
}

/**
  * Technology-independent LPDDR4 PHY core. The module mirrors LiteDRAM's
  * LPDDR4PHY and leaves only physical (de-)serialization and tristates to a
  * target-specific wrapper.
  */
class Lpddr4Phy(config: DramConfig, readLatency: Int, writeLatency: Int,
    maskedWrite: Boolean = true, extendedOverlapCheck: Boolean = false) extends Module {
  require(config.memType == "LPDDR4")
  require(config.nranks == 1, "portable LPDDR4 PHY currently supports one rank")
  require(config.nPhases == 8, "LPDDR4 requires eight DFI phases for its 16n prefetch")
  require(config.bankBits >= 6 && config.rowBits.max(config.columnBits).max(11) >= 17)
  require(config.dfiDataBits == 2 * config.effectivePadDataBits,
    "each LPDDR4 DFI phase must contain two physical DQ edges")
  require(readLatency >= 1 && writeLatency >= 1)

  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  private val edgeCount = 2 * config.nPhases
  private val writeTap = writeLatency - 1

  val io = IO(new Bundle {
    val dfi = Input(new DfiInterface(config))
    val dqIn = Input(Vec(padBits, UInt(edgeCount.W)))
    val delaySelect = Input(UInt(padBytes.W))
    val phyReset = Input(Bool())
    val readBitslipReset = Input(Bool())
    val readBitslip = Input(Bool())
    val writeBitslipReset = Input(Bool())
    val writeBitslip = Input(Bool())
    val writeLevelingEnable = Input(Bool())
    val writeLevelingStrobe = Input(Bool())
    val output = Output(new Lpddr4PhyOutput(config))
    val read = Output(Vec(config.nPhases, new DfiReadResponse(config)))
    val commandAccepted = Output(UInt(config.nPhases.W))
  })

  private val commands = Module(new Lpddr4CommandPipeline(config, maskedWrite,
    extendedOverlapCheck))
  commands.io.dfi := io.dfi
  io.output.cs := commands.io.cs
  io.output.ca := commands.io.ca
  io.commandAccepted := commands.io.accepted

  io.output.clock := "h5555".U
  private val clockEnable = VecInit(io.dfi.phases.map(_.cke(0))).asUInt
  private val termination = VecInit(io.dfi.phases.map(_.odt(0))).asUInt
  private val resetN = VecInit(io.dfi.phases.map(_.resetN)).asUInt
  io.output.clockEnable := RegNext(clockEnable, 0.U(config.nPhases.W))
  io.output.onDieTermination := RegNext(termination, 0.U(config.nPhases.W))
  io.output.resetN := RegNext(resetN, 0.U(config.nPhases.W))

  private def selectedReset(byte: Int, read: Boolean): Bool = io.phyReset ||
    (io.delaySelect(byte) && (if (read) io.readBitslipReset else io.writeBitslipReset))
  private def selectedSlip(byte: Int, read: Boolean): Bool = io.delaySelect(byte) &&
    (if (read) io.readBitslip else io.writeBitslip)

  private val writeSlips = Seq.tabulate(padBits) { bit =>
    val value = VecInit((0 until edgeCount).map { edge =>
      io.dfi.phases(edge / 2).wrdata((edge % 2) * padBits + bit)
    }).asUInt
    val bitslip = Module(new BitSlip(edgeCount))
    bitslip.io.in := value
    bitslip.io.resetSlip := selectedReset(bit / 8, read = false)
    bitslip.io.slip := selectedSlip(bit / 8, read = false)
    io.output.dq(bit) := bitslip.io.out
    bitslip
  }

  private val readSlips = Seq.tabulate(padBits) { bit =>
    val bitslip = Module(new BitSlip(edgeCount))
    bitslip.io.in := io.dqIn(bit)
    bitslip.io.resetSlip := selectedReset(bit / 8, read = true)
    bitslip.io.slip := selectedSlip(bit / 8, read = true)
    bitslip
  }
  for (phase <- 0 until config.nPhases) {
    val low = VecInit((0 until padBits).map(bit => readSlips(bit).io.out(2 * phase))).asUInt
    val high = VecInit((0 until padBits).map(bit => readSlips(bit).io.out(2 * phase + 1))).asUInt
    io.read(phase).data := Cat(high, low)
  }

  private val dmiSlips = Seq.tabulate(padBytes) { byte =>
    val value = VecInit((0 until edgeCount).map { edge =>
      io.dfi.phases(edge / 2).wrdataMask((edge % 2) * padBytes + byte)
    }).asUInt
    val bitslip = Module(new BitSlip(edgeCount))
    bitslip.io.in := Mux(maskedWrite.B, value, 0.U)
    bitslip.io.resetSlip := selectedReset(byte, read = false)
    bitslip.io.slip := selectedSlip(byte, read = false)
    io.output.dmi(byte) := bitslip.io.out
    bitslip
  }

  private val writeEnable = io.dfi.phases.map(_.wrdataEn).reduce(_ || _)
  private val writeTaps = RegInit(VecInit(Seq.fill(writeTap + 2)(false.B)))
  writeTaps(0) := writeEnable
  for (tap <- 1 until writeTaps.length) writeTaps(tap) := writeTaps(tap - 1)
  private def writeEnableAt(tap: Int): Bool = if (tap < 0) writeEnable else writeTaps(tap)
  private val dqOutputEnable = writeEnableAt(writeTap)
  private val dqsPreamble = writeEnableAt(writeTap - 1) && !writeEnableAt(writeTap)
  private val dqsPostamble = writeEnableAt(writeTap + 1) && !writeEnableAt(writeTap)

  private val dqsPattern = WireDefault("h5555".U(16.W))
  when(dqsPreamble) { dqsPattern := "h5055".U }
  when(dqsPostamble) { dqsPattern := "h5554".U }
  when(io.writeLevelingEnable) {
    dqsPattern := Mux(io.writeLevelingStrobe, 5.U, 0.U)
  }
  for (byte <- 0 until padBytes) {
    val bitslip = Module(new BitSlip(edgeCount))
    bitslip.io.in := dqsPattern
    bitslip.io.resetSlip := selectedReset(byte, read = false)
    bitslip.io.slip := selectedSlip(byte, read = false)
    io.output.dqs(byte) := bitslip.io.out
  }

  private val dqsOutputEnable = io.writeLevelingEnable || dqsPreamble ||
    dqOutputEnable || dqsPostamble
  io.output.dqOutputEnable := RegNext(dqOutputEnable, false.B)
  io.output.dqsOutputEnable := RegNext(dqsOutputEnable, false.B)
  io.output.dmiOutputEnable := (if (maskedWrite) io.output.dqOutputEnable else false.B)

  private val readEnable = io.dfi.phases.map(_.rddataEn).reduce(_ || _)
  private val readValid = RegInit(VecInit(Seq.fill(readLatency)(false.B)))
  readValid(0) := readEnable
  for (tap <- 1 until readLatency) readValid(tap) := readValid(tap - 1)
  for (phase <- 0 until config.nPhases) {
    io.read(phase).valid := readValid.last || io.writeLevelingEnable
  }
}
