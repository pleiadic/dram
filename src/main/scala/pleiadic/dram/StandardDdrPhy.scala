package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/** Eight-edge, LSB-first parallel boundary for a 1:4 standard DDR PHY. */
class StandardDdrPhyOutput(config: DramConfig) extends Bundle {
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  val clock = UInt(8.W)
  val address = Vec(addressBits, UInt(8.W))
  val bank = Vec(config.bankBits, UInt(8.W))
  val chipSelectN = Vec(config.nranks, UInt(8.W))
  val rowStrobeN = UInt(8.W)
  val columnStrobeN = UInt(8.W)
  val writeEnableN = UInt(8.W)
  val activateN = UInt(8.W)
  val clockEnable = Vec(config.nranks, UInt(8.W))
  val onDieTermination = Vec(config.nranks, UInt(8.W))
  val resetN = UInt(8.W)
  val dq = Vec(padBits, UInt(8.W))
  val dqOutputEnable = Bool()
  val dqs = Vec(padBytes, UInt(8.W))
  val dqsOutputEnable = Bool()
  val dataMask = Vec(padBytes, UInt(8.W))
}

/**
  * Technology-independent 1:4 DDR2/DDR3/DDR4 PHY core. Physical SerDes,
  * delay cells, differential buffers and tristates belong to a family wrapper.
  * The layout follows S7DDRPHY's 8:1 OSERDESE2/ISERDESE2 boundary.
  */
class StandardDdrPhy(config: DramConfig, readLatency: Int, writeLatency: Int,
    withDataMask: Boolean = true) extends Module {
  require(Set("DDR2", "DDR3", "DDR4").contains(config.memType))
  require(config.nPhases == 4, "the portable standard DDR PHY currently implements 1:4")
  require(config.dfiDataBits == 2 * config.effectivePadDataBits,
    "each DFI phase must contain the rising and falling DQ words")
  require(readLatency >= 1)
  require(writeLatency >= 2, "DQS preamble requires a write-enable tap before the data tap")
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  private val edgeCount = 8
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
    val output = Output(new StandardDdrPhyOutput(config))
    val read = Output(Vec(config.nPhases, new DfiReadResponse(config)))
    val writePreamble = Output(Bool())
    val writePostamble = Output(Bool())
  })

  private val effectiveDfi = Wire(new DfiInterface(config))
  if (config.memType == "DDR4") {
    val mux = Module(new Ddr4DfiMux(config))
    mux.io.input := io.dfi
    effectiveDfi := mux.io.output
  } else {
    effectiveDfi := io.dfi
  }

  private def commandWord(bitAtPhase: Int => Bool): UInt =
    VecInit((0 until edgeCount).map(edge => bitAtPhase(edge / 2))).asUInt

  io.output.clock := "b10101010".U
  for (bit <- 0 until addressBits) {
    io.output.address(bit) := commandWord(phase => effectiveDfi.phases(phase).address(bit))
  }
  for (bit <- 0 until config.bankBits) {
    io.output.bank(bit) := commandWord(phase => effectiveDfi.phases(phase).bank(bit))
  }
  for (rank <- 0 until config.nranks) {
    io.output.chipSelectN(rank) := commandWord(phase => effectiveDfi.phases(phase).csN(rank))
    io.output.clockEnable(rank) := commandWord(phase => effectiveDfi.phases(phase).cke(rank))
    io.output.onDieTermination(rank) :=
      commandWord(phase => effectiveDfi.phases(phase).odt(rank))
  }
  io.output.rowStrobeN := commandWord(phase => effectiveDfi.phases(phase).rasN)
  io.output.columnStrobeN := commandWord(phase => effectiveDfi.phases(phase).casN)
  io.output.writeEnableN := commandWord(phase => effectiveDfi.phases(phase).weN)
  io.output.activateN := commandWord(phase => effectiveDfi.phases(phase).actN)
  io.output.resetN := commandWord(phase => effectiveDfi.phases(phase).resetN)

  private def selectedReset(byte: Int, read: Boolean): Bool = io.phyReset ||
    (io.delaySelect(byte) && (if (read) io.readBitslipReset else io.writeBitslipReset))
  private def selectedSlip(byte: Int, read: Boolean): Bool = io.delaySelect(byte) &&
    (if (read) io.readBitslip else io.writeBitslip)

  for (bit <- 0 until padBits) {
    val word = VecInit((0 until edgeCount).map { edge =>
      effectiveDfi.phases(edge / 2).wrdata((edge % 2) * padBits + bit)
    }).asUInt
    val bitslip = Module(new BitSlip(edgeCount))
    bitslip.io.in := word
    bitslip.io.resetSlip := selectedReset(bit / 8, read = false)
    bitslip.io.slip := selectedSlip(bit / 8, read = false)
    io.output.dq(bit) := bitslip.io.out
  }

  for (byte <- 0 until padBytes) {
    val word = VecInit((0 until edgeCount).map { edge =>
      effectiveDfi.phases(edge / 2).wrdataMask((edge % 2) * padBytes + byte)
    }).asUInt
    val polarityAdjusted = if (config.memType == "DDR4") ~word else word
    val bitslip = Module(new BitSlip(edgeCount))
    bitslip.io.in := polarityAdjusted
    bitslip.io.resetSlip := selectedReset(byte, read = false)
    bitslip.io.slip := selectedSlip(byte, read = false)
    io.output.dataMask(byte) := (if (withDataMask) bitslip.io.out
      else if (config.memType == "DDR4") "hff".U else 0.U)
  }

  private val writeEnableInput = effectiveDfi.phases.map(_.wrdataEn).reduce(_ || _)
  private val writeEnable = RegInit(VecInit(Seq.fill(writeLatency + 1)(false.B)))
  writeEnable(0) := writeEnableInput
  for (index <- 1 until writeEnable.length) writeEnable(index) := writeEnable(index - 1)
  private val dataEnable = writeEnable(writeTap)
  private val preamble = writeEnable(writeTap - 1) && !dataEnable
  private val postamble = writeEnable(writeTap + 1) && !dataEnable
  io.writePreamble := preamble
  io.writePostamble := postamble

  private val dqEnableDelay = RegInit(VecInit(Seq.fill(2)(false.B)))
  private val dqsEnableDelay = RegInit(VecInit(Seq.fill(2)(false.B)))
  dqEnableDelay(0) := preamble || dataEnable || postamble
  dqsEnableDelay(0) := preamble || Mux(io.writeLevelingEnable, true.B, dataEnable) || postamble
  dqEnableDelay(1) := dqEnableDelay(0)
  dqsEnableDelay(1) := dqsEnableDelay(0)
  io.output.dqOutputEnable := dqEnableDelay(1)
  io.output.dqsOutputEnable := dqsEnableDelay(1)

  private val dqsPattern = Module(new DqsPattern)
  // S7DDRPHY extends the tristate window for pre/postamble but leaves the
  // serialized toggle pattern unchanged; preserve that hardware behavior.
  dqsPattern.io.preamble := false.B
  dqsPattern.io.postamble := false.B
  dqsPattern.io.writeLevelingEnable := io.writeLevelingEnable
  dqsPattern.io.writeLevelingStrobe := io.writeLevelingStrobe
  for (byte <- 0 until padBytes) {
    val bitslip = Module(new BitSlip(edgeCount))
    bitslip.io.in := dqsPattern.io.out
    bitslip.io.resetSlip := selectedReset(byte, read = false)
    bitslip.io.slip := selectedSlip(byte, read = false)
    io.output.dqs(byte) := bitslip.io.out
  }

  private val readDataBits = Wire(Vec(config.nPhases,
    Vec(config.dfiDataBits, Bool())))
  readDataBits.foreach(_.foreach(_ := false.B))
  for (bit <- 0 until padBits) {
    val bitslip = Module(new BitSlip(edgeCount))
    bitslip.io.in := io.dqIn(bit)
    bitslip.io.resetSlip := selectedReset(bit / 8, read = true)
    bitslip.io.slip := selectedSlip(bit / 8, read = true)
    for (edge <- 0 until edgeCount) {
      readDataBits(edge / 2)((edge % 2) * padBits + bit) := bitslip.io.out(edge)
    }
  }

  private val readEnableInput = effectiveDfi.phases.map(_.rddataEn).reduce(_ || _)
  private val readEnable = RegInit(VecInit(Seq.fill(readLatency)(false.B)))
  readEnable(0) := readEnableInput
  for (index <- 1 until readLatency) readEnable(index) := readEnable(index - 1)
  for (phase <- 0 until config.nPhases) {
    io.read(phase).data := readDataBits(phase).asUInt
    io.read(phase).valid := readEnable.last || io.writeLevelingEnable
  }
}
