package pleiadic.dram

import chisel3._
import chisel3.util.Cat
import scala.language.reflectiveCalls

/** Four-edge boundary consumed by the ECP5 x2 DDR I/O primitives. */
class Ecp5DdrPhyOutput(config: DramConfig) extends Bundle {
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  val clock = UInt(4.W)
  val address = Vec(addressBits, UInt(4.W))
  val bank = Vec(config.bankBits, UInt(4.W))
  val chipSelectN = Vec(config.nranks, UInt(4.W))
  val rowStrobeN = UInt(4.W)
  val columnStrobeN = UInt(4.W)
  val writeEnableN = UInt(4.W)
  val clockEnable = Vec(config.nranks, UInt(4.W))
  val onDieTermination = Vec(config.nranks, UInt(4.W))
  val resetN = UInt(4.W)
  val dq = Vec(padBits, UInt(4.W))
  val dataMask = Vec(padBytes, UInt(4.W))
  val dqOutputEnable = Bool()
  val dqs = Vec(padBytes, UInt(4.W))
  val dqsOutputEnable = Bool()
  val dqsPreamble = Bool()
  val dqsPostamble = Bool()
  val dqsReadEnable = Bool()
}

/**
  * Technology-independent half-rate DDR3 core matching ECP5DDRPHY's logical
  * boundary. Each DFI phase carries four DQ edges; the ECP5 x2 primitives emit
  * one four-edge half per system cycle, so a BL8 transfer spans two cycles.
  */
class Ecp5DdrPhy(config: DramConfig, readLatency: Int, readCommandTap: Int,
    writeLatency: Int, withDataMask: Boolean = true,
    dataMaskRemapping: Seq[Int] = Seq.empty) extends Module {
  require(config.memType == "DDR3")
  require(config.nPhases == 2)
  require(config.dfiDataBits == 4 * config.effectivePadDataBits,
    "each ECP5 DFI phase must contain four physical DQ words")
  require(readLatency >= 1)
  require(readCommandTap >= 0 && readCommandTap + 1 < readLatency)
  require(writeLatency >= 1)
  private val addressBits = config.rowBits.max(config.columnBits).max(11)
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  private val maskMap = if (dataMaskRemapping.isEmpty) 0.until(padBytes)
    else dataMaskRemapping
  require(maskMap.length == padBytes)
  require(maskMap.sorted == 0.until(padBytes))

  val io = IO(new Bundle {
    val dfi = Input(new DfiInterface(config))
    val dqHalfIn = Input(Vec(padBits, UInt(4.W)))
    val delaySelect = Input(UInt(padBytes.W))
    val readBitslipReset = Input(Bool())
    val readBitslip = Input(Bool())
    val output = Output(new Ecp5DdrPhyOutput(config))
    val read = Output(Vec(2, new DfiReadResponse(config)))
    val bl8Chunk = Output(Bool())
  })

  private def commandWord(bitAtPhase: Int => Bool): UInt =
    VecInit((0 until 4).map(edge => bitAtPhase(edge / 2))).asUInt

  io.output.clock := "b1010".U
  for (bit <- 0 until addressBits) {
    io.output.address(bit) := commandWord(phase => io.dfi.phases(phase).address(bit))
  }
  for (bit <- 0 until config.bankBits) {
    io.output.bank(bit) := commandWord(phase => io.dfi.phases(phase).bank(bit))
  }
  for (rank <- 0 until config.nranks) {
    io.output.chipSelectN(rank) :=
      commandWord(phase => io.dfi.phases(phase).csN(rank))
    io.output.clockEnable(rank) :=
      commandWord(phase => io.dfi.phases(phase).cke(rank))
    io.output.onDieTermination(rank) :=
      commandWord(phase => io.dfi.phases(phase).odt(rank))
  }
  io.output.rowStrobeN := commandWord(phase => io.dfi.phases(phase).rasN)
  io.output.columnStrobeN := commandWord(phase => io.dfi.phases(phase).casN)
  io.output.writeEnableN := commandWord(phase => io.dfi.phases(phase).weN)
  io.output.resetN := commandWord(phase => io.dfi.phases(phase).resetN)

  private val writeEnableInput = io.dfi.phases.map(_.wrdataEn).reduce(_ || _)
  private val writeEnable = RegInit(VecInit(
    Seq.fill(writeLatency + 4)(false.B)))
  writeEnable(0) := writeEnableInput
  for (index <- 1 until writeEnable.length) {
    writeEnable(index) := writeEnable(index - 1)
  }
  private val dataEnable = writeEnable(writeLatency) ||
    writeEnable(writeLatency + 1)
  private val bl8Chunk = writeEnable(writeLatency)
  private val preamble = writeEnable(writeLatency - 1) &&
    !writeEnable(writeLatency)
  private val postamble = writeEnable(writeLatency + 2) &&
    !writeEnable(writeLatency + 1)
  io.bl8Chunk := bl8Chunk
  io.output.dqOutputEnable := dataEnable
  io.output.dqsOutputEnable := dataEnable
  io.output.dqsPreamble := preamble
  io.output.dqsPostamble := postamble

  private val fullDq = Wire(Vec(padBits, UInt(8.W)))
  for (bit <- 0 until padBits) {
    fullDq(bit) := VecInit((0 until 8).map { edge =>
      io.dfi.phases(edge / 4).wrdata((edge % 4) * padBits + bit)
    }).asUInt
  }
  private val heldDq = RegInit(VecInit(Seq.fill(padBits)(0.U(8.W))))
  heldDq := fullDq
  private val dqHalf = RegInit(VecInit(Seq.fill(padBits)(0.U(4.W))))
  for (bit <- 0 until padBits) {
    dqHalf(bit) := Mux(bl8Chunk, heldDq(bit)(7, 4), fullDq(bit)(3, 0))
  }
  io.output.dq := dqHalf

  private val fullMask = Wire(Vec(padBytes, UInt(8.W)))
  for (byte <- 0 until padBytes) {
    val mapped = maskMap(byte)
    fullMask(byte) := VecInit((0 until 8).map { edge =>
      io.dfi.phases(edge / 4).wrdataMask((edge % 4) * padBytes + mapped)
    }).asUInt
  }
  private val heldMask = RegInit(VecInit(Seq.fill(padBytes)(0.U(8.W))))
  heldMask := fullMask
  private val maskHalf = RegInit(VecInit(Seq.fill(padBytes)(0.U(4.W))))
  for (byte <- 0 until padBytes) {
    maskHalf(byte) := (if (withDataMask) {
      Mux(bl8Chunk, heldMask(byte)(7, 4), fullMask(byte)(3, 0))
    } else {
      0.U
    })
    io.output.dataMask(byte) := maskHalf(byte)
    io.output.dqs(byte) := "b1010".U
  }

  private val readEnableInput = io.dfi.phases.map(_.rddataEn).reduce(_ || _)
  private val readEnable = RegInit(VecInit(Seq.fill(readLatency)(false.B)))
  readEnable(0) := readEnableInput
  for (index <- 1 until readLatency) {
    readEnable(index) := readEnable(index - 1)
  }
  io.output.dqsReadEnable := readEnable(readCommandTap) ||
    readEnable(readCommandTap + 1)

  private val readDataBits = Wire(Vec(2, Vec(config.dfiDataBits, Bool())))
  readDataBits.foreach(_.foreach(_ := false.B))
  for (bit <- 0 until padBits) {
    val bitslip = Module(new BitSlip(4))
    bitslip.io.in := io.dqHalfIn(bit)
    bitslip.io.resetSlip := io.delaySelect(bit / 8) && io.readBitslipReset
    bitslip.io.slip := io.delaySelect(bit / 8) && io.readBitslip
    val previous = RegInit(0.U(4.W))
    previous := bitslip.io.out
    val full = Cat(bitslip.io.out, previous)
    for (edge <- 0 until 8) {
      readDataBits(edge / 4)((edge % 4) * padBits + bit) := full(edge)
    }
  }
  for (phase <- 0 until 2) {
    io.read(phase).data := readDataBits(phase).asUInt
    io.read(phase).valid := readEnable.last
  }
}
