package pleiadic.dram

import chisel3._
import scala.language.reflectiveCalls

/** Portable pad-side outputs of a full-rate generic SDR PHY. */
class GenericSdrPads(config: DramConfig) extends Bundle {
  val address = UInt(config.rowBits.max(config.columnBits).max(11).W)
  val bank = UInt(config.bankBits.W)
  val csN = Vec(config.nranks, Bool())
  val rasN = Bool()
  val casN = Bool()
  val weN = Bool()
  val cke = Vec(config.nranks, Bool())
  val dqOut = UInt(config.dfiDataBits.W)
  val dqOutputEnable = Bool()
  val dataMask = UInt((config.dfiDataBits / 8).W)
}

/**
  * Technology-independent 1:1 SDR PHY corresponding to LiteDRAM GENSDRPHY.
  * FPGA-specific I/O cells can be attached to this module's pad-side signals.
  */
class GenericSdrPhy(config: DramConfig, casLatency: Int,
    withDataMask: Boolean = true) extends Module {
  require(config.memType == "SDR", "GenericSdrPhy only supports SDR")
  require(config.nPhases == 1, "full-rate GenericSdrPhy requires one DFI phase")
  require(casLatency >= 1)
  val readLatency: Int = casLatency + 1

  val io = IO(new Bundle {
    val dfi = Input(new DfiInterface(config))
    val dqIn = Input(UInt(config.dfiDataBits.W))
    val pads = Output(new GenericSdrPads(config))
    val read = Output(new DfiReadResponse(config))
  })

  private val phase = io.dfi.phases(0)
  io.pads.address := phase.address
  io.pads.bank := phase.bank
  io.pads.csN := phase.csN
  io.pads.rasN := phase.rasN
  io.pads.casN := phase.casN
  io.pads.weN := phase.weN
  io.pads.cke := phase.cke
  io.pads.dqOut := phase.wrdata
  io.pads.dqOutputEnable := phase.wrdataEn
  io.pads.dataMask := (if (withDataMask) {
    Mux(phase.wrdataEn, phase.wrdataMask, 0.U)
  } else {
    0.U
  })

  private val readEnable = RegInit(VecInit(Seq.fill(readLatency)(false.B)))
  readEnable(0) := phase.rddataEn
  for (index <- 1 until readLatency) {
    readEnable(index) := readEnable(index - 1)
  }
  io.read.data := io.dqIn
  io.read.valid := readEnable.last
}
