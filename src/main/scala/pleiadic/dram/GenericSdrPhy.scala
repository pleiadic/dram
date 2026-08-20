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
  val dqOut = UInt(config.effectivePadDataBits.W)
  val dqOutputEnable = Bool()
  val dataMask = UInt((config.effectivePadDataBits / 8).W)
}

/**
  * Technology-independent 1:1 SDR PHY corresponding to LiteDRAM GENSDRPHY.
  * FPGA-specific I/O cells can be attached to this module's pad-side signals.
  */
class GenericSdrPhy(config: DramConfig, casLatency: Int,
    withDataMask: Boolean = true) extends Module {
  require(config.memType == "SDR", "GenericSdrPhy only supports SDR")
  require(config.nPhases == 1, "full-rate GenericSdrPhy requires one DFI phase")
  require(config.dfiDataBits == config.effectivePadDataBits,
    "full-rate GenericSdrPhy requires DFI and pad data widths to match")
  require(casLatency >= 1)
  val readLatency: Int = casLatency + 1

  val io = IO(new Bundle {
    val dfi = Input(new DfiInterface(config))
    val dqIn = Input(UInt(config.effectivePadDataBits.W))
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

/**
  * Related-clock 1:2 Generic SDR PHY. Phase 0 and phase 1 are emitted on
  * consecutive fast-clock cycles; the slow-side DFI must remain stable for a
  * complete pair. This mirrors LiteDRAM HalfRateGENSDRPHY without I/O cells.
  */
class HalfRateGenericSdrPhy(config: DramConfig, casLatency: Int,
    withDataMask: Boolean = true) extends RawModule {
  require(config.memType == "SDR", "HalfRateGenericSdrPhy only supports SDR")
  require(config.nPhases == 2, "half-rate Generic SDR PHY requires two DFI phases")
  require(config.dfiDataBits == config.effectivePadDataBits,
    "each half-rate DFI phase must match the physical DQ width")
  require(casLatency >= 1)
  val readLatency: Int = (casLatency + 1) / 2 + 1

  val io = IO(new Bundle {
    val fastClock = Input(Clock())
    val slowClock = Input(Clock())
    val reset = Input(AsyncReset())
    val dfi = Input(new DfiInterface(config))
    val dqIn = Input(UInt(config.effectivePadDataBits.W))
    val pads = Output(new GenericSdrPads(config))
    val read = Output(Vec(2, new DfiReadResponse(config)))
    val slot = Output(Bool())
  })

  private val slot = withClockAndReset(io.fastClock, io.reset) {
    val value = RegInit(false.B)
    value := !value
    value
  }
  io.slot := slot
  private val phase = io.dfi.phases(slot.asUInt)
  io.pads.address := phase.address
  io.pads.bank := phase.bank
  io.pads.csN := phase.csN
  io.pads.rasN := phase.rasN
  io.pads.casN := phase.casN
  io.pads.weN := phase.weN
  io.pads.cke := phase.cke
  io.pads.dqOut := phase.wrdata
  io.pads.dqOutputEnable := io.dfi.phases(0).wrdataEn
  io.pads.dataMask := (if (withDataMask) {
    Mux(io.dfi.phases(0).wrdataEn, phase.wrdataMask, 0.U)
  } else {
    0.U
  })

  private val captured = withClockAndReset(io.fastClock, io.reset) {
    RegInit(VecInit(Seq.fill(2)(0.U(config.dfiDataBits.W))))
  }
  withClockAndReset(io.fastClock, io.reset) {
    captured(slot.asUInt) := io.dqIn
  }
  private val readEnable = withClockAndReset(io.slowClock, io.reset) {
    val pipeline = RegInit(VecInit(Seq.fill(readLatency)(false.B)))
    pipeline(0) := io.dfi.phases(0).rddataEn
    for (index <- 1 until readLatency) {
      pipeline(index) := pipeline(index - 1)
    }
    pipeline
  }
  for (index <- 0 until 2) {
    io.read(index).data := captured(index)
    io.read(index).valid := readEnable.last
  }
}
