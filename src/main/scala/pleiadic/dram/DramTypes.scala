package pleiadic.dram

import chisel3._
import chisel3.util.{Decoupled, log2Ceil}

/** Address bit ordering used by LiteDRAM configurations. */
sealed trait DramAddressMapping
object DramAddressMapping {
  case object RowBankColumn extends DramAddressMapping
  case object BankRowColumn extends DramAddressMapping
  case object ColumnBankRow extends DramAddressMapping
}

/** Portable DRAM command encoding, intentionally independent of a vendor PHY. */
object DramCommandType {
  val width = 3
  val nop     = 0.U(width.W)
  val activate = 1.U(width.W)
  val precharge = 2.U(width.W)
  val read    = 3.U(width.W)
  val write   = 4.U(width.W)
  val refresh = 5.U(width.W)
  val zqCalibration = 6.U(width.W)
}

case class DramTiming(
  tRcd: Int = 2,
  tRp: Int = 2,
  tRas: Int = 4,
  tRc: Int = 6,
  tCcd: Int = 1,
  tWr: Int = 2,
  tWtr: Int = 2,
  tRtp: Int = 2,
  tRrd: Int = 2,
  tFaw: Int = 8,
  tRefi: Int = 64,
  tRfc: Int = 4,
  tZqcs: Option[Int] = None
) {
  require(tRcd >= 1 && tRp >= 1 && tRas >= 1 && tRc >= 1 && tCcd >= 1)
  require(tWr >= 1 && tWtr >= 1 && tRtp >= 1 && tRrd >= 1 && tFaw >= 1)
  require(tRefi >= 1 && tRfc >= 1)
}

case class DramConfig(
  addressBits: Int = 24,
  dataBits: Int = 32,
  bankBits: Int = 2,
  rowBits: Int = 10,
  columnBits: Int = 8,
  timing: DramTiming = DramTiming(),
  withAutoPrecharge: Boolean = false,
  memType: String = "SDR",
  nPhases: Int = 1,
  nranks: Int = 1,
  // A value of 0 means that the PHY data width follows dataBits. Scala 2
  // default arguments cannot refer to an earlier argument in the same list.
  phyDataBits: Int = 0,
  burstLength: Int = 1,
  addressMapping: DramAddressMapping = DramAddressMapping.RowBankColumn,
  refreshPostponing: Int = 1,
  cmdBufferDepth: Int = 8,
  readTime: Int = 32,
  writeTime: Int = 16,
  readLatency: Int = 1,
  writeLatency: Int = 0,
  // Physical DQ pad width. Zero preserves the historical behavior where it
  // follows the per-phase DFI width.
  padDataBits: Int = 0,
  // Optional controller-cycle period for automatic ZQ short calibration.
  // LiteDRAM derives this from clk_freq/zqcs_freq; keeping cycles here avoids
  // embedding a clock-frequency unit in the synthesizable configuration.
  zqCalibrationPeriodCycles: Option[Int] = None
) {
  val byteOffsetBits: Int = log2Ceil(dataBits / 8)
  require(addressBits > 0 && dataBits > 0 && dataBits % 8 == 0)
  require(bankBits > 0 && rowBits > 0 && columnBits > 0)
  require(nPhases > 0 && nranks > 0 && phyDataBits >= 0 && padDataBits >= 0)
  require((nranks & (nranks - 1)) == 0, "nranks must be a power of two")
  val rankAddressBits: Int = log2Ceil(nranks)
  require(byteOffsetBits + bankBits + rowBits + columnBits + rankAddressBits <= addressBits)
  require(burstLength > 0 && (burstLength & (burstLength - 1)) == 0)
  require(refreshPostponing >= 1 && refreshPostponing <= 8)
  require(cmdBufferDepth >= 1 && readTime >= 0 && writeTime >= 0)
  require(readLatency >= 1 && writeLatency >= 0)
  require(zqCalibrationPeriodCycles.forall(_ >= 1))
  require(zqCalibrationPeriodCycles.isEmpty || timing.tZqcs.nonEmpty,
    "automatic ZQ calibration requires timing.tZqcs")
  require(Set("SDR", "DDR", "LPDDR", "DDR2", "DDR3", "RPC", "DDR4", "LPDDR4", "LPDDR5").contains(memType))
  val bankCount: Int = 1 << bankBits
  val rankBankCount: Int = bankCount * nranks
  val memoryWords: Int = nranks * (1 << (bankBits + rowBits + columnBits))
  val columnAddressBits: Int = columnBits + byteOffsetBits
  val effectivePhyDataBits: Int = if (phyDataBits == 0) dataBits else phyDataBits
  val dfiDataBits: Int = effectivePhyDataBits / nPhases
  require(effectivePhyDataBits % nPhases == 0 && dfiDataBits % 8 == 0)
  require(dataBits >= dfiDataBits)
  val effectivePadDataBits: Int = if (padDataBits == 0) dfiDataBits else padDataBits
  require(effectivePadDataBits > 0 && effectivePadDataBits % 8 == 0)
}

class DramRequest(config: DramConfig) extends Bundle {
  val address = UInt(config.addressBits.W)
  val write = Bool()
  val data = UInt(config.dataBits.W)
  val mask = UInt((config.dataBits / 8).W)
}

class DramResponse(config: DramConfig) extends Bundle {
  val data = UInt(config.dataBits.W)
  val write = Bool()
}

class DramCommand(config: DramConfig) extends Bundle {
  val command = UInt(DramCommandType.width.W)
  val allBanks = Bool()
  val autoPrecharge = Bool()
  val rank = UInt(log2Ceil(config.nranks max 2).W)
  val bank = UInt(config.bankBits.W)
  val row = UInt(config.rowBits.W)
  val column = UInt(config.columnBits.W)
  val data = UInt(config.dataBits.W)
  val mask = UInt((config.dataBits / 8).W)
}

/** Row/column request already routed to one physical bank. */
class BankRequest(config: DramConfig) extends Bundle {
  val write = Bool()
  val row = UInt(config.rowBits.W)
  val column = UInt(config.columnBits.W)
}

class BankCompletion extends Bundle {
  val write = Bool()
}

/** One DFI phase. This is the portable contract implemented by vendor PHY adapters. */
class DfiPhase(config: DramConfig) extends Bundle {
  val address = UInt(config.rowBits.max(config.columnBits).max(11).W)
  val bank = UInt(config.bankBits.W)
  val csN = Vec(config.nranks, Bool())
  val rasN = Bool()
  val casN = Bool()
  val weN = Bool()
  val actN = Bool()
  val cke = Vec(config.nranks, Bool())
  val odt = Vec(config.nranks, Bool())
  val resetN = Bool()
  val rddataEn = Bool()
  val wrdataEn = Bool()
  val wrdata = UInt(config.dfiDataBits.W)
  val wrdataMask = UInt((config.dfiDataBits / 8).W)
  val rddata = UInt(config.dfiDataBits.W)
  val rddataValid = Bool()
}

/** Multi-phase DFI interface, analogous to litedram.phy.dfi.Interface. */
class DfiInterface(config: DramConfig) extends Bundle {
  val phases = Vec(config.nPhases, new DfiPhase(config))
}

class DfiReadResponse(config: DramConfig) extends Bundle {
  val data = UInt(config.dfiDataBits.W)
  val valid = Bool()
}

class NativeCommand(config: DramConfig) extends Bundle {
  val write = Bool()
  val address = UInt((config.addressBits - config.byteOffsetBits).W)
}

class NativeWriteData(config: DramConfig) extends Bundle {
  val data = UInt(config.dataBits.W)
  val byteEnable = UInt((config.dataBits / 8).W)
}

class NativeReadData(config: DramConfig) extends Bundle {
  val data = UInt(config.dataBits.W)
}

/** LiteDRAMNativePort-equivalent Decoupled port. */
class NativePort(config: DramConfig, mode: String = "both") extends Bundle {
  require(Set("read", "write", "both").contains(mode))
  val cmd = Decoupled(new NativeCommand(config))
  val wdata = Decoupled(new NativeWriteData(config))
  val rdata = Flipped(Decoupled(new NativeReadData(config)))
  val flush = Input(Bool())
  val lock = Output(Bool())
}
