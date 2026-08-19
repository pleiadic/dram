package pleiadic.dram

import chisel3._
import chisel3.util.log2Ceil

/** Portable DRAM command encoding, intentionally independent of a vendor PHY. */
object DramCommandType {
  val width = 3
  val nop     = 0.U(width.W)
  val activate = 1.U(width.W)
  val precharge = 2.U(width.W)
  val read    = 3.U(width.W)
  val write   = 4.U(width.W)
  val refresh = 5.U(width.W)
}

case class DramTiming(
  tRcd: Int = 2,
  tRp: Int = 2,
  tRas: Int = 4,
  tRc: Int = 6,
  tCcd: Int = 1,
  tRefi: Int = 64,
  tRfc: Int = 4
) {
  require(tRcd >= 1 && tRp >= 1 && tRas >= 1 && tRc >= 1 && tCcd >= 1)
  require(tRefi >= 1 && tRfc >= 1)
}

case class DramConfig(
  addressBits: Int = 24,
  dataBits: Int = 32,
  bankBits: Int = 2,
  rowBits: Int = 10,
  columnBits: Int = 8,
  timing: DramTiming = DramTiming(),
  withAutoPrecharge: Boolean = false
) {
  val byteOffsetBits: Int = log2Ceil(dataBits / 8)
  require(addressBits > 0 && dataBits > 0 && dataBits % 8 == 0)
  require(bankBits > 0 && rowBits > 0 && columnBits > 0)
  require(byteOffsetBits + bankBits + rowBits + columnBits <= addressBits)
  val bankCount: Int = 1 << bankBits
  val memoryWords: Int = 1 << (bankBits + rowBits + columnBits)
  val columnAddressBits: Int = columnBits + byteOffsetBits
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
  val bank = UInt(config.bankBits.W)
  val row = UInt(config.rowBits.W)
  val column = UInt(config.columnBits.W)
  val data = UInt(config.dataBits.W)
  val mask = UInt((config.dataBits / 8).W)
}
