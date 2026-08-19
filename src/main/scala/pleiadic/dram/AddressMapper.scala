package pleiadic.dram

import chisel3._
import chisel3.util.log2Ceil
import scala.language.reflectiveCalls

class MappedAddress(config: DramConfig) extends Bundle {
  val rank = UInt(log2Ceil(config.nranks max 2).W)
  val bank = UInt(config.bankBits.W)
  val row = UInt(config.rowBits.W)
  val column = UInt(config.columnBits.W)
}

/** Pure combinational address slicer for all mappings exposed by the Chisel core. */
class AddressMapper(config: DramConfig) extends Module {
  val io = IO(new Bundle {
    val address = Input(UInt(config.addressBits.W))
    val mapped = Output(new MappedAddress(config))
  })

  private val offset = config.byteOffsetBits
  private val rankBits = log2Ceil(config.nranks max 2)
  private def bits(lsb: Int, width: Int): UInt = io.address(lsb + width - 1, lsb)

  io.mapped := 0.U.asTypeOf(new MappedAddress(config))
  config.addressMapping match {
    case DramAddressMapping.RowBankColumn =>
      io.mapped.column := bits(offset, config.columnBits)
      io.mapped.bank := bits(offset + config.columnBits, config.bankBits)
      io.mapped.row := bits(offset + config.columnBits + config.bankBits, config.rowBits)
      if (config.nranks > 1) io.mapped.rank := bits(offset + config.columnBits + config.bankBits + config.rowBits, rankBits)
    case DramAddressMapping.BankRowColumn =>
      io.mapped.column := bits(offset, config.columnBits)
      io.mapped.row := bits(offset + config.columnBits, config.rowBits)
      io.mapped.bank := bits(offset + config.columnBits + config.rowBits, config.bankBits)
      if (config.nranks > 1) io.mapped.rank := bits(offset + config.columnBits + config.rowBits + config.bankBits, rankBits)
    case DramAddressMapping.ColumnBankRow =>
      io.mapped.row := bits(offset, config.rowBits)
      io.mapped.bank := bits(offset + config.rowBits, config.bankBits)
      io.mapped.column := bits(offset + config.rowBits + config.bankBits, config.columnBits)
      if (config.nranks > 1) io.mapped.rank := bits(offset + config.rowBits + config.bankBits + config.columnBits, rankBits)
  }
}
