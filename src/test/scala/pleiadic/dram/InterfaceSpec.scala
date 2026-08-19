package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class InterfaceSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "AddressMapper"

  it should "decode row-bank-column addresses" in {
    val cfg = DramConfig(addressBits = 16, dataBits = 32, bankBits = 2, rowBits = 5, columnBits = 4)
    test(new AddressMapper(cfg)) { dut =>
      val row = 0x12
      val bank = 2
      val column = 9
      val address = (((row << cfg.bankBits) | bank) << cfg.columnBits | column) << cfg.byteOffsetBits
      dut.io.address.poke(address.U)
      dut.io.mapped.row.expect(row.U)
      dut.io.mapped.bank.expect(bank.U)
      dut.io.mapped.column.expect(column.U)
    }
  }

  behavior of "DfiSteerer"

  it should "encode an activate as active-low DFI signals" in {
    val cfg = DramConfig(addressBits = 16, dataBits = 32, bankBits = 2, rowBits = 5, columnBits = 4)
    test(new DfiSteerer(cfg)) { dut =>
      dut.io.command.valid.poke(true.B)
      dut.io.command.bits.command.poke(DramCommandType.activate)
      dut.io.command.bits.allBanks.poke(false.B)
      dut.io.command.bits.autoPrecharge.poke(false.B)
      dut.io.command.bits.rank.poke(0.U)
      dut.io.command.bits.bank.poke(2.U)
      dut.io.command.bits.row.poke(17.U)
      dut.io.command.bits.column.poke(0.U)
      dut.io.command.bits.data.poke(0.U)
      dut.io.command.bits.mask.poke(0.U)
      dut.io.dfi.phases(0).rasN.expect(false.B)
      dut.io.dfi.phases(0).casN.expect(true.B)
      dut.io.dfi.phases(0).weN.expect(true.B)
      dut.io.dfi.phases(0).bank.expect(2.U)
      dut.io.dfi.phases(0).address.expect(17.U)
    }
  }
}
