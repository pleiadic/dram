package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class CrossbarSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 14, dataBits = 32, bankBits = 2,
    rowBits = 4, columnBits = 4, timing = DramTiming(tRefi = 100))

  private def address(row: Int, bank: Int, column: Int): BigInt =
    BigInt((((row << cfg.bankBits) | bank) << cfg.columnBits) | column)

  private def pokeMaster(dut: LiteDramCommandCrossbar, master: Int,
      row: Int, bank: Int, column: Int, write: Boolean): Unit = {
    val port = dut.io.masters(master).command
    port.valid.poke(true.B)
    port.bits.address.poke(address(row, bank, column).U)
    port.bits.write.poke(write.B)
  }

  behavior of "LiteDramDataCrossbar"

  it should "route write payloads and read responses by completion owner" in {
    test(new LiteDramDataCrossbar(cfg, 2)) { dut =>
      dut.io.bankCompletions.foreach(_.valid.poke(false.B))
      dut.io.bankOwnerValid.foreach(_.poke(false.B))
      dut.io.bankOwners.foreach(_.poke(0.U))
      dut.io.masters.foreach { master =>
        master.writeData.valid.poke(false.B)
        master.readData.ready.poke(true.B)
      }
      dut.io.writeData.ready.poke(true.B)
      dut.io.readData.valid.poke(false.B)
      dut.io.readData.bits.data.poke(0.U)

      dut.io.masters(1).writeData.valid.poke(true.B)
      dut.io.masters(1).writeData.bits.data.poke("hdeadbeef".U)
      dut.io.masters(1).writeData.bits.byteEnable.poke("hf".U)
      dut.clock.step()
      dut.io.masters(1).writeData.valid.poke(false.B)

      dut.io.bankOwnerValid(2).poke(true.B)
      dut.io.bankOwners(2).poke(1.U)
      dut.io.bankCompletions(2).valid.poke(true.B)
      dut.io.bankCompletions(2).bits.write.poke(true.B)
      dut.clock.step()
      dut.io.bankCompletions(2).valid.poke(false.B)
      dut.io.writeData.valid.expect(true.B)
      dut.io.writeData.bits.data.expect("hdeadbeef".U)
      dut.clock.step()

      dut.io.bankOwnerValid(0).poke(true.B)
      dut.io.bankOwners(0).poke(0.U)
      dut.io.bankCompletions(0).valid.poke(true.B)
      dut.io.bankCompletions(0).bits.write.poke(false.B)
      dut.clock.step()
      dut.io.bankCompletions(0).valid.poke(false.B)
      dut.io.readData.valid.poke(true.B)
      dut.io.readData.bits.data.poke("h12345678".U)
      dut.io.masters(0).readData.valid.expect(true.B)
      dut.io.masters(0).readData.bits.data.expect("h12345678".U)
      dut.io.masters(1).readData.valid.expect(false.B)
    }
  }

  behavior of "LiteDramCommandCrossbar"

  it should "map addresses and route concurrent masters to different banks" in {
    test(new LiteDramCommandCrossbar(cfg, 2)) { dut =>
      dut.io.bankLocks.foreach(_.poke(false.B))
      dut.io.bankRequests.foreach(_.ready.poke(true.B))
      pokeMaster(dut, 0, row = 3, bank = 1, column = 7, write = false)
      pokeMaster(dut, 1, row = 4, bank = 2, column = 5, write = true)
      dut.io.bankRequests(1).valid.expect(true.B)
      dut.io.bankRequests(1).bits.row.expect(3.U)
      dut.io.bankRequests(1).bits.column.expect(7.U)
      dut.io.bankRequests(2).valid.expect(true.B)
      dut.io.bankRequests(2).bits.row.expect(4.U)
      dut.io.bankRequests(2).bits.write.expect(true.B)
    }
  }

  it should "hold arbitration under backpressure and lock a master to its bank" in {
    test(new LiteDramCommandCrossbar(cfg, 2)) { dut =>
      dut.io.bankLocks.foreach(_.poke(false.B))
      dut.io.bankRequests.foreach(_.ready.poke(false.B))
      pokeMaster(dut, 0, row = 1, bank = 0, column = 0, write = false)
      dut.io.masters(1).command.valid.poke(false.B)
      dut.io.bankRequests(0).bits.row.expect(1.U)
      dut.clock.step()
      pokeMaster(dut, 1, row = 2, bank = 0, column = 0, write = false)
      dut.io.bankRequests(0).bits.row.expect(1.U)

      dut.io.bankRequests(0).ready.poke(true.B)
      dut.clock.step()
      dut.io.masters(0).command.valid.poke(false.B)
      dut.io.bankLocks(0).poke(true.B)
      pokeMaster(dut, 0, row = 1, bank = 1, column = 0, write = false)
      dut.io.masters(0).lock.expect(true.B)
      dut.io.bankRequests(1).valid.expect(false.B)
    }
  }
}
