package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class DfiMemoryModelSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val config = DramConfig(addressBits = 8, dataBits = 32,
    bankBits = 1, rowBits = 2, columnBits = 2, nPhases = 2,
    timing = DramTiming(tRefi = 100), readLatency = 1)

  private def nop(dut: DfiMemoryModel): Unit = {
    for (phase <- dut.io.dfi.phases) {
      phase.address.poke(0.U)
      phase.bank.poke(0.U)
      phase.csN.foreach(_.poke(true.B))
      phase.rasN.poke(true.B)
      phase.casN.poke(true.B)
      phase.weN.poke(true.B)
      phase.actN.poke(true.B)
      phase.cke.foreach(_.poke(true.B))
      phase.odt.foreach(_.poke(false.B))
      phase.resetN.poke(true.B)
      phase.rddataEn.poke(false.B)
      phase.wrdataEn.poke(false.B)
      phase.wrdata.poke(0.U)
      phase.wrdataMask.poke(0.U)
      phase.rddata.poke(0.U)
      phase.rddataValid.poke(false.B)
    }
    dut.io.clearErrors.poke(false.B)
  }

  private def activate(dut: DfiMemoryModel, bank: Int, row: Int): Unit = {
    nop(dut)
    val phase = dut.io.dfi.phases(0)
    phase.csN(0).poke(false.B)
    phase.bank.poke(bank.U)
    phase.address.poke(row.U)
    phase.rasN.poke(false.B)
    phase.casN.poke(true.B)
    phase.weN.poke(true.B)
    dut.clock.step()
  }

  private def write(dut: DfiMemoryModel, bank: Int, column: Int,
      low: Int, high: Int, mask: Int = 0): Unit = {
    nop(dut)
    val phase = dut.io.dfi.phases(0)
    phase.csN(0).poke(false.B)
    phase.bank.poke(bank.U)
    phase.address.poke(column.U)
    phase.casN.poke(false.B)
    phase.weN.poke(false.B)
    phase.wrdataEn.poke(true.B)
    dut.io.dfi.phases(0).wrdata.poke(low.U)
    dut.io.dfi.phases(1).wrdata.poke(high.U)
    dut.io.dfi.phases(0).wrdataMask.poke((mask & 3).U)
    dut.io.dfi.phases(1).wrdataMask.poke(((mask >> 2) & 3).U)
    dut.clock.step()
  }

  private def read(dut: DfiMemoryModel, bank: Int, column: Int): Unit = {
    nop(dut)
    val phase = dut.io.dfi.phases(0)
    phase.csN(0).poke(false.B)
    phase.bank.poke(bank.U)
    phase.address.poke(column.U)
    phase.casN.poke(false.B)
    phase.weN.poke(true.B)
    dut.clock.step()
    nop(dut)
  }

  behavior of "DfiMemoryModel"

  it should "track open rows and preserve multi-phase data with byte masks" in {
    test(new DfiMemoryModel(config)) { dut =>
      nop(dut)
      activate(dut, bank = 0, row = 1)
      dut.io.openBanks.expect(1.U)
      write(dut, bank = 0, column = 2, low = 0x1122, high = 0x3344)
      read(dut, bank = 0, column = 2)
      dut.io.read.foreach(_.valid.expect(true.B))
      dut.io.read(0).data.expect("h1122".U)
      dut.io.read(1).data.expect("h3344".U)

      // Mask bytes 0 and 3, replacing only the middle two bytes.
      write(dut, bank = 0, column = 2, low = 0xaabb, high = 0xccdd, mask = 0x9)
      read(dut, bank = 0, column = 2)
      dut.io.read(0).data.expect("haa22".U)
      dut.io.read(1).data.expect("h33dd".U)
      dut.io.errors.expect(0.U)
    }
  }

  it should "flag column commands to closed banks and refresh with open rows" in {
    test(new DfiMemoryModel(config)) { dut =>
      nop(dut)
      read(dut, bank = 1, column = 0)
      dut.io.errors.expect(1.U)

      activate(dut, bank = 0, row = 0)
      nop(dut)
      val refresh = dut.io.dfi.phases(0)
      refresh.csN(0).poke(false.B)
      refresh.rasN.poke(false.B)
      refresh.casN.poke(false.B)
      refresh.weN.poke(true.B)
      dut.io.protocolError.expect(true.B)
      dut.clock.step()
      dut.io.errors.expect(2.U)

      dut.io.clearErrors.poke(true.B)
      nop(dut)
      dut.io.clearErrors.poke(true.B)
      dut.clock.step()
      dut.io.errors.expect(0.U)
    }
  }

  it should "isolate identical bank row and column addresses across ranks" in {
    val rankConfig = DramConfig(addressBits = 9, dataBits = 16,
      bankBits = 1, rowBits = 2, columnBits = 2, nPhases = 1, nranks = 2,
      timing = DramTiming(tRefi = 100), readLatency = 1)
    test(new DfiMemoryModel(rankConfig)) { dut =>
      def idle(): Unit = {
        val phase = dut.io.dfi.phases(0)
        phase.address.poke(0.U)
        phase.bank.poke(0.U)
        phase.csN.foreach(_.poke(true.B))
        phase.rasN.poke(true.B)
        phase.casN.poke(true.B)
        phase.weN.poke(true.B)
        phase.actN.poke(true.B)
        phase.cke.foreach(_.poke(true.B))
        phase.odt.foreach(_.poke(false.B))
        phase.resetN.poke(true.B)
        phase.rddataEn.poke(false.B)
        phase.wrdataEn.poke(false.B)
        phase.wrdata.poke(0.U)
        phase.wrdataMask.poke(0.U)
        phase.rddata.poke(0.U)
        phase.rddataValid.poke(false.B)
        dut.io.clearErrors.poke(false.B)
      }
      def select(rank: Int): DfiPhase = {
        val phase = dut.io.dfi.phases(0)
        phase.csN(rank).poke(false.B)
        phase
      }
      def open(rank: Int, row: Int): Unit = {
        idle()
        val phase = select(rank)
        phase.address.poke(row.U)
        phase.rasN.poke(false.B)
        dut.clock.step()
      }
      def put(rank: Int, value: Int): Unit = {
        idle()
        val phase = select(rank)
        phase.casN.poke(false.B)
        phase.weN.poke(false.B)
        phase.wrdataEn.poke(true.B)
        phase.wrdata.poke(value.U)
        dut.clock.step()
      }
      def get(rank: Int, value: Int): Unit = {
        idle()
        val phase = select(rank)
        phase.casN.poke(false.B)
        phase.weN.poke(true.B)
        dut.clock.step()
        idle()
        dut.io.read(0).valid.expect(true.B)
        dut.io.read(0).data.expect(value.U)
      }

      idle()
      open(rank = 0, row = 1)
      open(rank = 1, row = 1)
      put(rank = 0, value = 0x1111)
      put(rank = 1, value = 0x2222)
      get(rank = 0, value = 0x1111)
      get(rank = 1, value = 0x2222)
      dut.io.errors.expect(0.U)
    }
  }
}
