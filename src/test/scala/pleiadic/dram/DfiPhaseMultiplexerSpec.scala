package pleiadic.dram

import chisel3._
import chisel3.util.DecoupledIO
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls
import scala.util.Random

class DfiPhaseMultiplexerSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 16, dataBits = 32, bankBits = 2,
    rowBits = 5, columnBits = 5, nPhases = 2, readPhase = 0, writePhase = 1,
    timing = DramTiming(tRefi = 100))

  private def pokeCommand(port: DecoupledIO[DramCommand], command: UInt,
      bank: Int, row: Int = 0, column: Int = 0, rank: Int = 0,
      allBanks: Boolean = false, autoPrecharge: Boolean = false): Unit = {
    port.valid.poke(true.B)
    port.bits.command.poke(command)
    port.bits.allBanks.poke(allBanks.B)
    port.bits.autoPrecharge.poke(autoPrecharge.B)
    port.bits.rank.poke(rank.U)
    port.bits.bank.poke(bank.U)
    port.bits.row.poke(row.U)
    port.bits.column.poke(column.U)
    port.bits.data.poke("h76543210".U)
    port.bits.mask.poke("hf".U)
  }

  behavior of "DualCommandChooser"

  it should "accept independent row and column streams in the same cycle" in {
    test(new DualCommandChooser(cfg, 4)) { dut =>
      dut.io.wantReads.poke(true.B)
      dut.io.wantWrites.poke(false.B)
      dut.io.wantActivates.poke(true.B)
      dut.io.rowCommand.ready.poke(true.B)
      dut.io.columnRequest.ready.poke(true.B)
      pokeCommand(dut.io.requests(0), DramCommandType.write, bank = 0)
      pokeCommand(dut.io.requests(1), DramCommandType.read, bank = 1)
      pokeCommand(dut.io.requests(2), DramCommandType.activate, bank = 2, row = 17)
      pokeCommand(dut.io.requests(3), DramCommandType.precharge, bank = 3)

      dut.io.columnRequest.valid.expect(true.B)
      dut.io.columnRequest.bits.bank.expect(1.U)
      dut.io.rowCommand.valid.expect(true.B)
      dut.io.rowCommand.bits.bank.expect(2.U)
      dut.io.requests(1).ready.expect(true.B)
      dut.io.requests(2).ready.expect(true.B)
      dut.io.requests(0).ready.expect(false.B)
      dut.io.requests(3).ready.expect(false.B)
      dut.clock.step()

      dut.io.rowCommand.bits.bank.expect(3.U)
    }
  }

  behavior of "DfiPhaseSteerer"

  it should "place read and write CMD/REQ streams on the LiteDRAM phases" in {
    test(new DfiPhaseSteerer(cfg)) { dut =>
      dut.io.refreshMode.poke(false.B)
      dut.io.writeMode.poke(false.B)
      dut.io.refreshCommand.valid.poke(false.B)
      pokeCommand(dut.io.rowCommand, DramCommandType.activate,
        bank = 3, row = 19)
      pokeCommand(dut.io.columnRequest, DramCommandType.read,
        bank = 2, column = 7)
      dut.io.rowCommand.ready.expect(true.B)
      dut.io.columnRequest.ready.expect(true.B)
      dut.clock.step()

      // READ REQ is phase 0; its preceding CMD wraps to phase 1.
      dut.io.dfi.phases(0).bank.expect(2.U)
      dut.io.dfi.phases(0).address.expect(7.U)
      dut.io.dfi.phases(0).casN.expect(false.B)
      dut.io.dfi.phases(0).rddataEn.expect(true.B)
      dut.io.dfi.phases(1).bank.expect(3.U)
      dut.io.dfi.phases(1).address.expect(19.U)
      dut.io.dfi.phases(1).rasN.expect(false.B)

      dut.io.writeMode.poke(true.B)
      pokeCommand(dut.io.rowCommand, DramCommandType.precharge,
        bank = 1, column = 3)
      pokeCommand(dut.io.columnRequest, DramCommandType.write,
        bank = 0, column = 11, autoPrecharge = true)
      dut.clock.step()

      // WRITE CMD is phase 0 and REQ/data-enable is phase 1.
      dut.io.dfi.phases(0).bank.expect(1.U)
      dut.io.dfi.phases(0).rasN.expect(false.B)
      dut.io.dfi.phases(0).weN.expect(false.B)
      dut.io.dfi.phases(0).address.expect(3.U)
      dut.io.dfi.phases(1).bank.expect(0.U)
      dut.io.dfi.phases(1).casN.expect(false.B)
      dut.io.dfi.phases(1).weN.expect(false.B)
      dut.io.dfi.phases(1).wrdataEn.expect(true.B)
      dut.io.dfi.phases(1).address.expect((11 | (1 << 10)).U)
    }
  }

  it should "serialize CMD before REQ on a single DFI phase" in {
    val single = cfg.copy(nPhases = 1, readPhase = 0, writePhase = 0)
    test(new DfiPhaseSteerer(single)) { dut =>
      dut.io.refreshMode.poke(false.B)
      dut.io.writeMode.poke(true.B)
      dut.io.refreshCommand.valid.poke(false.B)
      pokeCommand(dut.io.rowCommand, DramCommandType.activate,
        bank = 2, row = 21)
      pokeCommand(dut.io.columnRequest, DramCommandType.write,
        bank = 1, column = 5)
      dut.io.rowCommand.ready.expect(true.B)
      dut.io.columnRequest.ready.expect(false.B)
      dut.clock.step()
      dut.io.dfi.phases(0).rasN.expect(false.B)
      dut.io.dfi.phases(0).bank.expect(2.U)

      dut.io.rowCommand.valid.poke(false.B)
      dut.io.columnRequest.ready.expect(true.B)
      dut.clock.step()
      dut.io.dfi.phases(0).casN.expect(false.B)
      dut.io.dfi.phases(0).weN.expect(false.B)
      dut.io.dfi.phases(0).bank.expect(1.U)
    }
  }

  it should "select all ranks only for an all-rank refresh command" in {
    val ranked = cfg.copy(nranks = 2)
    test(new DfiPhaseSteerer(ranked)) { dut =>
      dut.io.writeMode.poke(false.B)
      dut.io.rowCommand.valid.poke(false.B)
      dut.io.columnRequest.valid.poke(false.B)
      dut.io.refreshMode.poke(true.B)
      pokeCommand(dut.io.refreshCommand, DramCommandType.refresh,
        bank = 0, allBanks = true)
      dut.clock.step()
      dut.io.dfi.phases(0).csN(0).expect(false.B)
      dut.io.dfi.phases(0).csN(1).expect(false.B)
      dut.io.dfi.phases(0).rasN.expect(false.B)
      dut.io.dfi.phases(0).casN.expect(false.B)

      dut.io.refreshMode.poke(false.B)
      dut.io.refreshCommand.valid.poke(false.B)
      pokeCommand(dut.io.rowCommand, DramCommandType.activate,
        bank = 2, row = 9, rank = 1)
      dut.clock.step()
      dut.io.dfi.phases(1).csN(0).expect(true.B)
      dut.io.dfi.phases(1).csN(1).expect(false.B)
    }
  }

  it should "match a phase-placement oracle over a long random command stream" in {
    val fourPhase = cfg.copy(nPhases = 4, readPhase = 1, writePhase = 3)
    test(new DfiPhaseSteerer(fourPhase)) { dut =>
      val rng = new Random(0x5354454552L)
      dut.io.refreshCommand.valid.poke(false.B)

      for (cycle <- 0 until 300) {
        val writeMode = rng.nextBoolean()
        val refresh = rng.nextInt(23) == 0
        val rowValid = !refresh && rng.nextInt(100) < 70
        val columnValid = !refresh && rng.nextInt(100) < 78
        val rowIsActivate = rng.nextBoolean()
        val rowBank = rng.nextInt(4)
        val columnBank = rng.nextInt(4)
        val rowAddress = rng.nextInt(32)
        val columnAddress = rng.nextInt(32)

        dut.io.writeMode.poke(writeMode.B)
        dut.io.refreshMode.poke(refresh.B)
        if (rowValid) pokeCommand(dut.io.rowCommand,
          if (rowIsActivate) DramCommandType.activate else DramCommandType.precharge,
          rowBank, row = rowAddress, column = columnAddress)
        else dut.io.rowCommand.valid.poke(false.B)
        if (columnValid) pokeCommand(dut.io.columnRequest,
          if (writeMode) DramCommandType.write else DramCommandType.read,
          columnBank, column = columnAddress)
        else dut.io.columnRequest.valid.poke(false.B)
        if (refresh) pokeCommand(dut.io.refreshCommand,
          DramCommandType.refresh, bank = 0, allBanks = true)
        else dut.io.refreshCommand.valid.poke(false.B)

        dut.clock.step()

        val dataPhase = if (writeMode) fourPhase.writePhase else fourPhase.readPhase
        val rowPhase = (dataPhase + fourPhase.nPhases - 1) % fourPhase.nPhases
        for (phase <- 0 until fourPhase.nPhases) {
          val dfi = dut.io.dfi.phases(phase)
          if (refresh && phase == 0) {
            dfi.rasN.expect(false.B)
            dfi.casN.expect(false.B)
          } else if (rowValid && phase == rowPhase) {
            dfi.bank.expect(rowBank.U)
            dfi.rasN.expect(false.B)
            dfi.casN.expect(true.B)
            dfi.weN.expect(rowIsActivate.B)
          } else if (columnValid && phase == dataPhase) {
            dfi.bank.expect(columnBank.U)
            dfi.rasN.expect(true.B)
            dfi.casN.expect(false.B)
            dfi.weN.expect((!writeMode).B)
            dfi.rddataEn.expect((!writeMode).B)
            dfi.wrdataEn.expect(writeMode.B)
          } else {
            dfi.rasN.expect(true.B)
            dfi.casN.expect(true.B)
            dfi.weN.expect(true.B)
            dfi.rddataEn.expect(false.B)
            dfi.wrdataEn.expect(false.B)
          }
        }
      }
    }
  }

  behavior of "DfiMultiplexer"

  it should "issue a row command and read request from different banks together" in {
    val muxCfg = cfg.copy(readTime = 8,
      timing = cfg.timing.copy(tRrd = 1, tFaw = 8, tCcd = 1, tWtr = 1))
    test(new DfiMultiplexer(muxCfg, bankCount = 4)) { dut =>
      dut.io.refreshMode.poke(false.B)
      dut.io.refreshCommand.valid.poke(false.B)
      dut.io.bankCommands.foreach(_.valid.poke(false.B))
      dut.clock.step(12) // Let LiteDRAM-compatible timing primitives become ready.

      pokeCommand(dut.io.bankCommands(0), DramCommandType.read,
        bank = 0, column = 6)
      pokeCommand(dut.io.bankCommands(1), DramCommandType.activate,
        bank = 1, row = 18)
      dut.io.bankCommands(0).ready.expect(true.B)
      dut.io.bankCommands(1).ready.expect(true.B)
      dut.clock.step()

      dut.io.dfi.phases(0).bank.expect(0.U)
      dut.io.dfi.phases(0).address.expect(6.U)
      dut.io.dfi.phases(0).casN.expect(false.B)
      dut.io.dfi.phases(0).rddataEn.expect(true.B)
      dut.io.dfi.phases(1).bank.expect(1.U)
      dut.io.dfi.phases(1).address.expect(18.U)
      dut.io.dfi.phases(1).rasN.expect(false.B)
    }
  }
}
