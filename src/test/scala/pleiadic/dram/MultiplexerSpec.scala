package pleiadic.dram

import chisel3._
import chisel3.util.DecoupledIO
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls

class MultiplexerSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 12, dataBits = 32, bankBits = 1,
    rowBits = 4, columnBits = 4, readTime = 3, writeTime = 2, readLatency = 2,
    timing = DramTiming(tRrd = 2, tFaw = 8, tCcd = 2, tWtr = 2, tRefi = 100))

  private def pokeCommand(port: DecoupledIO[DramCommand], command: UInt, bank: Int): Unit = {
    port.valid.poke(true.B)
    port.bits.command.poke(command)
    port.bits.allBanks.poke(false.B)
    port.bits.autoPrecharge.poke(false.B)
    port.bits.rank.poke(0.U)
    port.bits.bank.poke(bank.U)
    port.bits.row.poke(bank.U)
    port.bits.column.poke(bank.U)
    port.bits.data.poke(0.U)
    port.bits.mask.poke(0.U)
  }

  behavior of "CommandChooser"

  it should "filter command classes and rotate fairly after acceptance" in {
    test(new CommandChooser(cfg, 3)) { dut =>
      dut.io.output.ready.poke(true.B)
      dut.io.wantReads.poke(true.B)
      dut.io.wantWrites.poke(false.B)
      dut.io.wantCommands.poke(false.B)
      dut.io.wantActivates.poke(false.B)
      pokeCommand(dut.io.requests(0), DramCommandType.write, 0)
      pokeCommand(dut.io.requests(1), DramCommandType.read, 1)
      pokeCommand(dut.io.requests(2), DramCommandType.read, 0)
      dut.io.selected.expect(1.U)
      dut.clock.step()
      dut.io.selected.expect(2.U)
    }
  }

  it should "hold the selected request while the consumer stalls" in {
    test(new CommandChooser(cfg, 2)) { dut =>
      dut.io.output.ready.poke(false.B)
      dut.io.wantReads.poke(true.B)
      dut.io.wantWrites.poke(false.B)
      dut.io.wantCommands.poke(false.B)
      dut.io.wantActivates.poke(false.B)
      pokeCommand(dut.io.requests(0), DramCommandType.read, 0)
      pokeCommand(dut.io.requests(1), DramCommandType.read, 1)
      dut.io.selected.expect(0.U)
      dut.clock.step(3)
      dut.io.selected.expect(0.U)
      dut.io.output.bits.bank.expect(0.U)
    }
  }

  behavior of "Multiplexer"

  it should "enforce tCCD between accepted column commands" in {
    test(new Multiplexer(cfg, 2)) { dut =>
      dut.io.command.ready.poke(true.B)
      dut.io.refreshMode.poke(false.B)
      dut.io.refreshCommand.valid.poke(false.B)
      pokeCommand(dut.io.bankCommands(0), DramCommandType.read, 0)
      pokeCommand(dut.io.bankCommands(1), DramCommandType.read, 1)
      val accepted = ArrayBuffer.empty[Int]
      for (cycle <- 0 until 12) {
        if (dut.io.command.valid.peek().litToBoolean) accepted += cycle
        dut.clock.step()
      }
      assert(accepted.size >= 2)
      assert(accepted.sliding(2).forall(pair => pair(1) - pair(0) >= cfg.timing.tCcd))
    }
  }

  it should "switch to writes and give refresh exclusive access" in {
    test(new Multiplexer(cfg.copy(readTime = 1, readLatency = 1), 1)) { dut =>
      dut.io.command.ready.poke(true.B)
      dut.io.refreshMode.poke(false.B)
      pokeCommand(dut.io.bankCommands(0), DramCommandType.write, 0)
      pokeCommand(dut.io.refreshCommand, DramCommandType.refresh, 0)
      while (!dut.io.servingWrites.peek().litToBoolean) dut.clock.step()
      dut.io.refreshMode.poke(true.B)
      dut.clock.step()
      dut.io.command.bits.command.expect(DramCommandType.refresh)
      dut.io.bankCommands(0).ready.expect(false.B)
    }
  }
}
