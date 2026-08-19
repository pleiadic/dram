package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls

class BankMachineSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 12, dataBits = 32, bankBits = 1,
    rowBits = 4, columnBits = 4, cmdBufferDepth = 4,
    timing = DramTiming(tRcd = 2, tRp = 2, tRas = 3, tRc = 4, tCcd = 1,
      tWr = 2, tRtp = 2, tRefi = 100))

  private def enqueue(dut: BankMachine, row: Int, column: Int, write: Boolean): Unit = {
    dut.io.request.valid.poke(true.B)
    dut.io.request.bits.row.poke(row.U)
    dut.io.request.bits.column.poke(column.U)
    dut.io.request.bits.write.poke(write.B)
    while (!dut.io.request.ready.peek().litToBoolean) dut.clock.step()
    dut.clock.step()
    dut.io.request.valid.poke(false.B)
  }

  behavior of "BankMachine"

  it should "open a row once and serve subsequent row hits" in {
    test(new BankMachine(cfg, bankIndex = 1)) { dut =>
      dut.io.command.ready.poke(false.B)
      dut.io.refreshRequest.poke(false.B)
      enqueue(dut, row = 3, column = 1, write = false)
      enqueue(dut, row = 3, column = 2, write = true)
      dut.io.command.ready.poke(true.B)

      val commands = ArrayBuffer.empty[BigInt]
      var completions = 0
      for (_ <- 0 until 20) {
        if (dut.io.command.valid.peek().litToBoolean)
          commands += dut.io.command.bits.command.peek().litValue
        if (dut.io.completion.valid.peek().litToBoolean) completions += 1
        dut.clock.step()
      }
      assert(commands == Seq(DramCommandType.activate.litValue,
        DramCommandType.read.litValue, DramCommandType.write.litValue))
      assert(completions == 2)
      dut.io.command.bits.bank.expect(1.U)
    }
  }

  it should "precharge before activating a conflicting row" in {
    test(new BankMachine(cfg, bankIndex = 0)) { dut =>
      dut.io.command.ready.poke(true.B)
      dut.io.refreshRequest.poke(false.B)
      enqueue(dut, row = 1, column = 0, write = false)
      dut.clock.step(10)
      enqueue(dut, row = 2, column = 0, write = false)
      val commands = ArrayBuffer.empty[BigInt]
      for (_ <- 0 until 16) {
        if (dut.io.command.valid.peek().litToBoolean)
          commands += dut.io.command.bits.command.peek().litValue
        dut.clock.step()
      }
      val precharge = commands.indexOf(DramCommandType.precharge.litValue)
      val activate = commands.indexOf(DramCommandType.activate.litValue)
      assert(precharge >= 0 && activate > precharge)
    }
  }

  it should "look ahead and set auto-precharge only across rows" in {
    val autoCfg = cfg.copy(withAutoPrecharge = true)
    test(new BankMachine(autoCfg, bankIndex = 0)) { dut =>
      dut.io.command.ready.poke(false.B)
      dut.io.refreshRequest.poke(false.B)
      enqueue(dut, row = 1, column = 0, write = false)
      enqueue(dut, row = 2, column = 0, write = false)
      dut.io.command.ready.poke(true.B)
      var sawAuto = false
      for (_ <- 0 until 24) {
        if (dut.io.command.valid.peek().litToBoolean &&
            dut.io.command.bits.command.peek().litValue == DramCommandType.read.litValue &&
            dut.io.command.bits.autoPrecharge.peek().litToBoolean) sawAuto = true
        dut.clock.step()
      }
      assert(sawAuto)
    }
  }

  it should "hold its command stable and grant refresh after draining timing" in {
    test(new BankMachine(cfg, bankIndex = 0)) { dut =>
      dut.io.command.ready.poke(false.B)
      dut.io.refreshRequest.poke(false.B)
      enqueue(dut, row = 5, column = 6, write = false)
      while (!dut.io.command.valid.peek().litToBoolean) dut.clock.step()
      val command = dut.io.command.bits.command.peek().litValue
      val row = dut.io.command.bits.row.peek().litValue
      dut.clock.step(3)
      assert(dut.io.command.bits.command.peek().litValue == command)
      assert(dut.io.command.bits.row.peek().litValue == row)
      dut.io.refreshRequest.poke(true.B)
      dut.io.command.ready.poke(true.B)
      while (!dut.io.refreshGrant.peek().litToBoolean) dut.clock.step()
      dut.io.rowOpen.expect(false.B)
    }
  }
}
