package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls

class LiteDramControllerSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 12, dataBits = 32, bankBits = 1,
    rowBits = 4, columnBits = 4, cmdBufferDepth = 4,
    timing = DramTiming(tRcd = 1, tRp = 1, tRas = 2, tRc = 2,
      tCcd = 1, tRrd = 1, tFaw = 8, tRefi = 24, tRfc = 2))

  private def clearRequests(dut: LiteDramController): Unit =
    dut.io.bankRequests.foreach(_.valid.poke(false.B))

  private def enqueue(dut: LiteDramController, bank: Int, row: Int, write: Boolean): Unit = {
    val port = dut.io.bankRequests(bank)
    port.valid.poke(true.B)
    port.bits.row.poke(row.U)
    port.bits.column.poke(0.U)
    port.bits.write.poke(write.B)
    while (!port.ready.peek().litToBoolean) dut.clock.step()
    dut.clock.step()
    port.valid.poke(false.B)
  }

  behavior of "LiteDramController"

  it should "schedule independent banks and complete both requests" in {
    test(new LiteDramController(cfg)) { dut =>
      clearRequests(dut)
      dut.io.command.ready.poke(true.B)
      enqueue(dut, bank = 0, row = 1, write = false)
      enqueue(dut, bank = 1, row = 2, write = false)

      val activatedBanks = ArrayBuffer.empty[BigInt]
      var completions = 0
      for (_ <- 0 until 20) {
        if (dut.io.command.valid.peek().litToBoolean &&
            dut.io.command.bits.command.peek().litValue == DramCommandType.activate.litValue)
          activatedBanks += dut.io.command.bits.bank.peek().litValue
        completions += dut.io.bankCompletions.count(_.valid.peek().litToBoolean)
        dut.clock.step()
      }
      assert(activatedBanks.toSet == Set(BigInt(0), BigInt(1)))
      assert(completions == 2)
    }
  }

  it should "quiesce every bank before issuing refresh" in {
    val refreshCfg = cfg.copy(timing = cfg.timing.copy(tRefi = 8))
    test(new LiteDramController(refreshCfg)) { dut =>
      clearRequests(dut)
      dut.io.command.ready.poke(true.B)
      enqueue(dut, bank = 0, row = 1, write = true)
      var sawRefresh = false
      for (_ <- 0 until 32) {
        if (dut.io.command.valid.peek().litToBoolean &&
            dut.io.command.bits.command.peek().litValue == DramCommandType.refresh.litValue) {
          sawRefresh = true
          assert(dut.io.bankLocks.forall(lock => !lock.peek().litToBoolean))
        }
        dut.clock.step()
      }
      assert(sawRefresh)
    }
  }
}
