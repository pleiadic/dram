package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls

class RefresherSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "RefreshTimer"

  it should "pulse periodically while enabled" in {
    test(new RefreshTimer(4)) { dut =>
      dut.io.waitEnable.poke(true.B)
      val pulses = ArrayBuffer.empty[Int]
      for (cycle <- 0 until 13) {
        if (dut.io.done.peek().litToBoolean) pulses += cycle
        dut.clock.step()
      }
      assert(pulses == Seq(3, 7, 11))
    }
  }

  behavior of "RefreshPostponer"

  it should "combine the configured number of requests" in {
    test(new RefreshPostponer(3)) { dut =>
      dut.io.requestIn.poke(false.B)
      var pulses = 0
      for (cycle <- 0 until 10) {
        dut.io.requestIn.poke((cycle % 2 == 0).B)
        dut.clock.step()
        if (dut.io.requestOut.peek().litToBoolean) pulses += 1
      }
      assert(pulses == 1)
    }
  }

  behavior of "RefreshSequencer"

  it should "execute every refresh and hold a command under backpressure" in {
    val cfg = DramConfig(addressBits = 12, dataBits = 32, bankBits = 1, rowBits = 4,
      columnBits = 4, refreshPostponing = 2,
      timing = DramTiming(tRp = 2, tRfc = 3, tRefi = 32))
    test(new RefreshSequencer(cfg)) { dut =>
      dut.io.command.ready.poke(false.B)
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)
      dut.io.command.valid.expect(true.B)
      dut.io.command.bits.command.expect(DramCommandType.precharge)
      dut.clock.step(3)
      dut.io.command.valid.expect(true.B)
      dut.io.command.bits.command.expect(DramCommandType.precharge)

      dut.io.command.ready.poke(true.B)
      val commands = ArrayBuffer.empty[BigInt]
      var done = false
      for (_ <- 0 until 24 if !done) {
        if (dut.io.command.valid.peek().litToBoolean) {
          commands += dut.io.command.bits.command.peek().litValue
          dut.io.command.bits.allBanks.expect(true.B)
        }
        dut.clock.step()
        done = dut.io.done.peek().litToBoolean
      }
      assert(done)
      assert(commands == Seq(
        DramCommandType.precharge.litValue, DramCommandType.refresh.litValue,
        DramCommandType.precharge.litValue, DramCommandType.refresh.litValue))
    }
  }

  behavior of "Refresher"

  it should "wait for all bank machines to grant the pending refresh" in {
    val cfg = DramConfig(addressBits = 10, dataBits = 32, bankBits = 1, rowBits = 3,
      columnBits = 3, refreshPostponing = 1,
      timing = DramTiming(tRp = 1, tRfc = 1, tRefi = 4))
    test(new Refresher(cfg)) { dut =>
      dut.io.command.ready.poke(true.B)
      dut.io.grant.poke(false.B)
      while (!dut.io.request.peek().litToBoolean) dut.clock.step()
      dut.clock.step(3)
      dut.io.busy.expect(false.B)
      dut.io.grant.poke(true.B)
      var sawPrecharge = false
      var sawRefresh = false
      for (_ <- 0 until 8) {
        if (dut.io.command.valid.peek().litToBoolean) {
          val command = dut.io.command.bits.command.peek().litValue
          sawPrecharge ||= command == DramCommandType.precharge.litValue
          sawRefresh ||= command == DramCommandType.refresh.litValue
        }
        dut.clock.step()
      }
      assert(sawPrecharge && sawRefresh)
    }
  }
}
