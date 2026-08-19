package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class BandwidthSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "LiteDramBandwidth"

  it should "publish only completed windows when update is requested" in {
    test(new LiteDramBandwidth(dataWidth = 128, periodBits = 4)) { dut =>
      dut.io.commandAccepted.poke(false.B)
      dut.io.write.poke(false.B)
      dut.io.update.poke(false.B)
      dut.io.reportedDataWidth.expect(128.U)

      for (cycle <- 0 until 16) {
        val accepted = Set(1, 2, 7, 14).contains(cycle)
        dut.io.commandAccepted.poke(accepted.B)
        dut.io.write.poke((cycle == 2 || cycle == 14).B)
        dut.clock.step()
      }
      dut.io.reads.expect(0.U)
      dut.io.writes.expect(0.U)
      dut.io.commandAccepted.poke(false.B)
      dut.io.update.poke(true.B)
      dut.clock.step()
      dut.io.update.poke(false.B)
      dut.io.reads.expect(2.U)
      dut.io.writes.expect(2.U)
    }
  }

  it should "count a command on every period boundary in the next window" in {
    test(new LiteDramBandwidth(dataWidth = 32, periodBits = 3)) { dut =>
      dut.io.commandAccepted.poke(true.B)
      dut.io.write.poke(true.B)
      dut.io.update.poke(false.B)
      dut.clock.step(16)

      dut.io.commandAccepted.poke(false.B)
      dut.io.update.poke(true.B)
      dut.clock.step()
      dut.io.update.poke(false.B)
      dut.io.writes.expect(8.U)
      dut.io.reads.expect(0.U)
    }
  }
}
