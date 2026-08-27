package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class TimingSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "TxxdController"

  private def bit(c: Char): Boolean = c == '-'

  private def checkTxxd(distance: Int, valids: String, readys: String): Unit = {
    test(new TxxdController(distance)) { dut =>
      for ((valid, expectedReady) <- valids.zip(readys)) {
        dut.io.valid.poke(bit(valid).B)
        dut.clock.step()
        dut.io.ready.expect(bit(expectedReady).B)
      }
    }
  }

  it should "match the LiteDRAM reference pulse sequences" in {
    checkTxxd(1, "__-______", "_--------")
    checkTxxd(2, "__-______", "_-_------")
    checkTxxd(3, "____-______", "___-__-----")
    checkTxxd(4, "____-______", "___-___----")
  }

  behavior of "TfawController"

  private def checkTfaw(valids: String, readys: String): Unit = {
    test(new TfawController(8)) { dut =>
      for ((valid, expectedReady) <- valids.zip(readys)) {
        dut.io.valid.poke(bit(valid).B)
        dut.clock.step()
        dut.io.ready.expect(bit(expectedReady).B)
      }
    }
  }

  it should "limit four activates in a rolling window" in {
    // Migen's coroutine driver applies its input one simulator tick after the
    // assignment. ChiselTest pokes before the active edge, so the same RTL
    // response is visible one character earlier than test_timing.py.
    checkTfaw("_----___________", "----______------")
    checkTfaw("_-_-_-_-________", "-------___------")
    checkTfaw("_-_-___-_-______", "---------_------")
    checkTfaw("_-_-____-_-______", "-----------------")
  }

  it should "retain the reference count width for full power-of-two windows" in {
    test(new TfawController(4)) { dut =>
      dut.io.valid.poke(true.B)
      Seq(true, true, true, false, true).foreach { ready =>
        dut.clock.step()
        dut.io.ready.expect(ready.B)
      }
    }
    test(new TfawController(8)) { dut =>
      dut.io.valid.poke(true.B)
      Seq(true, true, true, false, false, false, false, false, true).foreach { ready =>
        dut.clock.step()
        dut.io.ready.expect(ready.B)
      }
    }
  }

  behavior of "common PHY helpers"

  it should "delay values through every tap" in {
    test(new TappedDelayLine(width = 4, tapCount = 3)) { dut =>
      dut.io.in.poke(5.U)
      dut.clock.step()
      dut.io.taps(0).expect(5.U)
      dut.io.taps(1).expect(0.U)
      dut.clock.step(2)
      dut.io.out.expect(5.U)
    }
  }

  it should "select the full-width reset tap and wrap programmable slips" in {
    test(new BitSlip(width = 4)) { dut =>
      dut.io.in.poke("b0011".U)
      dut.io.resetSlip.poke(false.B)
      dut.io.slip.poke(false.B)
      dut.clock.step()
      dut.io.out.expect("b0011".U)

      // Position 3 wraps to 0. With two identical history words, selecting
      // bits [4:1] produces 1001.
      dut.io.slip.poke(true.B)
      dut.clock.step()
      dut.io.out.expect("b1001".U)
      dut.io.slip.poke(false.B)
      dut.io.resetSlip.poke(true.B)
      dut.clock.step()
      dut.io.out.expect("b0011".U)
    }
  }

  it should "generate all DQS patterns with write leveling priority" in {
    test(new DqsPattern) { dut =>
      dut.io.preamble.poke(false.B)
      dut.io.postamble.poke(false.B)
      dut.io.writeLevelingEnable.poke(false.B)
      dut.io.writeLevelingStrobe.poke(false.B)
      dut.io.out.expect("b01010101".U)
      dut.io.preamble.poke(true.B)
      dut.io.out.expect("b00010101".U)
      dut.io.postamble.poke(true.B)
      dut.io.out.expect("b01010100".U)
      dut.io.writeLevelingEnable.poke(true.B)
      dut.io.writeLevelingStrobe.poke(true.B)
      dut.io.out.expect(1.U)
    }
  }
}
