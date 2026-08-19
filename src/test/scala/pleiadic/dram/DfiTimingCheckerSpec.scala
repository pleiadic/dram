package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class DfiTimingCheckerSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val timing = DramTiming(tRcd = 2, tRp = 2, tRas = 4, tRc = 6,
    tCcd = 1, tWr = 2, tWtr = 2, tRtp = 2, tRrd = 2, tFaw = 8,
    tRefi = 4, tRfc = 4)
  private val cfg = DramConfig(addressBits = 16, dataBits = 32, bankBits = 1,
    rowBits = 5, columnBits = 4, nPhases = 2, timing = timing)

  private def nop(dut: DfiTimingChecker): Unit = {
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
  }

  private def command(dut: DfiTimingChecker, bank: Int,
      rasN: Boolean, casN: Boolean, weN: Boolean, phaseIndex: Int = 0): Unit = {
    val phase = dut.io.dfi.phases(phaseIndex)
    phase.bank.poke(bank.U)
    phase.csN(0).poke(false.B)
    phase.rasN.poke(rasN.B)
    phase.casN.poke(casN.B)
    phase.weN.poke(weN.B)
  }

  behavior of "DfiTimingChecker"

  it should "identify bank timing violations and retain their count and rule" in {
    test(new DfiTimingChecker(cfg)) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.clear.poke(false.B)
      nop(dut)

      // ACT bank 0.
      command(dut, bank = 0, rasN = false, casN = true, weN = true)
      dut.io.violation.expect(false.B)
      dut.clock.step()

      // RD only one controller cycle later violates tRCD=2.
      nop(dut)
      command(dut, bank = 0, rasN = true, casN = false, weN = true)
      dut.io.violation.expect(true.B)
      dut.io.rule.expect(DfiTimingRule.tRcd)
      dut.clock.step()
      dut.io.violations.expect(1.U)

      // After enough time, the same command is legal.
      nop(dut)
      dut.clock.step()
      nop(dut)
      command(dut, bank = 0, rasN = true, casN = false, weN = true)
      dut.io.violation.expect(false.B)
      dut.clock.step()

      // ACT on another bank too soon after the first global ACT violates tRRD.
      nop(dut)
      command(dut, bank = 1, rasN = false, casN = true, weN = true)
      dut.clock.step()
      nop(dut)
      command(dut, bank = 0, rasN = false, casN = true, weN = true)
      dut.io.violation.expect(true.B)
      dut.io.rule.expect(DfiTimingRule.tRrd)
      dut.clock.step()
      dut.io.violations.expect(2.U)

      dut.io.clear.poke(true.B)
      nop(dut)
      dut.clock.step()
      dut.io.violations.expect(0.U)

      nop(dut)
      command(dut, bank = 0, rasN = false, casN = true, weN = true, phaseIndex = 0)
      command(dut, bank = 1, rasN = false, casN = true, weN = true, phaseIndex = 1)
      dut.io.violation.expect(true.B)
      dut.io.rule.expect(DfiTimingRule.tRrd)
    }
  }

  it should "flag refresh intervals longer than tREFI" in {
    test(new DfiTimingChecker(cfg)) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.clear.poke(false.B)
      nop(dut)
      command(dut, bank = 0, rasN = false, casN = false, weN = true)
      dut.clock.step()
      nop(dut)
      dut.clock.step(timing.tRefi - 1)
      dut.io.refreshLate.expect(false.B)
      dut.clock.step()
      dut.io.refreshLate.expect(true.B)
    }
  }

  it should "retain every phase ACT in the four-activate window" in {
    test(new DfiTimingChecker(cfg)) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.clear.poke(false.B)
      nop(dut)

      // Two ACTs in each of two cycles fill all four history entries. The
      // following ACT is still inside tFAW and must report the tFAW rule.
      for (_ <- 0 until 2) {
        command(dut, bank = 0, rasN = false, casN = true, weN = true, phaseIndex = 0)
        command(dut, bank = 1, rasN = false, casN = true, weN = true, phaseIndex = 1)
        dut.clock.step()
        nop(dut)
      }
      command(dut, bank = 0, rasN = false, casN = true, weN = true, phaseIndex = 0)
      dut.io.violation.expect(true.B)
      dut.io.rule.expect(DfiTimingRule.tFaw)
    }
  }
}
