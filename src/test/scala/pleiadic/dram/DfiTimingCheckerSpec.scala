package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls
import scala.util.Random

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

  it should "enforce tZQCS before the next activate" in {
    val zqCfg = cfg.copy(timing = timing.copy(tZqcs = Some(3)))
    test(new DfiTimingChecker(zqCfg)) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.clear.poke(false.B)
      nop(dut)

      command(dut, bank = 0, rasN = true, casN = true, weN = false)
      dut.io.violation.expect(false.B)
      dut.clock.step()
      nop(dut)
      dut.clock.step()
      command(dut, bank = 0, rasN = false, casN = true, weN = true)
      dut.io.violation.expect(true.B)
      dut.io.rule.expect(DfiTimingRule.tZqcs)

      dut.clock.step()
      nop(dut)
      dut.clock.step(2)
      command(dut, bank = 1, rasN = false, casN = true, weN = true)
      dut.io.violation.expect(false.B)
    }
  }

  it should "match an independent phase-granular oracle over a long random stream" in {
    val randomTiming = timing.copy(tZqcs = Some(3), tRefi = 1000)
    val randomCfg = cfg.copy(nPhases = 4, timing = randomTiming)
    test(new DfiTimingChecker(randomCfg)) { dut =>
      val pre = 0
      val refresh = 1
      val activate = 2
      val read = 3
      val write = 4
      val zq = 5
      case class Rule(previous: Int, current: Int, delay: Int)
      val rules = Seq(
        Rule(pre, activate, randomTiming.tRp),
        Rule(pre, refresh, randomTiming.tRp),
        Rule(activate, write, randomTiming.tRcd),
        Rule(activate, read, randomTiming.tRcd),
        Rule(activate, pre, randomTiming.tRas),
        Rule(refresh, pre, randomTiming.tRfc),
        Rule(refresh, activate, randomTiming.tRfc),
        Rule(write, read, randomTiming.tCcd),
        Rule(write, write, randomTiming.tCcd),
        Rule(read, read, randomTiming.tCcd),
        Rule(read, write, randomTiming.tCcd),
        Rule(activate, activate, randomTiming.tRc),
        Rule(write, pre, randomTiming.tWr),
        Rule(write, read, randomTiming.tWtr),
        Rule(zq, activate, randomTiming.tZqcs.get))
      val timestamps = Array.fill(randomCfg.rankBankCount, 6)(Option.empty[Long])
      var actHistory = Vector.empty[Long]
      var lastAct = Option.empty[Long]
      var time = 0L
      var expectedViolations = 0L
      val random = new Random(0x54494d45)
      val coverage = new FunctionalCoverageBins("dfi-timing-checker", Seq(
        "precharge", "refresh", "activate", "read", "write", "zqcs",
        "all_bank_command", "bank_0", "bank_1", "phase_0", "phase_1",
        "phase_2", "phase_3", "simultaneous_commands", "checking_enabled",
        "checking_disabled", "legal_cycle", "violation_cycle", "clear_count"))

      dut.io.enable.poke(true.B)
      dut.io.clear.poke(false.B)
      nop(dut)

      for (cycle <- 0 until 700) {
        nop(dut)
        val enabled = random.nextInt(11) != 0
        val clear = cycle > 0 && cycle % 173 == 0
        dut.io.enable.poke(enabled.B)
        dut.io.clear.poke(clear.B)
        coverage.hitWhen("checking_enabled", enabled)
        coverage.hitWhen("checking_disabled", !enabled)
        coverage.hitWhen("clear_count", clear)
        val issued = Array.fill[Option[(Int, Int, Boolean)]](randomCfg.nPhases)(None)

        for (phaseIndex <- 0 until randomCfg.nPhases if random.nextInt(100) < 42) {
          val kind = random.nextInt(6)
          val bank = random.nextInt(randomCfg.rankBankCount)
          val allBanks = kind == refresh || (kind == pre && random.nextInt(5) == 0)
          val (rasN, casN, weN) = kind match {
            case `pre` => (false, true, false)
            case `refresh` => (false, false, true)
            case `activate` => (false, true, true)
            case `read` => (true, false, true)
            case `write` => (true, false, false)
            case `zq` => (true, true, false)
          }
          command(dut, bank, rasN, casN, weN, phaseIndex)
          if (allBanks && kind == pre)
            dut.io.dfi.phases(phaseIndex).address.poke((BigInt(1) << 10).U)
          issued(phaseIndex) = Some((kind, bank, allBanks))
          val commandBin = kind match {
            case `pre` => "precharge"
            case `refresh` => "refresh"
            case `activate` => "activate"
            case `read` => "read"
            case `write` => "write"
            case `zq` => "zqcs"
          }
          coverage.hit(commandBin)
          coverage.hit(s"bank_$bank")
          coverage.hit(s"phase_$phaseIndex")
          coverage.hitWhen("all_bank_command", allBanks)
        }
        coverage.hitWhen("simultaneous_commands", issued.count(_.nonEmpty) > 1)

        var expectedViolation = false
        for (phaseIndex <- 0 until randomCfg.nPhases) {
          issued(phaseIndex).foreach { case (kind, bank, allBanks) =>
            val now = time + phaseIndex
            val targets = if (allBanks) 0 until randomCfg.rankBankCount else Seq(bank)
            for (target <- targets) {
              for (rule <- rules if rule.current == kind) {
                timestamps(target)(rule.previous).foreach { previous =>
                  if (enabled && now - previous < rule.delay * randomCfg.nPhases)
                    expectedViolation = true
                }
              }
              timestamps(target)(kind) = Some(now)
            }
            if (kind == activate) {
              if (enabled && lastAct.exists(now - _ < randomTiming.tRrd * randomCfg.nPhases))
                expectedViolation = true
              if (enabled && actHistory.size >= 4 &&
                  now - actHistory(3) < randomTiming.tFaw * randomCfg.nPhases)
                expectedViolation = true
              lastAct = Some(now)
              actHistory = (now +: actHistory).take(4)
            }
          }
        }

        dut.io.violation.expect(expectedViolation.B)
        coverage.hitWhen("violation_cycle", expectedViolation)
        coverage.hitWhen("legal_cycle", !expectedViolation)
        dut.clock.step()
        expectedViolations = if (clear) 0L
          else if (expectedViolation) expectedViolations + 1L else expectedViolations
        dut.io.violations.expect(expectedViolations.U)
        time += randomCfg.nPhases
      }
      coverage.requireComplete()
    }
  }
}
