package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class DfiCommandPipelinesSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val lp4Config = DramConfig(addressBits = 40, dataBits = 128, bankBits = 6,
    rowBits = 17, columnBits = 10, memType = "LPDDR4", nPhases = 8)
  private val lp5Config = DramConfig(addressBits = 40, dataBits = 16, bankBits = 7,
    rowBits = 18, columnBits = 10, memType = "LPDDR5")

  private def idle(phase: DfiPhase): Unit = {
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

  private def command(phase: DfiPhase, address: Int, bank: Int,
      rasN: Boolean, casN: Boolean, weN: Boolean): Unit = {
    idle(phase)
    phase.address.poke(address.U)
    phase.bank.poke(bank.U)
    phase.csN(0).poke(false.B)
    phase.rasN.poke(rasN.B)
    phase.casN.poke(casN.B)
    phase.weN.poke(weN.B)
  }

  private def expectCaLines(ca: Vec[UInt], edges: Seq[Int], offset: Int = 0): Unit = {
    for (line <- 0 until 6) {
      val value = edges.zipWithIndex.map { case (edge, slot) =>
        ((edge >> line) & 1) << (offset + slot)
      }.reduce(_ | _)
      ca(line).expect(value.U, s"CA$line")
    }
  }

  behavior of "Lpddr4CommandPipeline"

  it should "delay and place a four-slot command in its originating phases" in {
    test(new Lpddr4CommandPipeline(lp4Config)) { dut =>
      dut.io.dfi.phases.foreach(idle)
      command(dut.io.dfi.phases(0), 0x15555, 0x2d,
        rasN = false, casN = true, weN = true)
      dut.io.accepted.expect(1.U)
      dut.io.cs.expect(0.U)
      dut.clock.step()

      dut.io.dfi.phases.foreach(idle)
      dut.io.cs.expect(5.U)
      expectCaLines(dut.io.ca, Seq(21, 29, 23, 21))
      dut.clock.step()
      dut.io.cs.expect(0.U)
    }
  }

  it should "carry late command slots into the next controller cycle" in {
    test(new Lpddr4CommandPipeline(lp4Config)) { dut =>
      dut.io.dfi.phases.foreach(idle)
      command(dut.io.dfi.phases(6), 0x15555, 0x2d,
        rasN = false, casN = true, weN = true)
      dut.io.accepted.expect((1 << 6).U)
      dut.clock.step()

      dut.io.dfi.phases.foreach(idle)
      dut.io.cs.expect((1 << 6).U)
      expectCaLines(dut.io.ca, Seq(21, 29), offset = 6)
      dut.clock.step()
      dut.io.cs.expect(1.U)
      expectCaLines(dut.io.ca, Seq(23, 21))
    }
  }

  it should "suppress overlapping commands using raw or accepted history" in {
    test(new Lpddr4CommandPipeline(lp4Config)) { dut =>
      dut.io.dfi.phases.foreach(idle)
      command(dut.io.dfi.phases(0), 0x15555, 0x2d, false, true, true)
      command(dut.io.dfi.phases(1), 0x6d4, 0x2d, true, false, true)
      command(dut.io.dfi.phases(4), 0x6d4, 0x2d, true, false, true)
      dut.io.accepted.expect(1.U)
    }
    test(new Lpddr4CommandPipeline(lp4Config, extendedOverlapCheck = true)) { dut =>
      dut.io.dfi.phases.foreach(idle)
      command(dut.io.dfi.phases(0), 0x15555, 0x2d, false, true, true)
      command(dut.io.dfi.phases(1), 0x6d4, 0x2d, true, false, true)
      command(dut.io.dfi.phases(4), 0x6d4, 0x2d, true, false, true)
      dut.io.accepted.expect(0x11.U)
    }
  }

  behavior of "Lpddr5CommandPipeline"

  it should "emit the two ACT commands on consecutive cycles" in {
    test(new Lpddr5CommandPipeline(lp5Config)) { dut =>
      command(dut.io.phase, 0x15555, 0x2d, rasN = false, casN = true, weN = true)
      dut.io.wckSyncDone.poke(false.B)
      dut.io.accepted.expect(true.B)
      dut.io.busy.expect(false.B)
      dut.io.cs.expect(true.B)
      dut.io.ca(0).expect(47.U)
      dut.io.ca(1).expect(45.U)
      dut.clock.step()

      idle(dut.io.phase)
      dut.io.busy.expect(true.B)
      dut.io.cs.expect(true.B)
      dut.io.ca(0).expect(83.U)
      dut.io.ca(1).expect(85.U)
      dut.clock.step()
      dut.io.busy.expect(false.B)
      dut.io.valid.expect(false.B)
    }
  }

  it should "delay one-part commands and report commands colliding with the buffer" in {
    test(new Lpddr5CommandPipeline(lp5Config)) { dut =>
      command(dut.io.phase, 0x400, 0x2d, rasN = false, casN = true, weN = false)
      dut.io.wckSyncDone.poke(false.B)
      dut.io.valid.expect(true.B)
      dut.io.cs.expect(false.B)
      dut.io.ca.foreach(_.expect(0.U))
      dut.clock.step()

      command(dut.io.phase, 0x6d4, 0x2d, rasN = true, casN = false, weN = true)
      dut.io.busy.expect(true.B)
      dut.io.dropped.expect(true.B)
      dut.io.accepted.expect(false.B)
      dut.io.cs.expect(true.B)
      dut.io.ca(0).expect(120.U)
      dut.io.ca(1).expect(77.U)
    }
  }
}
