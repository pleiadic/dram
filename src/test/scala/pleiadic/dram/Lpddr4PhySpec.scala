package pleiadic.dram

import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class Lpddr4PhySpec extends AnyFlatSpec with ChiselScalatestTester {
  private val config = DramConfig(addressBits = 40, dataBits = 256, bankBits = 6,
    rowBits = 17, columnBits = 10, memType = "LPDDR4", nPhases = 8,
    padDataBits = 16)

  private def initialize(dut: Lpddr4Phy): Unit = {
    for (phase <- dut.io.dfi.phases) {
      phase.address.poke(0.U)
      phase.bank.poke(0.U)
      phase.csN.foreach(_.poke(true.B))
      phase.rasN.poke(true.B)
      phase.casN.poke(true.B)
      phase.weN.poke(true.B)
      phase.actN.poke(true.B)
      phase.cke.foreach(_.poke(false.B))
      phase.odt.foreach(_.poke(false.B))
      phase.resetN.poke(false.B)
      phase.rddataEn.poke(false.B)
      phase.wrdataEn.poke(false.B)
      phase.wrdata.poke(0.U)
      phase.wrdataMask.poke(0.U)
      phase.rddata.poke(0.U)
      phase.rddataValid.poke(false.B)
    }
    dut.io.dqIn.foreach(_.poke(0.U))
    dut.io.delaySelect.poke(0.U)
    dut.io.phyReset.poke(false.B)
    dut.io.readBitslipReset.poke(false.B)
    dut.io.readBitslip.poke(false.B)
    dut.io.writeBitslipReset.poke(false.B)
    dut.io.writeBitslip.poke(false.B)
    dut.io.writeLevelingEnable.poke(false.B)
    dut.io.writeLevelingStrobe.poke(false.B)
  }

  private def lineValue(edges: Seq[Int], bit: Int): BigInt =
    edges.zipWithIndex.foldLeft(BigInt(0)) { case (value, (edge, index)) =>
      value | (BigInt((edge >> bit) & 1) << index)
    }

  private def pokeDqInput(dut: Lpddr4Phy, edges: Seq[Int]): Unit = {
    for (bit <- 0 until 16) dut.io.dqIn(bit).poke(lineValue(edges, bit).U)
  }

  private def expectDqOutput(dut: Lpddr4Phy, edges: Seq[Int]): Unit = {
    for (bit <- 0 until 16) {
      dut.io.output.dq(bit).expect(lineValue(edges, bit).U, s"DQ$bit")
    }
  }

  private def expectDmiOutput(dut: Lpddr4Phy, masks: Seq[Int]): Unit = {
    for (byte <- 0 until 2) {
      dut.io.output.dmi(byte).expect(lineValue(masks, byte).U, s"DMI$byte")
    }
  }

  behavior of "Lpddr4Phy"

  it should "transpose all DFI phases to pad edges and reconstruct read data" in {
    test(new Lpddr4Phy(config, readLatency = 2, writeLatency = 1)).withAnnotations(Seq(
      VerilatorBackendAnnotation, VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      initialize(dut)
      val writeEdges = (0 until 16).map(edge => 0x1200 + edge * 0x31)
      val masks = (0 until 16).map(edge => (edge ^ (edge >> 1)) & 3)
      for (phase <- 0 until 8) {
        val low = writeEdges(2 * phase)
        val high = writeEdges(2 * phase + 1)
        dut.io.dfi.phases(phase).wrdata.poke((BigInt(high) << 16 | low).U)
        val lowMask = masks(2 * phase)
        val highMask = masks(2 * phase + 1)
        dut.io.dfi.phases(phase).wrdataMask.poke((highMask << 2 | lowMask).U)
      }
      val readEdges = (0 until 16).map(edge => 0x8001 ^ (edge * 0x421))
      pokeDqInput(dut, readEdges)
      dut.clock.step()

      expectDqOutput(dut, writeEdges)
      expectDmiOutput(dut, masks)
      for (phase <- 0 until 8) {
        val expected = (BigInt(readEdges(2 * phase + 1)) << 16) | readEdges(2 * phase)
        dut.io.read(phase).data.expect(expected.U, s"read phase $phase")
      }
      dut.io.output.clock.expect("h5555".U)
    }
  }

  it should "align DQS preamble data and postamble with write output enable" in {
    test(new Lpddr4Phy(config, readLatency = 2, writeLatency = 1)) { dut =>
      initialize(dut)
      dut.io.dfi.phases(0).wrdataEn.poke(true.B)
      dut.clock.step()
      dut.io.output.dqOutputEnable.expect(false.B)
      dut.io.output.dqsOutputEnable.expect(true.B)
      dut.io.output.dqs.foreach(_.expect("h5055".U))

      dut.io.dfi.phases(0).wrdataEn.poke(false.B)
      dut.clock.step()
      dut.io.output.dqOutputEnable.expect(true.B)
      dut.io.output.dmiOutputEnable.expect(true.B)
      dut.io.output.dqsOutputEnable.expect(true.B)
      dut.io.output.dqs.foreach(_.expect("h5555".U))

      dut.clock.step()
      dut.io.output.dqOutputEnable.expect(false.B)
      dut.io.output.dqsOutputEnable.expect(true.B)
      dut.io.output.dqs.foreach(_.expect("h5554".U))
      dut.clock.step()
      dut.io.output.dqsOutputEnable.expect(false.B)
    }
  }

  it should "delay read-valid and give write leveling control priority" in {
    test(new Lpddr4Phy(config, readLatency = 2, writeLatency = 1)) { dut =>
      initialize(dut)
      dut.io.dfi.phases(3).rddataEn.poke(true.B)
      dut.clock.step()
      dut.io.read.foreach(_.valid.expect(false.B))
      dut.io.dfi.phases(3).rddataEn.poke(false.B)
      dut.clock.step()
      dut.io.read.foreach(_.valid.expect(true.B))

      dut.io.writeLevelingEnable.poke(true.B)
      dut.io.writeLevelingStrobe.poke(true.B)
      dut.clock.step()
      dut.io.read.foreach(_.valid.expect(true.B))
      dut.io.output.dqsOutputEnable.expect(true.B)
      dut.io.output.dqs.foreach(_.expect(5.U))
    }
  }

  it should "delay simple controls and integrate the command pipeline" in {
    test(new Lpddr4Phy(config, readLatency = 2, writeLatency = 1)) { dut =>
      initialize(dut)
      for (phase <- 0 until 8) {
        dut.io.dfi.phases(phase).cke(0).poke((phase % 2 == 0).B)
        dut.io.dfi.phases(phase).odt(0).poke((phase >= 4).B)
        dut.io.dfi.phases(phase).resetN.poke((phase != 0).B)
      }
      val command = dut.io.dfi.phases(0)
      command.csN(0).poke(false.B)
      command.address.poke("h15555".U)
      command.bank.poke("h2d".U)
      command.rasN.poke(false.B)
      command.casN.poke(true.B)
      command.weN.poke(true.B)
      dut.io.commandAccepted.expect(1.U)
      dut.clock.step()

      dut.io.output.clockEnable.expect("h55".U)
      dut.io.output.onDieTermination.expect("hf0".U)
      dut.io.output.resetN.expect("hfe".U)
      dut.io.output.cs.expect(5.U)
    }
  }

  it should "apply write bitslip only to selected byte lanes" in {
    test(new Lpddr4Phy(config, readLatency = 1, writeLatency = 1)) { dut =>
      initialize(dut)
      val edges = (0 until 16).map(edge => 0x3107 ^ (edge * 0x119))
      for (phase <- 0 until 8) {
        val value = (BigInt(edges(2 * phase + 1)) << 16) | edges(2 * phase)
        dut.io.dfi.phases(phase).wrdata.poke(value.U)
      }
      dut.clock.step()
      expectDqOutput(dut, edges)

      dut.io.delaySelect.poke(1.U)
      dut.io.writeBitslip.poke(true.B)
      dut.clock.step()
      for (bit <- 0 until 16) {
        val original = lineValue(edges, bit)
        val expected = if (bit < 8) (original >> 1) | ((original & 1) << 15) else original
        dut.io.output.dq(bit).expect(expected.U, s"slipped DQ$bit")
      }

      dut.io.writeBitslip.poke(false.B)
      dut.io.writeBitslipReset.poke(true.B)
      dut.clock.step()
      expectDqOutput(dut, edges)
    }
  }

  it should "disable DMI when unmasked writes are selected" in {
    test(new Lpddr4Phy(config, readLatency = 1, writeLatency = 1,
      maskedWrite = false)) { dut =>
      initialize(dut)
      dut.io.dfi.phases.foreach(_.wrdataMask.poke("hf".U))
      dut.io.dfi.phases(0).wrdataEn.poke(true.B)
      dut.clock.step(2)
      dut.io.output.dmi.foreach(_.expect(0.U))
      dut.io.output.dmiOutputEnable.expect(false.B)
    }
  }
}
