package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class StandardDdrPhySpec extends AnyFlatSpec with ChiselScalatestTester {
  private def config(memType: String): DramConfig = DramConfig(addressBits = 40,
    dataBits = 64, bankBits = (if (memType == "DDR4") 4 else 3),
    rowBits = (if (memType == "DDR4") 17 else 15), columnBits = 10,
    memType = memType, nPhases = 4, padDataBits = 8)

  private def clear(dut: StandardDdrPhy): Unit = {
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
      phase.resetN.poke(true.B)
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

  private def expanded(phases: Int): BigInt = {
    (0 until 8).foldLeft(BigInt(0)) { (word, edge) =>
      word | (BigInt((phases >> (edge / 2)) & 1) << edge)
    }
  }

  behavior of "StandardDdrPhy commands and writes"

  it should "expand DDR3 commands and transpose both DQ edges of every phase" in {
    test(new StandardDdrPhy(config("DDR3"), readLatency = 3, writeLatency = 2)) { dut =>
      clear(dut)
      val edgeBytes = Seq(0x96, 0x3c, 0xa5, 0x5a, 0x81, 0x7e, 0x33, 0xcc)
      val maskEdges = Seq(0, 1, 1, 0, 1, 0, 0, 1)
      for (phase <- 0 until 4) {
        dut.io.dfi.phases(phase).address.poke((if ((phase & 1) == 0) 1 else 0).U)
        dut.io.dfi.phases(phase).bank.poke(phase.U)
        dut.io.dfi.phases(phase).csN(0).poke(((phase & 1) != 0).B)
        dut.io.dfi.phases(phase).cke(0).poke(((phase & 2) != 0).B)
        dut.io.dfi.phases(phase).wrdata.poke(
          (edgeBytes(2 * phase) | (edgeBytes(2 * phase + 1) << 8)).U)
        dut.io.dfi.phases(phase).wrdataMask.poke(
          (maskEdges(2 * phase) | (maskEdges(2 * phase + 1) << 1)).U)
      }
      dut.io.output.clock.expect("haa".U)
      dut.io.output.address(0).expect(expanded(0x5).U)
      dut.io.output.chipSelectN(0).expect(expanded(0xa).U)
      dut.io.output.clockEnable(0).expect(expanded(0xc).U)
      dut.clock.step()
      for (bit <- 0 until 8) {
        val expected = edgeBytes.indices.foldLeft(BigInt(0)) { (word, edge) =>
          word | (BigInt((edgeBytes(edge) >> bit) & 1) << edge)
        }
        dut.io.output.dq(bit).expect(expected.U)
      }
      val expectedMask = maskEdges.indices.foldLeft(BigInt(0)) { (word, edge) =>
        word | (BigInt(maskEdges(edge)) << edge)
      }
      dut.io.output.dataMask(0).expect(expectedMask.U)
    }
  }

  it should "translate DDR4 ACTIVATE controls and invert DM polarity" in {
    test(new StandardDdrPhy(config("DDR4"), readLatency = 3, writeLatency = 2)) { dut =>
      clear(dut)
      val phase0 = dut.io.dfi.phases(0)
      phase0.rasN.poke(false.B)
      phase0.casN.poke(true.B)
      phase0.weN.poke(true.B)
      phase0.address.poke(((1 << 16) | (0 << 15) | (1 << 14)).U)
      phase0.wrdataMask.poke(1.U)
      dut.io.output.activateN.expect("hfc".U)
      dut.io.output.rowStrobeN.expect("hff".U)
      dut.io.output.columnStrobeN.expect("hfc".U)
      dut.io.output.writeEnableN.expect("hff".U)
      dut.clock.step()
      // Edge 0 is masked before DDR4's active-low DM conversion.
      dut.io.output.dataMask(0).expect("hfe".U)
    }
  }

  behavior of "StandardDdrPhy DQS and reads"

  it should "generate the pre/data/post tristate window and leveling pattern" in {
    test(new StandardDdrPhy(config("DDR3"), readLatency = 3, writeLatency = 2)) { dut =>
      clear(dut)
      dut.io.dfi.phases(0).wrdataEn.poke(true.B)
      dut.clock.step()
      dut.io.writePreamble.expect(true.B)
      dut.io.dfi.phases(0).wrdataEn.poke(false.B)
      dut.clock.step()
      dut.io.writePreamble.expect(false.B)
      dut.clock.step()
      dut.io.writePostamble.expect(true.B)
      dut.io.output.dqOutputEnable.expect(true.B)
      dut.io.output.dqsOutputEnable.expect(true.B)
      dut.io.output.dqs(0).expect("h55".U)

      dut.io.writeLevelingEnable.poke(true.B)
      dut.io.writeLevelingStrobe.poke(true.B)
      dut.clock.step()
      dut.io.output.dqs(0).expect(1.U)
      dut.clock.step(2)
      dut.io.output.dqsOutputEnable.expect(true.B)
    }
  }

  it should "transpose deserialized DQ and delay read-valid for all phases" in {
    test(new StandardDdrPhy(config("DDR2"), readLatency = 3, writeLatency = 2)) { dut =>
      clear(dut)
      val laneWords = (0 until 8).map(bit => (0x91 + 0x17 * bit) & 0xff)
      for (bit <- 0 until 8) dut.io.dqIn(bit).poke(laneWords(bit).U)
      dut.io.dfi.phases(2).rddataEn.poke(true.B)
      dut.clock.step()
      dut.io.dfi.phases(2).rddataEn.poke(false.B)
      for (phase <- 0 until 4) {
        val rising = (0 until 8).foldLeft(0) { (word, bit) =>
          word | (((laneWords(bit) >> (2 * phase)) & 1) << bit)
        }
        val falling = (0 until 8).foldLeft(0) { (word, bit) =>
          word | (((laneWords(bit) >> (2 * phase + 1)) & 1) << bit)
        }
        dut.io.read(phase).data.expect((rising | (falling << 8)).U)
        dut.io.read(phase).valid.expect(false.B)
      }
      dut.clock.step()
      dut.io.read.foreach(_.valid.expect(false.B))
      dut.clock.step()
      dut.io.read.foreach(_.valid.expect(true.B))
      dut.clock.step()
      dut.io.read.foreach(_.valid.expect(false.B))
    }
  }
}
