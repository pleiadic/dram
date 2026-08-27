package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class Ecp5DdrPhySpec extends AnyFlatSpec with ChiselScalatestTester {
  private val config = DramConfig(addressBits = 40, dataBits = 64,
    bankBits = 3, rowBits = 15, columnBits = 10, memType = "DDR3",
    nPhases = 2, phyDataBits = 64, padDataBits = 8)

  private def clear(dut: Ecp5DdrPhy): Unit = {
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
    dut.io.dqHalfIn.foreach(_.poke(0.U))
    dut.io.delaySelect.poke(0.U)
    dut.io.readBitslipReset.poke(false.B)
    dut.io.readBitslip.poke(false.B)
  }

  behavior of "ECP5 half-rate DDR3 PHY"

  it should "expand commands and emit both halves of a BL8 write" in {
    test(new Ecp5DdrPhy(config, readLatency = 4, readCommandTap = 1,
      writeLatency = 2)) { dut =>
      clear(dut)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      dut.io.dfi.phases(0).address.poke(1.U)
      dut.io.dfi.phases(1).address.poke(0.U)
      dut.io.dfi.phases(0).csN(0).poke(false.B)
      dut.io.dfi.phases(1).csN(0).poke(true.B)
      dut.io.output.clock.expect("ha".U)
      dut.io.output.address(0).expect(3.U)
      dut.io.output.chipSelectN(0).expect(12.U)

      val edgeBytes = Seq(0x96, 0x3c, 0xa5, 0x5a,
        0x81, 0x7e, 0x33, 0xcc)
      for (phase <- 0 until 2) {
        val packed = (0 until 4).foldLeft(BigInt(0)) { (word, edge) =>
          word | (BigInt(edgeBytes(4 * phase + edge)) << (8 * edge))
        }
        dut.io.dfi.phases(phase).wrdata.poke(packed.U)
      }
      dut.io.dfi.phases(0).wrdataMask.poke("ha".U)
      dut.io.dfi.phases(1).wrdataMask.poke("h5".U)
      dut.clock.step()

      for (bit <- 0 until 8) {
        val expectedLow = (0 until 4).foldLeft(0) { (word, edge) =>
          word | (((edgeBytes(edge) >> bit) & 1) << edge)
        }
        dut.io.output.dq(bit).expect(expectedLow.U)
      }
      dut.io.output.dataMask(0).expect("ha".U)

      dut.io.dfi.phases(0).wrdataEn.poke(true.B)
      dut.clock.step()
      dut.io.dfi.phases(0).wrdataEn.poke(false.B)
      dut.clock.step(2)
      dut.io.bl8Chunk.expect(true.B)
      dut.clock.step()
      for (bit <- 0 until 8) {
        val expectedHigh = (4 until 8).foldLeft(0) { (word, edge) =>
          word | (((edgeBytes(edge) >> bit) & 1) << (edge - 4))
        }
        dut.io.output.dq(bit).expect(expectedHigh.U)
      }
      dut.io.output.dataMask(0).expect(5.U)
      dut.io.output.dqOutputEnable.expect(true.B)
      dut.io.output.dqs(0).expect("ha".U)
    }
  }

  it should "join consecutive IDDRX2 halves and align read control" in {
    test(new Ecp5DdrPhy(config, readLatency = 4, readCommandTap = 1,
      writeLatency = 2)) { dut =>
      clear(dut)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      dut.io.dqHalfIn(0).poke(6.U)
      dut.clock.step()
      dut.io.dqHalfIn(0).poke(9.U)
      dut.clock.step()
      dut.io.read(0).data.expect(((1 << 8) | (1 << 16)).U)
      dut.io.read(1).data.expect(((1 << 0) | (1 << 24)).U)

      dut.io.dfi.phases(1).rddataEn.poke(true.B)
      dut.clock.step()
      dut.io.dfi.phases(1).rddataEn.poke(false.B)
      dut.io.output.dqsReadEnable.expect(false.B)
      dut.clock.step()
      dut.io.output.dqsReadEnable.expect(true.B)
      dut.clock.step()
      dut.io.output.dqsReadEnable.expect(true.B)
      dut.clock.step()
      dut.io.read.foreach(_.valid.expect(true.B))
      dut.clock.step()
      dut.io.read.foreach(_.valid.expect(false.B))
    }
  }
}
