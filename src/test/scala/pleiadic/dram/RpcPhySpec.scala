package pleiadic.dram

import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class RpcPhySpec extends AnyFlatSpec with ChiselScalatestTester {
  private val config = DramConfig(addressBits = 32, dataBits = 256,
    bankBits = 2, rowBits = 12, columnBits = 10, memType = "RPC",
    nPhases = 4, padDataBits = 16)

  private def newPhy(readLatency: Int = 5, writeLatency: Int = 1,
      readCaptureDelay: Int = 1, resetCycles: Int = 4,
      zqCalibrationCycles: Int = 2): RpcPhy =
    new RpcPhy(config, readLatency, writeLatency, readCaptureDelay,
      resetCycles, zqCalibrationCycles)

  private def initialize(dut: RpcPhy, bypass: Boolean = true): Unit = {
    for (phase <- dut.io.dfi.phases) {
      phase.address.poke(0.U)
      phase.bank.poke(0.U)
      phase.csN(0).poke(true.B)
      phase.rasN.poke(true.B)
      phase.casN.poke(true.B)
      phase.weN.poke(true.B)
      phase.actN.poke(true.B)
      phase.cke(0).poke(true.B)
      phase.odt(0).poke(false.B)
      phase.resetN.poke(true.B)
      phase.rddataEn.poke(false.B)
      phase.wrdataEn.poke(false.B)
      phase.wrdata.poke(0.U)
      phase.wrdataMask.poke(0.U)
      phase.rddata.poke(0.U)
      phase.rddataValid.poke(false.B)
    }
    dut.io.dbIn.foreach(_.poke(0.U))
    dut.io.dqsIn.poke(0.U)
    dut.io.delaySelect.poke(0.U)
    dut.io.phyReset.poke(false.B)
    dut.io.readBitslipReset.poke(false.B)
    dut.io.readBitslip.poke(false.B)
    dut.io.bypassInitialization.poke(bypass.B)
    dut.io.restartInitialization.poke(false.B)
    dut.io.clearChipSelectLock.poke(false.B)
  }

  private def deselect(phase: DfiPhase): Unit = {
    phase.csN(0).poke(true.B)
    phase.rasN.poke(true.B)
    phase.casN.poke(true.B)
    phase.weN.poke(true.B)
    phase.resetN.poke(true.B)
  }

  private def issueActivate(phase: DfiPhase, address: Int, bank: Int): Unit = {
    phase.address.poke(address.U)
    phase.bank.poke(bank.U)
    phase.csN(0).poke(false.B)
    phase.rasN.poke(false.B)
    phase.casN.poke(true.B)
    phase.weN.poke(true.B)
  }

  private def lineValue(edges: Seq[Int], bit: Int): BigInt =
    edges.zipWithIndex.foldLeft(BigInt(0)) { case (word, (edge, index)) =>
      word | (BigInt((edge >> bit) & 1) << index)
    }

  private def packedWords(words: Seq[Int]): BigInt =
    words.zipWithIndex.foldLeft(BigInt(0)) { case (result, (word, index)) =>
      result | (BigInt(word & 0xffff) << (16 * index))
    }

  private def pokeInputEdges(dut: RpcPhy, edges: Seq[Int]): Unit =
    for (bit <- 0 until 16) dut.io.dbIn(bit).poke(lineValue(edges, bit).U)

  private def expectOutputEdges(dut: RpcPhy, edges: Seq[Int]): Unit =
    for (bit <- 0 until 16) {
      dut.io.output.db(bit).expect(lineValue(edges, bit).U, s"DB$bit")
    }

  behavior of "RpcPhy"

  it should "insert the RPC preamble and serialize the golden Request Packet" in {
    test(newPhy()).withAnnotations(Seq(VerilatorBackendAnnotation,
      VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      initialize(dut)
      dut.io.output.clock.expect("h55".U)
      dut.io.output.strobe.expect("hff".U)

      val phase0 = dut.io.dfi.phases(0)
      issueActivate(phase0, address = 0xabc, bank = 1)
      dut.clock.step()
      deselect(phase0)
      dut.io.output.strobe.expect("h0f".U)
      dut.io.output.dqsOutputEnable.expect(true.B)
      dut.io.output.dqs.expect("h40".U)

      dut.clock.step()
      dut.io.commandOutputEnable.expect(true.B)
      dut.io.output.dbOutputEnable.expect(true.B)
      // ACT positive packet = 0x000d, negative packet = address << 1.
      val positive = 0x000d
      val negative = 0x1578
      for (bit <- 0 until 16) {
        val serialized = ((positive >> bit) & 1) | (((negative >> bit) & 1) << 1)
        dut.io.output.db(bit).expect(serialized.U, s"Request Packet DB$bit")
      }
      dut.io.output.chipSelectN.expect(0.U)
      dut.io.output.dqs.expect("h01".U)

      dut.clock.step()
      dut.io.commandOutputEnable.expect(false.B)
      dut.io.clearChipSelectLock.poke(true.B)
      dut.clock.step()
      dut.io.output.chipSelectN.expect("hff".U)
    }
  }

  it should "send a mask followed by the two halves of a BL16 write" in {
    test(newPhy()) { dut =>
      initialize(dut)
      val words = (0 until 16).map(index => 0x1201 ^ (index * 0x321))
      val masks = Seq(0x01, 0x80, 0x55, 0xaa)
      val phase3 = dut.io.dfi.phases(3)

      phase3.wrdataEn.poke(true.B)
      dut.clock.step()
      phase3.wrdataEn.poke(false.B)
      for (phase <- 0 until 4) {
        dut.io.dfi.phases(phase).wrdata.poke(packedWords(words.slice(4 * phase, 4 * phase + 4)).U)
        dut.io.dfi.phases(phase).wrdataMask.poke(masks(phase).U)
      }
      dut.io.maskOutputEnable.expect(true.B)
      dut.io.dataOutputEnable.expect(false.B)
      dut.io.output.dqs.expect("h54".U)
      for (bit <- 0 until 16) {
        val byte = bit / 8
        val lane = bit % 8
        val expected = 0xc0 |
          (((masks(byte) >> lane) & 1) << 4) |
          (((masks(byte + 2) >> lane) & 1) << 5)
        dut.io.output.db(bit).expect(expected.U, s"mask DB$bit")
      }

      dut.clock.step()
      dut.io.dataOutputEnable.expect(true.B)
      dut.io.output.dqs.expect("h55".U)
      expectOutputEdges(dut, words.take(8))
      for (phase <- 0 until 4) {
        dut.io.dfi.phases(phase).wrdata.poke(0.U)
        dut.io.dfi.phases(phase).wrdataMask.poke(0.U)
      }

      dut.clock.step()
      dut.io.dataOutputEnable.expect(true.B)
      expectOutputEdges(dut, words.drop(8))
      dut.clock.step()
      dut.io.dataOutputEnable.expect(false.B)
      dut.io.output.dbOutputEnable.expect(false.B)
    }
  }

  it should "append the LiteDRAM burst-stop STB sequence after phase-3 traffic" in {
    test(newPhy()) { dut =>
      initialize(dut)
      val phase3 = dut.io.dfi.phases(3)
      phase3.csN(0).poke(false.B)
      phase3.rasN.poke(true.B)
      phase3.casN.poke(false.B)
      phase3.weN.poke(true.B)
      dut.clock.step()
      deselect(phase3)
      dut.clock.step()
      dut.io.commandOutputEnable.expect(true.B)
      // Preamble occupies phase 1/2; burst-stop forces the phase-3 pair low.
      dut.io.output.strobe.expect("h03".U)
      dut.clock.step()
      // Continuation is the reference mapping 0,1,0,0,1,1,1,1 (LSB first).
      dut.io.output.strobe.expect("hf2".U)
    }
  }

  it should "aggregate two DB chunks into four phase-aligned read responses" in {
    test(newPhy()) { dut =>
      initialize(dut)
      val words = (0 until 16).map(index => 0x8041 ^ (index * 0x493))
      val phase3 = dut.io.dfi.phases(3)
      phase3.rddataEn.poke(true.B)
      dut.clock.step()
      phase3.rddataEn.poke(false.B)
      dut.io.readCapture.expect(true.B)

      pokeInputEdges(dut, words.take(8))
      dut.clock.step()
      dut.io.readCapture.expect(true.B)
      pokeInputEdges(dut, words.drop(8))
      dut.clock.step()
      dut.io.readCapture.expect(false.B)

      dut.clock.step(2)
      for (phase <- 0 until 4) {
        dut.io.read(phase).valid.expect(true.B)
        dut.io.read(phase).data.expect(
          packedWords(words.slice(phase * 4, phase * 4 + 4)).U, s"phase $phase")
      }
      dut.clock.step()
      dut.io.read.foreach(_.valid.expect(false.B))
    }
  }

  it should "enforce reset serial-reset ZQ and utility-mode command windows" in {
    test(newPhy()).withAnnotations(Seq(VerilatorBackendAnnotation,
      VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      initialize(dut, bypass = false)
      dut.io.initializationState.expect(RpcInitializationState.idle)
      dut.io.initializationDone.expect(false.B)
      dut.io.commandAllowed.expect(false.B)

      val phase0 = dut.io.dfi.phases(0)
      phase0.resetN.poke(false.B)
      issueActivate(phase0, address = 0, bank = 0)
      dut.io.commandAllowed.expect(true.B)
      dut.clock.step()
      deselect(phase0)
      dut.io.initializationState.expect(RpcInitializationState.resetReceived)
      dut.clock.step()
      dut.io.initializationState.expect(RpcInitializationState.serialReset)
      for (_ <- 0 until 4) {
        dut.io.output.strobe.expect(0.U)
        dut.clock.step()
      }
      dut.io.initializationState.expect(RpcInitializationState.resetDone)
      dut.io.resetDone.expect(true.B)

      issueActivate(phase0, address = 1, bank = 0)
      dut.io.commandAllowed.expect(false.B)
      deselect(phase0)
      phase0.address.poke((1 << 10).U)
      phase0.csN(0).poke(false.B)
      phase0.rasN.poke(true.B)
      phase0.casN.poke(true.B)
      phase0.weN.poke(false.B)
      phase0.resetN.poke(false.B)
      dut.io.commandAllowed.expect(true.B)
      dut.clock.step()
      deselect(phase0)
      dut.io.initializationState.expect(RpcInitializationState.zqCalibration)
      dut.clock.step(2)
      dut.io.initializationState.expect(RpcInitializationState.ready)
      dut.io.initializationDone.expect(true.B)

      // Special MRS with address bit 0 enters UTR mode.
      phase0.address.poke(1.U)
      phase0.csN(0).poke(false.B)
      phase0.rasN.poke(false.B)
      phase0.casN.poke(false.B)
      phase0.weN.poke(false.B)
      phase0.resetN.poke(false.B)
      dut.clock.step()
      deselect(phase0)
      dut.io.initializationState.expect(RpcInitializationState.utilityMode)

      // The reference PHY intentionally prolongs an accepted command window
      // across its two history stages; let that window drain first.
      dut.clock.step(2)

      issueActivate(phase0, address = 0, bank = 0)
      dut.io.commandAllowed.expect(false.B)
      deselect(phase0)
      phase0.address.poke(0.U)
      phase0.csN(0).poke(false.B)
      phase0.rasN.poke(false.B)
      phase0.casN.poke(false.B)
      phase0.weN.poke(false.B)
      phase0.resetN.poke(false.B)
      dut.clock.step()
      dut.io.initializationState.expect(RpcInitializationState.ready)
    }
  }
}
