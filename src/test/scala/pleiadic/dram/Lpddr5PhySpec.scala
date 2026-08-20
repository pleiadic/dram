package pleiadic.dram

import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class Lpddr5PhyWriteHarness extends Module {
  private val config = DramConfig(addressBits = 40, dataBits = 256, bankBits = 7,
    rowBits = 18, columnBits = 10, memType = "LPDDR5", padDataBits = 16)
  val io = IO(new Bundle {
    val writeWords = Input(Vec(4, UInt(64.W)))
    val writeMask = Input(UInt(32.W))
    val writeEnable = Input(Bool())
    val dq = Output(Vec(16, UInt(4.W)))
    val dmi = Output(Vec(2, UInt(4.W)))
    val dqOutputEnable = Output(Bool())
  })
  private val phy = Module(new Lpddr5Phy(config, wckCkRatio = 2,
    readLatency = 6, writeLatency = 1, tWckenlWrite = 0,
    tWckenlRead = 1, tWckpreStatic = 1))
  private val phase = phy.io.dfi.phases(0)
  phase.address := 0.U
  phase.bank := 0.U
  phase.csN.foreach(_ := true.B)
  phase.rasN := true.B
  phase.casN := true.B
  phase.weN := true.B
  phase.actN := true.B
  phase.cke.foreach(_ := true.B)
  phase.odt.foreach(_ := false.B)
  phase.resetN := true.B
  phase.rddataEn := false.B
  phase.wrdataEn := io.writeEnable
  phase.wrdata := io.writeWords.asUInt
  phase.wrdataMask := io.writeMask
  phase.rddata := 0.U
  phase.rddataValid := false.B
  phy.io.dqIn.foreach(_ := 0.U)
  phy.io.delaySelect := 0.U
  phy.io.phyReset := false.B
  phy.io.readBitslipReset := false.B
  phy.io.readBitslip := false.B
  phy.io.writeBitslipReset := false.B
  phy.io.writeBitslip := false.B
  phy.io.writeLevelingEnable := false.B
  phy.io.writeLevelingStrobe := false.B
  io.dq := phy.io.output.dq
  io.dmi := phy.io.output.dmi
  io.dqOutputEnable := phy.io.output.dqOutputEnable
}

class Lpddr5PhySpec extends AnyFlatSpec with ChiselScalatestTester {
  private val config = DramConfig(addressBits = 40, dataBits = 256, bankBits = 7,
    rowBits = 18, columnBits = 10, memType = "LPDDR5", padDataBits = 16)

  private def initialize(dut: Lpddr5Phy): Unit = {
    val phase = dut.io.dfi.phases(0)
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

  private def packedEdges(edges: Seq[Int]): BigInt =
    edges.zipWithIndex.foldLeft(BigInt(0)) { case (word, (edge, index)) =>
      word | (BigInt(edge) << (16 * index))
    }

  private def packedMasks(masks: Seq[Int]): BigInt =
    masks.zipWithIndex.foldLeft(BigInt(0)) { case (word, (mask, index)) =>
      word | (BigInt(mask) << (2 * index))
    }

  private def lineValue(edges: Seq[Int], bit: Int): BigInt =
    edges.zipWithIndex.foldLeft(BigInt(0)) { case (word, (edge, index)) =>
      word | (BigInt((edge >> bit) & 1) << index)
    }

  private def pokeInputChunk(dut: Lpddr5Phy, edges: Seq[Int]): Unit = {
    for (bit <- 0 until 16) dut.io.dqIn(bit).poke(lineValue(edges, bit).U)
  }

  private def expectOutputChunk(dut: Lpddr5Phy, edges: Seq[Int]): Unit = {
    for (bit <- 0 until 16) {
      dut.io.output.dq(bit).expect(lineValue(edges, bit).U, s"DQ$bit")
    }
  }

  private def newPhy(ratio: Int, readLatency: Int = 6, writeLatency: Int = 1,
      maskedWrite: Boolean = true): Lpddr5Phy =
    new Lpddr5Phy(config, ratio, readLatency, writeLatency,
      tWckenlWrite = 0, tWckenlRead = 1, tWckpreStatic = 1,
      maskedWrite = maskedWrite)

  behavior of "Lpddr5WckGenerator"

  it should "emit 2:1 static toggle and two-part postamble timing" in {
    test(new Lpddr5WckGenerator(wckCkRatio = 2, tWckenlWrite = 0,
      tWckenlRead = 1, tWckpreStatic = 1)) { dut =>
      dut.io.sync.poke(Lpddr5WckSync.write)
      dut.io.syncDone.poke(true.B)
      dut.io.levelingEnable.poke(false.B)
      dut.io.levelingStrobe.poke(false.B)
      dut.io.pattern.expect(0.U)
      dut.clock.step()
      dut.io.pattern.expect(0.U) // STATIC

      dut.io.sync.poke(Lpddr5WckSync.none)
      dut.clock.step()
      dut.io.pattern.expect("h5".U) // TOGGLE
      dut.clock.step()
      dut.io.pattern.expect("h5".U)

      dut.io.syncDone.poke(false.B)
      dut.clock.step()
      dut.io.pattern.expect("h5".U) // POSTAMBLE
      dut.clock.step()
      dut.io.pattern.expect(1.U) // POSTAMBLE_2:1
      dut.clock.step()
      dut.io.pattern.expect(0.U)
    }
  }

  it should "transition from half-rate to full-rate WCK at 4:1" in {
    test(new Lpddr5WckGenerator(wckCkRatio = 4, tWckenlWrite = 0,
      tWckenlRead = 0, tWckpreStatic = 1)) { dut =>
      dut.io.sync.poke(Lpddr5WckSync.write)
      dut.io.syncDone.poke(true.B)
      dut.io.levelingEnable.poke(false.B)
      dut.io.levelingStrobe.poke(false.B)
      dut.clock.step()
      dut.io.sync.poke(Lpddr5WckSync.none)
      dut.clock.step()
      dut.io.pattern.expect("h33".U)
      dut.clock.step()
      dut.io.pattern.expect("h55".U)

      dut.io.syncDone.poke(false.B)
      dut.clock.step()
      dut.io.pattern.expect("h15".U)
      dut.clock.step()
      dut.io.pattern.expect(0.U)
    }
  }

  it should "hold the leveling WCK pattern for four delayed cycles" in {
    test(new Lpddr5WckGenerator(wckCkRatio = 4, tWckenlWrite = 0,
      tWckenlRead = 0, tWckpreStatic = 1)) { dut =>
      dut.io.sync.poke(0.U)
      dut.io.syncDone.poke(false.B)
      dut.io.levelingEnable.poke(true.B)
      dut.io.levelingStrobe.poke(true.B)
      dut.clock.step()
      dut.io.levelingStrobe.poke(false.B)
      for (_ <- 0 until 4) {
        dut.io.pattern.expect("h33".U)
        dut.clock.step()
      }
      dut.io.pattern.expect(0.U)
    }
  }

  behavior of "Lpddr5Phy"

  it should "split a BL16 write into four 2:1 WCK chunks with masks" in {
    test(newPhy(ratio = 2)) { dut =>
      initialize(dut)
      val edges = (0 until 16).map(edge => 0x2103 ^ (edge * 0x319))
      val masks = (0 until 16).map(edge => (edge ^ (edge >> 1)) & 3)
      val phase = dut.io.dfi.phases(0)
      phase.wrdata.poke(packedEdges(edges).U)
      phase.wrdataMask.poke(packedMasks(masks).U)
      phase.wrdataEn.poke(true.B)
      dut.clock.step()
      phase.wrdataEn.poke(false.B)
      dut.io.output.dqOutputEnable.expect(false.B)

      for (chunk <- 0 until 4) {
        dut.clock.step()
        dut.io.output.dqOutputEnable.expect(true.B)
        dut.io.output.dmiOutputEnable.expect(true.B)
        expectOutputChunk(dut, edges.slice(4 * chunk, 4 * chunk + 4))
        for (byte <- 0 until 2) {
          dut.io.output.dmi(byte).expect(
            lineValue(masks.slice(4 * chunk, 4 * chunk + 4), byte).U, s"DMI$byte chunk $chunk")
        }
      }
      dut.clock.step()
      dut.io.output.dqOutputEnable.expect(false.B)
    }
  }

  it should "run the 256-bit write path through a narrow Verilator harness" in {
    test(new Lpddr5PhyWriteHarness).withAnnotations(Seq(VerilatorBackendAnnotation,
      VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      val edges = (0 until 16).map(edge => 0x5101 ^ (edge * 0x217))
      val masks = (0 until 16).map(edge => (edge + 1) & 3)
      for (word <- 0 until 4) {
        dut.io.writeWords(word).poke(packedEdges(edges.slice(4 * word, 4 * word + 4)).U)
      }
      dut.io.writeMask.poke(packedMasks(masks).U)
      dut.io.writeEnable.poke(true.B)
      dut.clock.step()
      dut.io.writeEnable.poke(false.B)
      for (chunk <- 0 until 4) {
        dut.clock.step()
        dut.io.dqOutputEnable.expect(true.B)
        val chunkEdges = edges.slice(4 * chunk, 4 * chunk + 4)
        for (bit <- 0 until 16) {
          dut.io.dq(bit).expect(lineValue(chunkEdges, bit).U, s"Verilator DQ$bit chunk $chunk")
        }
      }
    }
  }

  it should "aggregate four 2:1 WCK read chunks into one DFI response" in {
    test(newPhy(ratio = 2, readLatency = 6)) { dut =>
      initialize(dut)
      val edges = (0 until 16).map(edge => 0x8101 ^ (edge * 0x247))
      val phase = dut.io.dfi.phases(0)
      phase.rddataEn.poke(true.B)
      dut.clock.step()
      phase.rddataEn.poke(false.B)
      pokeInputChunk(dut, edges.slice(0, 4))
      dut.clock.step()
      dut.io.readCapture.expect(true.B)

      for (chunk <- 1 until 4) {
        pokeInputChunk(dut, edges.slice(4 * chunk, 4 * chunk + 4))
        dut.clock.step()
      }
      dut.io.read.valid.expect(false.B)
      dut.clock.step()
      dut.io.read.valid.expect(true.B)
      dut.io.read.data.expect(packedEdges(edges).U)
      dut.clock.step()
      dut.io.read.valid.expect(false.B)
    }
  }

  it should "support two BL16 chunks at WCK to CK 4:1" in {
    test(newPhy(ratio = 4, readLatency = 4)) { dut =>
      initialize(dut)
      val edges = (0 until 16).map(edge => 0x4109 ^ (edge * 0x151))
      val phase = dut.io.dfi.phases(0)
      phase.wrdata.poke(packedEdges(edges).U)
      phase.wrdataEn.poke(true.B)
      dut.clock.step()
      phase.wrdataEn.poke(false.B)
      for (chunk <- 0 until 2) {
        dut.clock.step()
        dut.io.output.dqOutputEnable.expect(true.B)
        expectOutputChunk(dut, edges.slice(8 * chunk, 8 * chunk + 8))
      }
      dut.clock.step()
      dut.io.output.dqOutputEnable.expect(false.B)
    }
  }

  it should "aggregate two read chunks at WCK to CK 4:1" in {
    test(newPhy(ratio = 4, readLatency = 4)) { dut =>
      initialize(dut)
      val edges = (0 until 16).map(edge => 0xa103 ^ (edge * 0x183))
      val phase = dut.io.dfi.phases(0)
      phase.rddataEn.poke(true.B)
      dut.clock.step()
      phase.rddataEn.poke(false.B)
      pokeInputChunk(dut, edges.slice(0, 8))
      dut.clock.step()
      dut.io.readCapture.expect(true.B)
      pokeInputChunk(dut, edges.slice(8, 16))
      dut.clock.step()
      dut.io.read.valid.expect(false.B)
      dut.clock.step()
      dut.io.read.valid.expect(true.B)
      dut.io.read.data.expect(packedEdges(edges).U)
    }
  }

  it should "integrate CA buffering and start WCK synchronization on a write" in {
    test(newPhy(ratio = 2)) { dut =>
      initialize(dut)
      val phase = dut.io.dfi.phases(0)
      phase.csN(0).poke(false.B)
      phase.address.poke("h6d4".U)
      phase.bank.poke("h2d".U)
      phase.casN.poke(false.B)
      phase.weN.poke(false.B)
      dut.io.commandAccepted.expect(true.B)
      dut.io.output.cs.expect(true.B)
      for (line <- 0 until 7) {
        dut.io.output.ca(line).expect(lineValue(Seq(28, 0), line).U, s"CAS CA$line")
      }
      dut.clock.step()

      phase.csN(0).poke(true.B)
      phase.casN.poke(true.B)
      phase.weN.poke(true.B)
      dut.io.wckSyncDone.expect(true.B)
      dut.io.output.cs.expect(true.B)
      for (line <- 0 until 7) {
        dut.io.output.ca(line).expect(lineValue(Seq(90, 109), line).U, s"MWR CA$line")
      }
      dut.clock.step(2)
      dut.io.output.wck.foreach(_.expect("h5".U))
    }
  }

  it should "disable DMI for unmasked WRITE16" in {
    test(newPhy(ratio = 2, maskedWrite = false)) { dut =>
      initialize(dut)
      val phase = dut.io.dfi.phases(0)
      phase.wrdataMask.poke(((BigInt(1) << 32) - 1).U)
      phase.wrdataEn.poke(true.B)
      dut.clock.step(2)
      dut.io.output.dmi.foreach(_.expect(0.U))
      dut.io.output.dmiOutputEnable.expect(false.B)
    }
  }
}
