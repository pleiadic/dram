package pleiadic.dram

import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class PhySerdesSpec extends AnyFlatSpec with ChiselScalatestTester {
  private def bit(word: BigInt, index: Int): Boolean = ((word >> index) & 1) != 0

  behavior of "PhySerializer"

  it should "serialize LSB first, hold enables, and accept a back-to-back word" in {
    test(new PhySerializer(edgeCount = 4, laneCount = 2)) { dut =>
      dut.io.load.poke(false.B)
      dut.io.parallel(0).poke("h9".U)
      dut.io.parallel(1).poke("h6".U)
      dut.io.parallelOutputEnable.poke(1.U)
      dut.clock.step()

      dut.io.load.poke(true.B)
      dut.io.loadReady.expect(true.B)
      dut.clock.step()
      dut.io.load.poke(false.B)
      val first = Seq(BigInt(0x9), BigInt(0x6))
      for (edge <- 0 until 4) {
        dut.io.edge.expect(edge.U)
        for (lane <- 0 until 2) dut.io.serial(lane).expect(bit(first(lane), edge).B)
        dut.io.serialOutputEnable.expect(1.U)
        if (edge == 1) {
          // Parallel values and enables are not allowed to disturb this word.
          dut.io.parallel(0).poke(0.U)
          dut.io.parallel(1).poke(0.U)
          dut.io.parallelOutputEnable.poke(0.U)
        }
        if (edge == 3) {
          dut.io.parallel(0).poke("h3".U)
          dut.io.parallel(1).poke("hc".U)
          dut.io.parallelOutputEnable.poke(2.U)
          dut.io.load.poke(true.B)
          dut.io.loadReady.expect(true.B)
        }
        dut.clock.step()
      }

      dut.io.load.poke(false.B)
      val second = Seq(BigInt(0x3), BigInt(0xc))
      for (edge <- 0 until 4) {
        dut.io.edge.expect(edge.U)
        for (lane <- 0 until 2) dut.io.serial(lane).expect(bit(second(lane), edge).B)
        dut.io.serialOutputEnable.expect(2.U)
        dut.clock.step()
      }
      dut.io.active.expect(false.B)
      dut.io.serialOutputEnable.expect(0.U)
    }
  }

  behavior of "PhyDeserializer"

  it should "reassemble aligned serial lanes and pulse valid on the last edge" in {
    test(new PhyDeserializer(edgeCount = 8, laneCount = 3)) { dut =>
      val words = Seq(BigInt(0xa5), BigInt(0x3c), BigInt(0x81))
      dut.io.start.poke(true.B)
      for (lane <- 0 until 3) dut.io.serial(lane).poke(bit(words(lane), 0).B)
      dut.clock.step()
      dut.io.start.poke(false.B)
      dut.io.parallelValid.expect(false.B)
      for (edge <- 1 until 8) {
        for (lane <- 0 until 3) dut.io.serial(lane).poke(bit(words(lane), edge).B)
        dut.clock.step()
      }
      dut.io.parallelValid.expect(true.B)
      for (lane <- 0 until 3) dut.io.parallel(lane).expect(words(lane).U)
      dut.clock.step()
      dut.io.parallelValid.expect(false.B)
      dut.io.startReady.expect(true.B)
    }
  }

  behavior of "LPDDR simulation SerDes"

  it should "expand LPDDR4 SDR controls and transpose a complete 16-edge DQ word" in {
    val config = DramConfig(addressBits = 40, dataBits = 256, bankBits = 6,
      rowBits = 17, columnBits = 10, memType = "LPDDR4", nPhases = 8,
      padDataBits = 16)
    test(new Lpddr4SimulationSerdes(config)).withAnnotations(Seq(
      VerilatorBackendAnnotation, VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      val clockWord = BigInt(0xa55a)
      val ckeWord = BigInt(0xb3)
      val csWord = BigInt(0x69)
      val caWords = (0 until 6).map(line => BigInt((0x13 * (line + 1)) & 0xff))
      val dqWords = (0 until 16).map(line => BigInt((0x1021 * (line + 3)) & 0xffff))
      val inputWords = (0 until 16).map(line => BigInt((0x0843 * (line + 5)) & 0xffff))

      dut.io.parallel.clock.poke(clockWord.U)
      dut.io.parallel.clockEnable.poke(ckeWord.U)
      dut.io.parallel.onDieTermination.poke(0x5a.U)
      dut.io.parallel.resetN.poke(0xff.U)
      dut.io.parallel.cs.poke(csWord.U)
      for (line <- 0 until 6) dut.io.parallel.ca(line).poke(caWords(line).U)
      for (line <- 0 until 16) dut.io.parallel.dq(line).poke(dqWords(line).U)
      dut.io.parallel.dqOutputEnable.poke(true.B)
      dut.io.parallel.dqs(0).poke("h5555".U)
      dut.io.parallel.dqs(1).poke("haaaa".U)
      dut.io.parallel.dqsOutputEnable.poke(true.B)
      dut.io.parallel.dmi(0).poke("hf00f".U)
      dut.io.parallel.dmi(1).poke("h0ff0".U)
      dut.io.parallel.dmiOutputEnable.poke(false.B)
      dut.io.load.poke(true.B)
      dut.io.readStart.poke(true.B)
      for (line <- 0 until 16) dut.io.serialDqIn(line).poke(bit(inputWords(line), 0).B)
      dut.clock.step()
      dut.io.load.poke(false.B)
      dut.io.readStart.poke(false.B)

      for (edge <- 0 until 16) {
        dut.io.pads.clock.expect(bit(clockWord, edge).B)
        dut.io.pads.clockEnable.expect(bit(ckeWord, edge / 2).B)
        dut.io.pads.cs.expect(bit(csWord, edge / 2).B)
        for (line <- 0 until 6) dut.io.pads.ca(line).expect(bit(caWords(line), edge / 2).B)
        for (line <- 0 until 16) dut.io.pads.dq(line).expect(bit(dqWords(line), edge).B)
        dut.io.pads.dqOutputEnable.expect(true.B)
        dut.io.pads.dqsOutputEnable.expect(true.B)
        dut.io.pads.dmiOutputEnable.expect(false.B)
        if (edge == 2) {
          dut.io.parallel.dqOutputEnable.poke(false.B)
          dut.io.parallel.dqsOutputEnable.poke(false.B)
        }
        if (edge < 15) {
          for (line <- 0 until 16) {
            dut.io.serialDqIn(line).poke(bit(inputWords(line), edge + 1).B)
          }
          dut.clock.step()
        }
      }
      dut.io.parallelDqValid.expect(true.B)
      for (line <- 0 until 16) dut.io.parallelDqIn(line).expect(inputWords(line).U)
    }
  }

  it should "expand LPDDR5 CK and CA at WCK to CK 4:1" in {
    val config = DramConfig(addressBits = 40, dataBits = 256, bankBits = 7,
      rowBits = 18, columnBits = 10, memType = "LPDDR5", padDataBits = 16)
    test(new Lpddr5SimulationSerdes(config, wckCkRatio = 4)) { dut =>
      val caWords = (0 until 7).map(line => BigInt((line + 1) & 3))
      val dqWords = (0 until 16).map(line => BigInt((0x35 + 17 * line) & 0xff))
      dut.io.parallel.resetN.poke(true.B)
      dut.io.parallel.clock.poke(1.U)
      dut.io.parallel.cs.poke(true.B)
      for (line <- 0 until 7) dut.io.parallel.ca(line).poke(caWords(line).U)
      for (line <- 0 until 16) dut.io.parallel.dq(line).poke(dqWords(line).U)
      dut.io.parallel.dqOutputEnable.poke(true.B)
      dut.io.parallel.wck(0).poke("h33".U)
      dut.io.parallel.wck(1).poke("h55".U)
      dut.io.parallel.readDqs.foreach(_.poke(0.U))
      dut.io.parallel.readDqsOutputEnable.poke(false.B)
      dut.io.parallel.dmi(0).poke("h0f".U)
      dut.io.parallel.dmi(1).poke("hf0".U)
      dut.io.parallel.dmiOutputEnable.poke(true.B)
      dut.io.serialDqIn.foreach(_.poke(false.B))
      dut.io.readStart.poke(false.B)
      dut.io.load.poke(true.B)
      dut.clock.step()
      dut.io.load.poke(false.B)

      for (edge <- 0 until 8) {
        dut.io.pads.clock.expect(bit(1, edge / 4).B)
        for (line <- 0 until 7) dut.io.pads.ca(line).expect(bit(caWords(line), edge / 4).B)
        for (line <- 0 until 16) dut.io.pads.dq(line).expect(bit(dqWords(line), edge).B)
        dut.io.pads.dqOutputEnable.expect(true.B)
        dut.io.pads.dmiOutputEnable.expect(true.B)
        if (edge < 7) dut.clock.step()
      }
    }
  }

  behavior of "RPC simulation SerDes"

  it should "serialize all RPC pad groups and deserialize DB input" in {
    val config = DramConfig(addressBits = 32, dataBits = 256, bankBits = 2,
      rowBits = 12, columnBits = 10, memType = "RPC", nPhases = 4,
      padDataBits = 16)
    test(new RpcSimulationSerdes(config)) { dut =>
      val dbWords = (0 until 16).map(line => BigInt((0x31 * (line + 1)) & 0xff))
      val inputWords = (0 until 16).map(line => BigInt((0x57 * (line + 2)) & 0xff))
      dut.io.parallel.clock.poke("h55".U)
      dut.io.parallel.strobe.poke("hc3".U)
      dut.io.parallel.chipSelectN.poke(0.U)
      dut.io.parallel.dqs.poke("h14".U)
      dut.io.parallel.dqsOutputEnable.poke(true.B)
      for (line <- 0 until 16) dut.io.parallel.db(line).poke(dbWords(line).U)
      dut.io.parallel.dbOutputEnable.poke(true.B)
      dut.io.load.poke(true.B)
      dut.io.readStart.poke(true.B)
      for (line <- 0 until 16) dut.io.serialDbIn(line).poke(bit(inputWords(line), 0).B)
      dut.clock.step()
      dut.io.load.poke(false.B)
      dut.io.readStart.poke(false.B)

      for (edge <- 0 until 8) {
        dut.io.pads.clock.expect(bit(0x55, edge).B)
        dut.io.pads.strobe.expect(bit(0xc3, edge).B)
        dut.io.pads.dqs.expect(bit(0x14, edge).B)
        for (line <- 0 until 16) dut.io.pads.db(line).expect(bit(dbWords(line), edge).B)
        dut.io.pads.dbOutputEnable.expect(true.B)
        dut.io.pads.dqsOutputEnable.expect(true.B)
        if (edge < 7) {
          for (line <- 0 until 16) {
            dut.io.serialDbIn(line).poke(bit(inputWords(line), edge + 1).B)
          }
          dut.clock.step()
        }
      }
      dut.io.parallelDbValid.expect(true.B)
      for (line <- 0 until 16) dut.io.parallelDbIn(line).expect(inputWords(line).U)
    }
  }
}
