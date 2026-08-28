package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.util.Random

class BistSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 16, dataBits = 32, bankBits = 1,
    rowBits = 5, columnBits = 4, timing = DramTiming(tRefi = 100))

  private def repeated31(value: BigInt, width: Int): BigInt = {
    var result = BigInt(0)
    var offset = 0
    while (offset < width) {
      result |= value << offset
      offset += 31
    }
    result & ((BigInt(1) << width) - 1)
  }

  behavior of "BistSequence"

  it should "advance as a counter or a non-repeating LFSR" in {
    test(new BistSequence(23, stateWidth = 23, taps = Seq(17, 22))) { dut =>
      dut.io.random.poke(false.B)
      dut.io.advance.poke(false.B)
      dut.io.clear.poke(true.B)
      dut.clock.step()
      dut.io.clear.poke(false.B)
      dut.io.advance.poke(true.B)
      for (value <- 0 until 64) {
        dut.io.output.expect(value.U)
        dut.clock.step()
      }

      dut.io.clear.poke(true.B)
      dut.clock.step()
      dut.io.clear.poke(false.B)
      dut.io.random.poke(true.B)
      val values = mutable.Set.empty[BigInt]
      for (_ <- 0 until 256) {
        values += dut.io.output.peek().litValue
        dut.clock.step()
      }
      assert(values.size == 256)
    }
  }

  behavior of "LiteDramBistGenerator"

  it should "write the configured incremental pattern and drain buffered data" in {
    test(new LiteDramBistGenerator(cfg, fifoDepth = 4)) { dut =>
      dut.io.start.poke(false.B)
      dut.io.base.poke(4.U)
      dut.io.end.poke(12.U)
      dut.io.length.poke(8.U)
      dut.io.randomData.poke(false.B)
      dut.io.randomAddress.poke(false.B)
      dut.io.cascadeIn.poke(true.B)
      dut.io.nativeCommand.ready.poke(false.B)
      dut.io.nativeWriteData.ready.poke(false.B)
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      val addresses = mutable.ArrayBuffer.empty[BigInt]
      val data = mutable.ArrayBuffer.empty[BigInt]
      val random = new Random(0x42495354)
      var cycles = 0
      while (!dut.io.done.peek().litToBoolean && cycles < 300) {
        dut.io.nativeCommand.ready.poke((random.nextInt(4) != 0).B)
        dut.io.nativeWriteData.ready.poke(random.nextBoolean().B)
        if (dut.io.nativeCommand.valid.peek().litToBoolean &&
            dut.io.nativeCommand.ready.peek().litToBoolean) {
          dut.io.nativeCommand.bits.write.expect(true.B)
          addresses += dut.io.nativeCommand.bits.address.peek().litValue
        }
        if (dut.io.nativeWriteData.valid.peek().litToBoolean &&
            dut.io.nativeWriteData.ready.peek().litToBoolean) {
          dut.io.nativeWriteData.bits.byteEnable.expect("hf".U)
          data += dut.io.nativeWriteData.bits.data.peek().litValue
        }
        dut.clock.step()
        cycles += 1
      }
      assert(cycles < 300)
      assert(addresses == (4 until 12).map(BigInt(_)))
      assert(data == (0 until 8).map(i => repeated31(i, 32)))
    }
  }

  behavior of "LiteDramBistChecker"

  it should "count corrupted words while accepting out-of-phase read responses" in {
    test(new LiteDramBistChecker(cfg, fifoDepth = 4)) { dut =>
      dut.io.start.poke(false.B)
      dut.io.base.poke(4.U)
      dut.io.end.poke(12.U)
      dut.io.length.poke(8.U)
      dut.io.randomData.poke(false.B)
      dut.io.randomAddress.poke(false.B)
      dut.io.cascadeIn.poke(true.B)
      dut.io.nativeCommand.ready.poke(false.B)
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.nativeReadData.bits.data.poke(0.U)

      val memory = (4 until 12).map(address =>
        BigInt(address) -> repeated31(address - 4, 32)).toMap ++ Map(
          BigInt(6) -> BigInt("bad0bad0", 16),
          BigInt(10) -> BigInt("deadbeef", 16))
      val responses = mutable.Queue.empty[BigInt]
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      val random = new Random(0x43484543)
      var cycles = 0
      while (!dut.io.done.peek().litToBoolean && cycles < 400) {
        dut.io.nativeCommand.ready.poke((random.nextInt(3) != 0).B)
        dut.io.nativeReadData.valid.poke(responses.nonEmpty.B)
        if (responses.nonEmpty) dut.io.nativeReadData.bits.data.poke(responses.front.U)

        val commandFire = dut.io.nativeCommand.valid.peek().litToBoolean &&
          dut.io.nativeCommand.ready.peek().litToBoolean
        val responseFire = dut.io.nativeReadData.valid.peek().litToBoolean &&
          dut.io.nativeReadData.ready.peek().litToBoolean
        val address = dut.io.nativeCommand.bits.address.peek().litValue
        if (commandFire) dut.io.nativeCommand.bits.write.expect(false.B)
        dut.clock.step()
        if (commandFire) responses.enqueue(memory(address))
        if (responseFire) responses.dequeue()
        cycles += 1
      }
      assert(cycles < 400)
      dut.io.errors.expect(2.U)
    }
  }

  behavior of "LiteDramPatternGenerator"

  it should "write an irregular fixed table and wait for buffered data to drain" in {
    val pattern = Seq(
      BigInt(9) -> BigInt("11223344", 16),
      BigInt(3) -> BigInt("55667788", 16),
      BigInt(9) -> BigInt("99aabbcc", 16),
      BigInt(14) -> BigInt("ddeeff00", 16))
    test(new LiteDramPatternGenerator(cfg, pattern, fifoDepth = 2)) { dut =>
      dut.io.start.poke(false.B)
      dut.io.cascadeIn.poke(true.B)
      dut.io.nativeCommand.ready.poke(false.B)
      dut.io.nativeWriteData.ready.poke(false.B)
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      val addresses = mutable.ArrayBuffer.empty[BigInt]
      val data = mutable.ArrayBuffer.empty[BigInt]
      val random = new Random(0x50415447)
      var cycles = 0
      while (!dut.io.done.peek().litToBoolean && cycles < 300) {
        dut.io.cascadeIn.poke((random.nextInt(4) != 0).B)
        dut.io.nativeCommand.ready.poke((random.nextInt(3) != 0).B)
        dut.io.nativeWriteData.ready.poke(random.nextBoolean().B)
        if (dut.io.nativeCommand.valid.peek().litToBoolean &&
            dut.io.nativeCommand.ready.peek().litToBoolean) {
          dut.io.nativeCommand.bits.write.expect(true.B)
          addresses += dut.io.nativeCommand.bits.address.peek().litValue
        }
        if (dut.io.nativeWriteData.valid.peek().litToBoolean &&
            dut.io.nativeWriteData.ready.peek().litToBoolean) {
          dut.io.nativeWriteData.bits.byteEnable.expect("hf".U)
          data += dut.io.nativeWriteData.bits.data.peek().litValue
        }
        dut.clock.step()
        cycles += 1
      }
      assert(cycles < 300)
      assert(addresses == pattern.map(_._1))
      assert(data == pattern.map(_._2))
      dut.io.cascadeOut.expect(true.B)
    }
  }

  behavior of "LiteDramPatternChecker"

  it should "check fixed-table responses in order including duplicate addresses" in {
    val pattern = Seq(
      BigInt(5) -> BigInt("01020304", 16),
      BigInt(11) -> BigInt("11121314", 16),
      BigInt(5) -> BigInt("21222324", 16),
      BigInt(2) -> BigInt("31323334", 16),
      BigInt(15) -> BigInt("41424344", 16))
    test(new LiteDramPatternChecker(cfg, pattern, fifoDepth = 3)) { dut =>
      dut.io.start.poke(false.B)
      dut.io.cascadeIn.poke(true.B)
      dut.io.nativeCommand.ready.poke(false.B)
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.nativeReadData.bits.data.poke(0.U)
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      // Duplicate address 5 contains the last value after the generator runs,
      // so the first table entry intentionally reports one mismatch.
      val memory = pattern.toMap
      val responses = mutable.Queue.empty[BigInt]
      val issued = mutable.ArrayBuffer.empty[BigInt]
      val random = new Random(0x50415443)
      var cycles = 0
      while (!dut.io.done.peek().litToBoolean && cycles < 400) {
        dut.io.cascadeIn.poke((random.nextInt(5) != 0).B)
        dut.io.nativeCommand.ready.poke((random.nextInt(3) != 0).B)
        dut.io.nativeReadData.valid.poke((responses.nonEmpty && random.nextBoolean()).B)
        if (responses.nonEmpty) dut.io.nativeReadData.bits.data.poke(responses.front.U)

        val commandFire = dut.io.nativeCommand.valid.peek().litToBoolean &&
          dut.io.nativeCommand.ready.peek().litToBoolean
        val responseFire = dut.io.nativeReadData.valid.peek().litToBoolean &&
          dut.io.nativeReadData.ready.peek().litToBoolean
        val address = dut.io.nativeCommand.bits.address.peek().litValue
        if (commandFire) {
          dut.io.nativeCommand.bits.write.expect(false.B)
          issued += address
        }
        dut.clock.step()
        if (commandFire) responses.enqueue(memory(address))
        if (responseFire) responses.dequeue()
        cycles += 1
      }
      assert(cycles < 400)
      assert(issued == pattern.map(_._1))
      dut.io.errors.expect(1.U)
      dut.io.cascadeOut.expect(true.B)
    }
  }
}
