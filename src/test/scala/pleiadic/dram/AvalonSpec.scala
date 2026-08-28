package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls
import scala.util.Random

class AvalonSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 16, dataBits = 32, bankBits = 1,
    rowBits = 5, columnBits = 4, timing = DramTiming(tRefi = 100))

  private def defaults(dut: AvalonMmToNative): Unit = {
    dut.io.avalon.address.poke(0.U)
    dut.io.avalon.read.poke(false.B)
    dut.io.avalon.write.poke(false.B)
    dut.io.avalon.burstCount.poke(1.U)
    dut.io.avalon.writeData.poke(0.U)
    dut.io.avalon.byteEnable.poke("hf".U)
    dut.io.nativeCommand.ready.poke(false.B)
    dut.io.nativeWriteData.ready.poke(false.B)
    dut.io.nativeReadData.valid.poke(false.B)
    dut.io.nativeReadData.bits.data.poke(0.U)
  }

  behavior of "AvalonMmToNative"

  it should "accept a write beat only when command and data are both ready" in {
    test(new AvalonMmToNative(cfg, baseAddress = 0x100)) { dut =>
      defaults(dut)
      dut.io.avalon.write.poke(true.B)
      dut.io.avalon.address.poke((0x40 + 3).U)
      dut.io.avalon.writeData.poke("hdeadbeef".U)
      dut.io.nativeCommand.ready.poke(true.B)
      dut.io.nativeWriteData.ready.poke(false.B)
      dut.io.nativeCommand.valid.expect(false.B)
      dut.io.avalon.waitRequest.expect(true.B)
      dut.io.nativeWriteData.ready.poke(true.B)
      dut.io.nativeCommand.valid.expect(true.B)
      dut.io.nativeCommand.bits.address.expect(3.U)
      dut.io.nativeWriteData.valid.expect(true.B)
      dut.io.avalon.waitRequest.expect(false.B)
    }
  }

  it should "increment every command in write and read bursts" in {
    test(new AvalonMmToNative(cfg, maxBurstLength = 8)) { dut =>
      defaults(dut)
      val protocol = new AvalonMmProtocolChecker(dut)
      def step(): Unit = {
        protocol.sample()
        dut.clock.step()
      }
      dut.io.nativeCommand.ready.poke(true.B)
      dut.io.nativeWriteData.ready.poke(true.B)
      dut.io.avalon.write.poke(true.B)
      dut.io.avalon.address.poke(4.U)
      dut.io.avalon.burstCount.poke(3.U)
      val writeAddresses = ArrayBuffer.empty[BigInt]
      for (beat <- 0 until 3) {
        dut.io.avalon.writeData.poke((0x10 + beat).U)
        if (dut.io.nativeCommand.valid.peek().litToBoolean)
          writeAddresses += dut.io.nativeCommand.bits.address.peek().litValue
        step()
      }
      dut.io.avalon.write.poke(false.B)
      assert(writeAddresses == Seq(4, 5, 6))

      dut.io.avalon.read.poke(true.B)
      dut.io.avalon.address.poke(9.U)
      dut.io.avalon.burstCount.poke(3.U)
      val readAddresses = ArrayBuffer.empty[BigInt]
      if (dut.io.nativeCommand.valid.peek().litToBoolean)
        readAddresses += dut.io.nativeCommand.bits.address.peek().litValue
      step()
      dut.io.avalon.read.poke(false.B)
      for (_ <- 0 until 3) {
        if (dut.io.nativeCommand.valid.peek().litToBoolean)
          readAddresses += dut.io.nativeCommand.bits.address.peek().litValue
        step()
      }
      assert(readAddresses == Seq(9, 10, 11))

      for (data <- Seq(0x11, 0x22, 0x33)) {
        dut.io.nativeReadData.valid.poke(true.B)
        dut.io.nativeReadData.bits.data.poke(data.U)
        dut.io.avalon.readDataValid.expect(true.B)
        dut.io.avalon.readData.expect(data.U)
        step()
      }
      dut.io.nativeReadData.valid.poke(false.B)
      step()
      protocol.finish()
    }
  }

  it should "pass randomized burst traffic through an independent protocol checker" in {
    test(new AvalonMmToNative(cfg, maxBurstLength = 8)) { dut =>
      defaults(dut)
      val protocol = new AvalonMmProtocolChecker(dut)
      val random = new Random(0x4156414c)
      val nativeMemory = mutable.Map.empty[Int, BigInt].withDefaultValue(BigInt(0))
      val expectedMemory = mutable.Map.empty[Int, BigInt].withDefaultValue(BigInt(0))
      val readResponses = mutable.Queue.empty[BigInt]
      var responsePresented = false
      var sampledReadData = Option.empty[BigInt]

      def cycle(): Boolean = {
        dut.io.nativeCommand.ready.poke((random.nextInt(4) != 0).B)
        dut.io.nativeWriteData.ready.poke(random.nextBoolean().B)
        if (readResponses.nonEmpty && !responsePresented && random.nextBoolean())
          responsePresented = true
        dut.io.nativeReadData.valid.poke(responsePresented.B)
        dut.io.nativeReadData.bits.data.poke(
          readResponses.headOption.getOrElse(BigInt(0)).U)

        protocol.sample()
        sampledReadData = if (dut.io.avalon.readDataValid.peek().litToBoolean)
          Some(dut.io.avalon.readData.peek().litValue) else None
        val accepted = (dut.io.avalon.read.peek().litToBoolean ||
          dut.io.avalon.write.peek().litToBoolean) &&
          !dut.io.avalon.waitRequest.peek().litToBoolean
        val commandFire = dut.io.nativeCommand.valid.peek().litToBoolean &&
          dut.io.nativeCommand.ready.peek().litToBoolean
        val writeFire = dut.io.nativeWriteData.valid.peek().litToBoolean &&
          dut.io.nativeWriteData.ready.peek().litToBoolean
        if (commandFire) {
          val address = dut.io.nativeCommand.bits.address.peek().litValue.toInt
          if (dut.io.nativeCommand.bits.write.peek().litToBoolean) {
            assert(writeFire, "Avalon Native write command/data were not coupled")
            val data = dut.io.nativeWriteData.bits.data.peek().litValue
            val byteEnable = dut.io.nativeWriteData.bits.byteEnable.peek().litValue
            var value = nativeMemory(address)
            for (byte <- 0 until 4 if ((byteEnable >> byte) & 1) != 0) {
              val mask = BigInt(0xff) << (8 * byte)
              value = (value & ~mask) | (data & mask)
            }
            nativeMemory(address) = value
          } else {
            readResponses.enqueue(nativeMemory(address))
          }
        }
        val responseFire = responsePresented &&
          dut.io.nativeReadData.ready.peek().litToBoolean
        dut.clock.step()
        if (responseFire) {
          readResponses.dequeue()
          responsePresented = false
        }
        accepted
      }

      for (_ <- 0 until 60) {
        val burstLength = 1 + random.nextInt(6)
        val base = random.nextInt(64 - burstLength)
        if (random.nextBoolean()) {
          dut.io.avalon.address.poke(base.U)
          dut.io.avalon.burstCount.poke(burstLength.U)
          dut.io.avalon.write.poke(true.B)
          for (beat <- 0 until burstLength) {
            val data = BigInt(32, random)
            val byteEnable = random.nextInt(16)
            dut.io.avalon.writeData.poke(data.U)
            dut.io.avalon.byteEnable.poke(byteEnable.U)
            var accepted = false
            var cycles = 0
            while (!accepted && cycles < 100) {
              accepted = cycle()
              cycles += 1
            }
            assert(accepted, s"Avalon write beat $beat timed out")
            var expected = expectedMemory(base + beat)
            for (byte <- 0 until 4 if ((byteEnable >> byte) & 1) != 0) {
              val mask = BigInt(0xff) << (8 * byte)
              expected = (expected & ~mask) | (data & mask)
            }
            expectedMemory(base + beat) = expected
          }
          dut.io.avalon.write.poke(false.B)
        } else {
          dut.io.avalon.address.poke(base.U)
          dut.io.avalon.burstCount.poke(burstLength.U)
          dut.io.avalon.read.poke(true.B)
          var accepted = false
          var cycles = 0
          while (!accepted && cycles < 100) {
            accepted = cycle()
            cycles += 1
          }
          assert(accepted, "Avalon read burst timed out")
          dut.io.avalon.read.poke(false.B)
          val observed = ArrayBuffer.empty[BigInt]
          cycles = 0
          while (observed.size < burstLength && cycles < 300) {
            cycle()
            sampledReadData.foreach(observed += _)
            cycles += 1
          }
          assert(cycles < 300, "Avalon read responses timed out")
          assert(observed == (0 until burstLength).map(i => expectedMemory(base + i)))
        }
        assert(nativeMemory.keySet.union(expectedMemory.keySet).forall(address =>
          nativeMemory(address) == expectedMemory(address)))
      }

      dut.io.avalon.read.poke(false.B)
      dut.io.avalon.write.poke(false.B)
      dut.io.nativeReadData.valid.poke(false.B)
      cycle()
      assert(readResponses.isEmpty && !responsePresented)
      protocol.finish()
    }
  }
}
