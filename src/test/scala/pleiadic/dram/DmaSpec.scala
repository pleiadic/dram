package pleiadic.dram

import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls
import scala.util.Random

class DmaSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 16, dataBits = 32, bankBits = 1,
    rowBits = 5, columnBits = 4, timing = DramTiming(tRefi = 100))

  behavior of "LiteDramDmaReader"

  it should "limit outstanding reads and realign last with backpressured responses" in {
    test(new LiteDramDmaReader(cfg, fifoDepth = 4)) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.request.valid.poke(false.B)
      dut.io.request.bits.address.poke(0.U)
      dut.io.request.bits.last.poke(false.B)
      dut.io.data.ready.poke(false.B)
      dut.io.nativeCommand.ready.poke(true.B)
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.nativeReadData.bits.data.poke(0.U)

      def request(address: Int, last: Boolean): Unit = {
        dut.io.request.bits.address.poke(address.U)
        dut.io.request.bits.last.poke(last.B)
        dut.io.request.valid.poke(true.B)
        while (!dut.io.request.ready.peek().litToBoolean) dut.clock.step()
        dut.clock.step()
        dut.io.request.valid.poke(false.B)
      }

      request(10, last = false)
      request(11, last = false)
      request(12, last = false)
      request(13, last = true)

      dut.io.request.bits.address.poke(14.U)
      dut.io.request.bits.last.poke(false.B)
      dut.io.request.valid.poke(true.B)
      dut.io.request.ready.expect(false.B)

      dut.io.nativeReadData.valid.poke(true.B)
      dut.io.nativeReadData.bits.data.poke(0x100.U)
      dut.clock.step()
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.data.valid.expect(true.B)
      dut.io.data.bits.data.expect(0x100.U)
      dut.io.data.bits.last.expect(false.B)
      dut.clock.step(2)
      dut.io.data.bits.data.expect(0x100.U)
      dut.io.data.ready.poke(true.B)
      dut.clock.step()

      dut.io.request.ready.expect(true.B)
      dut.clock.step()
      dut.io.request.valid.poke(false.B)

      dut.io.nativeReadData.bits.data.poke(0x101.U)
      dut.io.nativeReadData.valid.poke(true.B)
      dut.clock.step()
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.data.valid.expect(true.B)
      dut.io.data.bits.data.expect(0x101.U)
      dut.io.data.bits.last.expect(false.B)
      dut.clock.step()
      request(15, last = true)

      for ((data, last) <- Seq(
          0x102 -> false, 0x103 -> true,
          0x104 -> false, 0x105 -> true)) {
        dut.io.nativeReadData.bits.data.poke(data.U)
        dut.io.nativeReadData.valid.poke(true.B)
        while (!dut.io.nativeReadData.ready.peek().litToBoolean) dut.clock.step()
        dut.clock.step()
        dut.io.nativeReadData.valid.poke(false.B)
        dut.io.data.valid.expect(true.B)
        dut.io.data.bits.data.expect(data.U)
        dut.io.data.bits.last.expect(last.B)
        dut.clock.step()
      }

      request(20, last = true)
      dut.io.enable.poke(false.B)
      dut.io.nativeReadData.valid.poke(true.B)
      dut.io.nativeReadData.bits.data.poke("hfeedface".U)
      dut.clock.step()
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.data.valid.expect(false.B)
      dut.io.enable.poke(true.B)
      dut.clock.step()
      dut.io.data.valid.expect(false.B)
    }
  }

  behavior of "LiteDramDmaWriter"

  it should "preserve command/data order under independent randomized backpressure" in {
    test(new LiteDramDmaWriter(cfg, fifoDepth = 4)).withAnnotations(Seq(
      VerilatorBackendAnnotation, VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.request.valid.poke(false.B)
      dut.io.request.bits.address.poke(0.U)
      dut.io.request.bits.data.poke(0.U)
      dut.io.request.bits.byteEnable.poke(0.U)
      dut.io.request.bits.last.poke(false.B)
      dut.io.nativeCommand.ready.poke(false.B)
      dut.io.nativeWriteData.ready.poke(false.B)

      val expected = (0 until 20).map(i => (0x20 + i, BigInt(0x1000 + i), BigInt(i & 0xf)))
      val commands = ArrayBuffer.empty[(BigInt, Boolean)]
      val writes = ArrayBuffer.empty[(BigInt, BigInt)]
      val random = new Random(0x444d41)
      var inputIndex = 0
      var cycles = 0
      while ((inputIndex < expected.size || writes.size < expected.size) && cycles < 500) {
        if (inputIndex < expected.size) {
          val (address, data, mask) = expected(inputIndex)
          dut.io.request.valid.poke(true.B)
          dut.io.request.bits.address.poke(address.U)
          dut.io.request.bits.data.poke(data.U)
          dut.io.request.bits.byteEnable.poke(mask.U)
          dut.io.request.bits.last.poke((inputIndex == expected.size - 1).B)
        } else {
          dut.io.request.valid.poke(false.B)
        }
        dut.io.nativeCommand.ready.poke((random.nextInt(3) != 0).B)
        dut.io.nativeWriteData.ready.poke(random.nextBoolean().B)

        val requestFire = dut.io.request.valid.peek().litToBoolean &&
          dut.io.request.ready.peek().litToBoolean
        if (dut.io.nativeCommand.valid.peek().litToBoolean &&
            dut.io.nativeCommand.ready.peek().litToBoolean) {
          commands += dut.io.nativeCommand.bits.address.peek().litValue ->
            dut.io.nativeCommand.bits.write.peek().litToBoolean
        }
        if (dut.io.nativeWriteData.valid.peek().litToBoolean &&
            dut.io.nativeWriteData.ready.peek().litToBoolean) {
          writes += dut.io.nativeWriteData.bits.data.peek().litValue ->
            dut.io.nativeWriteData.bits.byteEnable.peek().litValue
        }
        dut.clock.step()
        if (requestFire) inputIndex += 1
        cycles += 1
      }

      assert(cycles < 500)
      assert(commands == expected.map { case (address, _, _) => BigInt(address) -> true })
      assert(writes == expected.map { case (_, data, mask) => data -> mask })
    }
  }

  behavior of "LiteDramDmaReaderControl"

  it should "issue a bounded contiguous transfer and preserve the final marker" in {
    test(new LiteDramDmaReaderControl(cfg, fifoDepth = 4)) { dut =>
      dut.io.enable.poke(false.B)
      dut.io.base.poke(30.U)
      dut.io.length.poke(3.U)
      dut.io.loop.poke(false.B)
      dut.io.data.ready.poke(true.B)
      dut.io.nativeCommand.ready.poke(true.B)
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.nativeReadData.bits.data.poke(0.U)
      dut.clock.step()
      dut.io.enable.poke(true.B)

      val addresses = ArrayBuffer.empty[BigInt]
      var cycles = 0
      while (!dut.io.done.peek().litToBoolean && cycles < 20) {
        if (dut.io.nativeCommand.valid.peek().litToBoolean) {
          dut.io.nativeCommand.bits.write.expect(false.B)
          addresses += dut.io.nativeCommand.bits.address.peek().litValue
        }
        dut.clock.step()
        cycles += 1
      }
      assert(addresses == Seq(30, 31, 32).map(BigInt(_)))
      dut.io.offset.expect(2.U)

      for ((value, last) <- Seq(0x81 -> false, 0x82 -> false, 0x83 -> true)) {
        dut.io.nativeReadData.valid.poke(true.B)
        dut.io.nativeReadData.bits.data.poke(value.U)
        dut.clock.step()
        dut.io.nativeReadData.valid.poke(false.B)
        dut.io.data.valid.expect(true.B)
        dut.io.data.bits.data.expect(value.U)
        dut.io.data.bits.last.expect(last.B)
        dut.clock.step()
      }
      dut.io.enable.poke(false.B)
      dut.clock.step()
      dut.io.done.expect(false.B)
      dut.io.offset.expect(0.U)
    }
  }

  behavior of "LiteDramDmaWriterControl"

  it should "address input data contiguously and report done before the final drain" in {
    test(new LiteDramDmaWriterControl(cfg, fifoDepth = 3)) { dut =>
      dut.io.enable.poke(false.B)
      dut.io.base.poke(40.U)
      dut.io.length.poke(6.U)
      dut.io.loop.poke(false.B)
      dut.io.input.valid.poke(false.B)
      dut.io.input.bits.poke(0.U)
      dut.io.nativeCommand.ready.poke(false.B)
      dut.io.nativeWriteData.ready.poke(false.B)
      dut.clock.step()
      dut.io.enable.poke(true.B)

      val commands = ArrayBuffer.empty[BigInt]
      val payloads = ArrayBuffer.empty[BigInt]
      val values = (0 until 6).map(i => BigInt(0x9000 + i))
      val random = new Random(0x43535257)
      var inputIndex = 0
      var sawDoneWhileBusy = false
      var cycles = 0
      while ((payloads.size < values.size || !dut.io.done.peek().litToBoolean) && cycles < 200) {
        dut.io.input.valid.poke((inputIndex < values.size).B)
        if (inputIndex < values.size) dut.io.input.bits.poke(values(inputIndex).U)
        dut.io.nativeCommand.ready.poke((random.nextInt(3) != 0).B)
        dut.io.nativeWriteData.ready.poke((random.nextInt(4) == 0).B)
        val inputFire = dut.io.input.valid.peek().litToBoolean &&
          dut.io.input.ready.peek().litToBoolean
        if (dut.io.nativeCommand.valid.peek().litToBoolean &&
            dut.io.nativeCommand.ready.peek().litToBoolean) {
          dut.io.nativeCommand.bits.write.expect(true.B)
          commands += dut.io.nativeCommand.bits.address.peek().litValue
        }
        if (dut.io.nativeWriteData.valid.peek().litToBoolean &&
            dut.io.nativeWriteData.ready.peek().litToBoolean) {
          dut.io.nativeWriteData.bits.byteEnable.expect("hf".U)
          payloads += dut.io.nativeWriteData.bits.data.peek().litValue
        }
        sawDoneWhileBusy ||= dut.io.done.peek().litToBoolean && dut.io.busy.peek().litToBoolean
        dut.clock.step()
        if (inputFire) inputIndex += 1
        cycles += 1
      }
      assert(cycles < 200)
      assert(commands == (40 until 46).map(BigInt(_)))
      assert(payloads == values)
      assert(sawDoneWhileBusy)
      dut.io.busy.expect(false.B)
      dut.io.offset.expect(5.U)
    }
  }
}
