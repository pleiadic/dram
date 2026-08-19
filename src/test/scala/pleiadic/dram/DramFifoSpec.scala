package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.util.Random

class DramFifoSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 16, dataBits = 32, bankBits = 1,
    rowBits = 5, columnBits = 4, timing = DramTiming(tRefi = 100))

  behavior of "LiteDramFifoControl"

  it should "track occupancy and wrap arbitrary-depth pointers" in {
    test(new LiteDramFifoControl(cfg, baseAddress = 7, depth = 3)) { dut =>
      dut.io.write.poke(false.B)
      dut.io.read.poke(false.B)
      dut.io.level.expect(0.U)
      dut.io.writeAddress.expect(7.U)
      dut.io.readAddress.expect(7.U)

      for (address <- Seq(7, 8, 9)) {
        dut.io.writable.expect(true.B)
        dut.io.writeAddress.expect(address.U)
        dut.io.write.poke(true.B)
        dut.clock.step()
        dut.io.write.poke(false.B)
      }
      dut.io.level.expect(3.U)
      dut.io.writable.expect(false.B)
      dut.io.writeAddress.expect(7.U)

      for (address <- Seq(7, 8, 9)) {
        dut.io.readable.expect(true.B)
        dut.io.readAddress.expect(address.U)
        dut.io.read.poke(true.B)
        dut.clock.step()
        dut.io.read.poke(false.B)
      }
      dut.io.level.expect(0.U)
      dut.io.readable.expect(false.B)
      dut.io.readAddress.expect(7.U)
    }
  }

  behavior of "LiteDramFifo"

  it should "preserve a backpressured stream through repeated DRAM address wraps" in {
    test(new LiteDramFifo(cfg, baseAddress = 12, depth = 4,
      writerFifoDepth = 4, readerFifoDepth = 4)) { dut =>
      dut.io.input.valid.poke(false.B)
      dut.io.input.bits.poke(0.U)
      dut.io.output.ready.poke(false.B)
      dut.io.nativeWriteCommand.ready.poke(false.B)
      dut.io.nativeWriteData.ready.poke(false.B)
      dut.io.nativeReadCommand.ready.poke(false.B)
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.nativeReadData.bits.data.poke(0.U)

      val values = (0 until 24).map(i => BigInt(0x7000 + i))
      val memory = mutable.Map.empty[BigInt, BigInt]
      val writeAddresses = mutable.Queue.empty[BigInt]
      val writePayloads = mutable.Queue.empty[BigInt]
      val readResponses = mutable.Queue.empty[BigInt]
      val random = new Random(0x4649464f)
      var inputIndex = 0
      var outputIndex = 0
      var cycles = 0

      while (outputIndex < values.size && cycles < 2000) {
        dut.io.input.valid.poke((inputIndex < values.size).B)
        if (inputIndex < values.size) dut.io.input.bits.poke(values(inputIndex).U)
        dut.io.output.ready.poke((random.nextInt(3) != 0).B)
        dut.io.nativeWriteCommand.ready.poke((random.nextInt(4) != 0).B)
        dut.io.nativeWriteData.ready.poke(random.nextBoolean().B)

        val readAddress = dut.io.nativeReadCommand.bits.address.peek().litValue
        // The software memory model serializes accepted writes before reads,
        // matching the ordering provided by the controller command path.
        val readReady = dut.io.nativeReadCommand.valid.peek().litToBoolean &&
          memory.contains(readAddress) && writeAddresses.isEmpty && writePayloads.isEmpty &&
          !dut.io.nativeWriteCommand.valid.peek().litToBoolean
        dut.io.nativeReadCommand.ready.poke(readReady.B)
        dut.io.nativeReadData.valid.poke(readResponses.nonEmpty.B)
        if (readResponses.nonEmpty) dut.io.nativeReadData.bits.data.poke(readResponses.front.U)

        val inputFire = dut.io.input.valid.peek().litToBoolean &&
          dut.io.input.ready.peek().litToBoolean
        val writeCommandFire = dut.io.nativeWriteCommand.valid.peek().litToBoolean &&
          dut.io.nativeWriteCommand.ready.peek().litToBoolean
        val writeDataFire = dut.io.nativeWriteData.valid.peek().litToBoolean &&
          dut.io.nativeWriteData.ready.peek().litToBoolean
        val readCommandFire = dut.io.nativeReadCommand.valid.peek().litToBoolean &&
          dut.io.nativeReadCommand.ready.peek().litToBoolean
        val readDataFire = dut.io.nativeReadData.valid.peek().litToBoolean &&
          dut.io.nativeReadData.ready.peek().litToBoolean
        val outputFire = dut.io.output.valid.peek().litToBoolean &&
          dut.io.output.ready.peek().litToBoolean

        val capturedWriteAddress = dut.io.nativeWriteCommand.bits.address.peek().litValue
        val capturedWriteData = dut.io.nativeWriteData.bits.data.peek().litValue
        val capturedReadData = if (readCommandFire) memory(readAddress) else BigInt(0)
        val capturedOutput = dut.io.output.bits.peek().litValue
        if (writeCommandFire) dut.io.nativeWriteCommand.bits.write.expect(true.B)
        if (writeDataFire) dut.io.nativeWriteData.bits.byteEnable.expect("hf".U)
        if (readCommandFire) dut.io.nativeReadCommand.bits.write.expect(false.B)

        dut.clock.step()

        if (inputFire) inputIndex += 1
        if (writeCommandFire) writeAddresses.enqueue(capturedWriteAddress)
        if (writeDataFire) writePayloads.enqueue(capturedWriteData)
        while (writeAddresses.nonEmpty && writePayloads.nonEmpty)
          memory(writeAddresses.dequeue()) = writePayloads.dequeue()
        if (readCommandFire) readResponses.enqueue(capturedReadData)
        if (readDataFire) readResponses.dequeue()
        if (outputFire) {
          assert(capturedOutput == values(outputIndex))
          outputIndex += 1
        }
        cycles += 1
      }

      assert(cycles < 2000)
      assert(inputIndex == values.size)
      assert(outputIndex == values.size)
    }
  }
}
