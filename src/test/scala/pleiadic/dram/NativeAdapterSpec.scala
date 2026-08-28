package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls
import scala.util.Random

class NativeAdapterSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "NativeDownConverter"

  it should "split wide commands and writes and join read responses" in {
    test(new NativeDownConverter(12, 64, 32)) { dut =>
      dut.io.outputCommand.ready.poke(true.B)
      dut.io.outputWriteData.ready.poke(true.B)
      dut.io.outputReadData.ready.poke(true.B)
      dut.io.inputCommand.valid.poke(true.B)
      dut.io.inputCommand.bits.address.poke(5.U)
      dut.io.inputCommand.bits.write.poke(true.B)
      dut.io.inputWriteData.valid.poke(true.B)
      dut.io.inputWriteData.bits.data.poke("h1122334455667788".U)
      dut.io.inputWriteData.bits.byteEnable.poke("ha5".U)
      dut.io.inputReadData.valid.poke(false.B)
      dut.io.inputReadData.bits.data.poke(0.U)
      dut.clock.step()
      dut.io.inputCommand.valid.poke(false.B)
      dut.io.inputWriteData.valid.poke(false.B)

      val addresses = ArrayBuffer.empty[BigInt]
      val writes = ArrayBuffer.empty[(BigInt, BigInt)]
      for (_ <- 0 until 3) {
        if (dut.io.outputCommand.valid.peek().litToBoolean)
          addresses += dut.io.outputCommand.bits.address.peek().litValue
        if (dut.io.outputWriteData.valid.peek().litToBoolean)
          writes += dut.io.outputWriteData.bits.data.peek().litValue ->
            dut.io.outputWriteData.bits.byteEnable.peek().litValue
        dut.clock.step()
      }
      assert(addresses == Seq(10, 11))
      assert(writes == Seq(BigInt("55667788", 16) -> 5, BigInt("11223344", 16) -> 10))

      dut.io.inputReadData.valid.poke(true.B)
      dut.io.inputReadData.bits.data.poke("h89abcdef".U)
      dut.clock.step()
      dut.io.inputReadData.bits.data.poke("h01234567".U)
      dut.clock.step()
      dut.io.inputReadData.valid.poke(false.B)
      dut.io.outputReadData.valid.expect(true.B)
      dut.io.outputReadData.bits.data.expect("h0123456789abcdef".U)
    }
  }

  behavior of "NativeUpConverter"

  it should "place writes into byte lanes and select the tagged read lane" in {
    test(new NativeUpConverter(12, 32, 128)) { dut =>
      dut.io.flush.poke(false.B)
      dut.io.outputCommand.ready.poke(true.B)
      dut.io.outputWriteData.ready.poke(true.B)
      dut.io.outputReadData.ready.poke(true.B)
      dut.io.inputCommand.valid.poke(true.B)
      dut.io.inputCommand.bits.address.poke(6.U) // wide address 1, lane 2
      dut.io.inputCommand.bits.write.poke(true.B)
      dut.io.inputWriteData.valid.poke(false.B)
      dut.io.inputReadData.valid.poke(false.B)
      dut.io.inputReadData.bits.data.poke(0.U)
      dut.io.outputCommand.bits.address.expect(1.U)
      dut.clock.step()
      dut.io.inputCommand.valid.poke(false.B)

      dut.io.inputWriteData.valid.poke(true.B)
      dut.io.inputWriteData.bits.data.poke("hdeadbeef".U)
      dut.io.inputWriteData.bits.byteEnable.poke("hf".U)
      dut.io.outputWriteData.valid.expect(true.B)
      dut.io.outputWriteData.bits.data.expect((BigInt("deadbeef", 16) << 64).U)
      dut.io.outputWriteData.bits.byteEnable.expect("h0f00".U)
      dut.clock.step()
      dut.io.inputWriteData.valid.poke(false.B)

      dut.io.inputCommand.valid.poke(true.B)
      dut.io.inputCommand.bits.address.poke(7.U) // wide address 1, lane 3
      dut.io.inputCommand.bits.write.poke(false.B)
      dut.clock.step()
      dut.io.inputCommand.valid.poke(false.B)
      dut.io.inputReadData.valid.poke(true.B)
      dut.io.inputReadData.bits.data.poke(BigInt("cafebabe112233445566778899aabbcc", 16).U)
      dut.io.outputReadData.valid.expect(true.B)
      dut.io.outputReadData.bits.data.expect("hcafebabe".U)
    }
  }

  it should "preserve long independent streams through randomized down-conversion stalls" in {
    for (reverse <- Seq(false, true)) {
      test(new NativeDownConverter(10, 64, 16, reverse = reverse)) { dut =>
        case class Command(address: Int, write: Boolean)
        case class Write(data: BigInt, mask: Int)
        val rng = new Random(if (reverse) 0x444f574e52L else 0x444f574eL)
        val commands = Seq.fill(90)(Command(rng.nextInt(1 << 10), rng.nextBoolean()))
        val writes = Seq.fill(75)(Write(BigInt(64, rng), rng.nextInt(256)))
        val reads = Seq.fill(70)(BigInt(64, rng))
        val commandOutputs = for {
          command <- commands
          lane <- 0 until 4
        } yield (command.address * 4 + lane, command.write)
        val writeOutputs = for {
          write <- writes
          index <- 0 until 4
          lane = if (reverse) 3 - index else index
        } yield ((write.data >> (16 * lane)) & 0xffff, (write.mask >> (2 * lane)) & 3)
        val readInputs = for {
          word <- reads
          index <- 0 until 4
          lane = if (reverse) 3 - index else index
        } yield (word >> (16 * lane)) & 0xffff

        var commandIn = 0
        var commandOut = 0
        var writeIn = 0
        var writeOut = 0
        var readIn = 0
        var readOut = 0
        var cycle = 0
        while ((commandOut < commandOutputs.size || writeOut < writeOutputs.size ||
            readOut < reads.size) && cycle < 20000) {
          dut.io.inputCommand.valid.poke((commandIn < commands.size).B)
          if (commandIn < commands.size) {
            dut.io.inputCommand.bits.address.poke(commands(commandIn).address.U)
            dut.io.inputCommand.bits.write.poke(commands(commandIn).write.B)
          }
          dut.io.inputWriteData.valid.poke((writeIn < writes.size).B)
          if (writeIn < writes.size) {
            dut.io.inputWriteData.bits.data.poke(writes(writeIn).data.U)
            dut.io.inputWriteData.bits.byteEnable.poke(writes(writeIn).mask.U)
          }
          dut.io.inputReadData.valid.poke((readIn < readInputs.size).B)
          if (readIn < readInputs.size)
            dut.io.inputReadData.bits.data.poke(readInputs(readIn).U)

          val commandReady = rng.nextInt(100) < 59
          val writeReady = rng.nextInt(100) < 63
          val readReady = rng.nextInt(100) < 55
          dut.io.outputCommand.ready.poke(commandReady.B)
          dut.io.outputWriteData.ready.poke(writeReady.B)
          dut.io.outputReadData.ready.poke(readReady.B)

          if (dut.io.outputCommand.valid.peek().litToBoolean) {
            val expected = commandOutputs(commandOut)
            dut.io.outputCommand.bits.address.expect(expected._1.U)
            dut.io.outputCommand.bits.write.expect(expected._2.B)
          }
          if (dut.io.outputWriteData.valid.peek().litToBoolean) {
            val expected = writeOutputs(writeOut)
            dut.io.outputWriteData.bits.data.expect(expected._1.U)
            dut.io.outputWriteData.bits.byteEnable.expect(expected._2.U)
          }
          if (dut.io.outputReadData.valid.peek().litToBoolean)
            dut.io.outputReadData.bits.data.expect(reads(readOut).U)

          val commandInputFire = commandIn < commands.size &&
            dut.io.inputCommand.ready.peek().litToBoolean
          val commandOutputFire = dut.io.outputCommand.valid.peek().litToBoolean && commandReady
          val writeInputFire = writeIn < writes.size &&
            dut.io.inputWriteData.ready.peek().litToBoolean
          val writeOutputFire = dut.io.outputWriteData.valid.peek().litToBoolean && writeReady
          val readInputFire = readIn < readInputs.size &&
            dut.io.inputReadData.ready.peek().litToBoolean
          val readOutputFire = dut.io.outputReadData.valid.peek().litToBoolean && readReady
          dut.clock.step()
          if (commandInputFire) commandIn += 1
          if (commandOutputFire) commandOut += 1
          if (writeInputFire) writeIn += 1
          if (writeOutputFire) writeOut += 1
          if (readInputFire) readIn += 1
          if (readOutputFire) readOut += 1
          cycle += 1
        }
        assert(cycle < 20000, s"reverse=$reverse down-converter timed out")
        assert(commandIn == commands.size && commandOut == commandOutputs.size)
        assert(writeIn == writes.size && writeOut == writeOutputs.size)
        assert(readIn == readInputs.size && readOut == reads.size)
      }
    }
  }

  it should "preserve lane tags through randomized up-conversion stalls" in {
    for (reverse <- Seq(false, true)) {
      test(new NativeUpConverter(10, 16, 64, reverse = reverse, tagDepth = 8)) { dut =>
        case class Operation(address: Int, write: Boolean)
        case class Write(data: Int, mask: Int, lane: Int)
        case class Read(data: BigInt, lane: Int)
        val rng = new Random(if (reverse) 0x5550524556L else 0x5550L)
        val operations = Seq.fill(180)(Operation(rng.nextInt(1 << 10), rng.nextBoolean()))
        def lane(address: Int): Int = {
          val raw = address & 3
          if (reverse) 3 - raw else raw
        }
        val writes = operations.filter(_.write).map(op =>
          Write(rng.nextInt(1 << 16), rng.nextInt(4), lane(op.address)))
        val reads = operations.filter(!_.write).map(op =>
          Read(BigInt(64, rng), lane(op.address)))

        var command = 0
        var write = 0
        var read = 0
        var cycle = 0
        while ((command < operations.size || write < writes.size || read < reads.size) &&
            cycle < 20000) {
          dut.io.flush.poke((rng.nextInt(97) == 0).B)
          dut.io.inputCommand.valid.poke((command < operations.size).B)
          if (command < operations.size) {
            dut.io.inputCommand.bits.address.poke(operations(command).address.U)
            dut.io.inputCommand.bits.write.poke(operations(command).write.B)
          }
          dut.io.inputWriteData.valid.poke((write < writes.size).B)
          if (write < writes.size) {
            dut.io.inputWriteData.bits.data.poke(writes(write).data.U)
            dut.io.inputWriteData.bits.byteEnable.poke(writes(write).mask.U)
          }
          dut.io.inputReadData.valid.poke((read < reads.size).B)
          if (read < reads.size) dut.io.inputReadData.bits.data.poke(reads(read).data.U)

          val commandReady = rng.nextInt(100) < 61
          val writeReady = rng.nextInt(100) < 57
          val readReady = rng.nextInt(100) < 53
          dut.io.outputCommand.ready.poke(commandReady.B)
          dut.io.outputWriteData.ready.poke(writeReady.B)
          dut.io.outputReadData.ready.poke(readReady.B)

          if (dut.io.outputCommand.valid.peek().litToBoolean) {
            dut.io.outputCommand.bits.address.expect((operations(command).address >> 2).U)
            dut.io.outputCommand.bits.write.expect(operations(command).write.B)
          }
          if (dut.io.outputWriteData.valid.peek().litToBoolean) {
            val expected = writes(write)
            dut.io.outputWriteData.bits.data.expect((BigInt(expected.data) << (16 * expected.lane)).U)
            dut.io.outputWriteData.bits.byteEnable.expect((expected.mask << (2 * expected.lane)).U)
          }
          if (dut.io.outputReadData.valid.peek().litToBoolean) {
            val expected = reads(read)
            dut.io.outputReadData.bits.data.expect(
              ((expected.data >> (16 * expected.lane)) & 0xffff).U)
          }

          val commandFire = dut.io.outputCommand.valid.peek().litToBoolean && commandReady
          val writeFire = dut.io.outputWriteData.valid.peek().litToBoolean && writeReady
          val readFire = dut.io.outputReadData.valid.peek().litToBoolean && readReady
          dut.clock.step()
          if (commandFire) command += 1
          if (writeFire) write += 1
          if (readFire) read += 1
          cycle += 1
        }
        assert(cycle < 20000, s"reverse=$reverse up-converter timed out")
        assert(command == operations.size && write == writes.size && read == reads.size)
      }
    }
  }
}
