package pleiadic.dram

import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls
import scala.util.Random

class AxiSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 16, dataBits = 32, bankBits = 1,
    rowBits = 5, columnBits = 4, timing = DramTiming(tRefi = 100))
  private val backend = Seq(VerilatorBackendAnnotation,
    VerilatorCFlags(Seq("-DWData=IData")))

  private def defaults(dut: Axi4ToNative): Unit = {
    dut.io.axi.aw.valid.poke(false.B)
    dut.io.axi.aw.bits.id.poke(0.U)
    dut.io.axi.aw.bits.address.poke(0.U)
    dut.io.axi.aw.bits.length.poke(0.U)
    dut.io.axi.aw.bits.size.poke(2.U)
    dut.io.axi.aw.bits.burst.poke(AxiBurst.increment)
    dut.io.axi.w.valid.poke(false.B)
    dut.io.axi.w.bits.data.poke(0.U)
    dut.io.axi.w.bits.strobe.poke(0.U)
    dut.io.axi.w.bits.last.poke(false.B)
    dut.io.axi.b.ready.poke(false.B)
    dut.io.axi.ar.valid.poke(false.B)
    dut.io.axi.ar.bits.id.poke(0.U)
    dut.io.axi.ar.bits.address.poke(0.U)
    dut.io.axi.ar.bits.length.poke(0.U)
    dut.io.axi.ar.bits.size.poke(2.U)
    dut.io.axi.ar.bits.burst.poke(AxiBurst.increment)
    dut.io.axi.r.ready.poke(false.B)
    dut.io.nativeCommand.ready.poke(false.B)
    dut.io.nativeWriteData.ready.poke(false.B)
    dut.io.nativeReadData.valid.poke(false.B)
    dut.io.nativeReadData.bits.data.poke(0.U)
  }

  private def sendAw(dut: Axi4ToNative, id: Int, address: Int,
      length: Int, burst: BigInt = 1): Unit = {
    dut.io.axi.aw.bits.id.poke(id.U)
    dut.io.axi.aw.bits.address.poke(address.U)
    dut.io.axi.aw.bits.length.poke(length.U)
    dut.io.axi.aw.bits.size.poke(2.U)
    dut.io.axi.aw.bits.burst.poke(burst.U)
    dut.io.axi.aw.valid.poke(true.B)
    while (!dut.io.axi.aw.ready.peek().litToBoolean) dut.clock.step()
    dut.clock.step()
    dut.io.axi.aw.valid.poke(false.B)
  }

  private def sendAr(dut: Axi4ToNative, id: Int, address: Int,
      length: Int, burst: BigInt = 1): Unit = {
    dut.io.axi.ar.bits.id.poke(id.U)
    dut.io.axi.ar.bits.address.poke(address.U)
    dut.io.axi.ar.bits.length.poke(length.U)
    dut.io.axi.ar.bits.size.poke(2.U)
    dut.io.axi.ar.bits.burst.poke(burst.U)
    dut.io.axi.ar.valid.poke(true.B)
    while (!dut.io.axi.ar.ready.peek().litToBoolean) dut.clock.step()
    dut.clock.step()
    dut.io.axi.ar.valid.poke(false.B)
  }

  private def sendW(dut: Axi4ToNative, data: BigInt, last: Boolean): Unit = {
    dut.io.axi.w.bits.data.poke(data.U)
    dut.io.axi.w.bits.strobe.poke("hf".U)
    dut.io.axi.w.bits.last.poke(last.B)
    dut.io.axi.w.valid.poke(true.B)
    while (!dut.io.axi.w.ready.peek().litToBoolean) dut.clock.step()
    dut.clock.step()
    dut.io.axi.w.valid.poke(false.B)
  }

  private def sendW(dut: Axi4ToNative, data: BigInt, strobe: Int,
      last: Boolean): Unit = {
    dut.io.axi.w.bits.data.poke(data.U)
    dut.io.axi.w.bits.strobe.poke(strobe.U)
    dut.io.axi.w.bits.last.poke(last.B)
    dut.io.axi.w.valid.poke(true.B)
    while (!dut.io.axi.w.ready.peek().litToBoolean) dut.clock.step()
    dut.clock.step()
    dut.io.axi.w.valid.poke(false.B)
  }

  behavior of "Axi4ToNative"

  it should "never release write data before its Native command and hold B under backpressure" in {
    test(new Axi4ToNative(cfg, baseAddress = 0x100)).withAnnotations(backend) { dut =>
      defaults(dut)
      sendAw(dut, id = 5, address = 0x110, length = 0)
      sendW(dut, data = BigInt("deadbeef", 16), last = true)

      dut.io.nativeCommand.valid.expect(true.B)
      dut.io.nativeCommand.bits.write.expect(true.B)
      dut.io.nativeCommand.bits.address.expect(4.U)
      dut.io.nativeWriteData.valid.expect(false.B)
      dut.clock.step(2)
      dut.io.nativeWriteData.valid.expect(false.B)

      dut.io.nativeCommand.ready.poke(true.B)
      dut.clock.step()
      dut.io.nativeCommand.ready.poke(false.B)
      dut.io.nativeWriteData.valid.expect(true.B)
      dut.io.nativeWriteData.bits.data.expect("hdeadbeef".U)
      dut.io.nativeWriteData.bits.byteEnable.expect("hf".U)

      dut.io.nativeWriteData.ready.poke(true.B)
      dut.clock.step()
      dut.io.nativeWriteData.ready.poke(false.B)
      dut.io.axi.b.valid.expect(true.B)
      dut.io.axi.b.bits.id.expect(5.U)
      dut.io.axi.b.bits.response.expect(AxiResponse.okay)
      dut.clock.step(2)
      dut.io.axi.b.valid.expect(true.B)
      dut.io.axi.b.bits.id.expect(5.U)
      dut.io.axi.b.ready.poke(true.B)
      dut.clock.step()
      dut.io.axi.b.valid.expect(false.B)
    }
  }

  it should "lock command arbitration across bursts under randomized Native backpressure" in {
    test(new Axi4ToNative(cfg, maxBurstLength = 8, writeDataQueueDepth = 8,
      baseAddress = 0x100)).withAnnotations(backend) { dut =>
      defaults(dut)
      sendAw(dut, id = 3, address = 0x120, length = 2)
      sendW(dut, 0x11, last = false)
      sendW(dut, 0x22, last = false)
      sendW(dut, 0x33, last = true)
      sendAr(dut, id = 7, address = 0x140, length = 1)

      val random = new Random(0x415849)
      val commands = ArrayBuffer.empty[(Boolean, BigInt)]
      val writes = ArrayBuffer.empty[BigInt]
      var cycles = 0
      while ((commands.size < 5 || writes.size < 3 ||
          !dut.io.axi.b.valid.peek().litToBoolean) && cycles < 200) {
        dut.io.nativeCommand.ready.poke((random.nextInt(4) != 0).B)
        dut.io.nativeWriteData.ready.poke(random.nextBoolean().B)
        if (dut.io.nativeCommand.valid.peek().litToBoolean &&
            dut.io.nativeCommand.ready.peek().litToBoolean) {
          commands += dut.io.nativeCommand.bits.write.peek().litToBoolean ->
            dut.io.nativeCommand.bits.address.peek().litValue
        }
        if (dut.io.nativeWriteData.valid.peek().litToBoolean &&
            dut.io.nativeWriteData.ready.peek().litToBoolean) {
          writes += dut.io.nativeWriteData.bits.data.peek().litValue
        }
        dut.clock.step()
        cycles += 1
      }

      assert(cycles < 200)
      assert(commands == Seq(true -> 8, true -> 9, true -> 10,
        false -> 16, false -> 17))
      assert(writes == Seq(0x11, 0x22, 0x33))
      dut.io.axi.b.bits.id.expect(3.U)
      dut.io.axi.b.bits.response.expect(AxiResponse.okay)
    }
  }

  it should "generate wrapping read addresses and preserve ID and RLAST while stalled" in {
    test(new Axi4ToNative(cfg, maxBurstLength = 8,
      baseAddress = 0x100)).withAnnotations(backend) { dut =>
      defaults(dut)
      sendAr(dut, id = 9, address = 0x118, length = 3, burst = 2)

      val addresses = ArrayBuffer.empty[BigInt]
      var cycle = 0
      while (addresses.size < 4 && cycle < 40) {
        dut.io.nativeCommand.ready.poke((cycle % 3 != 1).B)
        if (dut.io.nativeCommand.valid.peek().litToBoolean &&
            dut.io.nativeCommand.ready.peek().litToBoolean) {
          dut.io.nativeCommand.bits.write.expect(false.B)
          addresses += dut.io.nativeCommand.bits.address.peek().litValue
        }
        dut.clock.step()
        cycle += 1
      }
      assert(addresses == Seq(6, 7, 4, 5))
      dut.io.nativeCommand.ready.poke(false.B)

      for ((data, index) <- Seq(0x101, 0x202, 0x303, 0x404).zipWithIndex) {
        dut.io.nativeReadData.bits.data.poke(data.U)
        dut.io.nativeReadData.valid.poke(true.B)
        dut.io.axi.r.ready.poke(false.B)
        dut.io.axi.r.valid.expect(true.B)
        dut.io.axi.r.bits.id.expect(9.U)
        dut.io.axi.r.bits.data.expect(data.U)
        dut.io.axi.r.bits.response.expect(AxiResponse.okay)
        dut.io.axi.r.bits.last.expect((index == 3).B)
        dut.clock.step(2)
        dut.io.axi.r.bits.id.expect(9.U)
        dut.io.axi.r.bits.data.expect(data.U)
        dut.io.axi.r.bits.last.expect((index == 3).B)
        dut.io.axi.r.ready.poke(true.B)
        dut.clock.step()
      }
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.axi.r.valid.expect(false.B)
    }
  }

  it should "complete partial writes through RMW before responding under random backpressure" in {
    test(new Axi4ToNative(cfg, withReadModifyWrite = true)).withAnnotations(backend) { dut =>
      case class Write(address: Int, data: BigInt, strobe: Int)
      case class Read(address: Int)

      defaults(dut)
      val random = new Random(0x524d57)
      val nativeMemory = mutable.Map.empty[Int, BigInt]
      val expectedMemory = mutable.Map.empty[Int, BigInt]
      for (address <- 0 until 32) {
        val value = BigInt(32, random)
        nativeMemory(address) = value
        expectedMemory(address) = value
      }
      val operations: Seq[Any] = Seq(
        Write(8, BigInt("aabbccdd", 16), 5), Read(8),
        Write(9, BigInt("10203040", 16), 15), Read(9)) ++
        Seq.fill(36) {
          val address = random.nextInt(32)
          if (random.nextBoolean())
            Write(address, BigInt(32, random), random.nextInt(16))
          else Read(address)
        }

      var pendingWriteAddress = Option.empty[Int]
      var pendingRead = Option.empty[BigInt]
      var readPresented = false
      var stalledCommand = Option.empty[(Boolean, BigInt)]
      var stalledWrite = Option.empty[(BigInt, BigInt)]
      val commandTrace = ArrayBuffer.empty[(Boolean, BigInt)]

      def tick(): Unit = {
        val commandReady = random.nextInt(4) != 0
        val writeReady = random.nextBoolean()
        if (pendingRead.nonEmpty && !readPresented && random.nextBoolean())
          readPresented = true
        dut.io.nativeCommand.ready.poke(commandReady.B)
        dut.io.nativeWriteData.ready.poke(writeReady.B)
        dut.io.nativeReadData.valid.poke(readPresented.B)
        dut.io.nativeReadData.bits.data.poke(pendingRead.getOrElse(BigInt(0)).U)

        val commandValid = dut.io.nativeCommand.valid.peek().litToBoolean
        val command = (dut.io.nativeCommand.bits.write.peek().litToBoolean,
          dut.io.nativeCommand.bits.address.peek().litValue)
        stalledCommand.foreach { held =>
          assert(commandValid && command == held,
            "RMW Native command changed while backpressured")
        }
        stalledCommand = if (commandValid && !commandReady) Some(command) else None

        val writeValid = dut.io.nativeWriteData.valid.peek().litToBoolean
        val write = (dut.io.nativeWriteData.bits.data.peek().litValue,
          dut.io.nativeWriteData.bits.byteEnable.peek().litValue)
        stalledWrite.foreach { held =>
          assert(writeValid && write == held,
            "RMW Native write data changed while backpressured")
        }
        stalledWrite = if (writeValid && !writeReady) Some(write) else None

        if (commandValid && commandReady) {
          commandTrace += command
          if (command._1) {
            assert(pendingWriteAddress.isEmpty)
            pendingWriteAddress = Some(command._2.toInt)
          } else {
            assert(pendingRead.isEmpty)
            pendingRead = Some(nativeMemory(command._2.toInt))
            readPresented = false
          }
        }
        if (writeValid && writeReady) {
          val address = pendingWriteAddress.getOrElse(
            fail("Native write data arrived without a command"))
          var value = nativeMemory(address)
          for (byte <- 0 until 4 if ((write._2 >> byte) & 1) != 0) {
            val mask = BigInt(0xff) << (8 * byte)
            value = (value & ~mask) | (write._1 & mask)
          }
          nativeMemory(address) = value
          pendingWriteAddress = None
        }
        val readFire = readPresented && dut.io.nativeReadData.ready.peek().litToBoolean
        dut.clock.step()
        if (readFire) {
          pendingRead = None
          readPresented = false
        }
      }

      for ((operation, index) <- operations.zipWithIndex) {
        commandTrace.clear()
        dut.io.nativeCommand.ready.poke(false.B)
        dut.io.nativeWriteData.ready.poke(false.B)
        operation match {
          case Write(address, data, strobe) =>
            sendAw(dut, id = index & 15, address = address * 4, length = 0)
            sendW(dut, data, strobe, last = true)
            dut.io.axi.b.ready.poke(false.B)
            var cycles = 0
            while (!dut.io.axi.b.valid.peek().litToBoolean && cycles < 200) {
              tick()
              cycles += 1
            }
            assert(cycles < 200, s"RMW write $index timed out")
            dut.io.axi.b.bits.id.expect((index & 15).U)
            dut.io.axi.b.bits.response.expect(AxiResponse.okay)

            var expected = expectedMemory(address)
            for (byte <- 0 until 4 if ((strobe >> byte) & 1) != 0) {
              val mask = BigInt(0xff) << (8 * byte)
              expected = (expected & ~mask) | (data & mask)
            }
            expectedMemory(address) = expected
            assert(nativeMemory(address) == expected)
            if (strobe == 15)
              assert(commandTrace.map(_._1) == Seq(true))
            else
              assert(commandTrace.map(_._1) == Seq(false, true))
            assert(commandTrace.forall(_._2 == address))

            dut.io.axi.b.ready.poke(true.B)
            tick()
            dut.io.axi.b.ready.poke(false.B)

          case Read(address) =>
            sendAr(dut, id = index & 15, address = address * 4, length = 0)
            dut.io.axi.r.ready.poke(false.B)
            var cycles = 0
            while (!dut.io.axi.r.valid.peek().litToBoolean && cycles < 200) {
              tick()
              cycles += 1
            }
            assert(cycles < 200, s"RMW-mode read $index timed out")
            dut.io.axi.r.bits.id.expect((index & 15).U)
            dut.io.axi.r.bits.data.expect(expectedMemory(address).U)
            dut.io.axi.r.bits.last.expect(true.B)
            assert(commandTrace == Seq(false -> BigInt(address)))
            dut.io.axi.r.ready.poke(true.B)
            tick()
            dut.io.axi.r.ready.poke(false.B)
        }
      }
      assert(pendingWriteAddress.isEmpty && pendingRead.isEmpty)
      assert(nativeMemory == expectedMemory)
    }
  }
}
