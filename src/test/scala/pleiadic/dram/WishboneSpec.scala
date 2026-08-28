package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.util.Random

class WishboneSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 16, dataBits = 32, bankBits = 1,
    rowBits = 5, columnBits = 4, timing = DramTiming(tRefi = 100))

  private def defaults(dut: WishboneToNative): Unit = {
    dut.io.wishbone.cyc.poke(false.B)
    dut.io.wishbone.stb.poke(false.B)
    dut.io.wishbone.writeEnable.poke(false.B)
    dut.io.wishbone.address.poke(0.U)
    dut.io.wishbone.writeData.poke(0.U)
    dut.io.wishbone.select.poke(0.U)
    dut.io.wishbone.cycleType.poke(WishboneCycleType.classic)
    dut.io.wishbone.burstType.poke(0.U)
    dut.io.nativeCommand.ready.poke(false.B)
    dut.io.nativeWriteData.ready.poke(false.B)
    dut.io.nativeReadData.valid.poke(false.B)
    dut.io.nativeReadData.bits.data.poke(0.U)
  }

  behavior of "WishboneToNative"

  it should "hold independent command and write channels until accepted" in {
    test(new WishboneToNative(cfg, baseAddress = 0x100)) { dut =>
      defaults(dut)
      dut.io.wishbone.cyc.poke(true.B)
      dut.io.wishbone.stb.poke(true.B)
      dut.io.wishbone.writeEnable.poke(true.B)
      dut.io.wishbone.address.poke((0x40 + 3).U)
      dut.io.wishbone.writeData.poke("hdeadbeef".U)
      dut.io.wishbone.select.poke("hb".U)
      dut.clock.step()

      dut.io.nativeCommand.valid.expect(true.B)
      dut.io.nativeCommand.bits.address.expect(3.U)
      dut.io.nativeCommand.ready.poke(true.B)
      dut.clock.step()
      dut.io.nativeCommand.ready.poke(false.B)
      dut.io.nativeCommand.valid.expect(false.B)
      dut.io.nativeWriteData.valid.expect(true.B)
      dut.io.nativeWriteData.bits.data.expect("hdeadbeef".U)
      dut.io.nativeWriteData.bits.byteEnable.expect("hb".U)
      dut.io.nativeWriteData.ready.poke(true.B)
      dut.clock.step()
      dut.io.wishbone.acknowledge.expect(true.B)
    }
  }

  it should "return Native read data before acknowledging" in {
    test(new WishboneToNative(cfg)) { dut =>
      defaults(dut)
      dut.io.wishbone.cyc.poke(true.B)
      dut.io.wishbone.stb.poke(true.B)
      dut.io.wishbone.address.poke(7.U)
      dut.clock.step()
      dut.io.nativeCommand.ready.poke(true.B)
      dut.clock.step()
      dut.io.nativeCommand.ready.poke(false.B)
      dut.io.nativeReadData.valid.poke(true.B)
      dut.io.nativeReadData.bits.data.poke("h12345678".U)
      dut.clock.step()
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.wishbone.acknowledge.expect(true.B)
      dut.io.wishbone.readData.expect("h12345678".U)
    }
  }

  it should "accept incrementing B4 burst beats without dropping CYC" in {
    test(new WishboneToNative(cfg)) { dut =>
      defaults(dut)
      dut.io.wishbone.cyc.poke(true.B)
      dut.io.wishbone.stb.poke(true.B)
      dut.io.wishbone.writeEnable.poke(true.B)
      dut.io.wishbone.select.poke("hf".U)
      dut.io.nativeCommand.ready.poke(true.B)
      dut.io.nativeWriteData.ready.poke(true.B)

      for (beat <- 0 until 3) {
        dut.io.wishbone.address.poke((8 + beat).U)
        dut.io.wishbone.writeData.poke((0x40 + beat).U)
        dut.io.wishbone.cycleType.poke(
          (if (beat == 2) WishboneCycleType.endOfBurst else WishboneCycleType.incrementingBurst))
        do { dut.clock.step() } while (!dut.io.wishbone.acknowledge.peek().litToBoolean)
      }
      dut.io.wishbone.cyc.poke(false.B)
      dut.io.wishbone.stb.poke(false.B)
      dut.clock.step()
    }
  }

  behavior of "WishboneToNativeWidthAdapter"

  it should "split a wide Wishbone access into consecutive Native words" in {
    test(new WishboneToNativeWidthAdapter(cfg, wishboneDataBits = 64)) { dut =>
      dut.io.wishbone.cyc.poke(true.B)
      dut.io.wishbone.stb.poke(true.B)
      dut.io.wishbone.writeEnable.poke(true.B)
      dut.io.wishbone.address.poke(5.U)
      dut.io.wishbone.writeData.poke("h1122334455667788".U)
      dut.io.wishbone.select.poke("hff".U)
      dut.io.wishbone.cycleType.poke(WishboneCycleType.classic)
      dut.io.wishbone.burstType.poke(0.U)
      dut.io.nativeCommand.ready.poke(true.B)
      dut.io.nativeWriteData.ready.poke(true.B)
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.nativeReadData.bits.data.poke(0.U)

      val addresses = scala.collection.mutable.ArrayBuffer.empty[BigInt]
      val writes = scala.collection.mutable.ArrayBuffer.empty[BigInt]
      var cycles = 0
      while ((addresses.size < 2 || writes.size < 2) && cycles < 20) {
        if (dut.io.nativeCommand.valid.peek().litToBoolean)
          addresses += dut.io.nativeCommand.bits.address.peek().litValue
        if (dut.io.nativeWriteData.valid.peek().litToBoolean)
          writes += dut.io.nativeWriteData.bits.data.peek().litValue
        dut.clock.step()
        cycles += 1
      }
      assert(addresses == Seq(10, 11))
      assert(writes == Seq(BigInt("55667788", 16), BigInt("11223344", 16)))
    }
  }

  it should "place a narrow Wishbone access in the selected Native lane" in {
    test(new WishboneToNativeWidthAdapter(cfg, wishboneDataBits = 16)) { dut =>
      dut.io.wishbone.cyc.poke(true.B)
      dut.io.wishbone.stb.poke(true.B)
      dut.io.wishbone.writeEnable.poke(true.B)
      dut.io.wishbone.address.poke(3.U)
      dut.io.wishbone.writeData.poke("hbeef".U)
      dut.io.wishbone.select.poke("h3".U)
      dut.io.wishbone.cycleType.poke(WishboneCycleType.classic)
      dut.io.wishbone.burstType.poke(0.U)
      dut.io.nativeCommand.ready.poke(true.B)
      dut.io.nativeWriteData.ready.poke(true.B)
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.nativeReadData.bits.data.poke(0.U)

      while (!dut.io.nativeCommand.valid.peek().litToBoolean) dut.clock.step()
      dut.io.nativeCommand.bits.address.expect(1.U)
      dut.clock.step()
      while (!dut.io.nativeWriteData.valid.peek().litToBoolean) dut.clock.step()
      dut.io.nativeWriteData.bits.data.expect("hbeef0000".U)
      dut.io.nativeWriteData.bits.byteEnable.expect("hc".U)
    }
  }

  it should "merge a narrow incrementing write burst into one Native word" in {
    test(new WishboneToNativeWidthAdapter(cfg, wishboneDataBits = 8)) { dut =>
      dut.io.wishbone.cyc.poke(true.B)
      dut.io.wishbone.stb.poke(true.B)
      dut.io.wishbone.writeEnable.poke(true.B)
      dut.io.wishbone.select.poke(1.U)
      dut.io.wishbone.burstType.poke(0.U)
      dut.io.nativeCommand.ready.poke(true.B)
      dut.io.nativeWriteData.ready.poke(true.B)
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.nativeReadData.bits.data.poke(0.U)

      for (lane <- 0 until 4) {
        dut.io.wishbone.address.poke((12 + lane).U)
        dut.io.wishbone.writeData.poke((0x11 * (lane + 1)).U)
        dut.io.wishbone.cycleType.poke(
          (if (lane == 3) WishboneCycleType.endOfBurst
           else WishboneCycleType.incrementingBurst))
        var cycles = 0
        while (!dut.io.wishbone.acknowledge.peek().litToBoolean && cycles < 20) {
          dut.io.nativeCommand.valid.expect(false.B)
          dut.clock.step()
          cycles += 1
        }
        assert(cycles < 20, s"Wishbone write lane $lane was not acknowledged")
        if (lane != 3) dut.clock.step()
      }

      dut.io.wishbone.cyc.poke(false.B)
      dut.io.wishbone.stb.poke(false.B)
      dut.clock.step()
      dut.io.nativeCommand.valid.expect(true.B)
      dut.io.nativeCommand.bits.write.expect(true.B)
      dut.io.nativeCommand.bits.address.expect(3.U)
      dut.clock.step()
      dut.io.nativeWriteData.valid.expect(true.B)
      dut.io.nativeWriteData.bits.data.expect("h44332211".U)
      dut.io.nativeWriteData.bits.byteEnable.expect("hf".U)
      dut.clock.step()
      dut.io.nativeCommand.valid.expect(false.B)
      dut.io.nativeWriteData.valid.expect(false.B)
    }
  }

  it should "flush a partial narrow write burst when CYC is dropped" in {
    test(new WishboneToNativeWidthAdapter(cfg, wishboneDataBits = 8)) { dut =>
      dut.io.wishbone.cyc.poke(true.B)
      dut.io.wishbone.stb.poke(true.B)
      dut.io.wishbone.writeEnable.poke(true.B)
      dut.io.wishbone.address.poke(9.U)
      dut.io.wishbone.writeData.poke("haa".U)
      dut.io.wishbone.select.poke(1.U)
      dut.io.wishbone.cycleType.poke(WishboneCycleType.incrementingBurst)
      dut.io.wishbone.burstType.poke(0.U)
      dut.io.nativeCommand.ready.poke(false.B)
      dut.io.nativeWriteData.ready.poke(false.B)
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.nativeReadData.bits.data.poke(0.U)

      do { dut.clock.step() }
      while (!dut.io.wishbone.acknowledge.peek().litToBoolean)
      dut.io.wishbone.cyc.poke(false.B)
      dut.io.wishbone.stb.poke(false.B)
      dut.clock.step(2)

      dut.io.nativeCommand.valid.expect(true.B)
      dut.io.nativeCommand.bits.address.expect(2.U)
      dut.io.nativeCommand.ready.poke(true.B)
      dut.clock.step()
      dut.io.nativeWriteData.valid.expect(true.B)
      dut.io.nativeWriteData.bits.data.expect("h0000aa00".U)
      dut.io.nativeWriteData.bits.byteEnable.expect("h2".U)
      dut.io.nativeWriteData.ready.poke(true.B)
      dut.clock.step()
    }
  }

  it should "reuse one Native read for all lanes of a narrow read burst" in {
    test(new WishboneToNativeWidthAdapter(cfg, wishboneDataBits = 8)) { dut =>
      dut.io.wishbone.cyc.poke(true.B)
      dut.io.wishbone.stb.poke(true.B)
      dut.io.wishbone.writeEnable.poke(false.B)
      dut.io.wishbone.writeData.poke(0.U)
      dut.io.wishbone.select.poke(1.U)
      dut.io.wishbone.burstType.poke(0.U)
      dut.io.nativeCommand.ready.poke(true.B)
      dut.io.nativeWriteData.ready.poke(true.B)
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.nativeReadData.bits.data.poke(0.U)

      dut.io.wishbone.address.poke(20.U)
      dut.io.wishbone.cycleType.poke(WishboneCycleType.incrementingBurst)
      dut.clock.step()
      dut.io.nativeCommand.valid.expect(true.B)
      dut.io.nativeCommand.bits.write.expect(false.B)
      dut.io.nativeCommand.bits.address.expect(5.U)
      dut.clock.step()
      dut.io.nativeReadData.valid.poke(true.B)
      dut.io.nativeReadData.bits.data.poke("hd4c3b2a1".U)
      dut.clock.step()
      dut.io.nativeReadData.valid.poke(false.B)
      dut.io.wishbone.acknowledge.expect(true.B)
      dut.io.wishbone.readData.expect("ha1".U)

      val expected = Seq(0xb2, 0xc3, 0xd4)
      for (lane <- 1 until 4) {
        dut.io.wishbone.address.poke((20 + lane).U)
        dut.io.wishbone.cycleType.poke(
          (if (lane == 3) WishboneCycleType.endOfBurst
           else WishboneCycleType.incrementingBurst))
        dut.clock.step()
        dut.io.nativeCommand.valid.expect(false.B)
        dut.clock.step()
        dut.io.wishbone.acknowledge.expect(true.B)
        dut.io.wishbone.readData.expect(expected(lane - 1).U)
      }
      dut.io.wishbone.cyc.poke(false.B)
      dut.io.wishbone.stb.poke(false.B)
      dut.clock.step()
      dut.io.nativeCommand.valid.expect(false.B)
    }
  }

  it should "preserve narrow burst data and Decoupled stability under random backpressure" in {
    test(new WishboneToNativeWidthAdapter(cfg, wishboneDataBits = 8)) { dut =>
      val random = new Random(0x5eed)
      val nativeMemory = mutable.Map.empty[Int, BigInt].withDefaultValue(BigInt(0))
      val expectedMemory = mutable.Map.empty[Int, BigInt].withDefaultValue(BigInt(0))
      val readResponses = mutable.Queue.empty[BigInt]
      var pendingWriteAddress = Option.empty[Int]
      var stalledCommand = Option.empty[(Boolean, BigInt)]
      var stalledWrite = Option.empty[(BigInt, BigInt)]

      dut.io.wishbone.cyc.poke(false.B)
      dut.io.wishbone.stb.poke(false.B)
      dut.io.wishbone.writeEnable.poke(false.B)
      dut.io.wishbone.address.poke(0.U)
      dut.io.wishbone.writeData.poke(0.U)
      dut.io.wishbone.select.poke(0.U)
      dut.io.wishbone.cycleType.poke(WishboneCycleType.classic)
      dut.io.wishbone.burstType.poke(0.U)

      def tick(): Unit = {
        val commandReady = random.nextBoolean()
        val writeReady = random.nextBoolean()
        val returnRead = readResponses.nonEmpty && random.nextBoolean()
        dut.io.nativeCommand.ready.poke(commandReady.B)
        dut.io.nativeWriteData.ready.poke(writeReady.B)
        dut.io.nativeReadData.valid.poke(returnRead.B)
        dut.io.nativeReadData.bits.data.poke(
          (if (returnRead) readResponses.front else BigInt(0)).U)

        val commandValid = dut.io.nativeCommand.valid.peek().litToBoolean
        val command = (dut.io.nativeCommand.bits.write.peek().litToBoolean,
          dut.io.nativeCommand.bits.address.peek().litValue)
        stalledCommand.foreach { held =>
          assert(commandValid, "Native command valid dropped while stalled")
          assert(command == held, "Native command changed while stalled")
        }
        stalledCommand =
          if (commandValid && !commandReady) Some(command) else None

        val writeValid = dut.io.nativeWriteData.valid.peek().litToBoolean
        val write = (dut.io.nativeWriteData.bits.data.peek().litValue,
          dut.io.nativeWriteData.bits.byteEnable.peek().litValue)
        stalledWrite.foreach { held =>
          assert(writeValid, "Native write valid dropped while stalled")
          assert(write == held, "Native write data changed while stalled")
        }
        stalledWrite = if (writeValid && !writeReady) Some(write) else None

        if (commandValid && commandReady) {
          val (isWrite, address) = command
          if (isWrite) {
            assert(pendingWriteAddress.isEmpty)
            pendingWriteAddress = Some(address.toInt)
          } else {
            readResponses.enqueue(nativeMemory(address.toInt))
          }
        }
        if (writeValid && writeReady) {
          val address = pendingWriteAddress.getOrElse(
            fail("Native write data arrived without a write command"))
          var value = nativeMemory(address)
          for (byte <- 0 until 4 if ((write._2 >> byte) & 1) != 0) {
            val byteMask = BigInt(0xff) << (8 * byte)
            value = (value & ~byteMask) | (write._1 & byteMask)
          }
          nativeMemory(address) = value
          pendingWriteAddress = None
        }
        if (returnRead) {
          assert(dut.io.nativeReadData.ready.peek().litToBoolean)
          readResponses.dequeue()
        }
        dut.clock.step()
      }

      def leaveAcknowledge(): Unit = tick()

      for (_ <- 0 until 80) {
        val writeBurst = random.nextBoolean()
        val wideAddress = random.nextInt(32)
        val firstLane = random.nextInt(4)
        val length = 1 + random.nextInt(4 - firstLane)
        dut.io.wishbone.cyc.poke(true.B)
        dut.io.wishbone.stb.poke(true.B)
        dut.io.wishbone.writeEnable.poke(writeBurst.B)

        for (offset <- 0 until length) {
          val lane = firstLane + offset
          val address = wideAddress * 4 + lane
          val data = random.nextInt(256)
          val select = if (random.nextInt(5) == 0) 0 else 1
          dut.io.wishbone.address.poke(address.U)
          dut.io.wishbone.writeData.poke(data.U)
          dut.io.wishbone.select.poke(select.U)
          dut.io.wishbone.cycleType.poke(
            (if (offset == length - 1) WishboneCycleType.endOfBurst
             else WishboneCycleType.incrementingBurst))

          var waitCycles = 0
          while (!dut.io.wishbone.acknowledge.peek().litToBoolean && waitCycles < 100) {
            tick()
            waitCycles += 1
          }
          assert(waitCycles < 100, s"Wishbone burst beat timed out at address $address")
          if (writeBurst) {
            if (select != 0) {
              val byteMask = BigInt(0xff) << (8 * lane)
              expectedMemory(wideAddress) =
                (expectedMemory(wideAddress) & ~byteMask) | (BigInt(data) << (8 * lane))
            }
          } else {
            val expected = (expectedMemory(wideAddress) >> (8 * lane)) & 0xff
            dut.io.wishbone.readData.expect(expected.U)
          }
          leaveAcknowledge()
        }

        dut.io.wishbone.cyc.poke(false.B)
        dut.io.wishbone.stb.poke(false.B)
        for (_ <- 0 until 24) tick()
        assert(pendingWriteAddress.isEmpty)
        assert(readResponses.isEmpty)
        for (address <- nativeMemory.keySet ++ expectedMemory.keySet)
          assert(nativeMemory(address) == expectedMemory(address),
            s"Native memory mismatch at word $address")
      }
    }
  }
}
