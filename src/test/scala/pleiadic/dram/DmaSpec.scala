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

  behavior of "LiteDramAxiDmaReader"

  it should "preserve request markers and AXI responses under independent AR/R backpressure" in {
    test(new LiteDramAxiDmaReader(addressWidth = 16, dataWidth = 32,
      idWidth = 2, fifoDepth = 4)) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.request.valid.poke(false.B)
      dut.io.request.bits.address.poke(0.U)
      dut.io.request.bits.last.poke(false.B)
      dut.io.data.ready.poke(false.B)
      dut.io.ar.ready.poke(false.B)
      dut.io.r.valid.poke(false.B)
      dut.io.r.bits.id.poke(0.U)
      dut.io.r.bits.data.poke(0.U)
      dut.io.r.bits.response.poke(AxiResponse.okay)
      dut.io.r.bits.last.poke(true.B)

      val requests = (0 until 18).map(i =>
        (BigInt(0x100 + 4 * i), i == 17, BigInt(0x80000000L + i), i % 7 == 3))
      val pending = scala.collection.mutable.Queue.empty[(BigInt, BigInt)]
      val addresses = ArrayBuffer.empty[BigInt]
      val received = ArrayBuffer.empty[(BigInt, BigInt, Boolean)]
      val random = new Random(0x41585244)
      var inputIndex = 0
      var holdingResponse = false
      var cycles = 0
      while (received.size < requests.size && cycles < 1000) {
        dut.io.request.valid.poke((inputIndex < requests.size).B)
        if (inputIndex < requests.size) {
          dut.io.request.bits.address.poke(requests(inputIndex)._1.U)
          dut.io.request.bits.last.poke(requests(inputIndex)._2.B)
        }
        dut.io.ar.ready.poke((random.nextInt(3) != 0).B)
        dut.io.data.ready.poke((random.nextInt(4) != 0).B)
        if (!holdingResponse && pending.nonEmpty && random.nextBoolean())
          holdingResponse = true
        dut.io.r.valid.poke(holdingResponse.B)
        if (pending.nonEmpty) {
          dut.io.r.bits.data.poke(pending.front._1.U)
          dut.io.r.bits.response.poke(pending.front._2.U)
        }

        val requestFire = dut.io.request.valid.peek().litToBoolean &&
          dut.io.request.ready.peek().litToBoolean
        val arFire = dut.io.ar.valid.peek().litToBoolean && dut.io.ar.ready.peek().litToBoolean
        val rFire = dut.io.r.valid.peek().litToBoolean && dut.io.r.ready.peek().litToBoolean
        val outputFire = dut.io.data.valid.peek().litToBoolean &&
          dut.io.data.ready.peek().litToBoolean
        if (arFire) {
          dut.io.ar.bits.id.expect(0.U)
          dut.io.ar.bits.length.expect(0.U)
          dut.io.ar.bits.size.expect(2.U)
          dut.io.ar.bits.burst.expect(AxiBurst.increment)
          addresses += dut.io.ar.bits.address.peek().litValue
        }
        val output = if (outputFire) Some((
          dut.io.data.bits.data.peek().litValue,
          dut.io.data.bits.response.peek().litValue,
          dut.io.data.bits.last.peek().litToBoolean)) else None
        dut.clock.step()
        if (requestFire) {
          val request = requests(inputIndex)
          val response = if (request._4) BigInt(2) else BigInt(0)
          pending.enqueue(request._3 -> response)
          inputIndex += 1
        }
        if (rFire) {
          pending.dequeue()
          holdingResponse = false
        }
        output.foreach(received += _)
        cycles += 1
      }
      assert(cycles < 1000)
      assert(addresses == requests.map(_._1))
      assert(received == requests.map { case (_, last, data, error) =>
        (data, if (error) BigInt(2) else BigInt(0), last)
      })
      dut.io.busy.expect(false.B)
    }
  }

  behavior of "LiteDramAxiDmaWriter"

  it should "couple AW reservations to W data and return every B response" in {
    test(new LiteDramAxiDmaWriter(addressWidth = 16, dataWidth = 32,
      idWidth = 2, fifoDepth = 5)) { dut =>
      dut.io.request.valid.poke(false.B)
      dut.io.request.bits.address.poke(0.U)
      dut.io.request.bits.data.poke(0.U)
      dut.io.request.bits.strobe.poke(0.U)
      dut.io.request.bits.last.poke(false.B)
      dut.io.response.ready.poke(false.B)
      dut.io.aw.ready.poke(false.B)
      dut.io.w.ready.poke(false.B)
      dut.io.b.valid.poke(false.B)
      dut.io.b.bits.id.poke(0.U)
      dut.io.b.bits.response.poke(AxiResponse.okay)

      val requests = (0 until 24).map(i =>
        (BigInt(0x200 + 4 * i), BigInt(0x50000000 + i), BigInt((i * 3) & 0xf)))
      val addresses = ArrayBuffer.empty[BigInt]
      val writes = ArrayBuffer.empty[(BigInt, BigInt)]
      val responses = ArrayBuffer.empty[BigInt]
      val pendingResponses = scala.collection.mutable.Queue.empty[BigInt]
      val random = new Random(0x41585744)
      var inputIndex = 0
      var paired = 0
      var holdingResponse = false
      var cycles = 0
      while (responses.size < requests.size && cycles < 1500) {
        dut.io.request.valid.poke((inputIndex < requests.size).B)
        if (inputIndex < requests.size) {
          val (address, data, strobe) = requests(inputIndex)
          dut.io.request.bits.address.poke(address.U)
          dut.io.request.bits.data.poke(data.U)
          dut.io.request.bits.strobe.poke(strobe.U)
          dut.io.request.bits.last.poke((inputIndex == requests.size - 1).B)
        }
        dut.io.aw.ready.poke((random.nextInt(3) != 0).B)
        dut.io.w.ready.poke(random.nextBoolean().B)
        dut.io.response.ready.poke((random.nextInt(4) != 0).B)
        if (!holdingResponse && pendingResponses.nonEmpty && random.nextBoolean())
          holdingResponse = true
        dut.io.b.valid.poke(holdingResponse.B)
        if (pendingResponses.nonEmpty)
          dut.io.b.bits.response.poke(pendingResponses.front.U)

        val requestFire = dut.io.request.valid.peek().litToBoolean &&
          dut.io.request.ready.peek().litToBoolean
        val awFire = dut.io.aw.valid.peek().litToBoolean && dut.io.aw.ready.peek().litToBoolean
        val wFire = dut.io.w.valid.peek().litToBoolean && dut.io.w.ready.peek().litToBoolean
        val bFire = dut.io.b.valid.peek().litToBoolean && dut.io.b.ready.peek().litToBoolean
        val responseFire = dut.io.response.valid.peek().litToBoolean &&
          dut.io.response.ready.peek().litToBoolean
        if (awFire) {
          dut.io.aw.bits.id.expect(0.U)
          dut.io.aw.bits.length.expect(0.U)
          dut.io.aw.bits.size.expect(2.U)
          addresses += dut.io.aw.bits.address.peek().litValue
        }
        if (wFire) {
          dut.io.w.bits.last.expect(true.B)
          writes += dut.io.w.bits.data.peek().litValue -> dut.io.w.bits.strobe.peek().litValue
        }
        val response = if (responseFire) Some(dut.io.response.bits.response.peek().litValue)
          else None
        dut.clock.step()
        if (requestFire) inputIndex += 1
        while (paired < addresses.size.min(writes.size)) {
          pendingResponses.enqueue(if (paired % 9 == 4) BigInt(2) else BigInt(0))
          paired += 1
        }
        if (bFire) {
          pendingResponses.dequeue()
          holdingResponse = false
        }
        response.foreach(responses += _)
        cycles += 1
      }
      assert(cycles < 1500)
      assert(addresses == requests.map(_._1))
      assert(writes == requests.map { case (_, data, strobe) => data -> strobe })
      assert(responses == requests.indices.map(i => if (i % 9 == 4) BigInt(2) else BigInt(0)))
      dut.io.busy.expect(false.B)
    }
  }

  behavior of "controlled AXI DMAs"

  it should "stride byte addresses by the AXI beat size" in {
    test(new LiteDramAxiDmaReaderControl(addressWidth = 16, dataWidth = 32,
      idWidth = 1, fifoDepth = 4)) { dut =>
      dut.io.enable.poke(false.B)
      dut.io.base.poke("h180".U)
      dut.io.length.poke(4.U)
      dut.io.loop.poke(false.B)
      dut.io.data.ready.poke(true.B)
      dut.io.ar.ready.poke(true.B)
      dut.io.r.valid.poke(false.B)
      dut.io.r.bits.id.poke(0.U)
      dut.io.r.bits.data.poke(0.U)
      dut.io.r.bits.response.poke(AxiResponse.okay)
      dut.io.r.bits.last.poke(true.B)
      dut.clock.step()
      dut.io.enable.poke(true.B)
      val addresses = ArrayBuffer.empty[BigInt]
      var cycles = 0
      while (!dut.io.done.peek().litToBoolean && cycles < 20) {
        if (dut.io.ar.valid.peek().litToBoolean) addresses +=
          dut.io.ar.bits.address.peek().litValue
        dut.clock.step()
        cycles += 1
      }
      assert(addresses == Seq(0x180, 0x184, 0x188, 0x18c).map(BigInt(_)))
      dut.io.offset.expect(3.U)
    }

    test(new LiteDramAxiDmaWriterControl(addressWidth = 16, dataWidth = 32,
      idWidth = 1, fifoDepth = 4)) { dut =>
      dut.io.enable.poke(false.B)
      dut.io.base.poke("h240".U)
      dut.io.length.poke(3.U)
      dut.io.loop.poke(false.B)
      dut.io.input.valid.poke(false.B)
      dut.io.input.bits.poke(0.U)
      dut.io.response.ready.poke(true.B)
      dut.io.aw.ready.poke(true.B)
      dut.io.w.ready.poke(true.B)
      dut.io.b.valid.poke(false.B)
      dut.io.b.bits.id.poke(0.U)
      dut.io.b.bits.response.poke(AxiResponse.okay)
      dut.clock.step()
      dut.io.enable.poke(true.B)
      val addresses = ArrayBuffer.empty[BigInt]
      val data = ArrayBuffer.empty[BigInt]
      var inputIndex = 0
      var cycles = 0
      while ((inputIndex < 3 || data.size < 3) && cycles < 30) {
        dut.io.input.valid.poke((inputIndex < 3).B)
        if (inputIndex < 3) dut.io.input.bits.poke((0xa0 + inputIndex).U)
        val inputFire = dut.io.input.valid.peek().litToBoolean &&
          dut.io.input.ready.peek().litToBoolean
        if (dut.io.aw.valid.peek().litToBoolean) addresses +=
          dut.io.aw.bits.address.peek().litValue
        if (dut.io.w.valid.peek().litToBoolean) {
          dut.io.w.bits.strobe.expect("hf".U)
          data += dut.io.w.bits.data.peek().litValue
        }
        dut.clock.step()
        if (inputFire) inputIndex += 1
        cycles += 1
      }
      assert(addresses == Seq(0x240, 0x244, 0x248).map(BigInt(_)))
      assert(data == Seq(0xa0, 0xa1, 0xa2).map(BigInt(_)))
      dut.io.done.expect(true.B)
      dut.io.offset.expect(2.U)
    }
  }
}
