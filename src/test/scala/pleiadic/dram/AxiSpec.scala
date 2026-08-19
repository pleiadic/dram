package pleiadic.dram

import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags}
import org.scalatest.flatspec.AnyFlatSpec
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
}
