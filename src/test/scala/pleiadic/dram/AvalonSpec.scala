package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls

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
        dut.clock.step()
      }
      dut.io.avalon.write.poke(false.B)
      assert(writeAddresses == Seq(4, 5, 6))

      dut.io.avalon.read.poke(true.B)
      dut.io.avalon.address.poke(9.U)
      dut.io.avalon.burstCount.poke(3.U)
      val readAddresses = ArrayBuffer.empty[BigInt]
      if (dut.io.nativeCommand.valid.peek().litToBoolean)
        readAddresses += dut.io.nativeCommand.bits.address.peek().litValue
      dut.clock.step()
      dut.io.avalon.read.poke(false.B)
      for (_ <- 0 until 3) {
        if (dut.io.nativeCommand.valid.peek().litToBoolean)
          readAddresses += dut.io.nativeCommand.bits.address.peek().litValue
        dut.clock.step()
      }
      assert(readAddresses == Seq(9, 10, 11))

      for (data <- Seq(0x11, 0x22, 0x33)) {
        dut.io.nativeReadData.valid.poke(true.B)
        dut.io.nativeReadData.bits.data.poke(data.U)
        dut.io.avalon.readDataValid.expect(true.B)
        dut.io.avalon.readData.expect(data.U)
        dut.clock.step()
      }
    }
  }
}
