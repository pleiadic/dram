package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

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
}
