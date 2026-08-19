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
      dut.io.wishbone.address.poke(3.U)
      dut.io.wishbone.writeData.poke("hdeadbeef".U)
      dut.io.wishbone.select.poke("hb".U)
      dut.clock.step()

      dut.io.nativeCommand.valid.expect(true.B)
      dut.io.nativeCommand.bits.address.expect((0x40 + 3).U)
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
}
