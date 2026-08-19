package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class LiteDramCoreSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 12, dataBits = 32, bankBits = 1,
    rowBits = 4, columnBits = 4, readTime = 2, writeTime = 2,
    timing = DramTiming(tRcd = 1, tRp = 1, tRas = 1, tRc = 1,
      tCcd = 1, tRrd = 1, tFaw = 8, tRefi = 100))

  behavior of "LiteDramCore"

  it should "carry a native write through the crossbar and controller" in {
    test(new LiteDramCore(cfg, 1)) { dut =>
      val master = dut.io.masters(0)
      master.flush.poke(false.B)
      master.readData.ready.poke(true.B)
      master.command.valid.poke(true.B)
      master.command.bits.address.poke(3.U)
      master.command.bits.write.poke(true.B)
      master.writeData.valid.poke(true.B)
      master.writeData.bits.data.poke("hcafebabe".U)
      master.writeData.bits.byteEnable.poke("hf".U)
      dut.io.command.ready.poke(true.B)
      dut.io.writeData.ready.poke(true.B)
      dut.io.readData.valid.poke(false.B)
      dut.io.readData.bits.data.poke(0.U)

      while (!master.command.ready.peek().litToBoolean) dut.clock.step()
      dut.clock.step()
      master.command.valid.poke(false.B)
      while (!master.writeData.ready.peek().litToBoolean) dut.clock.step()
      dut.clock.step()
      master.writeData.valid.poke(false.B)

      var sawWriteCommand = false
      var sawWriteData = false
      for (_ <- 0 until 24) {
        if (dut.io.command.valid.peek().litToBoolean &&
            dut.io.command.bits.command.peek().litValue == DramCommandType.write.litValue)
          sawWriteCommand = true
        if (dut.io.writeData.valid.peek().litToBoolean) {
          dut.io.writeData.bits.data.expect("hcafebabe".U)
          sawWriteData = true
        }
        dut.clock.step()
      }
      assert(sawWriteCommand && sawWriteData)
    }
  }
}
