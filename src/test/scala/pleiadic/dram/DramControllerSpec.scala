package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class DramControllerSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "DramController"

  it should "insert ACT before a row miss and return written data" in {
    val cfg = DramConfig(addressBits = 12, dataBits = 32, bankBits = 1, rowBits = 4, columnBits = 4,
      timing = DramTiming(tRcd = 2, tRp = 2, tCcd = 1, tRefi = 100))
    test(new DramController(cfg)) { dut =>
      dut.io.command.ready.poke(true.B)
      dut.io.response.ready.poke(true.B)
      dut.io.request.valid.poke(true.B)
      dut.io.request.bits.address.poke(0x24.U)
      dut.io.request.bits.write.poke(true.B)
      dut.io.request.bits.data.poke("h12345678".U)
      dut.io.request.bits.mask.poke("hf".U)
      while (!dut.io.request.ready.peek().litToBoolean) { dut.clock.step() }
      dut.clock.step()
      dut.io.request.valid.poke(false.B)
      var sawActivate = false
      var sawWrite = false
      for (_ <- 0 until 12) {
        if (dut.io.command.valid.peek().litToBoolean) {
          val c = dut.io.command.bits.command.peek().litValue
          sawActivate ||= c == DramCommandType.activate.litValue
          sawWrite ||= c == DramCommandType.write.litValue
        }
        dut.clock.step()
      }
      assert(sawActivate && sawWrite)

      dut.io.request.valid.poke(true.B)
      dut.io.request.bits.write.poke(false.B)
      while (!dut.io.request.ready.peek().litToBoolean) { dut.clock.step() }
      dut.clock.step()
      dut.io.request.valid.poke(false.B)
      var observed = false
      for (_ <- 0 until 12) {
        if (dut.io.response.valid.peek().litToBoolean) {
          assert(dut.io.response.bits.data.peek().litValue == BigInt("12345678", 16)); observed = true
        }
        dut.clock.step()
      }
      assert(observed)
    }
  }

  it should "generate a refresh command after the refresh interval" in {
    val cfg = DramConfig(addressBits = 10, dataBits = 32, bankBits = 1, rowBits = 3, columnBits = 3,
      timing = DramTiming(tRefi = 4, tRp = 1, tRfc = 2))
    test(new DramController(cfg)) { dut =>
      dut.io.command.ready.poke(true.B)
      dut.io.response.ready.poke(true.B)
      var sawRefresh = false
      for (_ <- 0 until 16) {
        if (dut.io.command.valid.peek().litToBoolean && dut.io.command.bits.command.peek().litValue == DramCommandType.refresh.litValue) sawRefresh = true
        dut.clock.step()
      }
      assert(sawRefresh)
    }
  }

  it should "precharge an open bank before activating a different row" in {
    val cfg = DramConfig(addressBits = 12, dataBits = 32, bankBits = 1, rowBits = 4, columnBits = 4,
      timing = DramTiming(tRcd = 1, tRp = 1, tRas = 1, tRc = 2, tCcd = 1, tRefi = 100))
    test(new DramController(cfg)) { dut =>
      dut.io.command.ready.poke(true.B)
      dut.io.response.ready.poke(true.B)

      def sendRead(address: Int): Seq[BigInt] = {
        dut.io.request.valid.poke(true.B)
        dut.io.request.bits.address.poke(address.U)
        dut.io.request.bits.write.poke(false.B)
        dut.io.request.bits.data.poke(0.U)
        dut.io.request.bits.mask.poke(0.U)
        while (!dut.io.request.ready.peek().litToBoolean) { dut.clock.step() }
        dut.clock.step()
        dut.io.request.valid.poke(false.B)
        val observed = collection.mutable.ArrayBuffer.empty[BigInt]
        for (_ <- 0 until 12) {
          if (dut.io.command.valid.peek().litToBoolean) observed += dut.io.command.bits.command.peek().litValue
          dut.clock.step()
        }
        observed.toSeq
      }

      sendRead(0x04)
      val commands = sendRead(0x84) // Same bank, next row according to the documented mapping.
      val precharge = commands.indexOf(DramCommandType.precharge.litValue)
      val activate = commands.indexOf(DramCommandType.activate.litValue)
      assert(precharge >= 0 && activate > precharge)
    }
  }
}
