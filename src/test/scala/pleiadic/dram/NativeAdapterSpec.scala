package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls

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
}
