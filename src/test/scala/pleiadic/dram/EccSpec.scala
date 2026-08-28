package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class SecDedHarness(dataBits: Int) extends Module {
  private val encodedBits = SecDed.encodedBits(dataBits)
  val io = IO(new Bundle {
    val input = Input(UInt(dataBits.W))
    val flip = Input(UInt(encodedBits.W))
    val enable = Input(Bool())
    val encoded = Output(UInt(encodedBits.W))
    val decoded = Output(UInt(dataBits.W))
    val singleError = Output(Bool())
    val doubleError = Output(Bool())
  })
  private val encoder = Module(new SecDedEncoder(dataBits))
  private val decoder = Module(new SecDedDecoder(dataBits))
  encoder.io.input := io.input
  decoder.io.input := encoder.io.output ^ io.flip
  decoder.io.enable := io.enable
  io.encoded := encoder.io.output
  io.decoded := decoder.io.output
  io.singleError := decoder.io.singleError
  io.doubleError := decoder.io.doubleError
}

class EccSpec extends AnyFlatSpec with ChiselScalatestTester {
  private def encodeLane(data: BigInt, dataBits: Int): BigInt = {
    val encodedBits = SecDed.encodedBits(dataBits)
    val codewordBits = encodedBits - 1
    val dataPositions = SecDed.dataPositions(codewordBits)
    val parityPositions = SecDed.parityPositions(codewordBits)
    var codeword = BigInt(0)
    for ((position, index) <- dataPositions.zipWithIndex)
      if (data.testBit(index)) codeword = codeword.setBit(position - 1)
    for (parity <- parityPositions) {
      val value = dataPositions.filter(position => (position & parity) != 0)
        .count(position => codeword.testBit(position - 1)) & 1
      if (value != 0) codeword = codeword.setBit(parity - 1)
    }
    (codeword << 1) | (codeword.bitCount & 1)
  }

  private def encodeWord(data: BigInt, dataBits: Int, burstCycles: Int): BigInt = {
    val laneBits = dataBits / burstCycles
    val codecBits = SecDed.encodedBits(laneBits)
    val encodedLaneBits = ((codecBits + 7) / 8) * 8
    (0 until burstCycles).foldLeft(BigInt(0)) { (encoded, lane) =>
      val laneData = (data >> (lane * laneBits)) & ((BigInt(1) << laneBits) - 1)
      encoded | (encodeLane(laneData, laneBits) << (lane * encodedLaneBits))
    }
  }

  behavior of "SEC-DED codec"

  it should "match 32-to-39 encoding and correct every codeword bit" in {
    test(new SecDedHarness(32)) { dut =>
      dut.io.enable.poke(true.B)
      for (pattern <- Seq(BigInt(0), BigInt(1), BigInt("deadbeef", 16), BigInt("ffffffff", 16))) {
        dut.io.input.poke(pattern.U)
        dut.io.flip.poke(0.U)
        dut.io.decoded.expect(pattern.U)
        dut.io.singleError.expect(false.B)
        dut.io.doubleError.expect(false.B)

        // Bit zero is LiteX's overall parity bit. Its corruption does not
        // alter data and is intentionally not reported as SEC.
        dut.io.flip.poke(1.U)
        dut.io.decoded.expect(pattern.U)
        dut.io.singleError.expect(false.B)
        dut.io.doubleError.expect(false.B)
        for (bit <- 1 until 39) {
          dut.io.flip.poke((BigInt(1) << bit).U)
          dut.io.decoded.expect(pattern.U)
          dut.io.singleError.expect(true.B)
          dut.io.doubleError.expect(false.B)
        }
      }

      dut.io.input.poke("h12345678".U)
      dut.io.flip.poke(((BigInt(1) << 1) | (BigInt(1) << 2)).U)
      dut.io.singleError.expect(false.B)
      dut.io.doubleError.expect(true.B)
      dut.io.enable.poke(false.B)
      dut.io.singleError.expect(false.B)
      dut.io.doubleError.expect(false.B)
    }
  }

  behavior of "LiteDramEccWrite"

  it should "encode eight lanes and expand byte enables per ECC word" in {
    test(new LiteDramEccWrite(dataBits = 256, burstCycles = 8)) { dut =>
      dut.io.input.valid.poke(true.B)
      dut.io.input.bits.data.poke(BigInt(
        "fedcba98765432100123456789abcdefdeadbeef55aa33cc1122334455667788", 16).U)
      dut.io.input.bits.byteEnable.poke(((BigInt(1) << 32) - 1).U)
      dut.io.output.ready.poke(false.B)
      dut.io.output.valid.expect(true.B)
      // LiteDRAM pads each 39-bit SEC-DED codeword to a 40-bit storage lane.
      dut.io.output.bits.byteEnable.expect(((BigInt(1) << 40) - 1).U)
      dut.io.writeEnableError.expect(false.B)

      // A partial mask in lane 2 expands to all five encoded bytes for that
      // lane and reports unsupported sub-ECC-word granularity.
      val partialMask = ((BigInt(1) << 32) - 1) ^ (BigInt(1) << 9)
      dut.io.input.bits.byteEnable.poke(partialMask.U)
      dut.io.output.bits.byteEnable.expect(((BigInt(1) << 40) - 1).U)
      dut.io.writeEnableError.expect(true.B)

      val laneZeroMask = partialMask & ~(((BigInt(1) << 4) - 1) << 8)
      dut.io.input.bits.byteEnable.poke(laneZeroMask.U)
      val encodedLaneMask = ((BigInt(1) << 40) - 1) & ~(((BigInt(1) << 5) - 1) << 10)
      dut.io.output.bits.byteEnable.expect(encodedLaneMask.U)
    }
  }

  behavior of "LiteDramEccRead"

  it should "report the lane containing a correctable or double error" in {
    test(new LiteDramEccRead(dataBits = 256, burstCycles = 8)) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.input.valid.poke(true.B)
      dut.io.output.ready.poke(true.B)
      dut.io.input.bits.data.poke((BigInt(1) << 5).U)
      dut.io.output.bits.data.expect(0.U)
      dut.io.singleErrors.expect(1.U)
      dut.io.doubleErrors.expect(0.U)

      dut.io.input.bits.data.poke(((BigInt(1) << (40 + 1)) |
        (BigInt(1) << (40 + 2))).U)
      dut.io.singleErrors.expect(0.U)
      dut.io.doubleErrors.expect(2.U)
    }
  }

  behavior of "LiteDramEccStatus"

  it should "count pulses and clear sticky detection" in {
    test(new LiteDramEccStatus) { dut =>
      dut.io.clear.poke(false.B)
      dut.io.singleError.poke(true.B)
      dut.io.doubleError.poke(false.B)
      dut.io.writeEnableError.poke(true.B)
      dut.clock.step(3)
      dut.io.singleCount.expect(3.U)
      dut.io.writeEnableCount.expect(3.U)
      dut.io.singleDetected.expect(true.B)
      dut.io.doubleDetected.expect(false.B)
      dut.io.singleError.poke(false.B)
      dut.io.doubleError.poke(true.B)
      dut.clock.step()
      dut.io.doubleCount.expect(1.U)
      dut.io.doubleDetected.expect(true.B)
      dut.io.clear.poke(true.B)
      dut.clock.step()
      dut.io.singleCount.expect(0.U)
      dut.io.doubleCount.expect(0.U)
      dut.io.writeEnableCount.expect(0.U)
      dut.io.singleDetected.expect(false.B)
      dut.io.doubleDetected.expect(false.B)
    }
  }

  behavior of "LiteDramEccNative"

  it should "buffer encoded traffic and count corrected read errors" in {
    test(new LiteDramEccNative(addressWidth = 8, dataBits = 256,
      burstCycles = 8, withErrorInjection = true)) { dut =>
      val data = BigInt(
        "fedcba98765432100123456789abcdefdeadbeef55aa33cc1122334455667788", 16)
      val encoded = encodeWord(data, 256, 8)
      val fullInputMask = (BigInt(1) << 32) - 1
      val fullEncodedMask = (BigInt(1) << 40) - 1

      dut.io.enable.poke(true.B)
      dut.io.clear.poke(false.B)
      dut.io.flip.poke(1.U)
      dut.io.inputCommand.valid.poke(true.B)
      dut.io.inputCommand.bits.write.poke(true.B)
      dut.io.inputCommand.bits.address.poke(5.U)
      dut.io.inputWriteData.valid.poke(true.B)
      dut.io.inputWriteData.bits.data.poke(data.U)
      dut.io.inputWriteData.bits.byteEnable.poke(fullInputMask.U)
      dut.io.outputReadData.ready.poke(false.B)
      dut.io.outputCommand.ready.poke(true.B)
      dut.io.outputWriteData.ready.poke(false.B)
      dut.io.inputReadData.valid.poke(false.B)
      dut.io.inputReadData.bits.data.poke(0.U)
      dut.clock.step()
      dut.io.inputCommand.valid.poke(false.B)
      dut.io.inputWriteData.valid.poke(false.B)

      dut.io.outputWriteData.valid.expect(true.B)
      dut.io.outputWriteData.bits.data.expect((encoded ^ 1).U)
      dut.io.outputWriteData.bits.byteEnable.expect(fullEncodedMask.U)
      dut.io.flip.poke(0.U)
      dut.clock.step(2)
      dut.io.outputWriteData.bits.data.expect((encoded ^ 1).U)
      dut.io.outputWriteData.ready.poke(true.B)
      dut.clock.step()

      dut.io.inputCommand.valid.poke(true.B)
      dut.io.inputCommand.bits.write.poke(false.B)
      dut.io.inputCommand.bits.address.poke(5.U)
      dut.io.inputReadData.valid.poke(true.B)
      dut.io.inputReadData.bits.data.poke((encoded ^ (BigInt(1) << 5)).U)
      dut.clock.step()
      dut.io.inputCommand.valid.poke(false.B)
      dut.io.inputReadData.valid.poke(false.B)
      dut.io.enable.poke(false.B)
      dut.io.outputReadData.valid.expect(true.B)
      dut.io.outputReadData.bits.data.expect(data.U)
      dut.io.singleCount.expect(1.U)
      dut.io.singleDetected.expect(true.B)
      dut.clock.step(2)
      dut.io.outputReadData.bits.data.expect(data.U)
      dut.io.outputReadData.ready.poke(true.B)
      dut.clock.step()

      dut.io.clear.poke(true.B)
      dut.clock.step()
      dut.io.singleCount.expect(0.U)
      dut.io.singleDetected.expect(false.B)
    }
  }

  it should "read merge and fully encode a partial write in RMW mode" in {
    test(new LiteDramEccNative(addressWidth = 8, dataBits = 256,
      burstCycles = 8, withReadModifyWrite = true)) { dut =>
      val oldData = BigInt(
        "00112233445566778899aabbccddeeff102132435465768798a9bacbdcedfe0f", 16)
      val newData = BigInt(
        "ffeeddccbbaa99887766554433221100deadbeefcafef00d0123456789abcdef", 16)
      val byteEnable = BigInt("00ff00ff", 16)
      val byteMask = (0 until 32).foldLeft(BigInt(0)) { (mask, byte) =>
        if (byteEnable.testBit(byte)) mask | (BigInt(0xff) << (8 * byte)) else mask
      }
      val merged = (oldData & ~byteMask) | (newData & byteMask)
      val encodedOld = encodeWord(oldData, 256, 8)
      val encodedMerged = encodeWord(merged, 256, 8)

      dut.io.enable.poke(true.B)
      dut.io.clear.poke(false.B)
      dut.io.flip.poke(0.U)
      dut.io.inputCommand.valid.poke(true.B)
      dut.io.inputCommand.bits.write.poke(true.B)
      dut.io.inputCommand.bits.address.poke(9.U)
      dut.io.inputWriteData.valid.poke(true.B)
      dut.io.inputWriteData.bits.data.poke(newData.U)
      dut.io.inputWriteData.bits.byteEnable.poke(byteEnable.U)
      dut.io.outputReadData.ready.poke(true.B)
      dut.io.outputCommand.ready.poke(true.B)
      dut.io.outputWriteData.ready.poke(false.B)
      dut.io.inputReadData.valid.poke(false.B)
      dut.io.inputReadData.bits.data.poke(0.U)

      var commandAccepted = false
      var writeDataAccepted = false
      var responsePending = false
      val commands = scala.collection.mutable.ArrayBuffer.empty[Boolean]
      var cycles = 0
      while ((!writeDataAccepted || !dut.io.outputWriteData.valid.peek().litToBoolean) &&
          cycles < 60) {
        if (responsePending) {
          dut.io.inputReadData.valid.poke(true.B)
          dut.io.inputReadData.bits.data.poke(encodedOld.U)
        }
        val inputCommandFire = !commandAccepted &&
          dut.io.inputCommand.ready.peek().litToBoolean
        val inputWriteFire = !writeDataAccepted &&
          dut.io.inputWriteData.ready.peek().litToBoolean
        val outputCommandFire = dut.io.outputCommand.valid.peek().litToBoolean &&
          dut.io.outputCommand.ready.peek().litToBoolean
        val inputReadFire = responsePending &&
          dut.io.inputReadData.ready.peek().litToBoolean
        if (outputCommandFire) {
          dut.io.outputCommand.bits.address.expect(9.U)
          val write = dut.io.outputCommand.bits.write.peek().litToBoolean
          commands += write
          if (!write) responsePending = true
        }
        dut.clock.step()
        if (inputCommandFire) {
          commandAccepted = true
          dut.io.inputCommand.valid.poke(false.B)
        }
        if (inputWriteFire) {
          writeDataAccepted = true
          dut.io.inputWriteData.valid.poke(false.B)
        }
        if (inputReadFire) {
          responsePending = false
          dut.io.inputReadData.valid.poke(false.B)
        }
        cycles += 1
      }
      assert(cycles < 60, "ECC RMW sequence timed out")
      assert(commands == Seq(false, true))
      assert(commandAccepted && writeDataAccepted)
      dut.io.outputWriteData.valid.expect(true.B)
      dut.io.outputWriteData.bits.data.expect(encodedMerged.U)
      dut.io.outputWriteData.bits.byteEnable.expect(((BigInt(1) << 40) - 1).U)
      dut.io.writeEnableError.expect(false.B)
      dut.io.writeEnableCount.expect(0.U)
      dut.clock.step(2)
      dut.io.outputWriteData.bits.data.expect(encodedMerged.U)
      dut.io.outputWriteData.ready.poke(true.B)
      dut.clock.step()
    }
  }
}
