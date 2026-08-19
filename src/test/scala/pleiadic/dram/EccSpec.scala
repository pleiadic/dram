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
      // LiteDRAM maps floor(39/8)=4 enable bits per 39-bit lane; the
      // remaining aggregate mask bits stay clear.
      dut.io.output.bits.byteEnable.expect(((BigInt(1) << 32) - 1).U)
      dut.io.writeEnableError.expect(false.B)

      // A partial mask in lane 2 expands to all five encoded bytes for that
      // lane and reports unsupported sub-ECC-word granularity.
      val partialMask = ((BigInt(1) << 32) - 1) ^ (BigInt(1) << 9)
      dut.io.input.bits.byteEnable.poke(partialMask.U)
      dut.io.output.bits.byteEnable.expect(((BigInt(1) << 32) - 1).U)
      dut.io.writeEnableError.expect(true.B)

      val laneZeroMask = partialMask & ~(((BigInt(1) << 4) - 1) << 8)
      dut.io.input.bits.byteEnable.poke(laneZeroMask.U)
      val encodedLaneMask = ((BigInt(1) << 32) - 1) & ~(((BigInt(1) << 4) - 1) << 8)
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

      dut.io.input.bits.data.poke(((BigInt(1) << (39 + 1)) |
        (BigInt(1) << (39 + 2))).U)
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
}
