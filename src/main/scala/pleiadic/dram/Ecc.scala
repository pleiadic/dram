package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

object SecDed {
  def syndromeBits(dataBits: Int): Int = {
    var bits = 1
    while ((1 << bits) < bits + dataBits + 1) bits += 1
    bits
  }
  def encodedBits(dataBits: Int): Int = dataBits + syndromeBits(dataBits) + 1
  private[dram] def parityPositions(codewordBits: Int): Seq[Int] =
    Iterator.iterate(1)(_ << 1).takeWhile(_ <= codewordBits).toSeq
  private[dram] def dataPositions(codewordBits: Int): Seq[Int] = {
    val parity = parityPositions(codewordBits).toSet
    (1 to codewordBits).filterNot(parity)
  }
}

object LiteDramEccNative {
  def encodedDataBits(dataBits: Int, burstCycles: Int): Int = {
    require(dataBits % burstCycles == 0)
    val codecBits = SecDed.encodedBits(dataBits / burstCycles)
    ((codecBits + 7) / 8) * 8 * burstCycles
  }
}

/** LiteX-compatible systematic extended-Hamming encoder (overall parity at bit 0). */
class SecDedEncoder(dataBits: Int) extends Module {
  private val encodedBits = SecDed.encodedBits(dataBits)
  private val codewordBits = encodedBits - 1
  private val parityPositions = SecDed.parityPositions(codewordBits)
  private val dataPositions = SecDed.dataPositions(codewordBits)
  require(dataPositions.size == dataBits)

  val io = IO(new Bundle {
    val input = Input(UInt(dataBits.W))
    val output = Output(UInt(encodedBits.W))
  })

  private val codeword = Wire(Vec(codewordBits, Bool()))
  codeword.foreach(_ := false.B)
  for ((position, index) <- dataPositions.zipWithIndex)
    codeword(position - 1) := io.input(index)
  for (parityPosition <- parityPositions) {
    val coveredData = dataPositions.filter(position => (position & parityPosition) != 0)
    codeword(parityPosition - 1) := coveredData
      .map(position => codeword(position - 1)).reduce(_ ^ _)
  }
  private val packed = codeword.asUInt
  io.output := Cat(packed, packed.xorR)
}

/** LiteX-compatible extended-Hamming decoder with SEC/DED status. */
class SecDedDecoder(dataBits: Int) extends Module {
  private val syndromeBits = SecDed.syndromeBits(dataBits)
  private val encodedBits = SecDed.encodedBits(dataBits)
  private val codewordBits = encodedBits - 1
  private val parityPositions = SecDed.parityPositions(codewordBits)
  private val dataPositions = SecDed.dataPositions(codewordBits)

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val input = Input(UInt(encodedBits.W))
    val output = Output(UInt(dataBits.W))
    val singleError = Output(Bool())
    val doubleError = Output(Bool())
  })

  private val codeword = io.input(encodedBits - 1, 1)
  private val syndromeVector = Wire(Vec(syndromeBits, Bool()))
  for ((parityPosition, index) <- parityPositions.zipWithIndex) {
    val covered = (1 to codewordBits).filter(position => (position & parityPosition) != 0)
    syndromeVector(index) := covered.map(position => codeword(position - 1)).reduce(_ ^ _)
  }
  private val rawSyndrome = syndromeVector.asUInt
  private val syndrome = Mux(io.enable, rawSyndrome, 0.U)
  private val correction = Mux(syndrome === 0.U, 0.U(codewordBits.W),
    (1.U(codewordBits.W) << (syndrome - 1.U))(codewordBits - 1, 0))
  private val corrected = codeword ^ correction
  private val outputBits = Wire(Vec(dataBits, Bool()))
  for ((position, index) <- dataPositions.zipWithIndex)
    outputBits(index) := corrected(position - 1)

  private val parity = io.input.xorR
  io.output := outputBits.asUInt
  io.singleError := io.enable && syndrome =/= 0.U && parity
  io.doubleError := io.enable && syndrome =/= 0.U && !parity
}

/** Parallel per-burst ECC encoding and byte-enable expansion. */
class LiteDramEccWrite(dataBits: Int, burstCycles: Int = 8) extends Module {
  require(dataBits % burstCycles == 0 && dataBits % 8 == 0)
  private val laneDataBits = dataBits / burstCycles
  private val laneCodecBits = SecDed.encodedBits(laneDataBits)
  private val laneEncodedBits = ((laneCodecBits + 7) / 8) * 8
  private val encodedBits = laneEncodedBits * burstCycles
  require(laneDataBits % 8 == 0 && encodedBits % 8 == 0,
    "ECC aggregate widths must preserve byte granularity")

  val io = IO(new Bundle {
    val input = Flipped(Decoupled(new NativeAdapterWriteData(dataBits)))
    val output = Decoupled(new NativeAdapterWriteData(encodedBits))
    val writeEnableError = Output(Bool())
  })

  io.output.valid := io.input.valid
  io.input.ready := io.output.ready
  io.writeEnableError := false.B
  private val encodedLanes = Wire(Vec(burstCycles, UInt(laneEncodedBits.W)))
  private val maskLanes = Wire(Vec(burstCycles, UInt((laneEncodedBits / 8).W)))
  io.output.bits.data := encodedLanes.asUInt
  io.output.bits.byteEnable := maskLanes.asUInt

  for (lane <- 0 until burstCycles) {
    val encoder = Module(new SecDedEncoder(laneDataBits))
    val dataLow = lane * laneDataBits
    val dataHigh = dataLow + laneDataBits - 1
    val inputByteLow = lane * (laneDataBits / 8)
    val inputByteHigh = inputByteLow + laneDataBits / 8 - 1
    val inputMask = io.input.bits.byteEnable(inputByteHigh, inputByteLow)
    encoder.io.input := io.input.bits.data(dataHigh, dataLow)
    encodedLanes(lane) := encoder.io.output
    maskLanes(lane) :=
      Mux(inputMask.orR, Fill(laneEncodedBits / 8, 1.U(1.W)), 0.U)
    when(io.input.valid && !inputMask.andR) { io.writeEnableError := true.B }
  }
}

/** Parallel per-burst ECC decoding with per-lane error pulses. */
class LiteDramEccRead(dataBits: Int, burstCycles: Int = 8) extends Module {
  require(dataBits % burstCycles == 0 && dataBits % 8 == 0)
  private val laneDataBits = dataBits / burstCycles
  private val laneCodecBits = SecDed.encodedBits(laneDataBits)
  private val laneEncodedBits = ((laneCodecBits + 7) / 8) * 8
  private val encodedBits = laneEncodedBits * burstCycles
  require(encodedBits % 8 == 0)

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val input = Flipped(Decoupled(new NativeAdapterReadData(encodedBits)))
    val output = Decoupled(new NativeAdapterReadData(dataBits))
    val singleErrors = Output(UInt(burstCycles.W))
    val doubleErrors = Output(UInt(burstCycles.W))
  })

  io.output.valid := io.input.valid
  io.input.ready := io.output.ready
  private val decodedLanes = Wire(Vec(burstCycles, UInt(laneDataBits.W)))
  io.output.bits.data := decodedLanes.asUInt
  private val single = Wire(Vec(burstCycles, Bool()))
  private val double = Wire(Vec(burstCycles, Bool()))
  for (lane <- 0 until burstCycles) {
    val decoder = Module(new SecDedDecoder(laneDataBits))
    val inputLow = lane * laneEncodedBits
    decoder.io.enable := io.enable
    decoder.io.input := io.input.bits.data(inputLow + laneCodecBits - 1, inputLow)
    decodedLanes(lane) := decoder.io.output
    single(lane) := io.output.valid && decoder.io.singleError
    double(lane) := io.output.valid && decoder.io.doubleError
  }
  io.singleErrors := single.asUInt
  io.doubleErrors := double.asUInt
}

/** Saturating error counters and sticky SEC/DED detection flags. */
class LiteDramEccStatus extends Module {
  val io = IO(new Bundle {
    val clear = Input(Bool())
    val singleError = Input(Bool())
    val doubleError = Input(Bool())
    val writeEnableError = Input(Bool())
    val singleDetected = Output(Bool())
    val doubleDetected = Output(Bool())
    val singleCount = Output(UInt(32.W))
    val doubleCount = Output(UInt(32.W))
    val writeEnableCount = Output(UInt(32.W))
  })

  private val singleCount = RegInit(0.U(32.W))
  private val doubleCount = RegInit(0.U(32.W))
  private val writeEnableCount = RegInit(0.U(32.W))
  private val singleDetected = RegInit(false.B)
  private val doubleDetected = RegInit(false.B)
  when(io.clear) {
    singleCount := 0.U
    doubleCount := 0.U
    writeEnableCount := 0.U
    singleDetected := false.B
    doubleDetected := false.B
  }.otherwise {
    when(io.singleError) {
      singleDetected := true.B
      when(!singleCount.andR) { singleCount := singleCount + 1.U }
    }
    when(io.doubleError) {
      doubleDetected := true.B
      when(!doubleCount.andR) { doubleCount := doubleCount + 1.U }
    }
    when(io.writeEnableError && !writeEnableCount.andR) {
      writeEnableCount := writeEnableCount + 1.U
    }
  }
  io.singleDetected := singleDetected
  io.doubleDetected := doubleDetected
  io.singleCount := singleCount
  io.doubleCount := doubleCount
  io.writeEnableCount := writeEnableCount
}

/**
  * Integrated LiteDRAM Native ECC datapath. Commands retain their logical word
  * address while write/read payloads expand/contract between the logical and
  * encoded widths. Optional RMW converts partial logical writes into full-word
  * writes before encoding, matching ECC memories without sub-word updates.
  */
class LiteDramEccNative(addressWidth: Int, dataBits: Int, burstCycles: Int = 8,
    withReadModifyWrite: Boolean = false, withErrorInjection: Boolean = false)
    extends Module {
  require(addressWidth >= 1 && dataBits >= 8 && dataBits % 8 == 0)
  private val encodedBits = LiteDramEccNative.encodedDataBits(dataBits, burstCycles)
  require(encodedBits % 8 == 0, "encoded Native width must preserve byte granularity")

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val clear = Input(Bool())
    val flip = Input(UInt(8.W))
    val inputCommand = Flipped(Decoupled(new NativeAdapterCommand(addressWidth)))
    val inputWriteData = Flipped(Decoupled(new NativeAdapterWriteData(dataBits)))
    val outputReadData = Decoupled(new NativeAdapterReadData(dataBits))
    val outputCommand = Decoupled(new NativeAdapterCommand(addressWidth))
    val outputWriteData = Decoupled(new NativeAdapterWriteData(encodedBits))
    val inputReadData = Flipped(Decoupled(new NativeAdapterReadData(encodedBits)))
    val singleErrors = Output(UInt(burstCycles.W))
    val doubleErrors = Output(UInt(burstCycles.W))
    val writeEnableError = Output(Bool())
    val singleDetected = Output(Bool())
    val doubleDetected = Output(Bool())
    val singleCount = Output(UInt(32.W))
    val doubleCount = Output(UInt(32.W))
    val writeEnableCount = Output(UInt(32.W))
  })

  private val encoder = Module(new LiteDramEccWrite(dataBits, burstCycles))
  private val decoder = Module(new LiteDramEccRead(dataBits, burstCycles))
  private val writeBuffer = Module(new Queue(
    new NativeAdapterWriteData(encodedBits), 1, pipe = false, flow = false))
  private val readBuffer = Module(new Queue(
    new NativeAdapterReadData(dataBits), 1, pipe = false, flow = false))
  private val status = Module(new LiteDramEccStatus)

  if (withReadModifyWrite) {
    val rmw = Module(new NativeAdapterReadModifyWrite(addressWidth, dataBits))
    rmw.io.inputCommand <> io.inputCommand
    rmw.io.inputWriteData <> io.inputWriteData
    io.outputReadData <> rmw.io.outputReadData
    io.outputCommand <> rmw.io.outputCommand
    encoder.io.input <> rmw.io.outputWriteData
    rmw.io.inputReadData <> readBuffer.io.deq
  } else {
    io.outputCommand <> io.inputCommand
    encoder.io.input <> io.inputWriteData
    io.outputReadData <> readBuffer.io.deq
  }

  writeBuffer.io.enq.valid := encoder.io.output.valid
  encoder.io.output.ready := writeBuffer.io.enq.ready
  writeBuffer.io.enq.bits.data := encoder.io.output.bits.data ^
    Mux(withErrorInjection.B, io.flip, 0.U)
  writeBuffer.io.enq.bits.byteEnable := encoder.io.output.bits.byteEnable
  io.outputWriteData <> writeBuffer.io.deq
  decoder.io.input <> io.inputReadData
  decoder.io.enable := io.enable
  readBuffer.io.enq <> decoder.io.output

  io.singleErrors := decoder.io.singleErrors
  io.doubleErrors := decoder.io.doubleErrors
  io.writeEnableError := encoder.io.writeEnableError
  status.io.clear := io.clear
  status.io.singleError := decoder.io.output.fire && decoder.io.singleErrors.orR
  status.io.doubleError := decoder.io.output.fire && decoder.io.doubleErrors.orR
  status.io.writeEnableError := encoder.io.input.fire && encoder.io.writeEnableError
  io.singleDetected := status.io.singleDetected
  io.doubleDetected := status.io.doubleDetected
  io.singleCount := status.io.singleCount
  io.doubleCount := status.io.doubleCount
  io.writeEnableCount := status.io.writeEnableCount

  if (!withErrorInjection) dontTouch(io.flip)
}
