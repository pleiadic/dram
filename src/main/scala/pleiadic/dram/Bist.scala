package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/** Counter/ XNOR-feedback LFSR sequence used by LiteDRAM's BIST. */
class BistSequence(outputWidth: Int, stateWidth: Int = 31,
    taps: Seq[Int] = Seq(27, 30)) extends Module {
  require(outputWidth >= 1 && stateWidth >= 2)
  require(taps.nonEmpty && taps.forall(t => t >= 0 && t < stateWidth))

  val io = IO(new Bundle {
    val clear = Input(Bool())
    val advance = Input(Bool())
    val random = Input(Bool())
    val output = Output(UInt(outputWidth.W))
  })

  private val state = RegInit(0.U(stateWidth.W))
  private val counter = RegInit(0.U(outputWidth.W))
  private var current: Seq[Bool] = (0 until stateWidth).map(state(_)) ++
    Seq.fill((outputWidth - stateWidth).max(0))(false.B)
  for (_ <- 0 until outputWidth) {
    val feedback = !taps.map(current(_)).reduce(_ ^ _)
    current = feedback +: current.dropRight(1)
  }
  private val randomOutput = VecInit(current.take(outputWidth)).asUInt
  private val nextState = VecInit(current.take(stateWidth)).asUInt

  io.output := Mux(io.random, randomOutput, counter)
  when(io.clear) {
    state := 0.U
    counter := 0.U
  }.elsewhen(io.advance) {
    state := nextState
    counter := counter + 1.U
  }
}

private object BistPattern {
  def repeat31(value: UInt, width: Int): UInt = {
    val copies = (width + 30) / 31
    Fill(copies, value)(width - 1, 0)
  }
}

/** Configurable incremental/PRBS Native-memory BIST writer. */
class LiteDramBistGenerator(config: DramConfig, fifoDepth: Int = 16) extends Module {
  private val addressWidth = config.addressBits - config.byteOffsetBits

  val io = IO(new Bundle {
    val start = Input(Bool())
    val base = Input(UInt(addressWidth.W))
    val end = Input(UInt(addressWidth.W))
    val length = Input(UInt(addressWidth.W))
    val randomData = Input(Bool())
    val randomAddress = Input(Bool())
    val cascadeIn = Input(Bool())
    val cascadeOut = Output(Bool())
    val done = Output(Bool())
    val ticks = Output(UInt(32.W))
    val nativeCommand = Decoupled(new NativeCommand(config))
    val nativeWriteData = Decoupled(new NativeWriteData(config))
  })

  private val writer = Module(new LiteDramDmaWriter(config, fifoDepth))
  private val dataSequence = Module(new BistSequence(31))
  private val addressSequence = Module(new BistSequence(addressWidth))
  private val running = RegInit(false.B)
  private val draining = RegInit(false.B)
  private val done = RegInit(false.B)
  private val count = RegInit(0.U(addressWidth.W))
  private val ticks = RegInit(0.U(32.W))
  private val rangeMask = io.end - io.base - 1.U

  dataSequence.io.clear := io.start
  addressSequence.io.clear := io.start
  dataSequence.io.random := io.randomData
  addressSequence.io.random := io.randomAddress
  dataSequence.io.advance := writer.io.request.fire
  addressSequence.io.advance := writer.io.request.fire

  writer.io.request.valid := running && io.cascadeIn
  writer.io.request.bits.address := io.base + (addressSequence.io.output & rangeMask)
  writer.io.request.bits.data := BistPattern.repeat31(dataSequence.io.output, config.dataBits)
  writer.io.request.bits.byteEnable := Fill(config.dataBits / 8, 1.U(1.W))
  writer.io.request.bits.last := count === io.length - 1.U
  io.nativeCommand <> writer.io.nativeCommand
  io.nativeWriteData <> writer.io.nativeWriteData

  io.cascadeOut := writer.io.request.fire || done
  io.done := done
  io.ticks := ticks

  when(io.start) {
    assert(io.length =/= 0.U, "BIST length must be non-zero")
    assert(io.end > io.base, "BIST end must be above base")
    when(io.randomAddress) {
      val range = io.end - io.base
      assert((range & (range - 1.U)) === 0.U,
        "random-address BIST range must be a power of two")
    }
    running := true.B
    draining := false.B
    done := false.B
    count := 0.U
    ticks := 0.U
  }.otherwise {
    when(running) { ticks := ticks + 1.U }
    when(writer.io.request.fire) {
      count := count + 1.U
      when(count === io.length - 1.U) {
        running := false.B
        draining := true.B
      }
    }
    when(draining && !writer.io.busy) {
      draining := false.B
      done := true.B
    }
  }
}

/** Configurable incremental/PRBS Native-memory BIST reader/checker. */
class LiteDramBistChecker(config: DramConfig, fifoDepth: Int = 16) extends Module {
  private val addressWidth = config.addressBits - config.byteOffsetBits

  val io = IO(new Bundle {
    val start = Input(Bool())
    val base = Input(UInt(addressWidth.W))
    val end = Input(UInt(addressWidth.W))
    val length = Input(UInt(addressWidth.W))
    val randomData = Input(Bool())
    val randomAddress = Input(Bool())
    val cascadeIn = Input(Bool())
    val cascadeOut = Output(Bool())
    val done = Output(Bool())
    val ticks = Output(UInt(32.W))
    val errors = Output(UInt(32.W))
    val nativeCommand = Decoupled(new NativeCommand(config))
    val nativeReadData = Flipped(Decoupled(new NativeReadData(config)))
  })

  private val reader = Module(new LiteDramDmaReader(config, fifoDepth))
  private val dataSequence = Module(new BistSequence(31))
  private val addressSequence = Module(new BistSequence(addressWidth))
  private val running = RegInit(false.B)
  private val issued = RegInit(0.U(addressWidth.W))
  private val received = RegInit(0.U(addressWidth.W))
  private val ticks = RegInit(0.U(32.W))
  private val errors = RegInit(0.U(32.W))
  private val done = RegInit(false.B)
  private val rangeMask = io.end - io.base - 1.U

  addressSequence.io.clear := io.start
  addressSequence.io.random := io.randomAddress
  addressSequence.io.advance := reader.io.request.fire
  dataSequence.io.clear := io.start
  dataSequence.io.random := io.randomData
  dataSequence.io.advance := reader.io.data.fire

  reader.io.enable := running
  reader.io.request.valid := running && issued < io.length && io.cascadeIn
  reader.io.request.bits.address := io.base + (addressSequence.io.output & rangeMask)
  reader.io.request.bits.last := issued === io.length - 1.U
  io.cascadeOut := reader.io.request.fire || done
  io.nativeCommand <> reader.io.nativeCommand
  reader.io.nativeReadData <> io.nativeReadData

  reader.io.data.ready := running
  private val expectedData = BistPattern.repeat31(dataSequence.io.output, config.dataBits)
  io.done := done
  io.ticks := ticks
  io.errors := errors

  when(io.start) {
    assert(io.length =/= 0.U, "BIST length must be non-zero")
    assert(io.end > io.base, "BIST end must be above base")
    when(io.randomAddress) {
      val range = io.end - io.base
      assert((range & (range - 1.U)) === 0.U,
        "random-address BIST range must be a power of two")
    }
    running := true.B
    issued := 0.U
    received := 0.U
    ticks := 0.U
    errors := 0.U
    done := false.B
  }.otherwise {
    when(running) { ticks := ticks + 1.U }
    when(reader.io.request.fire) { issued := issued + 1.U }
    when(reader.io.data.fire) {
      when(reader.io.data.bits.data =/= expectedData) { errors := errors + 1.U }
      received := received + 1.U
      when(received === io.length - 1.U) {
        running := false.B
        done := true.B
      }
    }
  }
}

/** Writes a compile-time table of Native word addresses and data values. */
class LiteDramPatternGenerator(config: DramConfig,
    pattern: Seq[(BigInt, BigInt)], fifoDepth: Int = 16) extends Module {
  require(pattern.nonEmpty, "BIST pattern must contain at least one entry")
  private val addressWidth = config.addressBits - config.byteOffsetBits
  private val addressLimit = BigInt(1) << addressWidth
  private val dataLimit = BigInt(1) << config.dataBits
  require(pattern.forall { case (address, data) =>
    address >= 0 && address < addressLimit && data >= 0 && data < dataLimit
  }, "BIST pattern entries must fit the Native address and data widths")
  private val indexWidth = log2Ceil(pattern.length.max(2))

  val io = IO(new Bundle {
    val start = Input(Bool())
    val cascadeIn = Input(Bool())
    val cascadeOut = Output(Bool())
    val done = Output(Bool())
    val ticks = Output(UInt(32.W))
    val nativeCommand = Decoupled(new NativeCommand(config))
    val nativeWriteData = Decoupled(new NativeWriteData(config))
  })

  private val addresses = VecInit(pattern.map(_._1.U(addressWidth.W)))
  private val data = VecInit(pattern.map(_._2.U(config.dataBits.W)))
  private val writer = Module(new LiteDramDmaWriter(config, fifoDepth))
  private val index = RegInit(0.U(indexWidth.W))
  private val running = RegInit(false.B)
  private val draining = RegInit(false.B)
  private val done = RegInit(false.B)
  private val ticks = RegInit(0.U(32.W))
  private val last = index === (pattern.length - 1).U

  writer.io.request.valid := running && io.cascadeIn
  writer.io.request.bits.address := addresses(index)
  writer.io.request.bits.data := data(index)
  writer.io.request.bits.byteEnable := Fill(config.dataBits / 8, 1.U(1.W))
  writer.io.request.bits.last := last
  io.nativeCommand <> writer.io.nativeCommand
  io.nativeWriteData <> writer.io.nativeWriteData
  io.cascadeOut := writer.io.request.fire || done
  io.done := done
  io.ticks := ticks

  when(io.start) {
    index := 0.U
    running := true.B
    draining := false.B
    done := false.B
    ticks := 0.U
  }.otherwise {
    when(running) { ticks := ticks + 1.U }
    when(writer.io.request.fire) {
      when(last) {
        running := false.B
        draining := true.B
      }.otherwise {
        index := index + 1.U
      }
    }
    when(draining && !writer.io.busy) {
      draining := false.B
      done := true.B
    }
  }
}

/** Reads and checks a compile-time table in issue order. */
class LiteDramPatternChecker(config: DramConfig,
    pattern: Seq[(BigInt, BigInt)], fifoDepth: Int = 16) extends Module {
  require(pattern.nonEmpty, "BIST pattern must contain at least one entry")
  private val addressWidth = config.addressBits - config.byteOffsetBits
  private val addressLimit = BigInt(1) << addressWidth
  private val dataLimit = BigInt(1) << config.dataBits
  require(pattern.forall { case (address, data) =>
    address >= 0 && address < addressLimit && data >= 0 && data < dataLimit
  }, "BIST pattern entries must fit the Native address and data widths")
  private val indexWidth = log2Ceil(pattern.length.max(2))

  val io = IO(new Bundle {
    val start = Input(Bool())
    val cascadeIn = Input(Bool())
    val cascadeOut = Output(Bool())
    val done = Output(Bool())
    val ticks = Output(UInt(32.W))
    val errors = Output(UInt(32.W))
    val nativeCommand = Decoupled(new NativeCommand(config))
    val nativeReadData = Flipped(Decoupled(new NativeReadData(config)))
  })

  private val addresses = VecInit(pattern.map(_._1.U(addressWidth.W)))
  private val data = VecInit(pattern.map(_._2.U(config.dataBits.W)))
  private val reader = Module(new LiteDramDmaReader(config, fifoDepth))
  private val issueIndex = RegInit(0.U(indexWidth.W))
  private val checkIndex = RegInit(0.U(indexWidth.W))
  private val allIssued = RegInit(false.B)
  private val running = RegInit(false.B)
  private val done = RegInit(false.B)
  private val ticks = RegInit(0.U(32.W))
  private val errors = RegInit(0.U(32.W))
  private val lastIssue = issueIndex === (pattern.length - 1).U
  private val lastCheck = checkIndex === (pattern.length - 1).U

  reader.io.enable := running
  reader.io.request.valid := running && !allIssued && io.cascadeIn
  reader.io.request.bits.address := addresses(issueIndex)
  reader.io.request.bits.last := lastIssue
  io.nativeCommand <> reader.io.nativeCommand
  reader.io.nativeReadData <> io.nativeReadData
  reader.io.data.ready := running
  io.cascadeOut := reader.io.request.fire || done
  io.done := done
  io.ticks := ticks
  io.errors := errors

  when(io.start) {
    issueIndex := 0.U
    checkIndex := 0.U
    allIssued := false.B
    running := true.B
    done := false.B
    ticks := 0.U
    errors := 0.U
  }.otherwise {
    when(running) { ticks := ticks + 1.U }
    when(reader.io.request.fire) {
      when(lastIssue) {
        allIssued := true.B
      }.otherwise {
        issueIndex := issueIndex + 1.U
      }
    }
    when(reader.io.data.fire) {
      when(reader.io.data.bits.data =/= data(checkIndex)) { errors := errors + 1.U }
      when(lastCheck) {
        running := false.B
        done := true.B
      }.otherwise {
        checkIndex := checkIndex + 1.U
      }
    }
  }
}
