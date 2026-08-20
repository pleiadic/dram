package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/**
  * Enforces a minimum command-to-command distance.
  *
  * `valid` is an accepted timing event rather than a Decoupled valid. After an
  * event, `ready` remains low until `distance` cycles have elapsed. This is the
  * Chisel equivalent of LiteDRAM's tXXDController.
  */
class TxxdController(distance: Int) extends Module {
  require(distance >= 1)

  val io = IO(new Bundle {
    val valid = Input(Bool())
    val ready = Output(Bool())
  })

  private val width = log2Ceil(distance max 2)
  private val count = RegInit(0.U(width.W))
  // LiteDRAM intentionally starts blocked and lets the counter reach ready.
  private val ready = RegInit(false.B)

  when(io.valid) {
    count := (distance - 1).U
    ready := (distance == 1).B
  }.elsewhen(!ready) {
    count := count - 1.U
    when(count === 1.U) { ready := true.B }
  }

  io.ready := ready
}

/** Enforces no more than four activation events in a rolling tFAW window. */
class TfawController(windowCycles: Int) extends Module {
  require(windowCycles >= 1)

  val io = IO(new Bundle {
    val valid = Input(Bool())
    val ready = Output(Bool())
  })

  private val window = RegInit(0.U(windowCycles.W))
  private val ready = RegInit(true.B)
  private val count = PopCount(window)

  if (windowCycles == 1) {
    window := io.valid
  } else {
    window := Cat(window(windowCycles - 2, 0), io.valid)
  }

  when(count < 4.U) {
    when(count === 3.U) { ready := !io.valid }
      .otherwise { ready := true.B }
  }

  io.ready := ready
}

/** A cycle-accurate tapped delay line with tap 0 delayed by one cycle. */
class TappedDelayLine(width: Int = 1, tapCount: Int = 1) extends Module {
  require(width >= 1 && tapCount >= 1)

  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val taps = Output(Vec(tapCount, UInt(width.W)))
    val out = Output(UInt(width.W))
  })

  private val taps = RegInit(VecInit(Seq.fill(tapCount)(0.U(width.W))))
  taps(0) := io.in
  for (i <- 1 until tapCount) taps(i) := taps(i - 1)
  io.taps := taps
  io.out := taps.last
}

/**
  * Programmable word bitslip used by the generic PHY helpers.
  * The reset position matches LiteDRAM: `cycles * width - 1`.
  */
class BitSlip(width: Int, cycles: Int = 1) extends Module {
  require(width >= 1 && cycles >= 1)

  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val resetSlip = Input(Bool())
    val slip = Input(Bool())
    val out = Output(UInt(width.W))
  })

  private val positions = cycles * width
  private val positionWidth = log2Ceil((positions max 2))
  private val position = RegInit((positions - 1).U(positionWidth.W))
  private val history = RegInit(0.U(((cycles + 1) * width).W))

  history := Cat(io.in, history((cycles + 1) * width - 1, width))
  when(io.slip) {
    position := Mux(position === (positions - 1).U, 0.U, position + 1.U)
  }
  when(io.resetSlip) { position := (positions - 1).U }

  // The extra carry bit is required at the reset position: for a 16-bit
  // bitslip, 15 + 1 must select history[31:16], not wrap to a 4-bit zero.
  io.out := (history >> (position +& 1.U))(width - 1, 0)
}

/** DDR DQS pattern generator. Patterns are transmitted LSB first. */
class DqsPattern extends Module {
  val io = IO(new Bundle {
    val preamble = Input(Bool())
    val postamble = Input(Bool())
    val writeLevelingEnable = Input(Bool())
    val writeLevelingStrobe = Input(Bool())
    val out = Output(UInt(8.W))
  })

  io.out := "b01010101".U
  when(io.preamble) { io.out := "b00010101".U }
  when(io.postamble) { io.out := "b01010100".U }
  when(io.writeLevelingEnable) {
    io.out := Mux(io.writeLevelingStrobe, "b00000001".U, 0.U)
  }
}
