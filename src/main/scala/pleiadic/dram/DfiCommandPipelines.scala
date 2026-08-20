package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/**
  * Merge per-phase LPDDR4 command expansions into one controller-cycle CA
  * vector. Commands occupy four consecutive phase slots and may cross a
  * controller-cycle boundary. Overlapping later commands are suppressed.
  */
class Lpddr4CommandPipeline(config: DramConfig, maskedWrite: Boolean = true,
    extendedOverlapCheck: Boolean = false) extends Module {
  require(config.nPhases >= 4, "LPDDR4 commands span as many as four phases")

  val io = IO(new Bundle {
    val dfi = Input(new DfiInterface(config))
    val cs = Output(UInt(config.nPhases.W))
    val ca = Output(Vec(6, UInt(config.nPhases.W)))
    val accepted = Output(UInt(config.nPhases.W))
  })

  private val adapters = Seq.tabulate(config.nPhases) { phase =>
    val adapter = Module(new Lpddr4DfiPhaseAdapter(config, maskedWrite))
    adapter.io.phase := io.dfi.phases(phase)
    adapter
  }
  private val rawValid = VecInit(adapters.map(_.io.valid))
  private val previousHistory = RegInit(0.U(config.nPhases.W))
  private val accepted = Wire(Vec(config.nPhases, Bool()))
  accepted.foreach(_ := false.B)

  for (phase <- 0 until config.nPhases) {
    val prior = (1 until 4).map { distance =>
      val index = phase - distance
      if (index >= 0) {
        if (extendedOverlapCheck) accepted(index) else rawValid(index)
      } else {
        previousHistory(config.nPhases + index)
      }
    }
    accepted(phase) := rawValid(phase) && !prior.reduce(_ || _)
  }
  previousHistory := (if (extendedOverlapCheck) accepted.asUInt else rawValid.asUInt)
  io.accepted := accepted.asUInt

  private val timelineWidth = 2 * config.nPhases
  private val csTimeline = RegInit(0.U(timelineWidth.W))
  private val caTimeline = RegInit(VecInit(Seq.fill(6)(0.U(timelineWidth.W))))
  var nextCs: UInt = Cat(0.U(config.nPhases.W),
    csTimeline(timelineWidth - 1, config.nPhases))
  val nextCa = Wire(Vec(6, UInt(timelineWidth.W)))

  for (line <- 0 until 6) {
    var value: UInt = Cat(0.U(config.nPhases.W),
      caTimeline(line)(timelineWidth - 1, config.nPhases))
    for (phase <- 0 until config.nPhases) {
      val sequence = VecInit((0 until 4).map(edge => adapters(phase).io.ca(edge)(line))).asUInt
      value = value | Mux(accepted(phase), (sequence << phase).pad(timelineWidth),
        0.U(timelineWidth.W))
    }
    nextCa(line) := value
  }
  for (phase <- 0 until config.nPhases) {
    nextCs = nextCs | Mux(accepted(phase),
      (adapters(phase).io.cs << phase).pad(timelineWidth), 0.U(timelineWidth.W))
  }
  csTimeline := nextCs
  caTimeline := nextCa

  io.cs := csTimeline(config.nPhases - 1, 0)
  for (line <- 0 until 6) {
    io.ca(line) := caTimeline(line)(config.nPhases - 1, 0)
  }
}

/**
  * LPDDR5 command scheduler matching LiteDRAM's one-entry second-command
  * buffer. A command arriving while the second half is emitted is dropped;
  * timing constraints are expected to prevent that condition.
  */
class Lpddr5CommandPipeline(config: DramConfig, maskedWrite: Boolean = true) extends Module {
  val io = IO(new Bundle {
    val phase = Input(new DfiPhase(config))
    val wckSyncDone = Input(Bool())
    val cs = Output(Bool())
    val ca = Output(Vec(2, UInt(7.W)))
    val valid = Output(Bool())
    val accepted = Output(Bool())
    val dropped = Output(Bool())
    val busy = Output(Bool())
    val wckSync = Output(UInt(2.W))
  })

  private val adapter = Module(new Lpddr5DfiPhaseAdapter(config, maskedWrite))
  adapter.io.phase := io.phase
  adapter.io.wckSyncDone := io.wckSyncDone

  private val bufferedValid = RegInit(false.B)
  private val bufferedCs = RegInit(false.B)
  private val bufferedCa = RegInit(VecInit(Seq.fill(2)(0.U(7.W))))
  private val accepted = adapter.io.valid && !bufferedValid

  when(bufferedValid) {
    bufferedValid := false.B
  }.elsewhen(adapter.io.valid) {
    bufferedValid := true.B
    bufferedCs := adapter.io.cs(1)
    bufferedCa(0) := adapter.io.ca(2)
    bufferedCa(1) := adapter.io.ca(3)
  }

  io.cs := Mux(bufferedValid, bufferedCs, adapter.io.cs(0))
  io.ca(0) := Mux(bufferedValid, bufferedCa(0), adapter.io.ca(0))
  io.ca(1) := Mux(bufferedValid, bufferedCa(1), adapter.io.ca(1))
  io.valid := bufferedValid || adapter.io.valid
  io.accepted := accepted
  io.dropped := bufferedValid && adapter.io.valid
  io.busy := bufferedValid
  io.wckSync := adapter.io.wckSync
}
