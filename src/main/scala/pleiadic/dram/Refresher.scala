package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/** Periodic timer used by refresh and calibration scheduling. */
class RefreshTimer(period: Int) extends Module {
  require(period >= 1)

  val io = IO(new Bundle {
    val waitEnable = Input(Bool())
    val done = Output(Bool())
    val count = Output(UInt(log2Ceil((period + 1) max 2).W))
  })

  private val count = RegInit((period - 1).U(log2Ceil((period + 1) max 2).W))
  private val done = count === 0.U

  when(io.waitEnable && !done) { count := count - 1.U }
    .otherwise { count := (period - 1).U }

  io.done := done
  io.count := count
}

/** Emits one pulse for each group of `postponing` input pulses. */
class RefreshPostponer(postponing: Int) extends Module {
  require(postponing >= 1 && postponing <= 8)

  val io = IO(new Bundle {
    val requestIn = Input(Bool())
    val requestOut = Output(Bool())
  })

  private val width = log2Ceil((postponing + 1) max 2)
  private val count = RegInit((postponing - 1).U(width.W))
  private val requestOut = RegInit(false.B)

  requestOut := false.B
  when(io.requestIn) {
    when(count === 0.U) {
      count := (postponing - 1).U
      requestOut := true.B
    }.otherwise { count := count - 1.U }
  }

  io.requestOut := requestOut
}

/**
  * Executes PRECHARGE-ALL, tRP, AUTO-REFRESH, tRFC `postponing` times.
  * Command bits remain stable under backpressure.
  */
class RefreshSequencer(config: DramConfig) extends Module {
  private val tRp = config.timing.tRp
  private val tRfc = config.timing.tRfc
  private val repetitions = config.refreshPostponing
  private val delayWidth = log2Ceil((tRp max tRfc) max 2)
  private val repeatWidth = log2Ceil((repetitions + 1) max 2)

  val io = IO(new Bundle {
    val start = Input(Bool())
    val command = Decoupled(new DramCommand(config))
    val busy = Output(Bool())
    val done = Output(Bool())
  })

  private val sIdle :: sPrecharge :: sWaitRp :: sRefresh :: sWaitRfc :: Nil = Enum(5)
  private val state = RegInit(sIdle)
  private val delay = RegInit(0.U(delayWidth.W))
  private val remaining = RegInit(0.U(repeatWidth.W))
  private val done = RegInit(false.B)

  done := false.B
  io.command.valid := false.B
  io.command.bits := 0.U.asTypeOf(new DramCommand(config))
  io.command.bits.rank := 0.U
  io.command.bits.allBanks := true.B
  io.busy := state =/= sIdle
  io.done := done

  switch(state) {
    is(sIdle) {
      when(io.start) {
        remaining := repetitions.U
        state := sPrecharge
      }
    }
    is(sPrecharge) {
      io.command.valid := true.B
      io.command.bits.command := DramCommandType.precharge
      when(io.command.fire) {
        if (tRp == 1) state := sRefresh
        else {
          delay := (tRp - 1).U
          state := sWaitRp
        }
      }
    }
    is(sWaitRp) {
      when(delay === 1.U) { state := sRefresh }
        .otherwise { delay := delay - 1.U }
    }
    is(sRefresh) {
      io.command.valid := true.B
      io.command.bits.command := DramCommandType.refresh
      when(io.command.fire) {
        if (tRfc == 1) {
          when(remaining === 1.U) {
            state := sIdle
            done := true.B
          }.otherwise {
            remaining := remaining - 1.U
            state := sPrecharge
          }
        } else {
          delay := (tRfc - 1).U
          state := sWaitRfc
        }
      }
    }
    is(sWaitRfc) {
      when(delay === 1.U) {
        when(remaining === 1.U) {
          state := sIdle
          done := true.B
        }.otherwise {
          remaining := remaining - 1.U
          state := sPrecharge
        }
      }.otherwise { delay := delay - 1.U }
    }
  }
}

/** Coordinates periodic requests with bank-machine grants and the sequencer. */
class Refresher(config: DramConfig) extends Module {
  val io = IO(new Bundle {
    val grant = Input(Bool())
    val command = Decoupled(new DramCommand(config))
    val request = Output(Bool())
    val busy = Output(Bool())
    val done = Output(Bool())
  })

  private val timer = Module(new RefreshTimer(config.timing.tRefi))
  private val postponer = Module(new RefreshPostponer(config.refreshPostponing))
  private val sequencer = Module(new RefreshSequencer(config))
  private val pending = RegInit(false.B)

  timer.io.waitEnable := true.B
  postponer.io.requestIn := timer.io.done
  when(postponer.io.requestOut) { pending := true.B }
  when(sequencer.io.done) { pending := false.B }

  sequencer.io.start := pending && io.grant && !sequencer.io.busy
  io.command <> sequencer.io.command
  io.request := pending
  io.busy := sequencer.io.busy
  io.done := sequencer.io.done
}
