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

/** Executes PRECHARGE-ALL, tRP, ZQ short calibration, then waits tZQCS. */
class ZqCalibrationSequencer(config: DramConfig) extends Module {
  private val tRp = config.timing.tRp
  private val tZqcs = config.timing.tZqcs.getOrElse(
    throw new IllegalArgumentException("ZQ calibration sequencer requires tZqcs"))
  private val delayWidth = log2Ceil((tRp max tZqcs) max 2)

  val io = IO(new Bundle {
    val start = Input(Bool())
    val command = Decoupled(new DramCommand(config))
    val busy = Output(Bool())
    val done = Output(Bool())
  })

  private val sIdle :: sPrecharge :: sWaitRp :: sCalibration :: sWaitZqcs :: Nil = Enum(5)
  private val state = RegInit(sIdle)
  private val delay = RegInit(0.U(delayWidth.W))
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
      when(io.start) { state := sPrecharge }
    }
    is(sPrecharge) {
      io.command.valid := true.B
      io.command.bits.command := DramCommandType.precharge
      when(io.command.fire) {
        if (tRp == 1) state := sCalibration
        else {
          delay := (tRp - 1).U
          state := sWaitRp
        }
      }
    }
    is(sWaitRp) {
      when(delay === 1.U) { state := sCalibration }
        .otherwise { delay := delay - 1.U }
    }
    is(sCalibration) {
      io.command.valid := true.B
      io.command.bits.command := DramCommandType.zqCalibration
      when(io.command.fire) {
        if (tZqcs == 1) {
          state := sIdle
          done := true.B
        } else {
          delay := (tZqcs - 1).U
          state := sWaitZqcs
        }
      }
    }
    is(sWaitZqcs) {
      when(delay === 1.U) {
        state := sIdle
        done := true.B
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
  private val zqSequencer = config.zqCalibrationPeriodCycles.map { _ =>
    Module(new ZqCalibrationSequencer(config))
  }
  private val pending = RegInit(false.B)
  private val zqPending = RegInit(false.B)

  private val sIdle :: sWaitGrant :: sRefresh :: sZqCalibration :: Nil = Enum(4)
  private val state = RegInit(sIdle)
  private val done = RegInit(false.B)

  timer.io.waitEnable := true.B
  postponer.io.requestIn := timer.io.done
  when(postponer.io.requestOut) { pending := true.B }
  done := false.B

  private val zqRequested = WireDefault(false.B)
  zqSequencer.zip(config.zqCalibrationPeriodCycles).foreach { case (calibration, period) =>
    val zqTimer = Module(new RefreshTimer(period))
    zqTimer.io.waitEnable := true.B
    when(zqTimer.io.done) { zqPending := true.B }
    zqRequested := zqPending || zqTimer.io.done
    calibration.io.start := false.B
    calibration.io.command.ready := false.B
  }

  sequencer.io.start := false.B
  sequencer.io.command.ready := false.B
  io.command.valid := false.B
  io.command.bits := 0.U.asTypeOf(new DramCommand(config))

  switch(state) {
    is(sIdle) {
      when(pending) { state := sWaitGrant }
    }
    is(sWaitGrant) {
      when(io.grant) {
        sequencer.io.start := true.B
        state := sRefresh
      }
    }
    is(sRefresh) {
      io.command <> sequencer.io.command
      when(sequencer.io.done) {
        zqSequencer match {
          case Some(calibration) =>
            when(zqRequested) {
              calibration.io.start := true.B
              zqPending := false.B
              state := sZqCalibration
            }.otherwise {
              pending := false.B
              done := true.B
              state := sIdle
            }
          case None =>
            pending := false.B
            done := true.B
            state := sIdle
        }
      }
    }
    is(sZqCalibration) {
      zqSequencer.foreach { calibration =>
        io.command <> calibration.io.command
        when(calibration.io.done) {
          pending := false.B
          done := true.B
          state := sIdle
        }
      }
    }
  }

  io.request := pending || state =/= sIdle
  io.busy := state === sRefresh || state === sZqCalibration
  io.done := done
}
