package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/**
  * LiteDRAM-style pair of arbiters for multi-phase operation.
  *
  * Row commands (ACT/PRE) and the selected read/write request are disjoint, so
  * they may be accepted from different banks in the same controller cycle.
  */
class DualCommandChooser(config: DramConfig, requestCount: Int) extends Module {
  require(requestCount >= 1)

  val io = IO(new Bundle {
    val requests = Vec(requestCount, Flipped(Decoupled(new DramCommand(config))))
    val rowCommand = Decoupled(new DramCommand(config))
    val columnRequest = Decoupled(new DramCommand(config))
    val wantReads = Input(Bool())
    val wantWrites = Input(Bool())
    val wantActivates = Input(Bool())
  })

  private val rows = Module(new CommandChooser(config, requestCount))
  private val columns = Module(new CommandChooser(config, requestCount))

  for (index <- 0 until requestCount) {
    rows.io.requests(index).valid := io.requests(index).valid
    rows.io.requests(index).bits := io.requests(index).bits
    columns.io.requests(index).valid := io.requests(index).valid
    columns.io.requests(index).bits := io.requests(index).bits
    io.requests(index).ready := rows.io.requests(index).ready ||
      columns.io.requests(index).ready
  }

  rows.io.wantReads := false.B
  rows.io.wantWrites := false.B
  rows.io.wantCommands := true.B
  rows.io.wantActivates := io.wantActivates
  columns.io.wantReads := io.wantReads
  columns.io.wantWrites := io.wantWrites
  columns.io.wantCommands := false.B
  columns.io.wantActivates := false.B

  io.rowCommand <> rows.io.output
  io.columnRequest <> columns.io.output
}

/**
  * Registers independently accepted CMD/REQ streams onto LiteDRAM DFI phases.
  * The column request uses readPhase/writePhase and its paired row command uses
  * the immediately preceding phase modulo nPhases. Refresh owns phase zero.
  */
class DfiPhaseSteerer(config: DramConfig) extends Module {
  val io = IO(new Bundle {
    val rowCommand = Flipped(Decoupled(new DramCommand(config)))
    val columnRequest = Flipped(Decoupled(new DramCommand(config)))
    val refreshCommand = Flipped(Decoupled(new DramCommand(config)))
    val writeMode = Input(Bool())
    val refreshMode = Input(Bool())
    val dfi = Output(new DfiInterface(config))
  })

  private def idleDfi(): DfiInterface = {
    val value = WireDefault(0.U.asTypeOf(new DfiInterface(config)))
    for (phase <- value.phases) {
      phase.csN.foreach(_ := true.B)
      phase.rasN := true.B
      phase.casN := true.B
      phase.weN := true.B
      phase.actN := true.B
      phase.cke.foreach(_ := true.B)
      phase.odt.foreach(_ := true.B)
      phase.resetN := true.B
    }
    value
  }

  private val nextDfi = idleDfi()
  private val dfiRegister = Reg(new DfiInterface(config))
  when(reset.asBool) { dfiRegister := idleDfi() }
    .otherwise { dfiRegister := nextDfi }
  io.dfi := dfiRegister

  private val dataPhaseWidth = log2Ceil(config.nPhases max 2)
  private val dataPhase = Wire(UInt(dataPhaseWidth.W))
  dataPhase := Mux(io.writeMode, config.writePhase.U, config.readPhase.U)
  private val rowPhase = Mux(dataPhase === 0.U,
    (config.nPhases - 1).U, dataPhase - 1.U)

  if (config.nPhases == 1) {
    // A single DFI phase cannot carry both streams; preserve the reference
    // behavior of serializing the selected row command before the request.
    io.rowCommand.ready := !io.refreshMode
    io.columnRequest.ready := !io.refreshMode && !io.rowCommand.valid
  } else {
    io.rowCommand.ready := !io.refreshMode
    io.columnRequest.ready := !io.refreshMode
  }
  io.refreshCommand.ready := io.refreshMode

  private def drive(phase: DfiPhase, command: DramCommand): Unit = {
    phase.bank := command.bank
    phase.address := Mux(command.command === DramCommandType.activate,
      command.row, command.column)
    when(command.allBanks) {
      phase.csN.foreach(_ := false.B)
    }.otherwise {
      if (config.nranks == 1) phase.csN.head := false.B
      else phase.csN(command.rank) := false.B
    }

    switch(command.command) {
      is(DramCommandType.activate) {
        phase.rasN := false.B
      }
      is(DramCommandType.precharge) {
        phase.rasN := false.B
        phase.weN := false.B
        when(command.allBanks) { phase.address := (1 << 10).U }
      }
      is(DramCommandType.read) {
        phase.casN := false.B
        phase.rddataEn := true.B
        when(command.autoPrecharge) {
          phase.address := command.column | (1 << 10).U
        }
      }
      is(DramCommandType.write) {
        phase.casN := false.B
        phase.weN := false.B
        phase.wrdataEn := true.B
        when(command.autoPrecharge) {
          phase.address := command.column | (1 << 10).U
        }
      }
      is(DramCommandType.refresh) {
        phase.rasN := false.B
        phase.casN := false.B
      }
      is(DramCommandType.zqCalibration) {
        phase.weN := false.B
      }
    }
  }

  // Keep write payloads phase-aligned even though their ownership moves to the
  // dedicated data path when the controller is integrated in P5.
  for (phase <- 0 until config.nPhases) {
    val dataLow = phase * config.dfiDataBits
    val maskLow = phase * (config.dfiDataBits / 8)
    if (dataLow + config.dfiDataBits <= config.dataBits) {
      nextDfi.phases(phase).wrdata :=
        io.columnRequest.bits.data(dataLow + config.dfiDataBits - 1, dataLow)
      nextDfi.phases(phase).wrdataMask :=
        ~io.columnRequest.bits.mask(maskLow + config.dfiDataBits / 8 - 1, maskLow)
    }
  }

  when(io.refreshCommand.fire) {
    drive(nextDfi.phases.head, io.refreshCommand.bits)
  }.otherwise {
    for (phase <- 0 until config.nPhases) {
      when(io.rowCommand.fire && rowPhase === phase.U) {
        drive(nextDfi.phases(phase), io.rowCommand.bits)
      }
      when(io.columnRequest.fire && dataPhase === phase.U) {
        drive(nextDfi.phases(phase), io.columnRequest.bits)
      }
    }
  }
}

/** Complete multi-phase control-path multiplexer with independent CMD/REQ issue. */
class DfiMultiplexer(config: DramConfig, bankCount: Int) extends Module {
  require(bankCount >= 1)

  val io = IO(new Bundle {
    val bankCommands = Vec(bankCount, Flipped(Decoupled(new DramCommand(config))))
    val refreshCommand = Flipped(Decoupled(new DramCommand(config)))
    val refreshMode = Input(Bool())
    val dfi = Output(new DfiInterface(config))
    val servingWrites = Output(Bool())
  })

  private val chooser = Module(new DualCommandChooser(config, bankCount))
  private val steerer = Module(new DfiPhaseSteerer(config))
  for (index <- 0 until bankCount) chooser.io.requests(index) <> io.bankCommands(index)

  private val trrd = Module(new TxxdController(config.timing.tRrd))
  private val tfaw = Module(new TfawController(config.timing.tFaw))
  private val tccd = Module(new TxxdController(config.timing.tCcd))
  private val twtr = Module(new TxxdController(
    config.timing.tWtr + config.writeLatency + config.timing.tCcd))

  private val sRead :: sReadToWrite :: sWrite :: sWriteToRead :: sRefresh :: Nil = Enum(5)
  private val state = RegInit(sRead)
  private val maxServiceTime = (config.readTime max config.writeTime) max 1
  private val serviceWidth = log2Ceil((maxServiceTime + 1) max 2)
  private val serviceTimer = RegInit(0.U(serviceWidth.W))
  private val turnaroundWidth = log2Ceil((config.readLatency + 1) max 2)
  private val turnaround = RegInit(0.U(turnaroundWidth.W))

  private val readAvailable = io.bankCommands.map(port =>
    port.valid && port.bits.command === DramCommandType.read).reduce(_ || _)
  private val writeAvailable = io.bankCommands.map(port =>
    port.valid && port.bits.command === DramCommandType.write).reduce(_ || _)

  chooser.io.wantReads := state === sRead
  chooser.io.wantWrites := state === sWrite
  chooser.io.wantActivates := trrd.io.ready && tfaw.io.ready

  private val rowIsActivate = chooser.io.rowCommand.bits.command ===
    DramCommandType.activate
  private val rowAllowed = !rowIsActivate || (trrd.io.ready && tfaw.io.ready)
  private val columnIsRead = chooser.io.columnRequest.bits.command ===
    DramCommandType.read
  private val columnAllowed = tccd.io.ready && (!columnIsRead || twtr.io.ready)

  steerer.io.rowCommand.valid := chooser.io.rowCommand.valid && rowAllowed
  steerer.io.rowCommand.bits := chooser.io.rowCommand.bits
  chooser.io.rowCommand.ready := steerer.io.rowCommand.ready && rowAllowed
  steerer.io.columnRequest.valid := chooser.io.columnRequest.valid && columnAllowed
  steerer.io.columnRequest.bits := chooser.io.columnRequest.bits
  chooser.io.columnRequest.ready := steerer.io.columnRequest.ready && columnAllowed
  steerer.io.refreshCommand <> io.refreshCommand
  steerer.io.writeMode := state === sWrite
  steerer.io.refreshMode := state === sRefresh
  io.dfi := steerer.io.dfi
  io.servingWrites := state === sWrite

  private val acceptedRow = chooser.io.rowCommand.fire
  private val acceptedColumn = chooser.io.columnRequest.fire
  trrd.io.valid := acceptedRow && rowIsActivate
  tfaw.io.valid := acceptedRow && rowIsActivate
  tccd.io.valid := acceptedColumn
  twtr.io.valid := acceptedColumn &&
    chooser.io.columnRequest.bits.command === DramCommandType.write

  switch(state) {
    is(sRead) {
      when(io.refreshMode) { state := sRefresh }
        .elsewhen(writeAvailable && (!readAvailable || serviceTimer === 0.U)) {
          if (config.readLatency == 1) {
            serviceTimer := (config.writeTime max 1).U - 1.U
            state := sWrite
          } else {
            turnaround := (config.readLatency - 1).U
            state := sReadToWrite
          }
        }
      if (config.readTime > 0) {
        when(serviceTimer === 0.U) { serviceTimer := (config.readTime - 1).U }
          .otherwise { serviceTimer := serviceTimer - 1.U }
      }
    }
    is(sReadToWrite) {
      when(io.refreshMode) { state := sRefresh }
        .elsewhen(turnaround <= 1.U) {
          serviceTimer := (config.writeTime max 1).U - 1.U
          state := sWrite
        }.otherwise { turnaround := turnaround - 1.U }
    }
    is(sWrite) {
      when(io.refreshMode) { state := sRefresh }
        .elsewhen(readAvailable && (!writeAvailable || serviceTimer === 0.U)) {
          state := sWriteToRead
        }
      if (config.writeTime > 0) {
        when(serviceTimer =/= 0.U) { serviceTimer := serviceTimer - 1.U }
      }
    }
    is(sWriteToRead) {
      when(io.refreshMode) { state := sRefresh }
        .elsewhen(twtr.io.ready) {
          serviceTimer := (config.readTime max 1).U - 1.U
          state := sRead
        }
    }
    is(sRefresh) {
      when(!io.refreshMode) {
        serviceTimer := (config.readTime max 1).U - 1.U
        state := sRead
      }
    }
  }
}
