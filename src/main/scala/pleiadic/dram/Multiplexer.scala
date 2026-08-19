package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/** Round-robin command arbiter with LiteDRAM-compatible type filters. */
class CommandChooser(config: DramConfig, requestCount: Int) extends Module {
  require(requestCount >= 1)

  val io = IO(new Bundle {
    val requests = Vec(requestCount, Flipped(Decoupled(new DramCommand(config))))
    val output = Decoupled(new DramCommand(config))
    val wantReads = Input(Bool())
    val wantWrites = Input(Bool())
    val wantCommands = Input(Bool())
    val wantActivates = Input(Bool())
    val selected = Output(UInt(log2Ceil(requestCount max 2).W))
  })

  private val indexWidth = log2Ceil(requestCount max 2)
  private val pointer = RegInit(0.U(indexWidth.W))
  private val eligible = Wire(Vec(requestCount, Bool()))

  for (i <- 0 until requestCount) {
    val command = io.requests(i).bits.command
    val isRead = command === DramCommandType.read
    val isWrite = command === DramCommandType.write
    val isActivate = command === DramCommandType.activate
    val isCommand = !isRead && !isWrite
    eligible(i) := io.requests(i).valid && (
      (isRead && io.wantReads) ||
      (isWrite && io.wantWrites) ||
      (isCommand && io.wantCommands && (!isActivate || io.wantActivates)))
  }

  private val selected = Wire(UInt(indexWidth.W))
  private val found = Wire(Bool())
  if (requestCount == 1) {
    selected := 0.U
    found := eligible(0)
  } else {
    val rotatedEligible = Wire(Vec(requestCount, Bool()))
    for (offset <- 0 until requestCount) {
      val sum = pointer +& offset.U
      val wrapped = Mux(sum >= requestCount.U, sum - requestCount.U, sum)
      rotatedEligible(offset) := eligible(wrapped(indexWidth - 1, 0))
    }
    val chosenOffset = PriorityEncoder(rotatedEligible)
    val selectedSum = pointer +& chosenOffset
    val wrappedSelected = Mux(selectedSum >= requestCount.U,
      selectedSum - requestCount.U, selectedSum)
    selected := wrappedSelected(indexWidth - 1, 0)
    found := rotatedEligible.asUInt.orR
  }

  io.output.valid := found
  if (requestCount == 1) io.output.bits := io.requests(0).bits
  else io.output.bits := io.requests(selected).bits
  io.selected := selected
  for (i <- 0 until requestCount) {
    io.requests(i).ready := found && selected === i.U && io.output.ready
  }

  when(io.output.fire) {
    pointer := Mux(selected === (requestCount - 1).U, 0.U, selected + 1.U)
  }
}

/**
  * Arbitrates bank commands, enforces global DRAM timings and alternates read
  * and write service so neither class can starve.
  */
class Multiplexer(config: DramConfig, bankCount: Int) extends Module {
  require(bankCount >= 1)

  val io = IO(new Bundle {
    val bankCommands = Vec(bankCount, Flipped(Decoupled(new DramCommand(config))))
    val refreshCommand = Flipped(Decoupled(new DramCommand(config)))
    val refreshMode = Input(Bool())
    val command = Decoupled(new DramCommand(config))
    val servingWrites = Output(Bool())
  })

  private val chooser = Module(new CommandChooser(config, bankCount))
  for (i <- 0 until bankCount) chooser.io.requests(i) <> io.bankCommands(i)

  private val trrd = Module(new TxxdController(config.timing.tRrd))
  private val tfaw = Module(new TfawController(config.timing.tFaw))
  private val tccd = Module(new TxxdController(config.timing.tCcd))
  private val twtr = Module(new TxxdController(config.timing.tWtr + config.writeLatency))

  private val sRead :: sReadToWrite :: sWrite :: sWriteToRead :: sRefresh :: Nil = Enum(5)
  private val state = RegInit(sRead)
  private val maxServiceTime = (config.readTime max config.writeTime) max 1
  private val serviceWidth = log2Ceil((maxServiceTime + 1) max 2)
  private val serviceTimer = RegInit(0.U(serviceWidth.W))
  private val turnaroundWidth = log2Ceil((config.readLatency + 1) max 2)
  private val turnaround = RegInit(0.U(turnaroundWidth.W))

  private val readAvailable = io.bankCommands.map(p =>
    p.valid && p.bits.command === DramCommandType.read).reduce(_ || _)
  private val writeAvailable = io.bankCommands.map(p =>
    p.valid && p.bits.command === DramCommandType.write).reduce(_ || _)

  chooser.io.wantReads := state === sRead
  chooser.io.wantWrites := state === sWrite
  chooser.io.wantCommands := state === sRead || state === sWrite
  chooser.io.wantActivates := trrd.io.ready && tfaw.io.ready

  private val chosen = chooser.io.output.bits
  private val chosenIsActivate = chosen.command === DramCommandType.activate
  private val chosenIsColumn = chosen.command === DramCommandType.read || chosen.command === DramCommandType.write
  private val chosenIsRead = chosen.command === DramCommandType.read
  private val timingAllowed = (!chosenIsActivate || (trrd.io.ready && tfaw.io.ready)) &&
    (!chosenIsColumn || tccd.io.ready) && (!chosenIsRead || twtr.io.ready)

  io.command.valid := false.B
  io.command.bits := 0.U.asTypeOf(new DramCommand(config))
  chooser.io.output.ready := false.B
  io.refreshCommand.ready := false.B
  io.servingWrites := state === sWrite

  when(state === sRefresh) {
    io.command <> io.refreshCommand
  }.elsewhen(state === sRead || state === sWrite) {
    io.command.valid := chooser.io.output.valid && timingAllowed
    io.command.bits := chooser.io.output.bits
    chooser.io.output.ready := io.command.ready && timingAllowed
  }

  private val acceptedBankCommand = chooser.io.output.fire
  trrd.io.valid := acceptedBankCommand && chosenIsActivate
  tfaw.io.valid := acceptedBankCommand && chosenIsActivate
  tccd.io.valid := acceptedBankCommand && chosenIsColumn
  twtr.io.valid := acceptedBankCommand && chosen.command === DramCommandType.write

  switch(state) {
    is(sRead) {
      when(io.refreshMode) { state := sRefresh }
        .elsewhen(writeAvailable && (!readAvailable || serviceTimer === 0.U)) {
          turnaround := (config.readLatency - 1).U
          state := sReadToWrite
        }
      if (config.readTime > 0) {
        when(serviceTimer === 0.U) { serviceTimer := (config.readTime - 1).U }
          .otherwise { serviceTimer := serviceTimer - 1.U }
      }
    }
    is(sReadToWrite) {
      when(io.refreshMode) { state := sRefresh }
        .elsewhen(turnaround === 0.U) {
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
