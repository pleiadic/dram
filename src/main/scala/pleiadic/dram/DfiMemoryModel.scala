package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/**
  * DFI-level DRAM memory model for simulation and small synthesizable tests.
  * The configured geometry determines storage size, so large production
  * geometries should use a reduced simulation configuration.
  */
class DfiMemoryModel(config: DramConfig, readLatency: Int = -1,
    writeLatency: Int = -1) extends Module {
  private val actualReadLatency = if (readLatency < 0) config.readLatency else readLatency
  private val actualWriteLatency = if (writeLatency < 0) config.writeLatency else writeLatency
  require(actualReadLatency >= 1 && actualWriteLatency >= 0)
  private val bytesPerWord = config.effectivePhyDataBits / 8
  private val memoryDepth = config.nranks * (1 <<
    (config.rowBits + config.bankBits + config.columnBits))
  private val addressWidth = log2Ceil(memoryDepth max 2)

  val io = IO(new Bundle {
    val dfi = Input(new DfiInterface(config))
    val read = Output(Vec(config.nPhases, new DfiReadResponse(config)))
    val clearErrors = Input(Bool())
    val protocolError = Output(Bool())
    val errors = Output(UInt(32.W))
    val openBanks = Output(UInt(config.rankBankCount.W))
  })

  private val memory = Mem(memoryDepth, Vec(bytesPerWord, UInt(8.W)))
  private val openValid = RegInit(VecInit(Seq.fill(config.rankBankCount)(false.B)))
  private val openRows = RegInit(VecInit(Seq.fill(config.rankBankCount)(0.U(config.rowBits.W))))
  private val protocolError = WireDefault(false.B)

  private def selected(phase: DfiPhase): Bool = !phase.csN.asUInt.andR
  private def rank(phase: DfiPhase): UInt = if (config.nranks == 1) 0.U
    else PriorityEncoder(~phase.csN.asUInt)
  private def rankBank(phase: DfiPhase): UInt = if (config.nranks == 1) phase.bank
    else Cat(rank(phase), phase.bank)
  private def activate(phase: DfiPhase): Bool = selected(phase) &&
    ((!phase.actN) || (!phase.rasN && phase.casN && phase.weN))
  private def precharge(phase: DfiPhase): Bool = selected(phase) && phase.actN &&
    !phase.rasN && phase.casN && !phase.weN
  private def refresh(phase: DfiPhase): Bool = selected(phase) && phase.actN &&
    !phase.rasN && !phase.casN && phase.weN
  private def read(phase: DfiPhase): Bool = selected(phase) && phase.actN &&
    phase.rasN && !phase.casN && phase.weN
  private def write(phase: DfiPhase): Bool = selected(phase) && phase.actN &&
    phase.rasN && !phase.casN && !phase.weN

  private def wordAddress(phase: DfiPhase): UInt = {
    val bankIndex = rankBank(phase)
    val column = phase.address(config.columnBits - 1, 0)
    val value = if (config.nranks == 1) {
      Cat(openRows(bankIndex), phase.bank, column)
    } else {
      Cat(rank(phase), openRows(bankIndex), phase.bank, column)
    }
    value(addressWidth - 1, 0)
  }

  for (phaseIndex <- 0 until config.nPhases) {
    val phase = io.dfi.phases(phaseIndex)
    val bankIndex = rankBank(phase)
    when(activate(phase)) {
      when(openValid(bankIndex)) { protocolError := true.B }
      openValid(bankIndex) := true.B
      openRows(bankIndex) := phase.address(config.rowBits - 1, 0)
    }
    when(precharge(phase)) {
      when(phase.address(10)) {
        openValid.foreach(_ := false.B)
      }.otherwise {
        when(!openValid(bankIndex)) { protocolError := true.B }
        openValid(bankIndex) := false.B
      }
    }
    when(refresh(phase)) {
      when(openValid.asUInt.orR) { protocolError := true.B }
    }
  }

  private val readCommands = VecInit(io.dfi.phases.map(read))
  private val writeCommands = VecInit(io.dfi.phases.map(write))
  private val hasRead = readCommands.asUInt.orR
  private val hasWrite = writeCommands.asUInt.orR
  when(PopCount(readCommands) > 1.U || PopCount(writeCommands) > 1.U ||
      (hasRead && hasWrite)) {
    protocolError := true.B
  }
  private val readPhase = io.dfi.phases(PriorityEncoder(readCommands.asUInt))
  private val writePhase = io.dfi.phases(PriorityEncoder(writeCommands.asUInt))
  private val readBank = rankBank(readPhase)
  private val writeBank = rankBank(writePhase)
  private val readAddress = wordAddress(readPhase)
  private val writeAddress = wordAddress(writePhase)
  private val validReadCommand = hasRead && openValid(readBank)
  private val validWriteCommand = hasWrite && openValid(writeBank)
  when(hasRead && !openValid(readBank)) { protocolError := true.B }
  when(hasWrite && !openValid(writeBank)) { protocolError := true.B }

  private val writeAddressValid = if (actualWriteLatency == 0) {
    validWriteCommand
  } else {
    val valid = RegInit(VecInit(Seq.fill(actualWriteLatency)(false.B)))
    valid(0) := validWriteCommand
    for (index <- 1 until actualWriteLatency) { valid(index) := valid(index - 1) }
    valid.last
  }
  private val delayedWriteAddress = if (actualWriteLatency == 0) {
    writeAddress
  } else {
    val addresses = RegInit(VecInit(Seq.fill(actualWriteLatency)(0.U(addressWidth.W))))
    when(validWriteCommand) { addresses(0) := writeAddress }
    for (index <- 1 until actualWriteLatency) {
      addresses(index) := addresses(index - 1)
    }
    addresses.last
  }
  private val writeDataEnable = io.dfi.phases.map(_.wrdataEn).reduce(_ || _)
  private val writeData = VecInit(io.dfi.phases.map(_.wrdata)).asUInt
    .asTypeOf(Vec(bytesPerWord, UInt(8.W)))
  private val writeMask = VecInit(io.dfi.phases.map(_.wrdataMask)).asUInt
  when(writeDataEnable =/= writeAddressValid) { protocolError := true.B }
  when(writeDataEnable && writeAddressValid) {
    memory.write(delayedWriteAddress, writeData,
      (0 until bytesPerWord).map(index => !writeMask(index)))
  }

  private val memoryRead = memory.read(readAddress).asUInt
  private val readValidPipe = RegInit(VecInit(Seq.fill(actualReadLatency)(false.B)))
  private val readDataPipe = RegInit(VecInit(Seq.fill(actualReadLatency)(
    0.U(config.effectivePhyDataBits.W))))
  readValidPipe(0) := validReadCommand
  when(validReadCommand) { readDataPipe(0) := memoryRead }
  for (index <- 1 until actualReadLatency) {
    readValidPipe(index) := readValidPipe(index - 1)
    when(readValidPipe(index - 1)) { readDataPipe(index) := readDataPipe(index - 1) }
  }
  for (phaseIndex <- 0 until config.nPhases) {
    val low = phaseIndex * config.dfiDataBits
    val high = low + config.dfiDataBits - 1
    io.read(phaseIndex).data := readDataPipe.last(high, low)
    io.read(phaseIndex).valid := readValidPipe.last
  }

  private val errors = RegInit(0.U(32.W))
  when(io.clearErrors) {
    errors := 0.U
  }.elsewhen(protocolError && !errors.andR) {
    errors := errors + 1.U
  }
  io.protocolError := protocolError
  io.errors := errors
  io.openBanks := openValid.asUInt
}
