package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/**
  * A small, synthesizable DRAM controller core.
  *
  * The architecture follows LiteDRAM's important contracts: one bank machine
  * tracks an open row, ACT/PRE are inserted around column commands, and refresh
  * has priority once its interval expires. A PHY can consume `command`; the
  * built-in memory array makes the module useful as a deterministic simulation
  * model and keeps the request/response behavior testable without vendor IP.
  */
class DramController(val config: DramConfig) extends Module {
  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new DramRequest(config)))
    val response = Decoupled(new DramResponse(config))
    val command = Decoupled(new DramCommand(config))
    val refreshPending = Output(Bool())
    val busy = Output(Bool())
  })

  private val bankWidth = config.bankBits
  private val rowWidth = config.rowBits
  private val colWidth = config.columnBits
  private val byteOffset = config.byteOffsetBits
  private val counterWidth = log2Ceil(config.timing.tRefi max config.timing.tRfc max config.timing.tRc max 2) + 1

  private val Seq(stateIdle, stateActivate, stateRcd, stateCas, stateCcd,
    statePrecharge, stateRp, stateRefreshPre, stateRefreshRp,
    stateRefresh, stateRefreshWait) = Enum(11)
  private val state = RegInit(stateIdle)
  private val delay = RegInit(0.U(counterWidth.W))
  private val refreshTimer = RegInit((config.timing.tRefi - 1).U(counterWidth.W))
  private val refreshPending = RegInit(false.B)
  private val prechargeForRowMiss = RegInit(false.B)

  private val openValid = RegInit(VecInit(Seq.fill(config.bankCount)(false.B)))
  private val openRow = Reg(Vec(config.bankCount, UInt(rowWidth.W)))
  private val rasDelay = RegInit(VecInit(Seq.fill(config.bankCount)(0.U(counterWidth.W))))
  private val rcDelay = RegInit(VecInit(Seq.fill(config.bankCount)(0.U(counterWidth.W))))
  private val memory = Mem(config.memoryWords, UInt(config.dataBits.W))

  private val reqReg = Reg(new DramRequest(config))
  private val respReg = Reg(new DramResponse(config))
  private val responseValid = RegInit(false.B)
  private val selectedBank = reqReg.address(byteOffset + bankWidth - 1, byteOffset)
  private val selectedColumn = reqReg.address(byteOffset + bankWidth + colWidth - 1, byteOffset + bankWidth)
  private val rowLsb = byteOffset + bankWidth + colWidth
  private val wordMsb = rowLsb + rowWidth - 1
  private val selectedRow = reqReg.address(wordMsb, rowLsb)
  private val wordAddress = reqReg.address(wordMsb, byteOffset)
  private val incomingBank = io.request.bits.address(byteOffset + bankWidth - 1, byteOffset)
  private val incomingRow = io.request.bits.address(wordMsb, rowLsb)
  private val bankIndex = selectedBank

  io.request.ready := state === stateIdle && !responseValid && !refreshPending
  io.response.valid := responseValid
  io.response.bits := respReg
  io.command.valid := false.B
  io.command.bits := 0.U.asTypeOf(new DramCommand(config))
  io.refreshPending := refreshPending
  io.busy := state =/= stateIdle

  private def issue(commandType: UInt, allBanks: Bool = false.B): Unit = {
    io.command.valid := true.B
    io.command.bits.command := commandType
    io.command.bits.allBanks := allBanks
    io.command.bits.bank := Mux(allBanks || commandType === DramCommandType.refresh, 0.U, selectedBank)
    io.command.bits.row := Mux(commandType === DramCommandType.activate, selectedRow, 0.U)
    io.command.bits.column := Mux(commandType === DramCommandType.read || commandType === DramCommandType.write, selectedColumn, 0.U)
    io.command.bits.data := reqReg.data
    io.command.bits.mask := reqReg.mask
  }

  when (responseValid && io.response.ready) { responseValid := false.B }

  when (refreshTimer === 0.U) {
    refreshTimer := (config.timing.tRefi - 1).U
    refreshPending := true.B
  }.otherwise { refreshTimer := refreshTimer - 1.U }

  when (delay =/= 0.U) { delay := delay - 1.U }
  for (bank <- 0 until config.bankCount) {
    when (rasDelay(bank) =/= 0.U) { rasDelay(bank) := rasDelay(bank) - 1.U }
    when (rcDelay(bank) =/= 0.U) { rcDelay(bank) := rcDelay(bank) - 1.U }
  }

  switch (state) {
    is (stateIdle) {
      when (io.request.fire) {
        reqReg := io.request.bits
        state := Mux(refreshPending, stateRefreshPre,
          Mux(!openValid(incomingBank), stateActivate,
            Mux(openRow(incomingBank) =/= incomingRow, statePrecharge, stateCas)))
        prechargeForRowMiss := openValid(incomingBank) && openRow(incomingBank) =/= incomingRow
      }.elsewhen (refreshPending) { state := stateRefreshPre }
    }
    is (stateActivate) {
      when (rcDelay(bankIndex) === 0.U) {
        issue(DramCommandType.activate)
        when (io.command.fire) {
          openValid(bankIndex) := true.B
          openRow(bankIndex) := selectedRow
          rasDelay(bankIndex) := (config.timing.tRas - 1).U
          rcDelay(bankIndex) := (config.timing.tRc - 1).U
          delay := (config.timing.tRcd - 1).U
          state := stateRcd
        }
      }
    }
    is (stateRcd) { when (delay === 0.U) { state := stateCas } }
    is (stateCas) {
      issue(Mux(reqReg.write, DramCommandType.write, DramCommandType.read))
      when (io.command.fire) {
        val oldData = memory.read(wordAddress)
        when (reqReg.write) {
          val bytes = config.dataBits / 8
          val mergedBytes = Wire(Vec(bytes, UInt(8.W)))
          for (i <- 0 until bytes) {
            mergedBytes(i) := Mux(reqReg.mask(i), reqReg.data(8 * i + 7, 8 * i), oldData(8 * i + 7, 8 * i))
          }
          memory.write(wordAddress, mergedBytes.asUInt)
        }.otherwise { respReg.data := oldData }
        when (reqReg.write) { respReg.data := 0.U }
        respReg.write := reqReg.write
        responseValid := true.B
        delay := (config.timing.tCcd - 1).U
        state := stateCcd
      }
    }
    is (stateCcd) {
      when (delay === 0.U) {
        when (config.withAutoPrecharge.B || refreshPending) { state := statePrecharge }
          .otherwise { state := stateIdle }
      }
    }
    is (statePrecharge) {
      when (rasDelay(bankIndex) === 0.U) {
        issue(DramCommandType.precharge)
        when (io.command.fire) { openValid(bankIndex) := false.B; delay := (config.timing.tRp - 1).U; state := stateRp }
      }
    }
    is (stateRp) {
      when (delay === 0.U) {
        when (prechargeForRowMiss) { prechargeForRowMiss := false.B; state := stateActivate }
          .elsewhen (refreshPending) { state := stateRefreshPre }
          .otherwise { state := stateIdle }
      }
    }
    is (stateRefreshPre) {
      issue(DramCommandType.precharge, true.B)
      when (io.command.fire) { openValid.foreach(_ := false.B); delay := (config.timing.tRp - 1).U; state := stateRefreshRp }
    }
    is (stateRefreshRp) { when (delay === 0.U) { state := stateRefresh } }
    is (stateRefresh) {
      issue(DramCommandType.refresh)
      when (io.command.fire) { delay := (config.timing.tRfc - 1).U; state := stateRefreshWait }
    }
    is (stateRefreshWait) { when (delay === 0.U) { refreshPending := false.B; state := stateIdle } }
  }
}
