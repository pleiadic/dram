package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/**
  * Per-bank row manager and command buffer.
  *
  * Requests are queued so the current request can inspect the following row
  * and use DRAM auto-precharge when profitable. Data is deliberately kept out
  * of this module, as in LiteDRAM: `completion` selects the write/read data path.
  */
class BankMachine(config: DramConfig, bankIndex: Int) extends Module {
  require(bankIndex >= 0 && bankIndex < config.rankBankCount)

  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new BankRequest(config)))
    val command = Decoupled(new DramCommand(config))
    val completion = Valid(new BankCompletion)
    val refreshRequest = Input(Bool())
    val refreshGrant = Output(Bool())
    val lock = Output(Bool())
    val rowOpen = Output(Bool())
    val openRow = Output(UInt(config.rowBits.W))
  })

  private val depth = config.cmdBufferDepth
  private val pointerWidth = log2Ceil(depth max 2)
  private val countWidth = log2Ceil(depth + 1)
  private val entries = Reg(Vec(depth, new BankRequest(config)))
  private val readPointer = RegInit(0.U(pointerWidth.W))
  private val writePointer = RegInit(0.U(pointerWidth.W))
  private val count = RegInit(0.U(countWidth.W))

  private def increment(pointer: UInt): UInt =
    if (depth == 1) 0.U else Mux(pointer === (depth - 1).U, 0.U, pointer + 1.U)

  private val head = if (depth == 1) entries.head else entries(readPointer)
  private val nextHead = if (depth == 1) entries.head else entries(increment(readPointer))
  private val headValid = count =/= 0.U
  private val lookaheadValid = count > 1.U
  private val enqueue = io.request.fire
  private val dequeue = WireDefault(false.B)

  io.request.ready := count =/= depth.U
  when(enqueue) {
    if (depth == 1) entries.head := io.request.bits
    else entries(writePointer) := io.request.bits
    writePointer := increment(writePointer)
  }
  when(dequeue) { readPointer := increment(readPointer) }
  switch(Cat(enqueue, dequeue)) {
    is("b10".U) { count := count + 1.U }
    is("b01".U) { count := count - 1.U }
  }

  // LiteDRAM's local tWTP controller includes the controller-cycle write
  // latency and the final column-command interval (AL=0).
  private val writeToPrecharge =
    config.writeLatency + config.timing.tWr + config.timing.tCcd
  private val maxTiming = Seq(config.timing.tRcd, config.timing.tRp, config.timing.tRas,
    config.timing.tRc, writeToPrecharge, config.timing.tRtp).max
  private val timingWidth = log2Ceil(maxTiming max 2)
  private val delay = RegInit(0.U(timingWidth.W))
  private val rasDelay = RegInit(0.U(timingWidth.W))
  private val rcDelay = RegInit(0.U(timingWidth.W))
  private val prechargeDelay = RegInit(0.U(timingWidth.W))
  private val rowOpen = RegInit(false.B)
  private val openRow = RegInit(0.U(config.rowBits.W))

  when(rasDelay =/= 0.U) { rasDelay := rasDelay - 1.U }
  when(rcDelay =/= 0.U) { rcDelay := rcDelay - 1.U }
  when(prechargeDelay =/= 0.U) { prechargeDelay := prechargeDelay - 1.U }

  private val Seq(sRegular, sActivate, sTrcd, sPrecharge, sTrp,
    sAutoPrecharge, sRefresh) = Enum(7)
  private val state = RegInit(sRegular)

  private val rank = bankIndex / config.bankCount
  private val bank = bankIndex % config.bankCount
  private val rowHit = rowOpen && openRow === head.row
  private val useAutoPrecharge = config.withAutoPrecharge.B && lookaheadValid && nextHead.row =/= head.row

  // Commands can remain backpressured while the lookahead FIFO or refresh
  // request changes.  Snapshot the first presented command so the Decoupled
  // payload remains irrevocable until it is accepted.
  private val rawCommandValid = WireDefault(false.B)
  private val rawCommand = WireDefault(0.U.asTypeOf(new DramCommand(config)))
  private val heldCommandValid = RegInit(false.B)
  private val heldCommand = Reg(new DramCommand(config))
  rawCommand.rank := rank.U
  rawCommand.bank := bank.U
  io.command.valid := heldCommandValid || rawCommandValid
  io.command.bits := Mux(heldCommandValid, heldCommand, rawCommand)
  when(heldCommandValid) {
    when(io.command.ready) { heldCommandValid := false.B }
  }.elsewhen(rawCommandValid && !io.command.ready) {
    heldCommandValid := true.B
    heldCommand := rawCommand
  }
  io.completion.valid := false.B
  io.completion.bits.write := false.B
  io.refreshGrant := false.B
  io.lock := headValid
  io.rowOpen := rowOpen
  io.openRow := openRow

  private def issue(command: UInt): Unit = {
    rawCommandValid := true.B
    rawCommand.command := command
    rawCommand.row := head.row
    rawCommand.column := head.column
    rawCommand.autoPrecharge := false.B
  }

  switch(state) {
    is(sRegular) {
      // A command already exposed to the downstream consumer must complete
      // before refresh can take ownership of the bank.
      when(io.refreshRequest && !heldCommandValid) {
        state := sRefresh
      }.elsewhen(headValid) {
        when(!rowOpen) { state := sActivate }
          .elsewhen(!rowHit) { state := sPrecharge }
          .otherwise {
            issue(Mux(head.write, DramCommandType.write, DramCommandType.read))
            rawCommand.autoPrecharge := useAutoPrecharge
            when(io.command.fire) {
              dequeue := true.B
              io.completion.valid := true.B
              io.completion.bits.write := head.write
              prechargeDelay := Mux(head.write,
                (writeToPrecharge - 1).U, (config.timing.tRtp - 1).U)
              when(io.command.bits.autoPrecharge) { state := sAutoPrecharge }
            }
          }
      }
    }
    is(sActivate) {
      when(rcDelay === 0.U) {
        issue(DramCommandType.activate)
        when(io.command.fire) {
          rowOpen := true.B
          openRow := head.row
          rasDelay := (config.timing.tRas - 1).U
          rcDelay := (config.timing.tRc - 1).U
          if (config.timing.tRcd == 1) state := sRegular
          else {
            delay := (config.timing.tRcd - 1).U
            state := sTrcd
          }
        }
      }
    }
    is(sTrcd) {
      when(delay === 1.U) { state := sRegular }
        .otherwise { delay := delay - 1.U }
    }
    is(sPrecharge) {
      when(rasDelay === 0.U && prechargeDelay === 0.U) {
        issue(DramCommandType.precharge)
        when(io.command.fire) {
          rowOpen := false.B
          if (config.timing.tRp == 1) state := sActivate
          else {
            delay := (config.timing.tRp - 1).U
            state := sTrp
          }
        }
      }
    }
    is(sTrp) {
      when(delay === 1.U) { state := sActivate }
        .otherwise { delay := delay - 1.U }
    }
    is(sAutoPrecharge) {
      when(rasDelay === 0.U && prechargeDelay === 0.U) {
        rowOpen := false.B
        if (config.timing.tRp == 1) state := sActivate
        else {
          delay := (config.timing.tRp - 1).U
          state := sTrp
        }
      }
    }
    is(sRefresh) {
      // The refresher itself emits PRECHARGE-ALL. This bank only waits until
      // its last column operation is safe and then relinquishes the open row.
      when(prechargeDelay === 0.U) {
        io.refreshGrant := true.B
        io.rowOpen := false.B
        rowOpen := false.B
      }
      when(!io.refreshRequest) { state := sRegular }
    }
  }
}
