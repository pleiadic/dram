package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/**
  * Synthesizable LiteDRAM controller core.
  *
  * Requests are already routed to physical rank/banks; the crossbar is a
  * separate layer. This mirrors LiteDRAMController's bankN interfaces and
  * keeps arbitration between user ports independent from DRAM scheduling.
  */
class LiteDramController(config: DramConfig) extends Module {
  val io = IO(new Bundle {
    val bankRequests = Vec(config.rankBankCount, Flipped(Decoupled(new BankRequest(config))))
    val bankCompletions = Output(Vec(config.rankBankCount, Valid(new BankCompletion)))
    val bankLocks = Output(Vec(config.rankBankCount, Bool()))
    val command = Decoupled(new DramCommand(config))
    val refreshPending = Output(Bool())
    val refreshBusy = Output(Bool())
  })

  private val bankMachines = Seq.tabulate(config.rankBankCount) { index =>
    Module(new BankMachine(config, index))
  }
  private val refresher = Module(new Refresher(config))
  private val multiplexer = Module(new Multiplexer(config, config.rankBankCount))

  for ((bankMachine, index) <- bankMachines.zipWithIndex) {
    bankMachine.io.request <> io.bankRequests(index)
    io.bankCompletions(index) := bankMachine.io.completion
    io.bankLocks(index) := bankMachine.io.lock
    bankMachine.io.refreshRequest := refresher.io.request
    multiplexer.io.bankCommands(index) <> bankMachine.io.command
  }

  refresher.io.grant := bankMachines.map(_.io.refreshGrant).reduce(_ && _)
  multiplexer.io.refreshCommand <> refresher.io.command
  multiplexer.io.refreshMode := refresher.io.busy
  io.command <> multiplexer.io.command
  io.refreshPending := refresher.io.request
  io.refreshBusy := refresher.io.busy
}
