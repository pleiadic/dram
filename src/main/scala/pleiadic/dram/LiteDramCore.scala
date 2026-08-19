package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/** Native multi-port crossbar and controller control path integrated together. */
class LiteDramCore(config: DramConfig, masterCount: Int = 1) extends Module {
  val io = IO(new Bundle {
    val masters = Vec(masterCount, new NativeSlavePort(config))
    val command = Decoupled(new DramCommand(config))
    val writeData = Decoupled(new NativeWriteData(config))
    val readData = Flipped(Decoupled(new NativeReadData(config)))
    val refreshPending = Output(Bool())
    val refreshBusy = Output(Bool())
  })

  private val crossbar = Module(new LiteDramCrossbar(config, masterCount))
  private val controller = Module(new LiteDramController(config))

  for (master <- 0 until masterCount) {
    crossbar.io.masters(master).command.valid := io.masters(master).command.valid
    crossbar.io.masters(master).command.bits := io.masters(master).command.bits
    io.masters(master).command.ready := crossbar.io.masters(master).command.ready
    crossbar.io.masters(master).writeData.valid := io.masters(master).writeData.valid
    crossbar.io.masters(master).writeData.bits := io.masters(master).writeData.bits
    io.masters(master).writeData.ready := crossbar.io.masters(master).writeData.ready
    io.masters(master).readData.valid := crossbar.io.masters(master).readData.valid
    io.masters(master).readData.bits := crossbar.io.masters(master).readData.bits
    crossbar.io.masters(master).readData.ready := io.masters(master).readData.ready
    crossbar.io.masters(master).flush := io.masters(master).flush
    io.masters(master).lock := crossbar.io.masters(master).lock
  }

  for (bank <- 0 until config.rankBankCount) {
    controller.io.bankRequests(bank) <> crossbar.io.bankRequests(bank)
  }
  crossbar.io.bankCompletions := controller.io.bankCompletions
  crossbar.io.bankLocks := controller.io.bankLocks
  io.command <> controller.io.command
  io.writeData <> crossbar.io.writeData
  crossbar.io.readData <> io.readData
  io.refreshPending := controller.io.refreshPending
  io.refreshBusy := controller.io.refreshBusy
}
