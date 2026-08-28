package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/** BankMachine/refresher controller with a complete registered DFI boundary. */
class LiteDramDfiController(config: DramConfig) extends Module {
  require(config.dataBits == config.effectivePhyDataBits,
    "integrated DFI data path requires one Native word per DFI phase group")

  val io = IO(new Bundle {
    val bankRequests = Vec(config.rankBankCount,
      Flipped(Decoupled(new BankRequest(config))))
    val bankCompletions = Output(Vec(config.rankBankCount, Valid(new BankCompletion)))
    val bankLocks = Output(Vec(config.rankBankCount, Bool()))
    val writeData = Flipped(Decoupled(new NativeWriteData(config)))
    val phyRead = Input(Vec(config.nPhases, new DfiReadResponse(config)))
    val readData = Decoupled(new NativeReadData(config))
    val dfi = Output(new DfiInterface(config))
    val refreshPending = Output(Bool())
    val refreshBusy = Output(Bool())
  })

  private val bankMachines = Seq.tabulate(config.rankBankCount) { index =>
    Module(new BankMachine(config, index))
  }
  private val refresher = Module(new Refresher(config))
  private val multiplexer = Module(new DfiMultiplexer(config, config.rankBankCount))

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

  io.dfi := multiplexer.io.dfi
  private val writeEnable = multiplexer.io.dfi.phases.map(_.wrdataEn).reduce(_ || _)
  io.writeData.ready := writeEnable
  when(writeEnable) {
    assert(io.writeData.valid, "DFI write command has no aligned Native write payload")
  }
  for (phase <- 0 until config.nPhases) {
    val dataLow = phase * config.dfiDataBits
    val maskLow = phase * (config.dfiDataBits / 8)
    io.dfi.phases(phase).wrdata :=
      io.writeData.bits.data(dataLow + config.dfiDataBits - 1, dataLow)
    io.dfi.phases(phase).wrdataMask :=
      ~io.writeData.bits.byteEnable(maskLow + config.dfiDataBits / 8 - 1, maskLow)
  }

  private val readValid = io.phyRead.map(_.valid).reduce(_ || _)
  when(readValid) {
    assert(io.phyRead.map(_.valid).reduce(_ && _),
      "all DFI phases must return one Native word together")
  }
  private val readQueue = Module(new Queue(new NativeReadData(config),
    (config.cmdBufferDepth * config.rankBankCount) max 2))
  readQueue.io.enq.valid := readValid
  readQueue.io.enq.bits.data := VecInit(io.phyRead.map(_.data)).asUInt
  when(readValid) {
    assert(readQueue.io.enq.ready, "DFI read-return FIFO overflow")
  }
  io.readData <> readQueue.io.deq

  io.refreshPending := refresher.io.request
  io.refreshBusy := refresher.io.busy
}

/** Native multi-port core connected directly to a multi-phase DFI interface. */
class LiteDramDfiCore(config: DramConfig, masterCount: Int = 1) extends Module {
  val io = IO(new Bundle {
    val masters = Vec(masterCount, new NativeSlavePort(config))
    val phyRead = Input(Vec(config.nPhases, new DfiReadResponse(config)))
    val dfi = Output(new DfiInterface(config))
    val refreshPending = Output(Bool())
    val refreshBusy = Output(Bool())
  })

  private val crossbar = Module(new LiteDramCrossbar(config, masterCount))
  private val controller = Module(new LiteDramDfiController(config))

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
  controller.io.writeData <> crossbar.io.writeData
  crossbar.io.readData <> controller.io.readData
  controller.io.phyRead := io.phyRead
  io.dfi := controller.io.dfi
  io.refreshPending := controller.io.refreshPending
  io.refreshBusy := controller.io.refreshBusy
}
