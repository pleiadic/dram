package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/** Converts one accepted abstract command into a selected DFI command/data phase. */
class DfiSteerer(config: DramConfig, commandPhase: Int = 0, dataPhase: Int = 0) extends Module {
  require(commandPhase >= 0 && commandPhase < config.nPhases)
  require(dataPhase >= 0 && dataPhase < config.nPhases)

  val io = IO(new Bundle {
    val command = Flipped(Decoupled(new DramCommand(config)))
    val dfi = Output(new DfiInterface(config))
  })

  io.command.ready := true.B
  io.dfi := 0.U.asTypeOf(new DfiInterface(config))
  for (phase <- io.dfi.phases) {
    phase.csN.foreach(_ := true.B)
    phase.cke.foreach(_ := true.B)
    phase.odt.foreach(_ := false.B)
    phase.rasN := true.B
    phase.casN := true.B
    phase.weN := true.B
    phase.actN := true.B
    phase.resetN := true.B
  }

  val accepted = io.command.fire
  val cmd = io.command.bits
  val phase = io.dfi.phases(commandPhase)
  phase.address := Mux(cmd.command === DramCommandType.activate, cmd.row, cmd.column)
  phase.bank := cmd.bank

  when (accepted) {
    when (cmd.allBanks) { phase.csN.foreach(_ := false.B) }
      .otherwise {
        if (config.nranks == 1) phase.csN(0) := false.B
        else phase.csN(cmd.rank) := false.B
      }
    switch (cmd.command) {
      is (DramCommandType.activate) { phase.rasN := false.B }
      is (DramCommandType.precharge) { phase.rasN := false.B; phase.weN := false.B; phase.address := (1 << 10).U }
      is (DramCommandType.read) { phase.casN := false.B; io.dfi.phases(dataPhase).rddataEn := true.B }
      is (DramCommandType.write) { phase.casN := false.B; phase.weN := false.B; io.dfi.phases(dataPhase).wrdataEn := true.B }
      is (DramCommandType.refresh) { phase.rasN := false.B; phase.casN := false.B }
    }
    when ((cmd.command === DramCommandType.read || cmd.command === DramCommandType.write) && cmd.autoPrecharge) {
      phase.address := cmd.column | (1 << 10).U
    }
  }

  io.dfi.phases(dataPhase).wrdata := cmd.data(config.dfiDataBits - 1, 0)
  io.dfi.phases(dataPhase).wrdataMask := ~cmd.mask(config.dfiDataBits / 8 - 1, 0)
}
