package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

class DfiSoftwarePhase(config: DramConfig) extends Bundle {
  val issue = Bool()
  val chipSelect = Bool()
  val writeEnable = Bool()
  val columnStrobe = Bool()
  val rowStrobe = Bool()
  val writeDataEnable = Bool()
  val readDataEnable = Bool()
  val address = UInt(config.rowBits.max(config.columnBits).max(11).W)
  val bank = UInt(config.bankBits.W)
  val writeData = UInt(config.dfiDataBits.W)
}

/** DDR4 command mux translating legacy RAS/CAS/WE ACTIVATE encoding to ACT_n. */
class Ddr4DfiMux(config: DramConfig) extends Module {
  private val dfiAddressBits = config.rowBits.max(config.columnBits).max(11)
  require(dfiAddressBits >= 17, "DDR4 DFI mux requires address bits A16..A14")
  val io = IO(new Bundle {
    val input = Input(new DfiInterface(config))
    val output = Output(new DfiInterface(config))
  })

  io.output := io.input
  for (phase <- 0 until config.nPhases) {
    val input = io.input.phases(phase)
    val output = io.output.phases(phase)
    val activate = !input.rasN && input.casN && input.weN
    output.actN := !activate
    when(activate) {
      output.weN := input.address(14)
      output.casN := input.address(15)
      output.rasN := input.address(16)
    }
  }
}

/** Hardware/external/software DFI selector with CSR-like phase injection inputs. */
class DfiInjector(config: DramConfig) extends Module {
  val io = IO(new Bundle {
    val hardware = Input(new DfiInterface(config))
    val external = Input(new DfiInterface(config))
    val hardwareControl = Input(Bool())
    val useExternal = Input(Bool())
    val clockEnable = Input(Bool())
    val onDieTermination = Input(Bool())
    val resetN = Input(Bool())
    val software = Input(Vec(config.nPhases, new DfiSoftwarePhase(config)))
    val phyRead = Input(Vec(config.nPhases, new DfiReadResponse(config)))
    val master = Output(new DfiInterface(config))
    val capturedRead = Output(Vec(config.nPhases, UInt(config.dfiDataBits.W)))
  })

  private val softwareDfi = Wire(new DfiInterface(config))
  softwareDfi := 0.U.asTypeOf(new DfiInterface(config))
  for (phaseIndex <- 0 until config.nPhases) {
    val control = io.software(phaseIndex)
    val phase = softwareDfi.phases(phaseIndex)
    phase.address := control.address
    phase.bank := control.bank
    phase.csN.foreach(_ := true.B)
    phase.rasN := true.B
    phase.casN := true.B
    phase.weN := true.B
    phase.actN := true.B
    phase.cke.foreach(_ := io.clockEnable)
    phase.odt.foreach(_ := io.onDieTermination)
    phase.resetN := io.resetN
    phase.wrdata := control.writeData
    phase.wrdataMask := 0.U
    phase.wrdataEn := control.issue && control.writeDataEnable
    phase.rddataEn := control.issue && control.readDataEnable
    when(control.issue) {
      phase.csN.foreach(_ := !control.chipSelect)
      phase.weN := !control.writeEnable
      phase.casN := !control.columnStrobe
      phase.rasN := !control.rowStrobe
    }
  }

  io.master := Mux(io.hardwareControl,
    Mux(io.useExternal, io.external, io.hardware), softwareDfi)
  private val captured = RegInit(VecInit(Seq.fill(config.nPhases)(0.U(config.dfiDataBits.W))))
  for (phase <- 0 until config.nPhases) {
    io.master.phases(phase).rddata := io.phyRead(phase).data
    io.master.phases(phase).rddataValid := io.phyRead(phase).valid
    when(io.phyRead(phase).valid) { captured(phase) := io.phyRead(phase).data }
  }
  io.capturedRead := captured
}
