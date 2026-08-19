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

/**
  * Related-clock DFI rate converter. The slow-side DFI has `ratio` times as
  * many phases and 1/ratio of the per-phase data width. Slow-side signals must
  * remain stable for the complete group of fast-clock slots.
  */
class DfiRateConverter(fastConfig: DramConfig, ratio: Int,
    writeDelay: Int = 0, readDelay: Int = 0) extends RawModule {
  require(ratio >= 2 && (ratio & (ratio - 1)) == 0)
  require(writeDelay >= 0 && writeDelay < ratio)
  require(readDelay >= 0 && readDelay < ratio)
  require(fastConfig.dfiDataBits % ratio == 0)
  val slowConfig: DramConfig = fastConfig.copy(nPhases = fastConfig.nPhases * ratio)
  require(slowConfig.dfiDataBits * ratio == fastConfig.dfiDataBits)

  val io = IO(new Bundle {
    val fastClock = Input(Clock())
    val fastReset = Input(AsyncReset())
    val slow = Input(new DfiInterface(slowConfig))
    val fast = Output(new DfiInterface(fastConfig))
    val fastRead = Input(Vec(fastConfig.nPhases, new DfiReadResponse(fastConfig)))
    val slowRead = Output(Vec(slowConfig.nPhases, new DfiReadResponse(slowConfig)))
    val slot = Output(UInt(log2Ceil(ratio).W))
  })

  private val slot = withClockAndReset(io.fastClock, io.fastReset) {
    val value = RegInit(0.U(log2Ceil(ratio).W))
    value := Mux(value === (ratio - 1).U, 0.U, value + 1.U)
    value
  }
  io.slot := slot
  io.fast := 0.U.asTypeOf(new DfiInterface(fastConfig))

  for (fastPhaseIndex <- 0 until fastConfig.nPhases) {
    val selectedIndex = (slot * fastConfig.nPhases.U + fastPhaseIndex.U)(
      log2Ceil(slowConfig.nPhases) - 1, 0)
    val source = io.slow.phases(selectedIndex)
    val destination = io.fast.phases(fastPhaseIndex)
    destination.address := source.address
    destination.bank := source.bank
    destination.csN := source.csN
    destination.rasN := source.rasN
    destination.casN := source.casN
    destination.weN := source.weN
    destination.actN := source.actN
    destination.cke := source.cke
    destination.odt := source.odt
    destination.resetN := source.resetN
    destination.rddataEn := source.rddataEn
    destination.wrdataEn := source.wrdataEn
    destination.rddata := io.fastRead(fastPhaseIndex).data
    destination.rddataValid := io.fastRead(fastPhaseIndex).valid

    val writeDataLanes = Wire(Vec(ratio, UInt(slowConfig.dfiDataBits.W)))
    val writeMaskLanes = Wire(Vec(ratio, UInt((slowConfig.dfiDataBits / 8).W)))
    for (lane <- 0 until ratio) {
      writeDataLanes(lane) := io.slow.phases(fastPhaseIndex * ratio + lane).wrdata
      writeMaskLanes(lane) := io.slow.phases(fastPhaseIndex * ratio + lane).wrdataMask
    }
    destination.wrdata := Mux(slot === writeDelay.U, writeDataLanes.asUInt, 0.U)
    destination.wrdataMask := Mux(slot === writeDelay.U, writeMaskLanes.asUInt, 0.U)
  }

  private val capturedRead = withClockAndReset(io.fastClock, io.fastReset) {
    RegInit(VecInit(Seq.fill(fastConfig.nPhases)(0.U(fastConfig.dfiDataBits.W))))
  }
  private val capturedValid = withClockAndReset(io.fastClock, io.fastReset) {
    RegInit(VecInit(Seq.fill(fastConfig.nPhases)(false.B)))
  }
  withClockAndReset(io.fastClock, io.fastReset) {
    when(slot === readDelay.U) {
      for (phase <- 0 until fastConfig.nPhases) {
        capturedRead(phase) := io.fastRead(phase).data
        capturedValid(phase) := io.fastRead(phase).valid
      }
    }
  }
  for (fastPhase <- 0 until fastConfig.nPhases; lane <- 0 until ratio) {
    val slowPhase = fastPhase * ratio + lane
    val low = lane * slowConfig.dfiDataBits
    val high = low + slowConfig.dfiDataBits - 1
    io.slowRead(slowPhase).data := capturedRead(fastPhase)(high, low)
    io.slowRead(slowPhase).valid := capturedValid(fastPhase)
  }
}
