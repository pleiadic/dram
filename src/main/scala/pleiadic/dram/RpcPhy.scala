package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/** Parallel, LSB-first RPC signals for an outer technology-specific serializer. */
class RpcPhyOutput(config: DramConfig) extends Bundle {
  val clock = UInt(8.W)
  val strobe = UInt(8.W)
  val chipSelectN = UInt(8.W)
  val dqs = UInt(8.W)
  val dqsOutputEnable = Bool()
  val db = Vec(config.effectivePadDataBits, UInt(8.W))
  val dbOutputEnable = Bool()
}

object RpcInitializationState {
  val width = 3
  val idle = 0.U(width.W)
  val resetReceived = 1.U(width.W)
  val serialReset = 2.U(width.W)
  val resetDone = 3.U(width.W)
  val zqCalibration = 4.U(width.W)
  val ready = 5.U(width.W)
  val utilityMode = 6.U(width.W)
}

/**
  * Technology-independent RPC PHY core corresponding to LiteDRAM's RPC
  * BasePHY. I/O delay elements, differential buffers, serializers and
  * tristates intentionally remain in an outer target-specific wrapper.
  *
  * `readCaptureDelay` is the number of controller cycles from rddataEn to the
  * first of two deserialized DB chunks. `readLatency` is the DFI response
  * latency and must leave one cycle for the 16-edge bitslip after capture.
  */
class RpcPhy(config: DramConfig, readLatency: Int, writeLatency: Int,
    readCaptureDelay: Int, resetCycles: Int, zqCalibrationCycles: Int,
    burstStop: Boolean = true) extends Module {
  require(config.memType == "RPC")
  require(config.nranks == 1 && config.nPhases == 4)
  require(config.effectivePadDataBits == 16)
  require(config.dfiDataBits == 64,
    "RPC uses four 64-bit DFI phases to carry one 256-bit BL16 transfer")
  require(config.bankBits >= 2 && config.rowBits.max(config.columnBits).max(11) >= 12)
  require(writeLatency >= 0)
  require(readCaptureDelay >= 0)
  require(readLatency >= readCaptureDelay + 3,
    "readLatency must cover two capture cycles and the read bitslip register")
  require(resetCycles >= 4, "RPC serial reset occupies four controller cycles")
  require(zqCalibrationCycles >= 1)

  private val nPhases = 4
  private val padBits = 16
  private val edgeCount = 8
  private val wrPhase = 3
  private val rdPhase = 3

  val io = IO(new Bundle {
    val dfi = Input(new DfiInterface(config))
    val dbIn = Input(Vec(padBits, UInt(edgeCount.W)))
    val dqsIn = Input(UInt(edgeCount.W))
    val delaySelect = Input(UInt(2.W))
    val phyReset = Input(Bool())
    val readBitslipReset = Input(Bool())
    val readBitslip = Input(Bool())
    val bypassInitialization = Input(Bool())
    val restartInitialization = Input(Bool())
    val clearChipSelectLock = Input(Bool())
    val output = Output(new RpcPhyOutput(config))
    val read = Output(Vec(nPhases, new DfiReadResponse(config)))
    val initializationState = Output(UInt(RpcInitializationState.width.W))
    val resetDone = Output(Bool())
    val initializationDone = Output(Bool())
    val commandAllowed = Output(Bool())
    val commandOutputEnable = Output(Bool())
    val maskOutputEnable = Output(Bool())
    val dataOutputEnable = Output(Bool())
    val readCapture = Output(Bool())
  })

  // The RPC command and write-data mux spans the current DFI cycle and the two
  // preceding cycles. Read-return fields are deliberately irrelevant here.
  private val history1 = RegInit(0.U.asTypeOf(new DfiInterface(config)))
  private val history2 = RegInit(0.U.asTypeOf(new DfiInterface(config)))
  private val history1Valid = RegNext(true.B, false.B)
  private val history2Valid = RegNext(history1Valid, false.B)
  history1 := io.dfi
  history2 := history1

  private def selected(phase: DfiPhase): Bool = !phase.csN.asUInt.andR
  private def activate(phase: DfiPhase): Bool = selected(phase) &&
    !phase.rasN && phase.casN && phase.weN
  private def readCommand(phase: DfiPhase): Bool = selected(phase) &&
    phase.rasN && !phase.casN && phase.weN
  private def writeCommand(phase: DfiPhase): Bool = selected(phase) &&
    phase.rasN && !phase.casN && !phase.weN
  private def precharge(phase: DfiPhase): Bool = selected(phase) &&
    !phase.rasN && phase.casN && !phase.weN
  private def zqCalibration(phase: DfiPhase): Bool = selected(phase) &&
    phase.rasN && phase.casN && !phase.weN
  private def modeRegister(phase: DfiPhase): Bool = selected(phase) &&
    !phase.rasN && !phase.casN && !phase.weN
  private def resetCommand(phase: DfiPhase): Bool = !phase.resetN && activate(phase)
  private def utilityCommand(phase: DfiPhase): Bool = !phase.resetN && modeRegister(phase)
  private def zqInitialization(phase: DfiPhase): Bool =
    !phase.resetN && zqCalibration(phase) && phase.address(10)

  // Oldest-to-newest ordering is part of the RPC preamble algorithm.
  private val adapterPhases = history2.phases ++ history1.phases ++ io.dfi.phases
  private val adapters = adapterPhases.map { phase =>
    val adapter = Module(new RpcDfiAdapter(config))
    adapter.io.phase := phase
    adapter.io.burstCount := 0.U
    adapter.io.refreshOperation := 0.U
    adapter
  }
  private val adapterCommandValid = adapters.indices.map { index =>
    val historyValid = if (index < nPhases) history2Valid
      else if (index < 2 * nPhases) history1Valid else true.B
    adapters(index).io.commandValid && historyValid
  }

  // Power-up sequencing. The timing parameters are explicit controller-cycle
  // counts so simulations can use short values while board wrappers can derive
  // the JEDEC 5 us and 1 us waits from their system clock.
  private val state = RegInit(RpcInitializationState.idle)
  private val resetCountWidth = log2Ceil((resetCycles + 1) max 2)
  private val resetCount = RegInit(0.U(resetCountWidth.W))
  private val serialCount = RegInit(0.U(3.W))
  private val zqCountWidth = log2Ceil((zqCalibrationCycles + 1) max 2)
  private val zqCount = RegInit(0.U(zqCountWidth.W))
  private val phase0 = io.dfi.phases(0)
  private val phase3 = io.dfi.phases(rdPhase)
  private val permissionInput = WireDefault(false.B)
  private val serialResetActive = WireDefault(false.B)

  switch(state) {
    is(RpcInitializationState.idle) {
      resetCount := 0.U
      serialCount := 0.U
      when(resetCommand(phase0)) {
        permissionInput := true.B
        state := RpcInitializationState.resetReceived
      }
    }
    is(RpcInitializationState.resetReceived) {
      resetCount := 0.U
      serialCount := 0.U
      state := RpcInitializationState.serialReset
    }
    is(RpcInitializationState.serialReset) {
      when(serialCount < 4.U) {
        serialResetActive := true.B
        serialCount := serialCount + 1.U
      }
      when(resetCount === (resetCycles - 1).U) {
        state := RpcInitializationState.resetDone
        resetCount := 0.U
      }.otherwise {
        resetCount := resetCount + 1.U
      }
    }
    is(RpcInitializationState.resetDone) {
      permissionInput := precharge(phase0) || modeRegister(phase0) ||
        zqInitialization(phase0)
      when(zqInitialization(phase0)) {
        zqCount := 0.U
        state := RpcInitializationState.zqCalibration
      }
    }
    is(RpcInitializationState.zqCalibration) {
      when(zqCount === (zqCalibrationCycles - 1).U) {
        state := RpcInitializationState.ready
        zqCount := 0.U
      }.otherwise {
        zqCount := zqCount + 1.U
      }
    }
    is(RpcInitializationState.ready) {
      permissionInput := true.B
      when(utilityCommand(phase0) && phase0.address(0)) {
        state := RpcInitializationState.utilityMode
      }
    }
    is(RpcInitializationState.utilityMode) {
      permissionInput := utilityCommand(phase0) || readCommand(phase3)
      when(utilityCommand(phase0) && !phase0.address(0)) {
        state := RpcInitializationState.ready
      }
    }
  }
  when(io.restartInitialization) {
    state := RpcInitializationState.idle
    resetCount := 0.U
    serialCount := 0.U
    zqCount := 0.U
  }

  private val permissionHistory1 = RegNext(permissionInput, false.B)
  private val permissionHistory2 = RegNext(permissionHistory1, false.B)
  private val commandAllowed = io.bypassInitialization || permissionInput ||
    permissionHistory1 || permissionHistory2
  io.commandAllowed := commandAllowed
  io.initializationState := state
  io.resetDone := io.bypassInitialization || state === RpcInitializationState.resetDone ||
    state === RpcInitializationState.zqCalibration || state === RpcInitializationState.ready ||
    state === RpcInitializationState.utilityMode
  io.initializationDone := io.bypassInitialization || state === RpcInitializationState.ready ||
    state === RpcInitializationState.utilityMode

  io.output.clock := "h55".U

  // Request Packet: positive then negative sample for each of four phases.
  private val commandEnable = adapterCommandValid.take(nPhases).reduce(_ || _)
  private val commandDb = Wire(Vec(padBits, UInt(edgeCount.W)))
  for (bit <- 0 until padBits) {
    commandDb(bit) := VecInit(adapters.take(nPhases).flatMap(adapter =>
      Seq(adapter.io.dbPositive(bit), adapter.io.dbNegative(bit)))).asUInt
  }

  // CS# locks low once phase 0 alone selects the device, matching the original
  // board workaround. It can be explicitly released by the wrapper/CSR path.
  private val chipSelectLock = RegInit(false.B)
  private val lockCondition = !phase0.csN(0) &&
    io.dfi.phases.drop(1).map(_.csN(0)).reduce(_ && _)
  when(lockCondition) { chipSelectLock := true.B }
  when(io.clearChipSelectLock || io.restartInitialization) { chipSelectLock := false.B }
  io.output.chipSelectN := Fill(edgeCount, !chipSelectLock)

  // STB preamble, serial reset, and optional burst-stop waveform.
  private val readOrWriteSent = adapterCommandValid(3) &&
    (readCommand(history2.phases(3)) || writeCommand(history2.phases(3)))
  private val readOrWriteSentDelayed = RegNext(readOrWriteSent, false.B)
  private val burstStopPatterns = Seq(Seq(false.B, false.B), Seq(false.B, true.B),
    Seq(false.B, false.B), Seq(true.B, true.B))
  private val strobeBits = (0 until nPhases).flatMap { phase =>
    val preamble = (adapterCommandValid(phase + 2) ||
      adapterCommandValid(phase + 1)) && commandAllowed
    val sent = if (phase == 3) readOrWriteSent else readOrWriteSentDelayed
    val pattern = burstStopPatterns((phase + 1) % 4)
    pattern.map(bit => !(preamble || serialResetActive ||
      (commandAllowed && sent && !bit && burstStop.B)))
  }
  io.output.strobe := VecInit(strobeBits).asUInt

  // Write enable timing follows the reference ShiftRegister convention: tap
  // zero is the current request and tap N is delayed N cycles.
  private val writeRequest = phase3.wrdataEn && commandAllowed
  private val writeDelay = RegInit(VecInit(Seq.fill(writeLatency + 2)(false.B)))
  if (writeDelay.nonEmpty) {
    writeDelay(0) := writeRequest
    for (tap <- 1 until writeDelay.length) writeDelay(tap) := writeDelay(tap - 1)
  }
  private def writeAt(delay: Int): Bool = if (delay == 0) writeRequest else writeDelay(delay - 1)
  private val maskEnable = writeAt(writeLatency)
  private val dataEnable = writeAt(writeLatency + 1) || writeAt(writeLatency + 2)
  private val dataCycle = RegInit(false.B)
  when(dataEnable) { dataCycle := !dataCycle }.otherwise { dataCycle := false.B }

  private val dataDb = Wire(Vec(padBits, UInt(edgeCount.W)))
  private val maskDb = Wire(Vec(padBits, UInt(edgeCount.W)))
  for (bit <- 0 until padBits) {
    val allEdges = (0 until nPhases).flatMap { phase =>
      val source = if (phase < 2) history1.phases(phase) else history2.phases(phase)
      (0 until 4).map(word => source.wrdata(word * padBits + bit))
    }
    dataDb(bit) := Mux(dataCycle,
      VecInit(allEdges.drop(edgeCount)).asUInt, VecInit(allEdges.take(edgeCount)).asUInt)
    val byte = bit / 8
    val lane = bit % 8
    maskDb(bit) := VecInit(Seq(false.B, false.B, false.B, false.B,
      io.dfi.phases(byte).wrdataMask(lane),
      io.dfi.phases(byte + 2).wrdataMask(lane), true.B, true.B)).asUInt
  }

  private val dbOutputEnable = commandAllowed && (dataEnable || maskEnable || commandEnable)
  for (bit <- 0 until padBits) {
    io.output.db(bit) := Mux(dataEnable, dataDb(bit),
      Mux(maskEnable, maskDb(bit), commandDb(bit)))
  }
  io.output.dbOutputEnable := dbOutputEnable
  io.commandOutputEnable := commandAllowed && commandEnable
  io.maskOutputEnable := commandAllowed && maskEnable
  io.dataOutputEnable := commandAllowed && dataEnable

  // DQS patterns are phase-specific for Request Packets, then use a mask
  // preamble followed by a continuous 0101 data pattern.
  private val phasePatterns = Seq(
    ("h40".U(8.W), "h01".U(8.W)),
    ("h00".U(8.W), "h05".U(8.W)),
    ("h00".U(8.W), "h14".U(8.W)),
    ("h00".U(8.W), "h50".U(8.W)))
  private val phaseValid = (0 until nPhases).map(phase =>
    adapterCommandValid(phase) || adapterCommandValid(nPhases + phase))
  private val commandDqsFirst = WireDefault(0.U(8.W))
  private val commandDqsSecond = WireDefault(0.U(8.W))
  // Higher-numbered phases have the same priority as the nested Migen Ifs.
  for (phase <- 0 until nPhases) {
    when(phaseValid(phase)) {
      commandDqsFirst := phasePatterns(phase)._1
      commandDqsSecond := phasePatterns(phase)._2
    }
  }
  private val dqsCounter = RegInit(false.B)
  private val anyPhaseValid = phaseValid.reduce(_ || _)
  private val dqsOutputEnable = commandAllowed && (anyPhaseValid || maskEnable || dataEnable)
  when(dqsOutputEnable && !maskEnable) { dqsCounter := !dqsCounter }
    .otherwise { dqsCounter := false.B }
  io.output.dqs := Mux(maskEnable, Mux(dqsCounter, "h55".U, "h54".U),
    Mux(dataEnable, "h55".U,
      Mux(dqsCounter, commandDqsSecond, commandDqsFirst)))
  io.output.dqsOutputEnable := dqsOutputEnable

  // Two deserialized 8-edge chunks form one BL16 read. BitSlip intentionally
  // adds the same one-cycle registered history as LiteDRAM's primitive.
  private val readRequest = phase3.rddataEn
  private val readDelay = RegInit(VecInit(Seq.fill(readLatency)(false.B)))
  readDelay(0) := readRequest
  for (tap <- 1 until readDelay.length) readDelay(tap) := readDelay(tap - 1)
  private def readAt(delay: Int): Bool = if (delay == 0) readRequest else readDelay(delay - 1)
  private val readCapture = readAt(readCaptureDelay) || readAt(readCaptureDelay + 1)
  private val readCaptureSecond = RegInit(false.B)
  private val capturedRead = RegInit(VecInit(Seq.fill(padBits)(0.U(16.W))))
  when(readCapture) {
    for (bit <- 0 until padBits) {
      when(readCaptureSecond) {
        capturedRead(bit) := Cat(io.dbIn(bit), capturedRead(bit)(7, 0))
      }.otherwise {
        capturedRead(bit) := Cat(0.U(8.W), io.dbIn(bit))
      }
    }
    readCaptureSecond := !readCaptureSecond
  }.otherwise {
    readCaptureSecond := false.B
  }
  private val slippedRead = Seq.tabulate(padBits) { bit =>
    val bitslip = Module(new BitSlip(16))
    bitslip.io.in := capturedRead(bit)
    bitslip.io.resetSlip := io.phyReset ||
      (io.delaySelect(bit / 8) && io.readBitslipReset)
    bitslip.io.slip := io.delaySelect(bit / 8) && io.readBitslip
    bitslip.io.out
  }
  private val readValid = readAt(readLatency)
  for (phase <- 0 until nPhases) {
    val words = (0 until 4).map { word =>
      VecInit((0 until padBits).map(bit => slippedRead(bit)(phase * 4 + word))).asUInt
    }
    io.read(phase).data := VecInit(words).asUInt
    io.read(phase).valid := readValid
  }
  io.readCapture := readCapture

  // DQS input is consumed by target-specific capture circuitry; keeping it in
  // this portable boundary makes wrappers structurally compatible.
  dontTouch(io.dqsIn)
}
