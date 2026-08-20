package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/** LPDDR5 WCK synchronization and leveling pattern generator. */
class Lpddr5WckGenerator(wckCkRatio: Int, tWckenlWrite: Int,
    tWckenlRead: Int, tWckpreStatic: Int) extends Module {
  require(Set(2, 4).contains(wckCkRatio))
  require(tWckenlWrite >= 0 && tWckenlRead >= 0 && tWckpreStatic >= 1)
  private val edgeCount = 2 * wckCkRatio
  private val syncDelay = Seq(tWckenlWrite, tWckenlRead).max + tWckpreStatic

  val io = IO(new Bundle {
    val sync = Input(UInt(2.W))
    val syncDone = Input(Bool())
    val levelingEnable = Input(Bool())
    val levelingStrobe = Input(Bool())
    val pattern = Output(UInt(edgeCount.W))
    val state = Output(UInt(3.W))
  })

  private val syncTaps = RegInit(VecInit(Seq.fill(syncDelay)(0.U(2.W))))
  syncTaps(0) := io.sync
  for (tap <- 1 until syncDelay) syncTaps(tap) := syncTaps(tap - 1)
  private def syncAt(delay: Int): UInt = if (delay == 0) io.sync else syncTaps(delay - 1)

  private val disabled :: static :: toggle :: toggleFull :: postamble :: postambleTwo :: Nil =
    Enum(6)
  private val state = RegInit(disabled)
  switch(state) {
    is(disabled) {
      when(syncAt(tWckenlWrite) === Lpddr5WckSync.write ||
          syncAt(tWckenlRead) === Lpddr5WckSync.read) {
        state := static
      }
    }
    is(static) {
      when(syncAt(tWckenlWrite + tWckpreStatic) === Lpddr5WckSync.write ||
          syncAt(tWckenlRead + tWckpreStatic) === Lpddr5WckSync.read) {
        state := toggle
      }
    }
    is(toggle) {
      when(!io.syncDone) { state := postamble }
        .elsewhen((wckCkRatio == 4).B) { state := toggleFull }
    }
    is(toggleFull) {
      when(!io.syncDone) { state := postamble }
    }
    is(postamble) {
      state := (if (wckCkRatio == 4) disabled else postambleTwo)
    }
    is(postambleTwo) { state := disabled }
  }

  private val normalPattern = WireDefault(0.U(edgeCount.W))
  switch(state) {
    is(toggle) { normalPattern := (if (wckCkRatio == 2) "h5".U else "h33".U) }
    is(toggleFull) { normalPattern := "h55".U }
    is(postamble) {
      normalPattern := (if (wckCkRatio == 2) "h5".U else "h15".U)
    }
    is(postambleTwo) { normalPattern := 1.U }
  }

  private val levelingTaps = RegInit(VecInit(Seq.fill(4)(false.B)))
  levelingTaps(0) := io.levelingStrobe
  for (tap <- 1 until 4) levelingTaps(tap) := levelingTaps(tap - 1)
  private val levelingPattern = if (wckCkRatio == 2) "h5".U else "h33".U
  io.pattern := Mux(io.levelingEnable,
    Mux(levelingTaps.asUInt.orR, levelingPattern, 0.U), normalPattern)
  io.state := state
}

/** Parallel, LSB-first signals to be connected to LPDDR5 I/O serializers. */
class Lpddr5PhyOutput(config: DramConfig, wckCkRatio: Int) extends Bundle {
  private val edgeCount = 2 * wckCkRatio
  private val padBytes = config.effectivePadDataBits / 8
  val resetN = Bool()
  val clock = UInt(2.W)
  val cs = Bool()
  val ca = Vec(7, UInt(2.W))
  val dq = Vec(config.effectivePadDataBits, UInt(edgeCount.W))
  val dqOutputEnable = Bool()
  val wck = Vec(padBytes, UInt(edgeCount.W))
  val readDqs = Vec(padBytes, UInt(edgeCount.W))
  val readDqsOutputEnable = Bool()
  val dmi = Vec(padBytes, UInt(edgeCount.W))
  val dmiOutputEnable = Bool()
}

/**
  * Technology-independent LPDDR5 PHY core with BL16 data conversion. Physical
  * serializers, differential buffers, and tristates remain in an outer
  * target-specific wrapper.
  */
class Lpddr5Phy(config: DramConfig, wckCkRatio: Int, readLatency: Int,
    writeLatency: Int, tWckenlWrite: Int, tWckenlRead: Int,
    tWckpreStatic: Int, maskedWrite: Boolean = true,
    readWckHoldCycles: Int = -1, writeWckHoldCycles: Int = -1) extends Module {
  require(config.memType == "LPDDR5")
  require(config.nranks == 1 && config.nPhases == 1)
  require(config.bankBits >= 7 && config.rowBits.max(config.columnBits).max(11) >= 18)
  require(Set(2, 4).contains(wckCkRatio))
  require(config.dfiDataBits == 16 * config.effectivePadDataBits,
    "LPDDR5 DFI data must contain one complete BL16 transfer")
  require(readLatency >= 1 && writeLatency >= 1)

  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  private val edgeCount = 2 * wckCkRatio
  private val burstCycles = 16 / edgeCount
  private val chunkWidth = edgeCount * padBits
  private val maskChunkWidth = edgeCount * padBytes
  private val writeTap = writeLatency - 1
  private val readStart = readLatency - burstCycles - 1
  require(readStart >= 0,
    s"readLatency must be at least burstCycles + 1 (${burstCycles + 1})")
  private val actualReadWckHold = if (readWckHoldCycles < 0) readLatency + 1 else readWckHoldCycles
  private val actualWriteWckHold = if (writeWckHoldCycles < 0) writeLatency + burstCycles + 1
    else writeWckHoldCycles
  require(actualReadWckHold >= 1 && actualWriteWckHold >= 1)

  val io = IO(new Bundle {
    val dfi = Input(new DfiInterface(config))
    val dqIn = Input(Vec(padBits, UInt(edgeCount.W)))
    val delaySelect = Input(UInt(padBytes.W))
    val phyReset = Input(Bool())
    val readBitslipReset = Input(Bool())
    val readBitslip = Input(Bool())
    val writeBitslipReset = Input(Bool())
    val writeBitslip = Input(Bool())
    val writeLevelingEnable = Input(Bool())
    val writeLevelingStrobe = Input(Bool())
    val output = Output(new Lpddr5PhyOutput(config, wckCkRatio))
    val read = Output(new DfiReadResponse(config))
    val commandAccepted = Output(Bool())
    val commandDropped = Output(Bool())
    val writeChunkValid = Output(Bool())
    val readCapture = Output(Bool())
    val wckSyncDone = Output(Bool())
    val wckState = Output(UInt(3.W))
  })

  private val phase = io.dfi.phases(0)
  private val command = Module(new Lpddr5CommandPipeline(config, maskedWrite))
  command.io.phase := phase

  private val syncDone = RegInit(false.B)
  private val holdWidth = log2Ceil((Seq(actualReadWckHold, actualWriteWckHold).max + 1) max 2)
  private val syncHold = RegInit(0.U(holdWidth.W))
  command.io.wckSyncDone := syncDone
  when(command.io.wckSync =/= Lpddr5WckSync.none) {
    syncDone := true.B
    syncHold := Mux(command.io.wckSync === Lpddr5WckSync.read,
      actualReadWckHold.U, actualWriteWckHold.U)
  }.elsewhen(syncHold =/= 0.U) {
    syncHold := syncHold - 1.U
    when(syncHold === 1.U) {
      syncDone := false.B
    }
  }

  io.output.resetN := phase.resetN
  io.output.clock := 1.U
  io.output.cs := command.io.cs
  for (line <- 0 until 7) {
    io.output.ca(line) := VecInit((0 until 2).map(edge => command.io.ca(edge)(line))).asUInt
  }
  io.commandAccepted := command.io.accepted
  io.commandDropped := command.io.dropped

  private def selectedReset(byte: Int, read: Boolean): Bool = io.phyReset ||
    (io.delaySelect(byte) && (if (read) io.readBitslipReset else io.writeBitslipReset))
  private def selectedSlip(byte: Int, read: Boolean): Bool = io.delaySelect(byte) &&
    (if (read) io.readBitslip else io.writeBitslip)

  private val wck = Module(new Lpddr5WckGenerator(wckCkRatio, tWckenlWrite,
    tWckenlRead, tWckpreStatic))
  wck.io.sync := command.io.wckSync
  wck.io.syncDone := syncDone
  wck.io.levelingEnable := io.writeLevelingEnable
  wck.io.levelingStrobe := io.writeLevelingStrobe
  for (byte <- 0 until padBytes) {
    val bitslip = Module(new BitSlip(edgeCount))
    bitslip.io.in := wck.io.pattern
    bitslip.io.resetSlip := selectedReset(byte, read = false)
    bitslip.io.slip := selectedSlip(byte, read = false)
    io.output.wck(byte) := bitslip.io.out
  }
  io.wckSyncDone := syncDone
  io.wckState := wck.io.state

  private val writeEnableTaps = RegInit(VecInit(Seq.fill(writeTap + burstCycles + 1)(false.B)))
  writeEnableTaps(0) := phase.wrdataEn
  for (tap <- 1 until writeEnableTaps.length) writeEnableTaps(tap) := writeEnableTaps(tap - 1)
  private val writeStart = writeEnableTaps(writeTap)
  private val writeWindow = writeEnableTaps.slice(writeTap, writeTap + burstCycles).reduce(_ || _)
  private val heldWrite = RegInit(0.U(config.dfiDataBits.W))
  private val heldMask = RegInit(0.U((config.dfiDataBits / 8).W))
  private val chunkIndexWidth = log2Ceil(burstCycles max 2)
  private val writeChunkIndex = RegInit(0.U(chunkIndexWidth.W))
  private val writeChunksActive = RegInit(false.B)
  when(writeStart) {
    heldWrite := phase.wrdata
    heldMask := phase.wrdataMask
    writeChunkIndex := (if (burstCycles == 1) 0.U else 1.U)
    writeChunksActive := (burstCycles > 1).B
  }.elsewhen(writeChunksActive) {
    when(writeChunkIndex === (burstCycles - 1).U) {
      writeChunksActive := false.B
      writeChunkIndex := 0.U
    }.otherwise {
      writeChunkIndex := writeChunkIndex + 1.U
    }
  }
  private val writeSource = Mux(writeStart, phase.wrdata, heldWrite)
  private val maskSource = Mux(writeStart, phase.wrdataMask, heldMask)
  private val selectedWriteChunk = Mux(writeStart, 0.U, writeChunkIndex)
  private val writeChunk = writeSource.asTypeOf(Vec(burstCycles, UInt(chunkWidth.W)))(selectedWriteChunk)
  private val maskChunk = maskSource.asTypeOf(Vec(burstCycles,
    UInt(maskChunkWidth.W)))(selectedWriteChunk)
  io.writeChunkValid := writeStart || writeChunksActive

  for (bit <- 0 until padBits) {
    val value = VecInit((0 until edgeCount).map(edge => writeChunk(edge * padBits + bit))).asUInt
    val bitslip = Module(new BitSlip(edgeCount))
    bitslip.io.in := value
    bitslip.io.resetSlip := selectedReset(bit / 8, read = false)
    bitslip.io.slip := selectedSlip(bit / 8, read = false)
    io.output.dq(bit) := bitslip.io.out
  }
  for (byte <- 0 until padBytes) {
    val value = VecInit((0 until edgeCount).map(edge =>
      maskChunk(edge * padBytes + byte))).asUInt
    val bitslip = Module(new BitSlip(edgeCount))
    bitslip.io.in := Mux(maskedWrite.B, value, 0.U)
    bitslip.io.resetSlip := selectedReset(byte, read = false)
    bitslip.io.slip := selectedSlip(byte, read = false)
    io.output.dmi(byte) := bitslip.io.out
  }
  io.output.dqOutputEnable := RegNext(writeWindow, false.B)
  io.output.dmiOutputEnable := (if (maskedWrite) io.output.dqOutputEnable else false.B)

  private val readSlips = Seq.tabulate(padBits) { bit =>
    val bitslip = Module(new BitSlip(edgeCount))
    bitslip.io.in := io.dqIn(bit)
    bitslip.io.resetSlip := selectedReset(bit / 8, read = true)
    bitslip.io.slip := selectedSlip(bit / 8, read = true)
    bitslip
  }
  private val readChunk = VecInit((0 until edgeCount).flatMap { edge =>
    (0 until padBits).map(bit => readSlips(bit).io.out(edge))
  }).asUInt
  private val readEnableTaps = RegInit(VecInit(Seq.fill(readLatency + burstCycles)(false.B)))
  readEnableTaps(0) := phase.rddataEn
  for (tap <- 1 until readEnableTaps.length) readEnableTaps(tap) := readEnableTaps(tap - 1)
  private val readWindow = readEnableTaps.slice(readStart,
    readStart + burstCycles).reduce(_ || _)
  private val readChunkIndex = RegInit(0.U(chunkIndexWidth.W))
  private val readChunks = RegInit(VecInit(Seq.fill(burstCycles)(0.U(chunkWidth.W))))
  private val readResult = RegInit(0.U(config.dfiDataBits.W))
  private val readResultValid = RegInit(false.B)
  readResultValid := false.B
  when(readWindow) {
    readChunks(readChunkIndex) := readChunk
    when(readChunkIndex === (burstCycles - 1).U) {
      val completed = Wire(Vec(burstCycles, UInt(chunkWidth.W)))
      completed := readChunks
      completed(readChunkIndex) := readChunk
      readResult := completed.asUInt
      readResultValid := true.B
      readChunkIndex := 0.U
    }.otherwise {
      readChunkIndex := readChunkIndex + 1.U
    }
  }.otherwise {
    readChunkIndex := 0.U
  }
  io.read.data := readResult
  io.read.valid := readResultValid
  io.readCapture := readWindow

  io.output.readDqs.foreach(_ := 0.U)
  io.output.readDqsOutputEnable := false.B
}
