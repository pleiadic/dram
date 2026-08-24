package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/**
  * Logical LSB-first serializer used by simulation PHY wrappers.
  *
  * The module runs in the serial clock domain. `load` may replace the current
  * word on its final edge, allowing back-to-back words without an idle edge.
  * Per-lane output enables are captured with the word and cannot glitch when
  * the parallel-side values change mid-word.
  */
class PhySerializer(edgeCount: Int, laneCount: Int) extends Module {
  require(edgeCount >= 2 && laneCount >= 1)
  private val indexWidth = log2Ceil(edgeCount)

  val io = IO(new Bundle {
    val load = Input(Bool())
    val loadReady = Output(Bool())
    val parallel = Input(Vec(laneCount, UInt(edgeCount.W)))
    val parallelOutputEnable = Input(UInt(laneCount.W))
    val serial = Output(Vec(laneCount, Bool()))
    val serialOutputEnable = Output(UInt(laneCount.W))
    val active = Output(Bool())
    val edge = Output(UInt(indexWidth.W))
  })

  private val held = RegInit(VecInit(Seq.fill(laneCount)(0.U(edgeCount.W))))
  private val heldEnable = RegInit(0.U(laneCount.W))
  private val active = RegInit(false.B)
  private val edge = RegInit(0.U(indexWidth.W))
  private val lastEdge = edge === (edgeCount - 1).U

  io.loadReady := !active || lastEdge
  when(io.load && io.loadReady) {
    held := io.parallel
    heldEnable := io.parallelOutputEnable
    active := true.B
    edge := 0.U
  }.elsewhen(active) {
    when(lastEdge) {
      active := false.B
      edge := 0.U
    }.otherwise {
      edge := edge + 1.U
    }
  }

  for (lane <- 0 until laneCount) {
    io.serial(lane) := active && held(lane)(edge)
  }
  io.serialOutputEnable := Mux(active, heldEnable, 0.U)
  io.active := active
  io.edge := edge
}

/** Logical LSB-first deserializer with explicit first-edge alignment. */
class PhyDeserializer(edgeCount: Int, laneCount: Int) extends Module {
  require(edgeCount >= 2 && laneCount >= 1)
  private val indexWidth = log2Ceil(edgeCount)

  val io = IO(new Bundle {
    val start = Input(Bool())
    val startReady = Output(Bool())
    val serial = Input(Vec(laneCount, Bool()))
    val parallel = Output(Vec(laneCount, UInt(edgeCount.W)))
    val parallelValid = Output(Bool())
    val active = Output(Bool())
    val edge = Output(UInt(indexWidth.W))
  })

  private val samples = RegInit(VecInit(Seq.fill(laneCount)(0.U(edgeCount.W))))
  private val result = RegInit(VecInit(Seq.fill(laneCount)(0.U(edgeCount.W))))
  private val valid = RegInit(false.B)
  private val active = RegInit(false.B)
  private val edge = RegInit(0.U(indexWidth.W))

  valid := false.B
  io.startReady := !active
  when(io.start && io.startReady) {
    for (lane <- 0 until laneCount) {
      samples(lane) := io.serial(lane).asUInt
    }
    active := true.B
    edge := 1.U
  }.elsewhen(active) {
    for (lane <- 0 until laneCount) {
      samples(lane) := samples(lane) | (io.serial(lane).asUInt << edge)
    }
    when(edge === (edgeCount - 1).U) {
      for (lane <- 0 until laneCount) {
        result(lane) := samples(lane) | (io.serial(lane).asUInt << edge)
      }
      valid := true.B
      active := false.B
      edge := 0.U
    }.otherwise {
      edge := edge + 1.U
    }
  }

  io.parallel := result
  io.parallelValid := valid
  io.active := active
  io.edge := edge
}

class Lpddr4SerialPads(config: DramConfig) extends Bundle {
  private val padBytes = config.effectivePadDataBits / 8
  val clock = Bool()
  val clockEnable = Bool()
  val onDieTermination = Bool()
  val resetN = Bool()
  val cs = Bool()
  val ca = Vec(6, Bool())
  val dq = Vec(config.effectivePadDataBits, Bool())
  val dqOutputEnable = Bool()
  val dqs = Vec(padBytes, Bool())
  val dqsOutputEnable = Bool()
  val dmi = Vec(padBytes, Bool())
  val dmiOutputEnable = Bool()
}

/** LPDDR4 parallel-PHY to logical serial-pad simulation boundary. */
class Lpddr4SimulationSerdes(config: DramConfig) extends Module {
  require(config.memType == "LPDDR4" && config.nPhases == 8)
  private val edgeCount = 16
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  private val commandLanes = 5 + 6
  private val laneCount = commandLanes + padBits + 2 * padBytes

  val io = IO(new Bundle {
    val load = Input(Bool())
    val loadReady = Output(Bool())
    val parallel = Input(new Lpddr4PhyOutput(config))
    val serialDqIn = Input(Vec(padBits, Bool()))
    val readStart = Input(Bool())
    val parallelDqIn = Output(Vec(padBits, UInt(edgeCount.W)))
    val parallelDqValid = Output(Bool())
    val pads = Output(new Lpddr4SerialPads(config))
  })

  private def expandSdr(value: UInt): UInt =
    VecInit((0 until edgeCount).map(edge => value(edge / 2))).asUInt

  private val serializer = Module(new PhySerializer(edgeCount, laneCount))
  private val words = Wire(Vec(laneCount, UInt(edgeCount.W)))
  private val enables = Wire(Vec(laneCount, Bool()))
  enables.foreach(_ := true.B)
  private var lane = 0
  words(lane) := io.parallel.clock; lane += 1
  words(lane) := expandSdr(io.parallel.clockEnable); lane += 1
  words(lane) := expandSdr(io.parallel.onDieTermination); lane += 1
  words(lane) := expandSdr(io.parallel.resetN); lane += 1
  words(lane) := expandSdr(io.parallel.cs); lane += 1
  for (line <- 0 until 6) { words(lane) := expandSdr(io.parallel.ca(line)); lane += 1 }
  private val dqBase = lane
  for (bit <- 0 until padBits) {
    words(lane) := io.parallel.dq(bit)
    enables(lane) := io.parallel.dqOutputEnable
    lane += 1
  }
  private val dqsBase = lane
  for (byte <- 0 until padBytes) {
    words(lane) := io.parallel.dqs(byte)
    enables(lane) := io.parallel.dqsOutputEnable
    lane += 1
  }
  private val dmiBase = lane
  for (byte <- 0 until padBytes) {
    words(lane) := io.parallel.dmi(byte)
    enables(lane) := io.parallel.dmiOutputEnable
    lane += 1
  }

  serializer.io.load := io.load
  serializer.io.parallel := words
  serializer.io.parallelOutputEnable := enables.asUInt
  io.loadReady := serializer.io.loadReady
  io.pads.clock := serializer.io.serial(0)
  io.pads.clockEnable := serializer.io.serial(1)
  io.pads.onDieTermination := serializer.io.serial(2)
  io.pads.resetN := serializer.io.serial(3)
  io.pads.cs := serializer.io.serial(4)
  for (line <- 0 until 6) io.pads.ca(line) := serializer.io.serial(5 + line)
  for (bit <- 0 until padBits) io.pads.dq(bit) := serializer.io.serial(dqBase + bit)
  io.pads.dqOutputEnable := serializer.io.serialOutputEnable(dqBase)
  for (byte <- 0 until padBytes) io.pads.dqs(byte) := serializer.io.serial(dqsBase + byte)
  io.pads.dqsOutputEnable := serializer.io.serialOutputEnable(dqsBase)
  for (byte <- 0 until padBytes) io.pads.dmi(byte) := serializer.io.serial(dmiBase + byte)
  io.pads.dmiOutputEnable := serializer.io.serialOutputEnable(dmiBase)

  private val deserializer = Module(new PhyDeserializer(edgeCount, padBits))
  deserializer.io.start := io.readStart
  deserializer.io.serial := io.serialDqIn
  io.parallelDqIn := deserializer.io.parallel
  io.parallelDqValid := deserializer.io.parallelValid
}

class Lpddr5SerialPads(config: DramConfig) extends Bundle {
  private val padBytes = config.effectivePadDataBits / 8
  val resetN = Bool()
  val clock = Bool()
  val cs = Bool()
  val ca = Vec(7, Bool())
  val dq = Vec(config.effectivePadDataBits, Bool())
  val dqOutputEnable = Bool()
  val wck = Vec(padBytes, Bool())
  val readDqs = Vec(padBytes, Bool())
  val readDqsOutputEnable = Bool()
  val dmi = Vec(padBytes, Bool())
  val dmiOutputEnable = Bool()
}

/** LPDDR5 parallel-PHY to logical WCK-edge simulation boundary. */
class Lpddr5SimulationSerdes(config: DramConfig, wckCkRatio: Int) extends Module {
  require(config.memType == "LPDDR5" && config.nPhases == 1)
  require(Set(2, 4).contains(wckCkRatio))
  private val edgeCount = 2 * wckCkRatio
  private val padBits = config.effectivePadDataBits
  private val padBytes = padBits / 8
  private val commandLanes = 3 + 7
  private val laneCount = commandLanes + padBits + 3 * padBytes

  val io = IO(new Bundle {
    val load = Input(Bool())
    val loadReady = Output(Bool())
    val parallel = Input(new Lpddr5PhyOutput(config, wckCkRatio))
    val serialDqIn = Input(Vec(padBits, Bool()))
    val readStart = Input(Bool())
    val parallelDqIn = Output(Vec(padBits, UInt(edgeCount.W)))
    val parallelDqValid = Output(Bool())
    val pads = Output(new Lpddr5SerialPads(config))
  })

  private def expandCk(value: UInt): UInt =
    VecInit((0 until edgeCount).map(edge => value(edge / wckCkRatio))).asUInt

  private val serializer = Module(new PhySerializer(edgeCount, laneCount))
  private val words = Wire(Vec(laneCount, UInt(edgeCount.W)))
  private val enables = Wire(Vec(laneCount, Bool()))
  enables.foreach(_ := true.B)
  private var lane = 0
  words(lane) := Fill(edgeCount, io.parallel.resetN); lane += 1
  words(lane) := expandCk(io.parallel.clock); lane += 1
  words(lane) := Fill(edgeCount, io.parallel.cs); lane += 1
  for (line <- 0 until 7) { words(lane) := expandCk(io.parallel.ca(line)); lane += 1 }
  private val dqBase = lane
  for (bit <- 0 until padBits) {
    words(lane) := io.parallel.dq(bit)
    enables(lane) := io.parallel.dqOutputEnable
    lane += 1
  }
  private val wckBase = lane
  for (byte <- 0 until padBytes) { words(lane) := io.parallel.wck(byte); lane += 1 }
  private val readDqsBase = lane
  for (byte <- 0 until padBytes) {
    words(lane) := io.parallel.readDqs(byte)
    enables(lane) := io.parallel.readDqsOutputEnable
    lane += 1
  }
  private val dmiBase = lane
  for (byte <- 0 until padBytes) {
    words(lane) := io.parallel.dmi(byte)
    enables(lane) := io.parallel.dmiOutputEnable
    lane += 1
  }

  serializer.io.load := io.load
  serializer.io.parallel := words
  serializer.io.parallelOutputEnable := enables.asUInt
  io.loadReady := serializer.io.loadReady
  io.pads.resetN := serializer.io.serial(0)
  io.pads.clock := serializer.io.serial(1)
  io.pads.cs := serializer.io.serial(2)
  for (line <- 0 until 7) io.pads.ca(line) := serializer.io.serial(3 + line)
  for (bit <- 0 until padBits) io.pads.dq(bit) := serializer.io.serial(dqBase + bit)
  io.pads.dqOutputEnable := serializer.io.serialOutputEnable(dqBase)
  for (byte <- 0 until padBytes) io.pads.wck(byte) := serializer.io.serial(wckBase + byte)
  for (byte <- 0 until padBytes) io.pads.readDqs(byte) := serializer.io.serial(readDqsBase + byte)
  io.pads.readDqsOutputEnable := serializer.io.serialOutputEnable(readDqsBase)
  for (byte <- 0 until padBytes) io.pads.dmi(byte) := serializer.io.serial(dmiBase + byte)
  io.pads.dmiOutputEnable := serializer.io.serialOutputEnable(dmiBase)

  private val deserializer = Module(new PhyDeserializer(edgeCount, padBits))
  deserializer.io.start := io.readStart
  deserializer.io.serial := io.serialDqIn
  io.parallelDqIn := deserializer.io.parallel
  io.parallelDqValid := deserializer.io.parallelValid
}

class RpcSerialPads(config: DramConfig) extends Bundle {
  val clock = Bool()
  val strobe = Bool()
  val chipSelectN = Bool()
  val dqs = Bool()
  val dqsOutputEnable = Bool()
  val db = Vec(config.effectivePadDataBits, Bool())
  val dbOutputEnable = Bool()
}

/** RPC parallel-PHY to logical 8-edge simulation boundary. */
class RpcSimulationSerdes(config: DramConfig) extends Module {
  require(config.memType == "RPC" && config.nPhases == 4)
  private val edgeCount = 8
  private val padBits = config.effectivePadDataBits
  private val dbBase = 4
  private val laneCount = dbBase + padBits

  val io = IO(new Bundle {
    val load = Input(Bool())
    val loadReady = Output(Bool())
    val parallel = Input(new RpcPhyOutput(config))
    val serialDbIn = Input(Vec(padBits, Bool()))
    val readStart = Input(Bool())
    val parallelDbIn = Output(Vec(padBits, UInt(edgeCount.W)))
    val parallelDbValid = Output(Bool())
    val pads = Output(new RpcSerialPads(config))
  })

  private val serializer = Module(new PhySerializer(edgeCount, laneCount))
  private val words = Wire(Vec(laneCount, UInt(edgeCount.W)))
  private val enables = Wire(Vec(laneCount, Bool()))
  enables.foreach(_ := true.B)
  words(0) := io.parallel.clock
  words(1) := io.parallel.strobe
  words(2) := io.parallel.chipSelectN
  words(3) := io.parallel.dqs
  enables(3) := io.parallel.dqsOutputEnable
  for (bit <- 0 until padBits) {
    words(dbBase + bit) := io.parallel.db(bit)
    enables(dbBase + bit) := io.parallel.dbOutputEnable
  }

  serializer.io.load := io.load
  serializer.io.parallel := words
  serializer.io.parallelOutputEnable := enables.asUInt
  io.loadReady := serializer.io.loadReady
  io.pads.clock := serializer.io.serial(0)
  io.pads.strobe := serializer.io.serial(1)
  io.pads.chipSelectN := serializer.io.serial(2)
  io.pads.dqs := serializer.io.serial(3)
  io.pads.dqsOutputEnable := serializer.io.serialOutputEnable(3)
  for (bit <- 0 until padBits) io.pads.db(bit) := serializer.io.serial(dbBase + bit)
  io.pads.dbOutputEnable := serializer.io.serialOutputEnable(dbBase)

  private val deserializer = Module(new PhyDeserializer(edgeCount, padBits))
  deserializer.io.start := io.readStart
  deserializer.io.serial := io.serialDbIn
  io.parallelDbIn := deserializer.io.parallel
  io.parallelDbValid := deserializer.io.parallelValid
}
