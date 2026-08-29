package pleiadic.dram

import chisel3._
import chisel3.experimental.IntParam
import chisel3.util._
import scala.language.reflectiveCalls

/** Reusable multi-flop CDC synchronizer with synthesis-recognized attributes. */
class CdcSynchronizer(width: Int = 1, stages: Int = 2)
    extends BlackBox(Map("WIDTH" -> IntParam(width), "STAGES" -> IntParam(stages)))
    with HasBlackBoxInline {
  require(width >= 1 && stages >= 2)
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(AsyncReset())
    val asyncInput = Input(UInt(width.W))
    val syncOutput = Output(UInt(width.W))
  })
  setInline("CdcSynchronizer.sv",
    """module CdcSynchronizer #(
      |  parameter integer WIDTH = 1,
      |  parameter integer STAGES = 2
      |) (
      |  input wire clock, input wire reset,
      |  input wire [WIDTH-1:0] asyncInput,
      |  output wire [WIDTH-1:0] syncOutput
      |);
      |  (* ASYNC_REG = "TRUE", SHREG_EXTRACT = "NO" *)
      |  reg [WIDTH-1:0] pipeline [0:STAGES-1];
      |  integer index;
      |  always @(posedge clock or posedge reset) begin
      |    if (reset) begin
      |      for (index = 0; index < STAGES; index = index + 1)
      |        pipeline[index] <= {WIDTH{1'b0}};
      |    end else begin
      |      pipeline[0] <= asyncInput;
      |      for (index = 1; index < STAGES; index = index + 1)
      |        pipeline[index] <= pipeline[index-1];
      |    end
      |  end
      |  assign syncOutput = pipeline[STAGES-1];
      |endmodule
      |""".stripMargin)
}

/** Toggle-based single-bit pulse transfer. Source pulses must be spaced by the CDC latency. */
class CdcPulseSynchronizer(stages: Int = 2) extends RawModule {
  require(stages >= 2)
  val io = IO(new Bundle {
    val sourceClock = Input(Clock())
    val sourceReset = Input(AsyncReset())
    val destinationClock = Input(Clock())
    val destinationReset = Input(AsyncReset())
    val sourcePulse = Input(Bool())
    val destinationPulse = Output(Bool())
  })
  private val sourceToggle = withClockAndReset(io.sourceClock, io.sourceReset) {
    RegInit(false.B)
  }
  withClockAndReset(io.sourceClock, io.sourceReset) {
    when(io.sourcePulse) { sourceToggle := !sourceToggle }
  }
  private val synchronizer = Module(new CdcSynchronizer(1, stages))
  synchronizer.io.clock := io.destinationClock
  synchronizer.io.reset := io.destinationReset
  synchronizer.io.asyncInput := sourceToggle.asUInt
  private val destinationPrevious = withClockAndReset(
      io.destinationClock, io.destinationReset) { RegInit(false.B) }
  withClockAndReset(io.destinationClock, io.destinationReset) {
    destinationPrevious := synchronizer.io.syncOutput(0)
  }
  io.destinationPulse := synchronizer.io.syncOutput(0) ^ destinationPrevious
}

/**
  * Power-of-two asynchronous FIFO using Gray-coded pointers and two-stage
  * pointer synchronizers. The storage is a dual-clock Mem, suitable for FPGA
  * dual-port RAM inference.
  */
class AsyncQueue[T <: Data](gen: T, depth: Int = 4) extends RawModule {
  require(depth >= 4 && (depth & (depth - 1)) == 0)

  val io = IO(new Bundle {
    val enqueueClock = Input(Clock())
    val enqueueReset = Input(AsyncReset())
    val dequeueClock = Input(Clock())
    val dequeueReset = Input(AsyncReset())
    val enqueue = Flipped(Decoupled(gen.cloneType))
    val dequeue = Decoupled(gen.cloneType)
  })

  private val addressBits = log2Ceil(depth)
  private val pointerBits = addressBits + 1
  private val memory = Mem(depth, gen.cloneType)

  private val writeBinary = withClockAndReset(io.enqueueClock, io.enqueueReset) {
    RegInit(0.U(pointerBits.W))
  }
  private val writeGray = withClockAndReset(io.enqueueClock, io.enqueueReset) {
    RegInit(0.U(pointerBits.W))
  }
  private val full = withClockAndReset(io.enqueueClock, io.enqueueReset) { RegInit(false.B) }

  private val readBinary = withClockAndReset(io.dequeueClock, io.dequeueReset) {
    RegInit(0.U(pointerBits.W))
  }
  private val readGray = withClockAndReset(io.dequeueClock, io.dequeueReset) {
    RegInit(0.U(pointerBits.W))
  }
  private val empty = withClockAndReset(io.dequeueClock, io.dequeueReset) { RegInit(true.B) }

  private val readPointerSynchronizer = Module(new CdcSynchronizer(pointerBits))
  readPointerSynchronizer.io.clock := io.enqueueClock
  readPointerSynchronizer.io.reset := io.enqueueReset
  readPointerSynchronizer.io.asyncInput := readGray
  private val readGraySynchronized = readPointerSynchronizer.io.syncOutput
  private val writePointerSynchronizer = Module(new CdcSynchronizer(pointerBits))
  writePointerSynchronizer.io.clock := io.dequeueClock
  writePointerSynchronizer.io.reset := io.dequeueReset
  writePointerSynchronizer.io.asyncInput := writeGray
  private val writeGraySynchronized = writePointerSynchronizer.io.syncOutput

  private val writeIncrement = io.enqueue.valid && !full
  private val writeBinaryNext = writeBinary + writeIncrement
  private val writeGrayNext = (writeBinaryNext >> 1) ^ writeBinaryNext
  private val invertedReadGray = Cat(
    ~readGraySynchronized(pointerBits - 1, pointerBits - 2),
    readGraySynchronized(pointerBits - 3, 0))
  private val fullNext = writeGrayNext === invertedReadGray

  io.enqueue.ready := !full
  withClockAndReset(io.enqueueClock, io.enqueueReset) {
    when(io.enqueue.fire) { memory.write(writeBinary(addressBits - 1, 0), io.enqueue.bits) }
    writeBinary := writeBinaryNext
    writeGray := writeGrayNext
    full := fullNext
  }

  private val readIncrement = io.dequeue.ready && !empty
  private val readBinaryNext = readBinary + readIncrement
  private val readGrayNext = (readBinaryNext >> 1) ^ readBinaryNext
  private val emptyNext = readGrayNext === writeGraySynchronized

  io.dequeue.valid := !empty
  private val readData = withClock(io.dequeueClock) {
    memory.read(readBinary(addressBits - 1, 0))
  }
  io.dequeue.bits := readData
  withClockAndReset(io.dequeueClock, io.dequeueReset) {
    readBinary := readBinaryNext
    readGray := readGrayNext
    empty := emptyNext
  }
}

/** Clock-domain crossing for the three independent Native-port streams. */
class NativePortCdc(addressWidth: Int, dataWidth: Int, depth: Int = 4) extends RawModule {
  require(addressWidth >= 1 && dataWidth >= 8 && dataWidth % 8 == 0)

  val io = IO(new Bundle {
    val sourceClock = Input(Clock())
    val sourceReset = Input(AsyncReset())
    val destinationClock = Input(Clock())
    val destinationReset = Input(AsyncReset())
    val sourceCommand = Flipped(Decoupled(new NativeAdapterCommand(addressWidth)))
    val destinationCommand = Decoupled(new NativeAdapterCommand(addressWidth))
    val sourceWriteData = Flipped(Decoupled(new NativeAdapterWriteData(dataWidth)))
    val destinationWriteData = Decoupled(new NativeAdapterWriteData(dataWidth))
    val destinationReadData = Flipped(Decoupled(new NativeAdapterReadData(dataWidth)))
    val sourceReadData = Decoupled(new NativeAdapterReadData(dataWidth))
  })

  private def clocks[T <: Data](queue: AsyncQueue[T], forward: Boolean): Unit = {
    if (forward) {
      queue.io.enqueueClock := io.sourceClock
      queue.io.enqueueReset := io.sourceReset
      queue.io.dequeueClock := io.destinationClock
      queue.io.dequeueReset := io.destinationReset
    } else {
      queue.io.enqueueClock := io.destinationClock
      queue.io.enqueueReset := io.destinationReset
      queue.io.dequeueClock := io.sourceClock
      queue.io.dequeueReset := io.sourceReset
    }
  }

  private val commands = Module(new AsyncQueue(new NativeAdapterCommand(addressWidth), depth))
  clocks(commands, forward = true)
  commands.io.enqueue <> io.sourceCommand
  io.destinationCommand <> commands.io.dequeue

  private val writes = Module(new AsyncQueue(new NativeAdapterWriteData(dataWidth), depth))
  clocks(writes, forward = true)
  writes.io.enqueue <> io.sourceWriteData
  io.destinationWriteData <> writes.io.dequeue

  private val reads = Module(new AsyncQueue(new NativeAdapterReadData(dataWidth), depth))
  clocks(reads, forward = false)
  reads.io.enqueue <> io.destinationReadData
  io.sourceReadData <> reads.io.dequeue
}

class DmaControlConfig(addressWidth: Int) extends Bundle {
  val enable = Bool()
  val base = UInt(addressWidth.W)
  val length = UInt(addressWidth.W)
  val loop = Bool()
  val clear = Bool()
}

class DmaControlStatus(addressWidth: Int) extends Bundle {
  val done = Bool()
  val busy = Bool()
  val offset = UInt(addressWidth.W)
}

/**
  * Coherent DMA control/status clock-domain crossing. Configuration updates
  * and status observations cross as atomic snapshots rather than separately
  * synchronized fields. `destinationClear` is reconstructed as a one-cycle
  * pulse when a configuration snapshot is consumed.
  */
class DmaControlCdc(addressWidth: Int, depth: Int = 4) extends RawModule {
  require(addressWidth >= 1)
  require(depth >= 4 && (depth & (depth - 1)) == 0)

  val io = IO(new Bundle {
    val sourceClock = Input(Clock())
    val sourceReset = Input(AsyncReset())
    val destinationClock = Input(Clock())
    val destinationReset = Input(AsyncReset())
    val sourceUpdate = Flipped(Decoupled(new DmaControlConfig(addressWidth)))
    val sourceStatus = Decoupled(new DmaControlStatus(addressWidth))
    val destinationEnable = Output(Bool())
    val destinationBase = Output(UInt(addressWidth.W))
    val destinationLength = Output(UInt(addressWidth.W))
    val destinationLoop = Output(Bool())
    val destinationClear = Output(Bool())
    val destinationDone = Input(Bool())
    val destinationBusy = Input(Bool())
    val destinationOffset = Input(UInt(addressWidth.W))
  })

  private val updates = Module(new AsyncQueue(new DmaControlConfig(addressWidth), depth))
  updates.io.enqueueClock := io.sourceClock
  updates.io.enqueueReset := io.sourceReset
  updates.io.dequeueClock := io.destinationClock
  updates.io.dequeueReset := io.destinationReset
  updates.io.enqueue <> io.sourceUpdate
  updates.io.dequeue.ready := true.B

  private val enable = withClockAndReset(io.destinationClock, io.destinationReset) {
    RegInit(false.B)
  }
  private val base = withClockAndReset(io.destinationClock, io.destinationReset) {
    RegInit(0.U(addressWidth.W))
  }
  private val length = withClockAndReset(io.destinationClock, io.destinationReset) {
    RegInit(0.U(addressWidth.W))
  }
  private val loop = withClockAndReset(io.destinationClock, io.destinationReset) {
    RegInit(false.B)
  }
  private val clear = withClockAndReset(io.destinationClock, io.destinationReset) {
    RegInit(false.B)
  }
  withClockAndReset(io.destinationClock, io.destinationReset) {
    clear := false.B
    when(updates.io.dequeue.fire) {
      enable := updates.io.dequeue.bits.enable
      base := updates.io.dequeue.bits.base
      length := updates.io.dequeue.bits.length
      loop := updates.io.dequeue.bits.loop
      clear := updates.io.dequeue.bits.clear
    }
  }
  io.destinationEnable := enable
  io.destinationBase := base
  io.destinationLength := length
  io.destinationLoop := loop
  io.destinationClear := clear

  private val statuses = Module(new AsyncQueue(new DmaControlStatus(addressWidth), depth))
  statuses.io.enqueueClock := io.destinationClock
  statuses.io.enqueueReset := io.destinationReset
  statuses.io.dequeueClock := io.sourceClock
  statuses.io.dequeueReset := io.sourceReset
  io.sourceStatus <> statuses.io.dequeue

  private val currentStatus = Wire(new DmaControlStatus(addressWidth))
  currentStatus.done := io.destinationDone
  currentStatus.busy := io.destinationBusy
  currentStatus.offset := io.destinationOffset
  private val lastStatus = withClockAndReset(io.destinationClock, io.destinationReset) {
    RegInit(0.U.asTypeOf(new DmaControlStatus(addressWidth)))
  }
  private val pendingStatus = withClockAndReset(io.destinationClock, io.destinationReset) {
    Reg(new DmaControlStatus(addressWidth))
  }
  private val pendingValid = withClockAndReset(io.destinationClock, io.destinationReset) {
    RegInit(false.B)
  }
  statuses.io.enqueue.valid := pendingValid
  statuses.io.enqueue.bits := pendingStatus
  withClockAndReset(io.destinationClock, io.destinationReset) {
    when(!pendingValid && currentStatus.asUInt =/= lastStatus.asUInt) {
      pendingStatus := currentStatus
      pendingValid := true.B
    }
    when(statuses.io.enqueue.fire) {
      lastStatus := pendingStatus
      pendingValid := false.B
    }
  }
}
