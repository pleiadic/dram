package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

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

  private val readGraySync1 = withClockAndReset(io.enqueueClock, io.enqueueReset) {
    RegNext(readGray, 0.U)
  }
  private val readGraySync2 = withClockAndReset(io.enqueueClock, io.enqueueReset) {
    RegNext(readGraySync1, 0.U)
  }
  private val writeGraySync1 = withClockAndReset(io.dequeueClock, io.dequeueReset) {
    RegNext(writeGray, 0.U)
  }
  private val writeGraySync2 = withClockAndReset(io.dequeueClock, io.dequeueReset) {
    RegNext(writeGraySync1, 0.U)
  }

  private val writeIncrement = io.enqueue.valid && !full
  private val writeBinaryNext = writeBinary + writeIncrement
  private val writeGrayNext = (writeBinaryNext >> 1) ^ writeBinaryNext
  private val invertedReadGray = Cat(~readGraySync2(pointerBits - 1, pointerBits - 2),
    readGraySync2(pointerBits - 3, 0))
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
  private val emptyNext = readGrayNext === writeGraySync2

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

  dontTouch(readGraySync1)
  dontTouch(readGraySync2)
  dontTouch(writeGraySync1)
  dontTouch(writeGraySync2)
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
