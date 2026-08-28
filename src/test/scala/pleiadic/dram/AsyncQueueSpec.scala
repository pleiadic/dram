package pleiadic.dram

import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class AsyncQueueHarness extends Module {
  val io = IO(new Bundle {
    val enqueue = Flipped(chisel3.util.Decoupled(UInt(16.W)))
    val dequeue = chisel3.util.Decoupled(UInt(16.W))
  })
  private val enqueueClockLevel = RegInit(false.B)
  enqueueClockLevel := !enqueueClockLevel
  private val dequeueClockLevel = RegInit(false.B)
  private val dequeueDivider = RegInit(0.U(2.W))
  when(dequeueDivider === 1.U) {
    dequeueDivider := 0.U
    dequeueClockLevel := !dequeueClockLevel
  }.otherwise { dequeueDivider := dequeueDivider + 1.U }

  private val queue = Module(new AsyncQueue(UInt(16.W), 4))
  queue.io.enqueueClock := enqueueClockLevel.asClock
  queue.io.enqueueReset := reset.asAsyncReset
  queue.io.dequeueClock := dequeueClockLevel.asClock
  queue.io.dequeueReset := reset.asAsyncReset
  queue.io.enqueue <> io.enqueue
  io.dequeue <> queue.io.dequeue
}

class CdcPrimitiveHarness extends Module {
  val io = IO(new Bundle {
    val asyncValue = Input(UInt(8.W))
    val synchronizedValue = Output(UInt(8.W))
    val sourcePulse = Input(Bool())
    val destinationPulse = Output(Bool())
  })
  private val level = Module(new CdcSynchronizer(width = 8, stages = 3))
  level.io.clock := clock
  level.io.reset := reset.asAsyncReset
  level.io.asyncInput := io.asyncValue
  io.synchronizedValue := level.io.syncOutput

  private val pulse = Module(new CdcPulseSynchronizer(stages = 2))
  pulse.io.sourceClock := clock
  pulse.io.sourceReset := reset.asAsyncReset
  pulse.io.destinationClock := clock
  pulse.io.destinationReset := reset.asAsyncReset
  pulse.io.sourcePulse := io.sourcePulse
  io.destinationPulse := pulse.io.destinationPulse
}

class NativePortCdcHarness extends Module {
  val io = IO(new Bundle {
    val sourceCommand = Flipped(chisel3.util.Decoupled(new NativeAdapterCommand(12)))
    val destinationCommand = chisel3.util.Decoupled(new NativeAdapterCommand(12))
    val sourceWriteData = Flipped(chisel3.util.Decoupled(new NativeAdapterWriteData(32)))
    val destinationWriteData = chisel3.util.Decoupled(new NativeAdapterWriteData(32))
    val destinationReadData = Flipped(chisel3.util.Decoupled(new NativeAdapterReadData(32)))
    val sourceReadData = chisel3.util.Decoupled(new NativeAdapterReadData(32))
  })
  private val cdc = Module(new NativePortCdc(addressWidth = 12,
    dataWidth = 32, depth = 4))
  cdc.io.sourceClock := clock
  cdc.io.sourceReset := reset.asAsyncReset
  cdc.io.destinationClock := clock
  cdc.io.destinationReset := reset.asAsyncReset
  cdc.io.sourceCommand <> io.sourceCommand
  io.destinationCommand <> cdc.io.destinationCommand
  cdc.io.sourceWriteData <> io.sourceWriteData
  io.destinationWriteData <> cdc.io.destinationWriteData
  cdc.io.destinationReadData <> io.destinationReadData
  io.sourceReadData <> cdc.io.sourceReadData
}

class AsyncQueueSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val verilator = Seq(VerilatorBackendAnnotation,
    // chiseltest 6's JNA harness uses Verilator's pre-5.050 WData alias.
    VerilatorCFlags(Seq("-DWData=IData")))

  behavior of "CDC primitives"

  it should "synchronize levels and transfer a spaced pulse" in {
    test(new CdcPrimitiveHarness).withAnnotations(verilator) { dut =>
      dut.io.asyncValue.poke(0.U)
      dut.io.sourcePulse.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      dut.io.asyncValue.poke("ha5".U)
      dut.clock.step(2)
      dut.io.synchronizedValue.expect(0.U)
      dut.clock.step()
      dut.io.synchronizedValue.expect("ha5".U)

      dut.io.sourcePulse.poke(true.B)
      dut.clock.step()
      dut.io.sourcePulse.poke(false.B)
      dut.io.destinationPulse.expect(false.B)
      dut.clock.step()
      dut.io.destinationPulse.expect(false.B)
      dut.clock.step()
      dut.io.destinationPulse.expect(true.B)
      dut.clock.step()
      dut.io.destinationPulse.expect(false.B)
    }
  }

  it should "cross all three Native-port streams in their proper directions" in {
    test(new NativePortCdcHarness).withAnnotations(verilator) { dut =>
      dut.io.sourceCommand.valid.poke(false.B)
      dut.io.sourceCommand.bits.write.poke(false.B)
      dut.io.sourceCommand.bits.address.poke(0.U)
      dut.io.sourceWriteData.valid.poke(false.B)
      dut.io.sourceWriteData.bits.data.poke(0.U)
      dut.io.sourceWriteData.bits.byteEnable.poke(0.U)
      dut.io.destinationCommand.ready.poke(false.B)
      dut.io.destinationWriteData.ready.poke(false.B)
      dut.io.destinationReadData.valid.poke(false.B)
      dut.io.destinationReadData.bits.data.poke(0.U)
      dut.io.sourceReadData.ready.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      dut.io.sourceCommand.valid.poke(true.B)
      dut.io.sourceCommand.bits.write.poke(true.B)
      dut.io.sourceCommand.bits.address.poke("h5a3".U)
      dut.io.sourceWriteData.valid.poke(true.B)
      dut.io.sourceWriteData.bits.data.poke("h89abcdef".U)
      dut.io.sourceWriteData.bits.byteEnable.poke("hb".U)
      dut.io.destinationReadData.valid.poke(true.B)
      dut.io.destinationReadData.bits.data.poke("h13579bdf".U)
      dut.clock.step()
      dut.io.sourceCommand.valid.poke(false.B)
      dut.io.sourceWriteData.valid.poke(false.B)
      dut.io.destinationReadData.valid.poke(false.B)

      while (!dut.io.destinationCommand.valid.peek().litToBoolean) dut.clock.step()
      dut.io.destinationCommand.bits.write.expect(true.B)
      dut.io.destinationCommand.bits.address.expect("h5a3".U)
      dut.io.destinationCommand.ready.poke(true.B)
      dut.clock.step()
      dut.io.destinationCommand.ready.poke(false.B)

      while (!dut.io.destinationWriteData.valid.peek().litToBoolean) dut.clock.step()
      dut.io.destinationWriteData.bits.data.expect("h89abcdef".U)
      dut.io.destinationWriteData.bits.byteEnable.expect("hb".U)
      dut.io.destinationWriteData.ready.poke(true.B)
      dut.clock.step()

      while (!dut.io.sourceReadData.valid.peek().litToBoolean) dut.clock.step()
      dut.io.sourceReadData.bits.data.expect("h13579bdf".U)
      dut.io.sourceReadData.ready.poke(true.B)
      dut.clock.step()
    }
  }

  behavior of "AsyncQueue"

  it should "preserve order, full and empty across different generated clocks" in {
    test(new AsyncQueueHarness).withAnnotations(verilator) { dut =>
      dut.reset.poke(true.B)
      dut.io.enqueue.valid.poke(false.B)
      dut.io.enqueue.bits.poke(0.U)
      dut.io.dequeue.ready.poke(false.B)
      dut.clock.step(4)
      dut.reset.poke(false.B)

      for (batch <- (1 to 24).grouped(4)) {
        for (value <- batch) {
          dut.io.enqueue.valid.poke(true.B)
          dut.io.enqueue.bits.poke(value.U)
          while (!dut.io.enqueue.ready.peek().litToBoolean) dut.clock.step()
          dut.clock.step(2)
        }
        dut.io.enqueue.valid.poke(false.B)
        dut.io.enqueue.ready.expect(false.B)

        dut.io.dequeue.ready.poke(true.B)
        for (expected <- batch) {
          while (!dut.io.dequeue.valid.peek().litToBoolean) dut.clock.step()
          dut.io.dequeue.bits.expect(expected.U)
          do { dut.clock.step() } while (dut.io.dequeue.valid.peek().litToBoolean &&
            dut.io.dequeue.bits.peek().litValue == expected)
        }
        dut.io.dequeue.valid.expect(false.B)
        dut.io.dequeue.ready.poke(false.B)
        while (!dut.io.enqueue.ready.peek().litToBoolean) dut.clock.step()
      }
    }
  }
}
