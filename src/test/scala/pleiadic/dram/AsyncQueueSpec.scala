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

class DmaControlCdcHarness extends Module {
  val io = IO(new Bundle {
    val update = Flipped(chisel3.util.Decoupled(new DmaControlConfig(12)))
    val status = chisel3.util.Decoupled(new DmaControlStatus(12))
    val destinationEnable = Output(Bool())
    val destinationBase = Output(UInt(12.W))
    val destinationLength = Output(UInt(12.W))
    val destinationLoop = Output(Bool())
    val destinationClear = Output(Bool())
    val destinationDone = Input(Bool())
    val destinationBusy = Input(Bool())
    val destinationOffset = Input(UInt(12.W))
  })

  private val destinationClockLevel = RegInit(false.B)
  private val divider = RegInit(0.U(2.W))
  when(divider === 2.U) {
    divider := 0.U
    destinationClockLevel := !destinationClockLevel
  }.otherwise {
    divider := divider + 1.U
  }

  private val cdc = Module(new DmaControlCdc(addressWidth = 12, depth = 4))
  cdc.io.sourceClock := clock
  cdc.io.sourceReset := reset.asAsyncReset
  cdc.io.destinationClock := destinationClockLevel.asClock
  cdc.io.destinationReset := reset.asAsyncReset
  cdc.io.sourceUpdate <> io.update
  io.status <> cdc.io.sourceStatus
  io.destinationEnable := cdc.io.destinationEnable
  io.destinationBase := cdc.io.destinationBase
  io.destinationLength := cdc.io.destinationLength
  io.destinationLoop := cdc.io.destinationLoop
  io.destinationClear := cdc.io.destinationClear
  cdc.io.destinationDone := io.destinationDone
  cdc.io.destinationBusy := io.destinationBusy
  cdc.io.destinationOffset := io.destinationOffset
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

  it should "cross coherent DMA configuration and backpressured status snapshots" in {
    test(new DmaControlCdcHarness).withAnnotations(verilator) { dut =>
      dut.io.update.valid.poke(false.B)
      dut.io.update.bits.enable.poke(false.B)
      dut.io.update.bits.base.poke(0.U)
      dut.io.update.bits.length.poke(0.U)
      dut.io.update.bits.loop.poke(false.B)
      dut.io.update.bits.clear.poke(false.B)
      dut.io.status.ready.poke(false.B)
      dut.io.destinationDone.poke(false.B)
      dut.io.destinationBusy.poke(false.B)
      dut.io.destinationOffset.poke(0.U)
      dut.reset.poke(true.B)
      dut.clock.step(4)
      dut.reset.poke(false.B)

      def send(enable: Boolean, base: Int, length: Int,
          loop: Boolean, clear: Boolean): Unit = {
        dut.io.update.bits.enable.poke(enable.B)
        dut.io.update.bits.base.poke(base.U)
        dut.io.update.bits.length.poke(length.U)
        dut.io.update.bits.loop.poke(loop.B)
        dut.io.update.bits.clear.poke(clear.B)
        dut.io.update.valid.poke(true.B)
        while (!dut.io.update.ready.peek().litToBoolean) dut.clock.step()
        dut.clock.step()
        dut.io.update.valid.poke(false.B)
      }

      send(enable = true, base = 0x123, length = 0x45,
        loop = true, clear = true)
      var sawClear = false
      var wait = 0
      while ((dut.io.destinationBase.peek().litValue != 0x123 || !sawClear) && wait < 80) {
        sawClear ||= dut.io.destinationClear.peek().litToBoolean
        dut.clock.step()
        wait += 1
      }
      assert(wait < 80 && sawClear)
      dut.io.destinationEnable.expect(true.B)
      dut.io.destinationBase.expect("h123".U)
      dut.io.destinationLength.expect("h045".U)
      dut.io.destinationLoop.expect(true.B)

      dut.io.destinationBusy.poke(true.B)
      dut.io.destinationOffset.poke("h012".U)
      wait = 0
      while (!dut.io.status.valid.peek().litToBoolean && wait < 100) {
        dut.clock.step()
        wait += 1
      }
      assert(wait < 100)
      dut.io.status.bits.done.expect(false.B)
      dut.io.status.bits.busy.expect(true.B)
      dut.io.status.bits.offset.expect("h012".U)

      // A queued status snapshot must remain stable while the source stalls,
      // even as all destination fields change together.
      dut.io.destinationDone.poke(true.B)
      dut.io.destinationBusy.poke(false.B)
      dut.io.destinationOffset.poke("h234".U)
      dut.clock.step(12)
      dut.io.status.bits.done.expect(false.B)
      dut.io.status.bits.busy.expect(true.B)
      dut.io.status.bits.offset.expect("h012".U)
      dut.io.status.ready.poke(true.B)
      dut.clock.step()

      wait = 0
      var sawFinal = false
      while (!sawFinal && wait < 120) {
        if (dut.io.status.valid.peek().litToBoolean) {
          sawFinal = dut.io.status.bits.done.peek().litToBoolean &&
            !dut.io.status.bits.busy.peek().litToBoolean &&
            dut.io.status.bits.offset.peek().litValue == 0x234
        }
        dut.clock.step()
        wait += 1
      }
      assert(sawFinal)

      send(enable = false, base = 0x321, length = 7,
        loop = false, clear = false)
      wait = 0
      while (dut.io.destinationBase.peek().litValue != 0x321 && wait < 80) {
        dut.clock.step()
        wait += 1
      }
      assert(wait < 80)
      dut.io.destinationEnable.expect(false.B)
      dut.io.destinationBase.expect("h321".U)
      dut.io.destinationLength.expect(7.U)
      dut.io.destinationLoop.expect(false.B)
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
