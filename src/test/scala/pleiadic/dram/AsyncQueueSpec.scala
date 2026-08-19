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

class AsyncQueueSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "AsyncQueue"

  it should "preserve order, full and empty across different generated clocks" in {
    test(new AsyncQueueHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      // chiseltest 6's JNA harness uses Verilator's pre-5.050 WData alias.
      VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
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
