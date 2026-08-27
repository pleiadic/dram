package pleiadic.dram

import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags, VerilatorFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class Ecp5DdrPhyInitHarness extends Module {
  val io = IO(new Bundle {
    val dllReset = Input(Bool())
    val pause = Output(Bool())
    val stop = Output(Bool())
    val resetDomain = Output(Bool())
    val delay = Output(Bool())
    val busy = Output(Bool())
  })
  private val init = Module(new Ecp5DdrPhyInit)
  init.io.sys2xClock := clock
  init.io.dllReset := io.dllReset
  io.pause := init.io.pause
  io.stop := init.io.stop
  io.resetDomain := init.io.resetDomain
  io.delay := init.io.delay
  io.busy := init.io.busy
}

class Ecp5DdrPhyInitSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ECP5 DDR PHY initialization"

  it should "reproduce the ten eight-cycle DLL and ECLK events" in {
    test(new Ecp5DdrPhyInitSequencer) { dut =>
      dut.io.dllLock.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      dut.io.dllLock.poke(true.B)
      dut.clock.step(3)
      dut.io.busy.expect(true.B)

      dut.clock.step(7)
      dut.io.freeze.expect(false.B)
      dut.clock.step()
      dut.io.freeze.expect(true.B)
      dut.clock.step(8)
      dut.io.stop.expect(true.B)
      dut.clock.step(8)
      dut.io.resetDomain.expect(true.B)
      dut.clock.step(8)
      dut.io.resetDomain.expect(false.B)
      dut.clock.step(8)
      dut.io.stop.expect(false.B)
      dut.clock.step(8)
      dut.io.freeze.expect(false.B)
      dut.clock.step(8)
      dut.io.pause.expect(true.B)
      dut.clock.step(8)
      dut.io.update.expect(true.B)
      dut.clock.step(8)
      dut.io.update.expect(false.B)
      dut.clock.step(8)
      dut.io.pause.expect(false.B)
      dut.io.busy.expect(false.B)
    }
  }

  it should "model DDRDLLA and parse its synthesis primitive" in {
    test(new Ecp5DdrPhyInitHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.dllReset.poke(true.B)
      dut.clock.step(2)
      dut.io.busy.expect(false.B)
      dut.io.dllReset.poke(false.B)
      dut.clock.step(4)
      dut.io.busy.expect(true.B)
      dut.io.delay.expect(false.B)
    }

    test(new Ecp5DdrPhyInitHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq("-DSYNTHESIS", "-Wno-MODMISSING")),
      VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.dllReset.poke(false.B)
      dut.clock.step()
    }
  }
}
