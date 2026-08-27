package pleiadic.dram

import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags, VerilatorFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class GowinDdrPhyInitHarness(family: GowinFamily) extends Module {
  val io = IO(new Bundle {
    val dllReset = Input(Bool())
    val pause = Output(Bool())
    val stop = Output(Bool())
    val resetDomain = Output(Bool())
    val delayCode = Output(UInt(8.W))
    val busy = Output(Bool())
  })
  private val init = Module(new GowinDdrPhyInit(family))
  init.io.sys2xClock := clock
  init.io.dllReset := io.dllReset
  io.pause := init.io.pause
  io.stop := init.io.stop
  io.resetDomain := init.io.resetDomain
  io.delayCode := init.io.delayCode
  io.busy := init.io.busy
}

class GowinDdrPhyInitSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val verilator = Seq(VerilatorBackendAnnotation,
    VerilatorCFlags(Seq("-DWData=IData")))
  private val synthesis = Seq(VerilatorBackendAnnotation,
    VerilatorFlags(Seq("-DSYNTHESIS", "-Wno-MODMISSING")),
    VerilatorCFlags(Seq("-DWData=IData")))

  behavior of "Gowin DDR PHY initialization"

  it should "reproduce the ten eight-cycle DLL and edge-clock events" in {
    test(new GowinDdrPhyInitSequencer) { dut =>
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

  it should "model and parse the GW2A DLL primitive" in {
    test(new GowinDdrPhyInitHarness(GowinFamily.GW2A)).withAnnotations(verilator) { dut =>
      dut.io.dllReset.poke(true.B)
      dut.clock.step(2)
      dut.io.busy.expect(false.B)
      dut.io.dllReset.poke(false.B)
      dut.clock.step(4)
      dut.io.busy.expect(true.B)
      dut.io.delayCode.expect(0.U)
    }
    test(new GowinDdrPhyInitHarness(GowinFamily.GW2A)).withAnnotations(synthesis) { dut =>
      dut.io.dllReset.poke(false.B)
      dut.clock.step()
    }
  }

  it should "model and parse the GW5A DDRDLL primitive" in {
    test(new GowinDdrPhyInitHarness(GowinFamily.GW5A)).withAnnotations(verilator) { dut =>
      dut.io.dllReset.poke(false.B)
      dut.clock.step(4)
      dut.io.delayCode.expect(0.U)
    }
    test(new GowinDdrPhyInitHarness(GowinFamily.GW5A)).withAnnotations(synthesis) { dut =>
      dut.io.dllReset.poke(false.B)
      dut.clock.step()
    }
  }
}
