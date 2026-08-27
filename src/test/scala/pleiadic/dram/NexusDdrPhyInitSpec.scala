package pleiadic.dram

import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags, VerilatorFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class NexusDdrPhyInitHarness extends Module {
  val io = IO(new Bundle {
    val dllReset = Input(Bool())
    val pause = Output(Bool())
    val loadN = Output(Bool())
    val move = Output(Bool())
    val delayCode = Output(UInt(9.W))
    val busy = Output(Bool())
  })
  private val init = Module(new NexusDdrPhyInit)
  init.io.sys2xClock := clock
  init.io.dllReset := io.dllReset
  io.pause := init.io.pause
  io.loadN := init.io.loadN
  io.move := init.io.move
  io.delayCode := init.io.delayCode
  io.busy := init.io.busy
}

class NexusWriteBitSlipHarness extends Module {
  val io = IO(new Bundle {
    val data4 = Input(UInt(4.W))
    val data2 = Input(UInt(2.W))
    val slipped4 = Output(UInt(4.W))
    val slipped2 = Output(UInt(2.W))
  })
  private val slip4 = Module(new NexusWriteBitSlip(4))
  private val slip2 = Module(new NexusWriteBitSlip(2))
  for ((slip, data) <- Seq((slip4, io.data4), (slip2, io.data2))) {
    slip.io.clock := clock
    slip.io.reset := reset.asBool
    slip.io.input := data
  }
  io.slipped4 := slip4.io.output
  io.slipped2 := slip2.io.output
}

class NexusDdrPhyInitSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Lattice Nexus DDR PHY initialization"

  it should "perform the additional DQSBUF load and move events" in {
    test(new NexusDdrPhyInitSequencer) { dut =>
      dut.io.dllLock.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      dut.io.dllLock.poke(true.B)
      dut.clock.step(3)
      dut.clock.step(80)
      dut.io.loadN.expect(false.B)
      dut.io.pause.expect(true.B)
      dut.clock.step(8)
      dut.io.move.expect(true.B)
      dut.clock.step(8)
      dut.io.move.expect(false.B)
      dut.clock.step(8)
      dut.io.loadN.expect(true.B)
      dut.clock.step(8)
      dut.io.pause.expect(false.B)
      dut.io.busy.expect(false.B)
    }
  }

  it should "shift write and tristate streams by half a word" in {
    test(new NexusWriteBitSlipHarness) { dut =>
      dut.io.data4.poke("hc".U)
      dut.io.data2.poke(2.U)
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      dut.clock.step()
      dut.io.data4.poke(3.U)
      dut.io.data2.poke(1.U)
      dut.io.slipped4.expect(15.U)
      dut.io.slipped2.expect(3.U)
    }
  }

  it should "model and parse the Nexus DDRDLL primitive" in {
    test(new NexusDdrPhyInitHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.dllReset.poke(false.B)
      dut.clock.step(4)
      dut.io.busy.expect(true.B)
      dut.io.delayCode.expect(0.U)
    }
    test(new NexusDdrPhyInitHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq("-DSYNTHESIS", "-Wno-MODMISSING")),
      VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.dllReset.poke(false.B)
      dut.clock.step()
    }
  }
}
