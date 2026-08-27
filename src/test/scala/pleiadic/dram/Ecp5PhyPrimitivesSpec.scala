package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags, VerilatorFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class Ecp5PhyPrimitivesHarness extends Module {
  val io = IO(new Bundle {
    val data = Input(UInt(4.W))
    val serialInput = Input(Bool())
    val outputEnable = Input(Bool())
    val commandOutput = Output(Bool())
    val dqsOutput = Output(Bool())
    val dqsTristate = Output(Bool())
    val laneInput = Output(UInt(4.W))
    val dataValid = Output(Bool())
    val pad = Analog(1.W)
  })

  private val command = Module(new Ecp5OutputDdrX2)
  private val commandDelay = Module(new Ecp5CommandDelay(7))
  command.io.reset := reset.asBool
  command.io.systemClock := clock
  command.io.edgeClock := clock
  command.io.data := io.data
  commandDelay.io.dataIn := command.io.serial
  io.commandOutput := commandDelay.io.dataOut

  private val dqsBuffer = Module(new Ecp5DqsBuffer)
  dqsBuffer.io.reset := reset.asBool
  dqsBuffer.io.systemClock := clock
  dqsBuffer.io.edgeClock := clock
  dqsBuffer.io.dllDelay := false.B
  dqsBuffer.io.pause := false.B
  dqsBuffer.io.readEnable := true.B
  dqsBuffer.io.readDelay := 0.U
  dqsBuffer.io.dqsInput := io.serialInput
  io.dataValid := dqsBuffer.io.dataValid

  private val dqs = Module(new Ecp5DqsOutputSerdes)
  dqs.io.reset := reset.asBool
  dqs.io.systemClock := clock
  dqs.io.edgeClock := clock
  dqs.io.writeClock := dqsBuffer.io.writeClock
  dqs.io.data := io.data
  dqs.io.outputEnable := io.outputEnable
  dqs.io.preamble := false.B
  dqs.io.postamble := false.B
  io.dqsOutput := dqs.io.serial
  io.dqsTristate := dqs.io.tristate

  private val lane = Module(new Ecp5DataSerdesLane)
  lane.io.reset := reset.asBool
  lane.io.systemClock := clock
  lane.io.edgeClock := clock
  lane.io.writeClock270 := dqsBuffer.io.writeClock270
  lane.io.readClock90 := dqsBuffer.io.readClock90
  lane.io.readPointer := dqsBuffer.io.readPointer
  lane.io.writePointer := dqsBuffer.io.writePointer
  lane.io.parallelOut := io.data
  lane.io.outputEnable := io.outputEnable
  io.laneInput := lane.io.parallelIn
  attach(lane.io.pad, io.pad)
}

class Ecp5PhyPrimitivesSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val verilator = Seq(VerilatorBackendAnnotation,
    VerilatorCFlags(Seq("-DWData=IData")))

  behavior of "Lattice ECP5 DDR PHY primitives"

  it should "model x2 command DQS and bidirectional data lanes" in {
    test(new Ecp5PhyPrimitivesHarness).withAnnotations(verilator) { dut =>
      dut.io.data.poke("ha".U)
      dut.io.serialInput.poke(true.B)
      dut.io.outputEnable.poke(true.B)
      dut.io.commandOutput.expect(false.B)
      dut.io.dqsOutput.expect(false.B)
      dut.io.dqsTristate.expect(false.B)
      dut.io.dataValid.expect(true.B)
      dut.clock.step()
      dut.io.laneInput.expect(0.U)
    }
  }

  it should "parse all ECP5 memory I/O primitive branches" in {
    test(new Ecp5PhyPrimitivesHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq("-DSYNTHESIS", "-Wno-MODMISSING")),
      VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.data.poke(5.U)
      dut.io.serialInput.poke(false.B)
      dut.io.outputEnable.poke(false.B)
      dut.clock.step()
    }
  }
}
