package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags, VerilatorFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class UltraScalePrimitiveHarness(device: String, refClockFrequencyMHz: Int)
    extends Module {
  val io = IO(new Bundle {
    val data = Input(UInt(8.W))
    val outputEnable = Input(Bool())
    val serialInput = Input(Bool())
    val serialOutput = Output(Bool())
    val tristate = Output(Bool())
    val parallelInput = Output(UInt(8.W))
    val inputDelayed = Output(Bool())
    val outputDelayed = Output(Bool())
    val inputDelayValue = Output(UInt(9.W))
    val outputDelayValue = Output(UInt(9.W))
    val laneInputDelayValue = Output(UInt(9.W))
    val laneOutputDelayValue = Output(UInt(9.W))
    val dqsOutputDelayValue = Output(UInt(9.W))
    val pad = Analog(1.W)
    val dqsPositive = Analog(1.W)
    val dqsNegative = Analog(1.W)
  })

  private val output = Module(new UltraScaleOutputSerdes(device))
  output.io.reset := reset.asBool
  output.io.serialClock := clock
  output.io.dividedClock := clock
  output.io.data := io.data
  output.io.outputEnable := io.outputEnable
  io.serialOutput := output.io.serial
  io.tristate := output.io.tristate

  private val input = Module(new UltraScaleInputSerdes(device))
  input.io.reset := reset.asBool
  input.io.serialClock := clock
  input.io.dividedClock := clock
  input.io.serial := io.serialInput
  io.parallelInput := input.io.data

  private val inputDelay = Module(new UltraScaleInputDelay(device,
    refClockFrequencyMHz, initialValuePs = 123))
  inputDelay.io.clock := clock
  inputDelay.io.reset := false.B
  inputDelay.io.enableVtc := true.B
  inputDelay.io.increment := false.B
  inputDelay.io.dataIn := io.serialInput
  io.inputDelayed := inputDelay.io.dataOut
  io.inputDelayValue := inputDelay.io.value

  private val outputDelay = Module(new UltraScaleOutputDelay(device,
    refClockFrequencyMHz, initialValuePs = 333))
  outputDelay.io.clock := clock
  outputDelay.io.reset := false.B
  outputDelay.io.enableVtc := true.B
  outputDelay.io.increment := false.B
  outputDelay.io.dataIn := io.serialInput
  io.outputDelayed := outputDelay.io.dataOut
  io.outputDelayValue := outputDelay.io.value

  private val lane = Module(new UltraScaleBidirectionalSerdesLane(device,
    refClockFrequencyMHz))
  lane.io.reset := reset.asBool
  lane.io.serialClock := clock
  lane.io.dividedClock := clock
  lane.io.delayClock := clock
  lane.io.enableVtc := true.B
  lane.io.inputDelayReset := false.B
  lane.io.inputDelayIncrement := false.B
  lane.io.outputDelayReset := false.B
  lane.io.outputDelayIncrement := false.B
  lane.io.parallelOut := io.data
  lane.io.outputEnable := io.outputEnable
  io.laneInputDelayValue := lane.io.inputDelayValue
  io.laneOutputDelayValue := lane.io.outputDelayValue
  attach(lane.io.pad, io.pad)

  private val dqs = Module(new UltraScaleDifferentialOutputSerdesIoLane(device,
    refClockFrequencyMHz, outputInitialValuePs = 250))
  dqs.io.reset := reset.asBool
  dqs.io.serialClock := clock
  dqs.io.dividedClock := clock
  dqs.io.delayClock := clock
  dqs.io.enableVtc := true.B
  dqs.io.delayReset := false.B
  dqs.io.delayIncrement := false.B
  dqs.io.parallelOut := io.data
  dqs.io.outputEnable := io.outputEnable
  io.dqsOutputDelayValue := dqs.io.delayValue
  attach(dqs.io.padPositive, io.dqsPositive)
  attach(dqs.io.padNegative, io.dqsNegative)
}

class UltraScalePhyPrimitivesSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val verilator = Seq(VerilatorBackendAnnotation,
    VerilatorCFlags(Seq("-DWData=IData")))

  behavior of "Xilinx UltraScale PHY primitives"

  it should "model UltraScale E3 SerDes and nine-bit TIME delays" in {
    test(new UltraScalePrimitiveHarness(UltraScaleDevice.UltraScale, 200))
      .withAnnotations(verilator) { dut =>
        dut.io.data.poke("ha5".U)
        dut.io.outputEnable.poke(true.B)
        dut.io.serialInput.poke(true.B)
        dut.io.serialOutput.expect(true.B)
        dut.io.tristate.expect(false.B)
        dut.io.parallelInput.expect("hff".U)
        dut.io.inputDelayed.expect(true.B)
        dut.io.outputDelayed.expect(true.B)
        dut.io.inputDelayValue.expect(123.U)
        dut.io.outputDelayValue.expect(333.U)
        dut.io.laneInputDelayValue.expect(0.U)
        dut.io.laneOutputDelayValue.expect(0.U)
        dut.io.dqsOutputDelayValue.expect(250.U)
        dut.clock.step()
      }
  }

  it should "parse the UltraScale Plus primitive branches" in {
    test(new UltraScalePrimitiveHarness(UltraScaleDevice.UltraScalePlus, 300))
      .withAnnotations(Seq(
        VerilatorBackendAnnotation,
        VerilatorFlags(Seq("-DSYNTHESIS", "-Wno-MODMISSING")),
        VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
        dut.io.data.poke("h69".U)
        dut.io.outputEnable.poke(false.B)
        dut.io.serialInput.poke(false.B)
        dut.clock.step()
      }
  }
}
