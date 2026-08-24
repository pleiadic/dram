package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, attach}
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags, VerilatorFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class S7PrimitiveSimulationHarness extends Module {
  val io = IO(new Bundle {
    val data = Input(UInt(8.W))
    val outputEnable = Input(Bool())
    val serialInput = Input(Bool())
    val delayReset = Input(Bool())
    val delayIncrement = Input(Bool())
    val ddrSerial = Output(Bool())
    val ddrTristate = Output(Bool())
    val sdrSerial = Output(Bool())
    val ddrParallel = Output(UInt(8.W))
    val sdrParallel = Output(UInt(8.W))
    val inputDelayed = Output(Bool())
    val outputDelayed = Output(Bool())
    val inputDelayValue = Output(UInt(5.W))
    val outputDelayValue = Output(UInt(5.W))
  })

  private val outputDdr = Module(new S7OutputSerdes(8, "DDR"))
  private val outputSdr = Module(new S7OutputSerdes(4, "SDR"))
  private val inputDdr = Module(new S7InputSerdes(8, "DDR"))
  private val inputSdr = Module(new S7InputSerdes(4, "SDR"))
  private val inputDelay = Module(new S7InputDelay(300, initialValue = 7))
  private val outputDelay = Module(new S7OutputDelay(400, initialValue = 11))

  for (serializer <- Seq(outputDdr, outputSdr)) {
    serializer.io.reset := reset.asBool
    serializer.io.serialClock := clock
    serializer.io.dividedClock := clock
    serializer.io.data := io.data
    serializer.io.outputEnable := io.outputEnable
  }
  for (deserializer <- Seq(inputDdr, inputSdr)) {
    deserializer.io.reset := reset.asBool
    deserializer.io.serialClock := clock
    deserializer.io.invertedSerialClock := (!clock.asBool).asClock
    deserializer.io.dividedClock := clock
    deserializer.io.serial := io.serialInput
    deserializer.io.bitslip := false.B
  }
  inputDelay.io.clock := clock
  inputDelay.io.reset := io.delayReset
  inputDelay.io.increment := io.delayIncrement
  inputDelay.io.dataIn := io.serialInput
  outputDelay.io.clock := clock
  outputDelay.io.reset := io.delayReset
  outputDelay.io.increment := io.delayIncrement
  outputDelay.io.dataIn := io.serialInput

  io.ddrSerial := outputDdr.io.serial
  io.ddrTristate := outputDdr.io.tristate
  io.sdrSerial := outputSdr.io.serial
  io.ddrParallel := inputDdr.io.data
  io.sdrParallel := inputSdr.io.data
  io.inputDelayed := inputDelay.io.dataOut
  io.outputDelayed := outputDelay.io.dataOut
  io.inputDelayValue := inputDelay.io.value
  io.outputDelayValue := outputDelay.io.value
}

class S7BidirectionalLaneHarness extends Module {
  val io = IO(new Bundle {
    val parallelOut = Input(UInt(8.W))
    val outputEnable = Input(Bool())
    val delayReset = Input(Bool())
    val delayIncrement = Input(Bool())
    val bitslip = Input(Bool())
    val parallelIn = Output(UInt(8.W))
    val delayValue = Output(UInt(5.W))
    val pad = Analog(1.W)
    val differentialPadPositive = Analog(1.W)
    val differentialPadNegative = Analog(1.W)
    val outputPadPositive = Analog(1.W)
    val outputPadNegative = Analog(1.W)
  })

  private val lane = Module(new S7BidirectionalSerdesLane(8, "DDR", 200))
  lane.io.reset := reset.asBool
  lane.io.serialClock := clock
  lane.io.invertedSerialClock := (!clock.asBool).asClock
  lane.io.dividedClock := clock
  lane.io.delayClock := clock
  lane.io.delayReset := io.delayReset
  lane.io.delayIncrement := io.delayIncrement
  lane.io.bitslip := io.bitslip
  lane.io.parallelOut := io.parallelOut
  lane.io.outputEnable := io.outputEnable
  io.parallelIn := lane.io.parallelIn
  io.delayValue := lane.io.delayValue
  attach(lane.io.pad, io.pad)

  private val outputDelay = Module(new S7OutputDelay(200, initialValue = 3))
  outputDelay.io.clock := clock
  outputDelay.io.reset := io.delayReset
  outputDelay.io.increment := io.delayIncrement
  outputDelay.io.dataIn := io.parallelOut(0)
  private val differentialIo = Module(new S7DifferentialIoBuffer)
  differentialIo.io.outputData := outputDelay.io.dataOut
  differentialIo.io.tristate := !io.outputEnable
  attach(differentialIo.io.padPositive, io.differentialPadPositive)
  attach(differentialIo.io.padNegative, io.differentialPadNegative)
  private val differentialOutput = Module(new S7DifferentialOutputBuffer)
  differentialOutput.io.dataIn := outputDelay.io.dataOut
  attach(differentialOutput.io.padPositive, io.outputPadPositive)
  attach(differentialOutput.io.padNegative, io.outputPadNegative)
}

class S7PhyPrimitivesSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Xilinx 7-series primitive wrappers"

  it should "provide Verilator models for DDR/SDR SerDes and delay primitives" in {
    test(new S7PrimitiveSimulationHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.data.poke("ha5".U)
      dut.io.outputEnable.poke(true.B)
      dut.io.serialInput.poke(true.B)
      dut.io.delayReset.poke(false.B)
      dut.io.delayIncrement.poke(false.B)
      dut.io.ddrSerial.expect(true.B)
      dut.io.sdrSerial.expect(true.B)
      dut.io.ddrTristate.expect(false.B)
      dut.io.ddrParallel.expect("hff".U)
      dut.io.sdrParallel.expect("hff".U)
      dut.io.inputDelayed.expect(true.B)
      dut.io.outputDelayed.expect(true.B)
      dut.io.inputDelayValue.expect(7.U)
      dut.io.outputDelayValue.expect(11.U)

      dut.io.outputEnable.poke(false.B)
      dut.io.serialInput.poke(false.B)
      dut.io.ddrTristate.expect(true.B)
      dut.io.ddrParallel.expect(0.U)
      dut.io.inputDelayed.expect(false.B)
    }
  }

  it should "elaborate the complete bidirectional lane with an Analog pad" in {
    test(new S7BidirectionalLaneHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.parallelOut.poke("h96".U)
      dut.io.outputEnable.poke(true.B)
      dut.io.delayReset.poke(false.B)
      dut.io.delayIncrement.poke(false.B)
      dut.io.bitslip.poke(false.B)
      dut.io.delayValue.expect(0.U)
      dut.clock.step(2)
    }
  }

  it should "parse the real S7 primitive synthesis branches" in {
    test(new S7BidirectionalLaneHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq("-DSYNTHESIS", "-Wno-MODMISSING")),
      VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.parallelOut.poke("h69".U)
      dut.io.outputEnable.poke(false.B)
      dut.io.delayReset.poke(false.B)
      dut.io.delayIncrement.poke(false.B)
      dut.io.bitslip.poke(false.B)
      dut.clock.step()
    }
  }
}
