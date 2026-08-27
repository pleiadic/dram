package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, IntParam, StringParam, attach}
import chisel3.util.HasBlackBoxInline
import scala.language.reflectiveCalls

object UltraScaleDevice {
  val UltraScale = "ULTRASCALE"
  val UltraScalePlus = "ULTRASCALE_PLUS"
  val supported = Set(UltraScale, UltraScalePlus)
}

/** Xilinx UltraScale OSERDESE3 wrapper for the PHY's fixed 8:1 boundary. */
class UltraScaleOutputSerdes(device: String) extends BlackBox(Map(
    "SIM_DEVICE" -> StringParam(device))) with HasBlackBoxInline {
  require(UltraScaleDevice.supported.contains(device))

  val io = IO(new Bundle {
    val reset = Input(Bool())
    val serialClock = Input(Clock())
    val dividedClock = Input(Clock())
    val data = Input(UInt(8.W))
    val outputEnable = Input(Bool())
    val serial = Output(Bool())
    val tristate = Output(Bool())
  })

  setInline("UltraScaleOutputSerdes.sv",
    """module UltraScaleOutputSerdes #(
      |  parameter SIM_DEVICE = "ULTRASCALE"
      |) (
      |  input wire reset, input wire serialClock, input wire dividedClock,
      |  input wire [7:0] data, input wire outputEnable,
      |  output wire serial, output wire tristate
      |);
      |`ifdef SYNTHESIS
      |  OSERDESE3 #(
      |    .SIM_DEVICE(SIM_DEVICE), .DATA_WIDTH(8), .INIT(1'b0),
      |    .IS_RST_INVERTED(1'b0), .IS_CLK_INVERTED(1'b0),
      |    .IS_CLKDIV_INVERTED(1'b0)
      |  ) u_primitive (
      |    .RST(reset), .CLK(serialClock), .CLKDIV(dividedClock),
      |    .D(data), .T(~outputEnable), .OQ(serial), .T_OUT(tristate)
      |  );
      |`else
      |  assign serial = data[0];
      |  assign tristate = ~outputEnable;
      |`endif
      |endmodule
      |""".stripMargin)
}

/** Xilinx UltraScale ISERDESE3 wrapper for an 8-edge input word. */
class UltraScaleInputSerdes(device: String) extends BlackBox(Map(
    "SIM_DEVICE" -> StringParam(device))) with HasBlackBoxInline {
  require(UltraScaleDevice.supported.contains(device))

  val io = IO(new Bundle {
    val reset = Input(Bool())
    val serialClock = Input(Clock())
    val dividedClock = Input(Clock())
    val serial = Input(Bool())
    val data = Output(UInt(8.W))
  })

  setInline("UltraScaleInputSerdes.sv",
    """module UltraScaleInputSerdes #(
      |  parameter SIM_DEVICE = "ULTRASCALE"
      |) (
      |  input wire reset, input wire serialClock, input wire dividedClock,
      |  input wire serial, output wire [7:0] data
      |);
      |`ifdef SYNTHESIS
      |  ISERDESE3 #(
      |    .SIM_DEVICE(SIM_DEVICE), .DATA_WIDTH(8),
      |    .IS_RST_INVERTED(1'b0), .IS_CLK_INVERTED(1'b0),
      |    .IS_CLK_B_INVERTED(1'b1)
      |  ) u_primitive (
      |    .RST(reset), .CLK(serialClock), .CLK_B(serialClock),
      |    .CLKDIV(dividedClock), .D(serial), .FIFO_RD_EN(1'b0),
      |    .Q(data), .FIFO_EMPTY(), .INTERNAL_DIVCLK()
      |  );
      |`else
      |  assign data = {8{serial}};
      |`endif
      |endmodule
      |""".stripMargin)
}

/** Variable UltraScale IDELAYE3 in TIME mode. */
class UltraScaleInputDelay(device: String, refClockFrequencyMHz: Int,
    initialValuePs: Int = 0) extends BlackBox(Map(
      "SIM_DEVICE" -> StringParam(device),
      "REFCLK_FREQUENCY" -> IntParam(refClockFrequencyMHz),
      "INITIAL_VALUE_PS" -> IntParam(initialValuePs))) with HasBlackBoxInline {
  require(UltraScaleDevice.supported.contains(device))
  require(refClockFrequencyMHz >= (if (device == UltraScaleDevice.UltraScalePlus) 300 else 200))
  require(initialValuePs >= 0 && initialValuePs <= 1250)

  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val enableVtc = Input(Bool())
    val increment = Input(Bool())
    val dataIn = Input(Bool())
    val dataOut = Output(Bool())
    val value = Output(UInt(9.W))
  })

  setInline("UltraScaleInputDelay.sv",
    """module UltraScaleInputDelay #(
      |  parameter SIM_DEVICE = "ULTRASCALE",
      |  parameter integer REFCLK_FREQUENCY = 200,
      |  parameter integer INITIAL_VALUE_PS = 0
      |) (
      |  input wire clock, input wire reset, input wire enableVtc,
      |  input wire increment, input wire dataIn,
      |  output wire dataOut, output wire [8:0] value
      |);
      |`ifdef SYNTHESIS
      |  IDELAYE3 #(
      |    .SIM_DEVICE(SIM_DEVICE), .CASCADE("NONE"),
      |    .UPDATE_MODE("ASYNC"), .REFCLK_FREQUENCY(REFCLK_FREQUENCY),
      |    .DELAY_FORMAT("TIME"), .DELAY_SRC("IDATAIN"),
      |    .DELAY_TYPE("VARIABLE"), .DELAY_VALUE(INITIAL_VALUE_PS),
      |    .IS_CLK_INVERTED(1'b0), .IS_RST_INVERTED(1'b0)
      |  ) u_primitive (
      |    .RST(reset), .CLK(clock), .EN_VTC(enableVtc),
      |    .CE(increment), .INC(1'b1), .LOAD(1'b0),
      |    .CNTVALUEIN(9'b0), .CNTVALUEOUT(value),
      |    .IDATAIN(dataIn), .DATAIN(1'b0), .DATAOUT(dataOut),
      |    .CASC_IN(1'b0), .CASC_RETURN(1'b0), .CASC_OUT()
      |  );
      |`else
      |  assign dataOut = dataIn;
      |  assign value = INITIAL_VALUE_PS[8:0];
      |`endif
      |endmodule
      |""".stripMargin)
}

/** Variable UltraScale ODELAYE3 in TIME mode. */
class UltraScaleOutputDelay(device: String, refClockFrequencyMHz: Int,
    initialValuePs: Int = 0) extends BlackBox(Map(
      "SIM_DEVICE" -> StringParam(device),
      "REFCLK_FREQUENCY" -> IntParam(refClockFrequencyMHz),
      "INITIAL_VALUE_PS" -> IntParam(initialValuePs))) with HasBlackBoxInline {
  require(UltraScaleDevice.supported.contains(device))
  require(refClockFrequencyMHz >= (if (device == UltraScaleDevice.UltraScalePlus) 300 else 200))
  require(initialValuePs >= 0 && initialValuePs <= 1250)

  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val enableVtc = Input(Bool())
    val increment = Input(Bool())
    val dataIn = Input(Bool())
    val dataOut = Output(Bool())
    val value = Output(UInt(9.W))
  })

  setInline("UltraScaleOutputDelay.sv",
    """module UltraScaleOutputDelay #(
      |  parameter SIM_DEVICE = "ULTRASCALE",
      |  parameter integer REFCLK_FREQUENCY = 200,
      |  parameter integer INITIAL_VALUE_PS = 0
      |) (
      |  input wire clock, input wire reset, input wire enableVtc,
      |  input wire increment, input wire dataIn,
      |  output wire dataOut, output wire [8:0] value
      |);
      |`ifdef SYNTHESIS
      |  ODELAYE3 #(
      |    .SIM_DEVICE(SIM_DEVICE), .CASCADE("NONE"),
      |    .UPDATE_MODE("ASYNC"), .REFCLK_FREQUENCY(REFCLK_FREQUENCY),
      |    .DELAY_FORMAT("TIME"), .DELAY_TYPE("VARIABLE"),
      |    .DELAY_VALUE(INITIAL_VALUE_PS),
      |    .IS_CLK_INVERTED(1'b0), .IS_RST_INVERTED(1'b0)
      |  ) u_primitive (
      |    .RST(reset), .CLK(clock), .EN_VTC(enableVtc),
      |    .CE(increment), .INC(1'b1), .LOAD(1'b0),
      |    .CNTVALUEIN(9'b0), .CNTVALUEOUT(value),
      |    .ODATAIN(dataIn), .DATAOUT(dataOut),
      |    .CASC_IN(1'b0), .CASC_RETURN(1'b0), .CASC_OUT()
      |  );
      |`else
      |  assign dataOut = dataIn;
      |  assign value = INITIAL_VALUE_PS[8:0];
      |`endif
      |endmodule
      |""".stripMargin)
}

/** UltraScale IOBUFDSE3 differential bidirectional buffer. */
class UltraScaleDifferentialIoBuffer extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val outputData = Input(Bool())
    val tristate = Input(Bool())
    val inputData = Output(Bool())
    val padPositive = Analog(1.W)
    val padNegative = Analog(1.W)
  })

  setInline("UltraScaleDifferentialIoBuffer.sv",
    """module UltraScaleDifferentialIoBuffer(
      |  input wire outputData, input wire tristate, output wire inputData,
      |  inout wire padPositive, inout wire padNegative
      |);
      |`ifdef SYNTHESIS
      |  IOBUFDSE3 u_primitive(
      |    .I(outputData), .T(tristate), .O(inputData),
      |    .IO(padPositive), .IOB(padNegative)
      |  );
      |`else
      |  assign padPositive = tristate ? 1'bz : outputData;
      |  assign padNegative = tristate ? 1'bz : ~outputData;
      |  assign inputData = padPositive;
      |`endif
      |endmodule
      |""".stripMargin)
}

/** OSERDESE3 followed by ODELAYE3. */
class UltraScaleDelayedOutputSerdes(device: String, refClockFrequencyMHz: Int,
    initialValuePs: Int = 0) extends RawModule {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val serialClock = Input(Clock())
    val dividedClock = Input(Clock())
    val delayClock = Input(Clock())
    val enableVtc = Input(Bool())
    val delayReset = Input(Bool())
    val delayIncrement = Input(Bool())
    val parallelOut = Input(UInt(8.W))
    val outputEnable = Input(Bool())
    val serial = Output(Bool())
    val tristate = Output(Bool())
    val delayValue = Output(UInt(9.W))
  })

  private val serializer = Module(new UltraScaleOutputSerdes(device))
  private val delay = Module(new UltraScaleOutputDelay(device,
    refClockFrequencyMHz, initialValuePs))
  serializer.io.reset := io.reset
  serializer.io.serialClock := io.serialClock
  serializer.io.dividedClock := io.dividedClock
  serializer.io.data := io.parallelOut
  serializer.io.outputEnable := io.outputEnable
  delay.io.clock := io.delayClock
  delay.io.reset := io.delayReset
  delay.io.enableVtc := io.enableVtc
  delay.io.increment := io.delayIncrement
  delay.io.dataIn := serializer.io.serial
  io.serial := delay.io.dataOut
  io.tristate := serializer.io.tristate
  io.delayValue := delay.io.value
}

/** Complete single-ended UltraScale data lane with independent I/O delays. */
class UltraScaleBidirectionalSerdesLane(device: String,
    refClockFrequencyMHz: Int) extends RawModule {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val serialClock = Input(Clock())
    val dividedClock = Input(Clock())
    val delayClock = Input(Clock())
    val enableVtc = Input(Bool())
    val inputDelayReset = Input(Bool())
    val inputDelayIncrement = Input(Bool())
    val outputDelayReset = Input(Bool())
    val outputDelayIncrement = Input(Bool())
    val parallelOut = Input(UInt(8.W))
    val outputEnable = Input(Bool())
    val parallelIn = Output(UInt(8.W))
    val inputDelayValue = Output(UInt(9.W))
    val outputDelayValue = Output(UInt(9.W))
    val pad = Analog(1.W)
  })

  private val output = Module(new UltraScaleDelayedOutputSerdes(device,
    refClockFrequencyMHz))
  private val buffer = Module(new S7IoBuffer)
  private val inputDelay = Module(new UltraScaleInputDelay(device,
    refClockFrequencyMHz))
  private val input = Module(new UltraScaleInputSerdes(device))
  output.io.reset := io.reset
  output.io.serialClock := io.serialClock
  output.io.dividedClock := io.dividedClock
  output.io.delayClock := io.delayClock
  output.io.enableVtc := io.enableVtc
  output.io.delayReset := io.outputDelayReset
  output.io.delayIncrement := io.outputDelayIncrement
  output.io.parallelOut := io.parallelOut
  output.io.outputEnable := io.outputEnable
  buffer.io.outputData := output.io.serial
  buffer.io.tristate := output.io.tristate
  attach(buffer.io.pad, io.pad)
  inputDelay.io.clock := io.delayClock
  inputDelay.io.reset := io.inputDelayReset
  inputDelay.io.enableVtc := io.enableVtc
  inputDelay.io.increment := io.inputDelayIncrement
  inputDelay.io.dataIn := buffer.io.inputData
  input.io.reset := io.reset
  input.io.serialClock := io.serialClock
  input.io.dividedClock := io.dividedClock
  input.io.serial := inputDelay.io.dataOut
  io.parallelIn := input.io.data
  io.inputDelayValue := inputDelay.io.value
  io.outputDelayValue := output.io.delayValue
}

/** Differential DQS output/tristate lane with ODELAYE3 phase shift. */
class UltraScaleDifferentialOutputSerdesIoLane(device: String,
    refClockFrequencyMHz: Int, outputInitialValuePs: Int) extends RawModule {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val serialClock = Input(Clock())
    val dividedClock = Input(Clock())
    val delayClock = Input(Clock())
    val enableVtc = Input(Bool())
    val delayReset = Input(Bool())
    val delayIncrement = Input(Bool())
    val parallelOut = Input(UInt(8.W))
    val outputEnable = Input(Bool())
    val delayValue = Output(UInt(9.W))
    val padPositive = Analog(1.W)
    val padNegative = Analog(1.W)
  })

  private val output = Module(new UltraScaleDelayedOutputSerdes(device,
    refClockFrequencyMHz, outputInitialValuePs))
  private val buffer = Module(new UltraScaleDifferentialIoBuffer)
  output.io.reset := io.reset
  output.io.serialClock := io.serialClock
  output.io.dividedClock := io.dividedClock
  output.io.delayClock := io.delayClock
  output.io.enableVtc := io.enableVtc
  output.io.delayReset := io.delayReset
  output.io.delayIncrement := io.delayIncrement
  output.io.parallelOut := io.parallelOut
  output.io.outputEnable := io.outputEnable
  buffer.io.outputData := output.io.serial
  buffer.io.tristate := output.io.tristate
  io.delayValue := output.io.delayValue
  attach(buffer.io.padPositive, io.padPositive)
  attach(buffer.io.padNegative, io.padNegative)
}
