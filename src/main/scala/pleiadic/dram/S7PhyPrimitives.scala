package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, IntParam, StringParam, attach}
import chisel3.util.{Cat, HasBlackBoxInline}
import scala.language.reflectiveCalls

/** Fixed cross-word bitslip used for S7 command/address phase alignment. */
class S7ConstantBitSlip(width: Int, slip: Int) extends RawModule {
  require(width >= 2 && slip >= 0 && slip < width)
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val input = Input(UInt(width.W))
    val output = Output(UInt(width.W))
  })
  private val previous = withClockAndReset(io.clock, io.reset) {
    RegInit(0.U(width.W))
  }
  previous := io.input
  private val combined = Cat(io.input, previous)
  private val internalPosition = width - 1 - slip
  io.output := (combined >> (internalPosition + 1))(width - 1, 0)
}

/** Parameterized Xilinx 7-series OSERDESE2 wrapper (4:1 SDR or 8:1 DDR). */
class S7OutputSerdes(dataWidth: Int = 8, dataRate: String = "DDR") extends
    BlackBox(Map("DATA_WIDTH" -> IntParam(dataWidth),
      "DATA_RATE_OQ" -> StringParam(dataRate))) with HasBlackBoxInline {
  require((dataRate == "DDR" && dataWidth == 8) ||
    (dataRate == "SDR" && dataWidth >= 2 && dataWidth <= 8))

  val io = IO(new Bundle {
    val reset = Input(Bool())
    val serialClock = Input(Clock())
    val dividedClock = Input(Clock())
    val data = Input(UInt(8.W))
    val outputEnable = Input(Bool())
    val serial = Output(Bool())
    val tristate = Output(Bool())
  })

  setInline("S7OutputSerdes.sv",
    """module S7OutputSerdes #(
      |  parameter integer DATA_WIDTH = 8,
      |  parameter DATA_RATE_OQ = "DDR"
      |) (
      |  input  wire       reset,
      |  input  wire       serialClock,
      |  input  wire       dividedClock,
      |  input  wire [7:0] data,
      |  input  wire       outputEnable,
      |  output wire       serial,
      |  output wire       tristate
      |);
      |`ifdef SYNTHESIS
      |  OSERDESE2 #(
      |    .SERDES_MODE("MASTER"),
      |    .DATA_WIDTH(DATA_WIDTH),
      |    .DATA_RATE_OQ(DATA_RATE_OQ),
      |    .DATA_RATE_TQ("BUF"),
      |    .TRISTATE_WIDTH(1)
      |  ) u_primitive (
      |    .OQ(serial), .TQ(tristate), .OFB(), .TFB(),
      |    .SHIFTOUT1(), .SHIFTOUT2(),
      |    .CLK(serialClock), .CLKDIV(dividedClock), .RST(reset),
      |    .OCE(1'b1), .TCE(1'b1),
      |    .D1(data[0]), .D2(data[1]), .D3(data[2]), .D4(data[3]),
      |    .D5(data[4]), .D6(data[5]), .D7(data[6]), .D8(data[7]),
      |    .T1(~outputEnable), .T2(~outputEnable),
      |    .T3(~outputEnable), .T4(~outputEnable),
      |    .SHIFTIN1(1'b0), .SHIFTIN2(1'b0)
      |  );
      |`else
      |  assign serial = data[0];
      |  assign tristate = ~outputEnable;
      |`endif
      |endmodule
      |""".stripMargin)
}

/** Parameterized Xilinx 7-series ISERDESE2 NETWORKING wrapper. */
class S7InputSerdes(dataWidth: Int = 8, dataRate: String = "DDR") extends
    BlackBox(Map("DATA_WIDTH" -> IntParam(dataWidth),
      "DATA_RATE" -> StringParam(dataRate))) with HasBlackBoxInline {
  require((dataRate == "DDR" && dataWidth == 8) ||
    (dataRate == "SDR" && dataWidth >= 2 && dataWidth <= 8))

  val io = IO(new Bundle {
    val reset = Input(Bool())
    val serialClock = Input(Clock())
    val invertedSerialClock = Input(Clock())
    val dividedClock = Input(Clock())
    val serial = Input(Bool())
    val bitslip = Input(Bool())
    val data = Output(UInt(8.W))
  })

  setInline("S7InputSerdes.sv",
    """module S7InputSerdes #(
      |  parameter integer DATA_WIDTH = 8,
      |  parameter DATA_RATE = "DDR"
      |) (
      |  input  wire       reset,
      |  input  wire       serialClock,
      |  input  wire       invertedSerialClock,
      |  input  wire       dividedClock,
      |  input  wire       serial,
      |  input  wire       bitslip,
      |  output wire [7:0] data
      |);
      |`ifdef SYNTHESIS
      |  wire [7:0] q;
      |  ISERDESE2 #(
      |    .SERDES_MODE("MASTER"),
      |    .INTERFACE_TYPE("NETWORKING"),
      |    .DATA_WIDTH(DATA_WIDTH),
      |    .DATA_RATE(DATA_RATE),
      |    .NUM_CE(1),
      |    .IOBDELAY("IFD")
      |  ) u_primitive (
      |    .Q1(q[7]), .Q2(q[6]), .Q3(q[5]), .Q4(q[4]),
      |    .Q5(q[3]), .Q6(q[2]), .Q7(q[1]), .Q8(q[0]),
      |    .O(), .SHIFTOUT1(), .SHIFTOUT2(),
      |    .CLK(serialClock), .CLKB(invertedSerialClock),
      |    .CLKDIV(dividedClock), .OCLK(1'b0), .OCLKB(1'b0),
      |    .RST(reset), .BITSLIP(bitslip), .CE1(1'b1), .CE2(1'b0),
      |    .D(1'b0), .DDLY(serial),
      |    .SHIFTIN1(1'b0), .SHIFTIN2(1'b0), .DYNCLKDIVSEL(1'b0),
      |    .DYNCLKSEL(1'b0), .OFB(1'b0)
      |  );
      |  assign data = q;
      |`else
      |  assign data = {8{serial}};
      |`endif
      |endmodule
      |""".stripMargin)
}

/** Variable Xilinx 7-series input delay with 32 taps. */
class S7InputDelay(refClockFrequencyMHz: Int = 200, initialValue: Int = 0) extends
    BlackBox(Map("REFCLK_FREQUENCY" -> IntParam(refClockFrequencyMHz),
      "INITIAL_VALUE" -> IntParam(initialValue))) with HasBlackBoxInline {
  require(Set(200, 300, 400).contains(refClockFrequencyMHz))
  require(initialValue >= 0 && initialValue < 32)

  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val increment = Input(Bool())
    val dataIn = Input(Bool())
    val dataOut = Output(Bool())
    val value = Output(UInt(5.W))
  })

  setInline("S7InputDelay.sv",
    """module S7InputDelay #(
      |  parameter integer REFCLK_FREQUENCY = 200,
      |  parameter integer INITIAL_VALUE = 0
      |) (
      |  input wire clock, input wire reset, input wire increment,
      |  input wire dataIn, output wire dataOut, output wire [4:0] value
      |);
      |`ifdef SYNTHESIS
      |  IDELAYE2 #(
      |    .SIGNAL_PATTERN("DATA"), .DELAY_SRC("IDATAIN"),
      |    .IDELAY_TYPE("VARIABLE"), .IDELAY_VALUE(INITIAL_VALUE),
      |    .REFCLK_FREQUENCY(REFCLK_FREQUENCY),
      |    .CINVCTRL_SEL("FALSE"), .HIGH_PERFORMANCE_MODE("TRUE"),
      |    .PIPE_SEL("FALSE")
      |  ) u_primitive (
      |    .C(clock), .LD(reset), .CE(increment), .INC(1'b1),
      |    .IDATAIN(dataIn), .DATAIN(1'b0), .DATAOUT(dataOut),
      |    .CNTVALUEOUT(value), .CNTVALUEIN(5'b0),
      |    .CINVCTRL(1'b0), .LDPIPEEN(1'b0), .REGRST(1'b0)
      |  );
      |`else
      |  assign dataOut = dataIn;
      |  assign value = INITIAL_VALUE[4:0];
      |`endif
      |endmodule
      |""".stripMargin)
}

/** Variable Xilinx 7-series output delay (Kintex-7/Virtex-7). */
class S7OutputDelay(refClockFrequencyMHz: Int = 200, initialValue: Int = 0) extends
    BlackBox(Map("REFCLK_FREQUENCY" -> IntParam(refClockFrequencyMHz),
      "INITIAL_VALUE" -> IntParam(initialValue))) with HasBlackBoxInline {
  require(Set(200, 300, 400).contains(refClockFrequencyMHz))
  require(initialValue >= 0 && initialValue < 32)

  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val increment = Input(Bool())
    val dataIn = Input(Bool())
    val dataOut = Output(Bool())
    val value = Output(UInt(5.W))
  })

  setInline("S7OutputDelay.sv",
    """module S7OutputDelay #(
      |  parameter integer REFCLK_FREQUENCY = 200,
      |  parameter integer INITIAL_VALUE = 0
      |) (
      |  input wire clock, input wire reset, input wire increment,
      |  input wire dataIn, output wire dataOut, output wire [4:0] value
      |);
      |`ifdef SYNTHESIS
      |  ODELAYE2 #(
      |    .SIGNAL_PATTERN("DATA"), .DELAY_SRC("ODATAIN"),
      |    .ODELAY_TYPE("VARIABLE"), .ODELAY_VALUE(INITIAL_VALUE),
      |    .REFCLK_FREQUENCY(REFCLK_FREQUENCY),
      |    .CINVCTRL_SEL("FALSE"), .HIGH_PERFORMANCE_MODE("TRUE"),
      |    .PIPE_SEL("FALSE")
      |  ) u_primitive (
      |    .C(clock), .LD(reset), .CE(increment), .INC(1'b1),
      |    .ODATAIN(dataIn), .DATAOUT(dataOut),
      |    .CNTVALUEOUT(value), .CNTVALUEIN(5'b0),
      |    .CINVCTRL(1'b0), .LDPIPEEN(1'b0), .REGRST(1'b0),
      |    .CLKIN(1'b0)
      |  );
      |`else
      |  assign dataOut = dataIn;
      |  assign value = INITIAL_VALUE[4:0];
      |`endif
      |endmodule
      |""".stripMargin)
}

class S7IoBuffer extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val outputData = Input(Bool())
    val tristate = Input(Bool())
    val inputData = Output(Bool())
    val pad = Analog(1.W)
  })
  setInline("S7IoBuffer.sv",
    """module S7IoBuffer(input wire outputData, input wire tristate,
      |  output wire inputData, inout wire pad);
      |`ifdef SYNTHESIS
      |  IOBUF u_primitive(.I(outputData), .T(tristate), .O(inputData), .IO(pad));
      |`else
      |  assign pad = tristate ? 1'bz : outputData;
      |  assign inputData = pad;
      |`endif
      |endmodule
      |""".stripMargin)
}

class S7DifferentialIoBuffer extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val outputData = Input(Bool())
    val tristate = Input(Bool())
    val inputData = Output(Bool())
    val padPositive = Analog(1.W)
    val padNegative = Analog(1.W)
  })
  setInline("S7DifferentialIoBuffer.sv",
    """module S7DifferentialIoBuffer(input wire outputData, input wire tristate,
      |  output wire inputData, inout wire padPositive, inout wire padNegative);
      |`ifdef SYNTHESIS
      |  IOBUFDS u_primitive(.I(outputData), .T(tristate), .O(inputData),
      |    .IO(padPositive), .IOB(padNegative));
      |`else
      |  assign padPositive = tristate ? 1'bz : outputData;
      |  assign padNegative = tristate ? 1'bz : ~outputData;
      |  assign inputData = padPositive;
      |`endif
      |endmodule
      |""".stripMargin)
}

class S7DifferentialOutputBuffer extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val dataIn = Input(Bool())
    val padPositive = Analog(1.W)
    val padNegative = Analog(1.W)
  })
  setInline("S7DifferentialOutputBuffer.sv",
    """module S7DifferentialOutputBuffer(input wire dataIn,
      |  output wire padPositive, output wire padNegative);
      |`ifdef SYNTHESIS
      |  OBUFDS u_primitive(.I(dataIn), .O(padPositive), .OB(padNegative));
      |`else
      |  assign padPositive = dataIn;
      |  assign padNegative = ~dataIn;
      |`endif
      |endmodule
      |""".stripMargin)
}

/**
  * Reusable bidirectional S7 lane: OSERDESE2 -> IOBUF -> IDELAYE2 ->
  * ISERDESE2. It is directly usable for RPC DB and LPDDR5 4:1 DQ/DMI lanes.
  */
class S7BidirectionalSerdesLane(dataWidth: Int = 8, dataRate: String = "DDR",
    refClockFrequencyMHz: Int = 200) extends RawModule {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val outputSerialClock = Input(Clock())
    val inputSerialClock = Input(Clock())
    val invertedSerialClock = Input(Clock())
    val dividedClock = Input(Clock())
    val delayClock = Input(Clock())
    val delayReset = Input(Bool())
    val delayIncrement = Input(Bool())
    val bitslip = Input(Bool())
    val parallelOut = Input(UInt(8.W))
    val outputEnable = Input(Bool())
    val parallelIn = Output(UInt(8.W))
    val delayValue = Output(UInt(5.W))
    val pad = Analog(1.W)
  })

  private val output = Module(new S7OutputSerdes(dataWidth, dataRate))
  private val buffer = Module(new S7IoBuffer)
  private val delay = Module(new S7InputDelay(refClockFrequencyMHz))
  private val input = Module(new S7InputSerdes(dataWidth, dataRate))

  output.io.reset := io.reset
  output.io.serialClock := io.outputSerialClock
  output.io.dividedClock := io.dividedClock
  output.io.data := io.parallelOut
  output.io.outputEnable := io.outputEnable
  buffer.io.outputData := output.io.serial
  buffer.io.tristate := output.io.tristate
  attach(buffer.io.pad, io.pad)
  delay.io.clock := io.delayClock
  delay.io.reset := io.delayReset
  delay.io.increment := io.delayIncrement
  delay.io.dataIn := buffer.io.inputData
  input.io.reset := io.reset
  input.io.serialClock := io.inputSerialClock
  input.io.invertedSerialClock := io.invertedSerialClock
  input.io.dividedClock := io.dividedClock
  input.io.serial := delay.io.dataOut
  input.io.bitslip := io.bitslip
  io.parallelIn := input.io.data
  io.delayValue := delay.io.value
}

/** Differential output-only 8:1 SerDes lane for CK and WCK. */
class S7DifferentialOutputSerdesLane extends RawModule {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val serialClock = Input(Clock())
    val dividedClock = Input(Clock())
    val parallelOut = Input(UInt(8.W))
    val padPositive = Analog(1.W)
    val padNegative = Analog(1.W)
  })

  private val serializer = Module(new S7OutputSerdes(8, "DDR"))
  private val buffer = Module(new S7DifferentialOutputBuffer)
  serializer.io.reset := io.reset
  serializer.io.serialClock := io.serialClock
  serializer.io.dividedClock := io.dividedClock
  serializer.io.data := io.parallelOut
  serializer.io.outputEnable := true.B
  buffer.io.dataIn := serializer.io.serial
  attach(buffer.io.padPositive, io.padPositive)
  attach(buffer.io.padNegative, io.padNegative)
}

/** Differential bidirectional S7 SerDes lane for DQS/RDQS. */
class S7DifferentialBidirectionalSerdesLane(refClockFrequencyMHz: Int = 200)
    extends RawModule {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val outputSerialClock = Input(Clock())
    val inputSerialClock = Input(Clock())
    val invertedInputSerialClock = Input(Clock())
    val dividedClock = Input(Clock())
    val delayClock = Input(Clock())
    val delayReset = Input(Bool())
    val delayIncrement = Input(Bool())
    val bitslip = Input(Bool())
    val parallelOut = Input(UInt(8.W))
    val outputEnable = Input(Bool())
    val parallelIn = Output(UInt(8.W))
    val delayValue = Output(UInt(5.W))
    val padPositive = Analog(1.W)
    val padNegative = Analog(1.W)
  })

  private val output = Module(new S7OutputSerdes(8, "DDR"))
  private val buffer = Module(new S7DifferentialIoBuffer)
  private val delay = Module(new S7InputDelay(refClockFrequencyMHz))
  private val input = Module(new S7InputSerdes(8, "DDR"))
  output.io.reset := io.reset
  output.io.serialClock := io.outputSerialClock
  output.io.dividedClock := io.dividedClock
  output.io.data := io.parallelOut
  output.io.outputEnable := io.outputEnable
  buffer.io.outputData := output.io.serial
  buffer.io.tristate := output.io.tristate
  attach(buffer.io.padPositive, io.padPositive)
  attach(buffer.io.padNegative, io.padNegative)
  delay.io.clock := io.delayClock
  delay.io.reset := io.delayReset
  delay.io.increment := io.delayIncrement
  delay.io.dataIn := buffer.io.inputData
  input.io.reset := io.reset
  input.io.serialClock := io.inputSerialClock
  input.io.invertedSerialClock := io.invertedInputSerialClock
  input.io.dividedClock := io.dividedClock
  input.io.serial := delay.io.dataOut
  input.io.bitslip := io.bitslip
  io.parallelIn := input.io.data
  io.delayValue := delay.io.value
}
