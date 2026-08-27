package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, IntParam, attach}
import chisel3.util.HasBlackBoxInline
import scala.language.reflectiveCalls

/** Four-edge control/clock serializer shared by GW2A and GW5A. */
class GowinOutputSerdes extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val data = Input(UInt(4.W))
    val serial = Output(Bool())
  })
  setInline("GowinOutputSerdes.sv",
    """module GowinOutputSerdes(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire [3:0] data, output wire serial
      |);
      |`ifdef SYNTHESIS
      |  OSER4 #(.TXCLK_POL(1'b0)) u_primitive(
      |    .RESET(reset), .PCLK(systemClock), .FCLK(edgeClock),
      |    .TX0(1'b0), .TX1(1'b0), .D0(data[0]), .D1(data[1]),
      |    .D2(data[2]), .D3(data[3]), .Q0(serial), .Q1()
      |  );
      |`else
      |  assign serial = data[0];
      |`endif
      |endmodule
      |""".stripMargin)
}

/** Family-specific static IODELAY port mapping. */
class GowinCommandDelay(family: GowinFamily, initialValue: Int = 0)
    extends BlackBox(Map("INITIAL_VALUE" -> IntParam(initialValue)))
    with HasBlackBoxInline {
  require(initialValue >= 0 && initialValue < 128)
  override def desiredName: String = s"GowinCommandDelay${family.suffix}"
  val io = IO(new Bundle {
    val dataIn = Input(Bool())
    val dataOut = Output(Bool())
  })
  private val moduleName = desiredName
  private val primitive = family match {
    case GowinFamily.GW2A =>
      """IODELAY #(.C_STATIC_DLY(INITIAL_VALUE)) u_primitive(
        |    .SDTAP(1'b0), .SETN(1'b0), .VALUE(1'b0), .DI(dataIn),
        |    .DF(), .DO(dataOut)
        |  );""".stripMargin
    case GowinFamily.GW5A =>
      """IODELAY #(.C_STATIC_DLY(INITIAL_VALUE), .DYN_DLY_EN("FALSE"),
        |    .ADAPT_EN("FALSE")) u_primitive(
        |    .SDTAP(1'b0), .DLYSTEP(8'b0), .VALUE(1'b0), .DI(dataIn),
        |    .DF(), .DO(dataOut)
        |  );""".stripMargin
  }
  setInline(s"$moduleName.sv",
    s"""module $moduleName #(
       |  parameter integer INITIAL_VALUE = 0
       |) (input wire dataIn, output wire dataOut);
       |`ifdef SYNTHESIS
       |  $primitive
       |`else
       |  assign dataOut = dataIn;
       |`endif
       |endmodule
       |""".stripMargin)
}

class GowinDifferentialOutputBuffer extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val dataIn = Input(Bool())
    val positive = Output(Bool())
    val negative = Output(Bool())
  })
  setInline("GowinDifferentialOutputBuffer.sv",
    """module GowinDifferentialOutputBuffer(
      |  input wire dataIn, output wire positive, output wire negative
      |);
      |`ifdef SYNTHESIS
      |  ELVDS_OBUF u_primitive(.I(dataIn), .O(positive), .OB(negative));
      |`else
      |  assign positive = dataIn;
      |  assign negative = ~dataIn;
      |`endif
      |endmodule
      |""".stripMargin)
}

/** Gowin DQS clock/pointer generation and burst observation. */
class GowinDqsBuffer extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val dllDelay = Input(UInt(8.W))
    val pause = Input(Bool())
    val readEnable = Input(Bool())
    val readDelay = Input(UInt(3.W))
    val dqsInput = Input(Bool())
    val readClock90 = Output(Clock())
    val writeClock270 = Output(Clock())
    val writeClock = Output(Clock())
    val readPointer = Output(UInt(3.W))
    val writePointer = Output(UInt(3.W))
    val dataValid = Output(Bool())
    val burstDetected = Output(Bool())
  })
  setInline("GowinDqsBuffer.sv",
    """module GowinDqsBuffer(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire [7:0] dllDelay, input wire pause, input wire readEnable,
      |  input wire [2:0] readDelay, input wire dqsInput,
      |  output wire readClock90, output wire writeClock270,
      |  output wire writeClock, output wire [2:0] readPointer,
      |  output wire [2:0] writePointer, output wire dataValid,
      |  output wire burstDetected
      |);
      |`ifdef SYNTHESIS
      |  DQS #(.DQS_MODE("X2_DDR3")) u_primitive(
      |    .RESET(reset), .PCLK(systemClock), .FCLK(edgeClock),
      |    .DLLSTEP(dllDelay), .HOLD(pause),
      |    .RLOADN(1'b0), .RMOVE(1'b0), .RDIR(1'b1),
      |    .WLOADN(1'b0), .WMOVE(1'b0), .WDIR(1'b1),
      |    .RFLAG(), .WFLAG(), .READ({4{readEnable}}),
      |    .RCLKSEL(readDelay), .DQSIN(dqsInput), .DQSR90(readClock90),
      |    .RPOINT(readPointer), .WPOINT(writePointer),
      |    .RVALID(dataValid), .RBURST(burstDetected), .WSTEP(8'b0),
      |    .DQSW270(writeClock270), .DQSW0(writeClock)
      |  );
      |`else
      |  assign readClock90 = edgeClock;
      |  assign writeClock270 = edgeClock;
      |  assign writeClock = edgeClock;
      |  assign readPointer = 3'b0;
      |  assign writePointer = 3'b0;
      |  assign dataValid = readEnable;
      |  assign burstDetected = dqsInput;
      |`endif
      |endmodule
      |""".stripMargin)
}

class GowinDqsOutputSerdes extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val writeClock = Input(Clock())
    val data = Input(UInt(4.W))
    val tristateData = Input(UInt(2.W))
    val serial = Output(Bool())
    val tristate = Output(Bool())
  })
  setInline("GowinDqsOutputSerdes.sv",
    """module GowinDqsOutputSerdes(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire writeClock, input wire [3:0] data,
      |  input wire [1:0] tristateData, output wire serial,
      |  output wire tristate
      |);
      |`ifdef SYNTHESIS
      |  OSER4_MEM #(.TCLK_SOURCE("DQSW"), .TXCLK_POL(1'b1)) u_primitive(
      |    .RESET(reset), .PCLK(systemClock), .FCLK(edgeClock),
      |    .TCLK(writeClock), .TX0(tristateData[0]), .TX1(tristateData[1]),
      |    .D0(data[0]), .D1(data[1]), .D2(data[2]), .D3(data[3]),
      |    .Q0(serial), .Q1(tristate)
      |  );
      |`else
      |  assign serial = data[0];
      |  assign tristate = tristateData[0];
      |`endif
      |endmodule
      |""".stripMargin)
}

class GowinDataOutputSerdes extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val writeClock270 = Input(Clock())
    val data = Input(UInt(4.W))
    val tristateData = Input(UInt(2.W))
    val serial = Output(Bool())
    val tristate = Output(Bool())
  })
  setInline("GowinDataOutputSerdes.sv",
    """module GowinDataOutputSerdes(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire writeClock270, input wire [3:0] data,
      |  input wire [1:0] tristateData, output wire serial,
      |  output wire tristate
      |);
      |`ifdef SYNTHESIS
      |  OSER4_MEM #(.TCLK_SOURCE("DQSW270"), .TXCLK_POL(1'b0)) u_primitive(
      |    .RESET(reset), .PCLK(systemClock), .FCLK(edgeClock),
      |    .TCLK(writeClock270), .TX0(tristateData[0]),
      |    .TX1(tristateData[1]), .D0(data[0]), .D1(data[1]),
      |    .D2(data[2]), .D3(data[3]), .Q0(serial), .Q1(tristate)
      |  );
      |`else
      |  assign serial = data[0];
      |  assign tristate = tristateData[0];
      |`endif
      |endmodule
      |""".stripMargin)
}

class GowinDataInputSerdes extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val readClock90 = Input(Clock())
    val readPointer = Input(UInt(3.W))
    val writePointer = Input(UInt(3.W))
    val serial = Input(Bool())
    val data = Output(UInt(4.W))
  })
  setInline("GowinDataInputSerdes.sv",
    """module GowinDataInputSerdes(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire readClock90, input wire [2:0] readPointer,
      |  input wire [2:0] writePointer, input wire serial,
      |  output wire [3:0] data
      |);
      |`ifdef SYNTHESIS
      |  IDES4_MEM u_primitive(
      |    .RESET(reset), .PCLK(systemClock), .FCLK(edgeClock),
      |    .ICLK(readClock90), .RADDR(readPointer), .WADDR(writePointer),
      |    .D(serial), .CALIB(1'b0), .Q0(data[0]), .Q1(data[1]),
      |    .Q2(data[2]), .Q3(data[3])
      |  );
      |`else
      |  assign data = {4{serial}};
      |`endif
      |endmodule
      |""".stripMargin)
}

class GowinSingleEndedTristateBuffer extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val outputData = Input(Bool())
    val tristate = Input(Bool())
    val inputData = Output(Bool())
    val pad = Analog(1.W)
  })
  setInline("GowinSingleEndedTristateBuffer.sv",
    """module GowinSingleEndedTristateBuffer(
      |  input wire outputData, input wire tristate,
      |  output wire inputData, inout wire pad
      |);
      |`ifdef SYNTHESIS
      |  IOBUF u_primitive(.I(outputData), .OEN(tristate), .O(inputData), .IO(pad));
      |`else
      |  assign pad = tristate ? 1'bz : outputData;
      |  assign inputData = pad;
      |`endif
      |endmodule
      |""".stripMargin)
}

class GowinDifferentialTristateBuffer extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val outputData = Input(Bool())
    val tristate = Input(Bool())
    val inputData = Output(Bool())
    val positive = Analog(1.W)
    val negative = Analog(1.W)
  })
  setInline("GowinDifferentialTristateBuffer.sv",
    """module GowinDifferentialTristateBuffer(
      |  input wire outputData, input wire tristate, output wire inputData,
      |  inout wire positive, inout wire negative
      |);
      |`ifdef SYNTHESIS
      |  ELVDS_IOBUF u_primitive(
      |    .I(outputData), .OEN(tristate), .O(inputData),
      |    .IO(positive), .IOB(negative)
      |  );
      |`else
      |  assign positive = tristate ? 1'bz : outputData;
      |  assign negative = tristate ? 1'bz : ~outputData;
      |  assign inputData = positive;
      |`endif
      |endmodule
      |""".stripMargin)
}

class GowinDataSerdesLane extends RawModule {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val writeClock270 = Input(Clock())
    val readClock90 = Input(Clock())
    val readPointer = Input(UInt(3.W))
    val writePointer = Input(UInt(3.W))
    val parallelOut = Input(UInt(4.W))
    val tristateData = Input(UInt(2.W))
    val parallelIn = Output(UInt(4.W))
    val pad = Analog(1.W)
  })
  private val output = Module(new GowinDataOutputSerdes)
  private val buffer = Module(new GowinSingleEndedTristateBuffer)
  private val input = Module(new GowinDataInputSerdes)
  output.io.reset := io.reset
  output.io.systemClock := io.systemClock
  output.io.edgeClock := io.edgeClock
  output.io.writeClock270 := io.writeClock270
  output.io.data := io.parallelOut
  output.io.tristateData := io.tristateData
  buffer.io.outputData := output.io.serial
  buffer.io.tristate := output.io.tristate
  attach(buffer.io.pad, io.pad)
  input.io.reset := io.reset
  input.io.systemClock := io.systemClock
  input.io.edgeClock := io.edgeClock
  input.io.readClock90 := io.readClock90
  input.io.readPointer := io.readPointer
  input.io.writePointer := io.writePointer
  input.io.serial := buffer.io.inputData
  io.parallelIn := input.io.data
}
