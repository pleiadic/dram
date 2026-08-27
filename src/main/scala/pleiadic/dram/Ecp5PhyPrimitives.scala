package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, IntParam, attach}
import chisel3.util.HasBlackBoxInline
import scala.language.reflectiveCalls

class Ecp5OutputDdrX2 extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val data = Input(UInt(4.W))
    val serial = Output(Bool())
  })
  setInline("Ecp5OutputDdrX2.sv",
    """module Ecp5OutputDdrX2(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire [3:0] data, output wire serial
      |);
      |`ifdef SYNTHESIS
      |  ODDRX2F u_primitive(
      |    .RST(reset), .SCLK(systemClock), .ECLK(edgeClock),
      |    .D0(data[0]), .D1(data[1]), .D2(data[2]), .D3(data[3]), .Q(serial)
      |  );
      |`else
      |  assign serial = data[0];
      |`endif
      |endmodule
      |""".stripMargin)
}

class Ecp5CommandDelay(initialValue: Int = 0) extends BlackBox(Map(
    "INITIAL_VALUE" -> IntParam(initialValue))) with HasBlackBoxInline {
  require(initialValue >= 0 && initialValue < 128)
  val io = IO(new Bundle {
    val dataIn = Input(Bool())
    val dataOut = Output(Bool())
  })
  setInline("Ecp5CommandDelay.sv",
    """module Ecp5CommandDelay #(
      |  parameter integer INITIAL_VALUE = 0
      |) (input wire dataIn, output wire dataOut);
      |`ifdef SYNTHESIS
      |  DELAYG #(.DEL_VALUE(INITIAL_VALUE)) u_primitive(.A(dataIn), .Z(dataOut));
      |`else
      |  assign dataOut = dataIn;
      |`endif
      |endmodule
      |""".stripMargin)
}

/** DQSBUFM clock/pointer generation and burst observation. */
class Ecp5DqsBuffer extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val dllDelay = Input(Bool())
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
  setInline("Ecp5DqsBuffer.sv",
    """module Ecp5DqsBuffer(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire dllDelay, input wire pause, input wire readEnable,
      |  input wire [2:0] readDelay, input wire dqsInput,
      |  output wire readClock90, output wire writeClock270,
      |  output wire writeClock, output wire [2:0] readPointer,
      |  output wire [2:0] writePointer, output wire dataValid,
      |  output wire burstDetected
      |);
      |`ifdef SYNTHESIS
      |  DQSBUFM #(
      |    .DQS_LI_DEL_ADJ("MINUS"), .DQS_LI_DEL_VAL(1),
      |    .DQS_LO_DEL_ADJ("MINUS"), .DQS_LO_DEL_VAL(4)
      |  ) u_primitive (
      |    .RST(reset), .SCLK(systemClock), .ECLK(edgeClock),
      |    .DDRDEL(dllDelay), .PAUSE(pause),
      |    .RDLOADN(1'b0), .RDMOVE(1'b0), .RDDIRECTION(1'b1),
      |    .WRLOADN(1'b0), .WRMOVE(1'b0), .WRDIRECTION(1'b1),
      |    .READ0(readEnable), .READ1(readEnable),
      |    .READCLKSEL0(readDelay[0]), .READCLKSEL1(readDelay[1]),
      |    .READCLKSEL2(readDelay[2]), .DQSI(dqsInput),
      |    .DQSR90(readClock90), .DQSW270(writeClock270), .DQSW(writeClock),
      |    .RDPNTR0(readPointer[0]), .RDPNTR1(readPointer[1]),
      |    .RDPNTR2(readPointer[2]), .WRPNTR0(writePointer[0]),
      |    .WRPNTR1(writePointer[1]), .WRPNTR2(writePointer[2]),
      |    .DATAVALID(dataValid), .BURSTDET(burstDetected)
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

class Ecp5DqsOutputSerdes extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val writeClock = Input(Clock())
    val data = Input(UInt(4.W))
    val outputEnable = Input(Bool())
    val preamble = Input(Bool())
    val postamble = Input(Bool())
    val serial = Output(Bool())
    val tristate = Output(Bool())
  })
  setInline("Ecp5DqsOutputSerdes.sv",
    """module Ecp5DqsOutputSerdes(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire writeClock, input wire [3:0] data,
      |  input wire outputEnable, input wire preamble, input wire postamble,
      |  output wire serial, output wire tristate
      |);
      |`ifdef SYNTHESIS
      |  ODDRX2DQSB u_data(
      |    .RST(reset), .SCLK(systemClock), .ECLK(edgeClock), .DQSW(writeClock),
      |    .D0(data[0]), .D1(data[1]), .D2(data[2]), .D3(data[3]), .Q(serial)
      |  );
      |  TSHX2DQSA u_tristate(
      |    .RST(reset), .SCLK(systemClock), .ECLK(edgeClock), .DQSW(writeClock),
      |    .T0(~(outputEnable | postamble)),
      |    .T1(~(outputEnable | preamble)), .Q(tristate)
      |  );
      |`else
      |  assign serial = data[0];
      |  assign tristate = ~(outputEnable | preamble | postamble);
      |`endif
      |endmodule
      |""".stripMargin)
}

class Ecp5DataOutputSerdes extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val writeClock270 = Input(Clock())
    val data = Input(UInt(4.W))
    val serial = Output(Bool())
  })
  setInline("Ecp5DataOutputSerdes.sv",
    """module Ecp5DataOutputSerdes(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire writeClock270, input wire [3:0] data, output wire serial
      |);
      |`ifdef SYNTHESIS
      |  ODDRX2DQA u_primitive(
      |    .RST(reset), .SCLK(systemClock), .ECLK(edgeClock),
      |    .DQSW270(writeClock270), .D0(data[0]), .D1(data[1]),
      |    .D2(data[2]), .D3(data[3]), .Q(serial)
      |  );
      |`else
      |  assign serial = data[0];
      |`endif
      |endmodule
      |""".stripMargin)
}

class Ecp5DataTristateSerdes extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val writeClock270 = Input(Clock())
    val outputEnable = Input(Bool())
    val tristate = Output(Bool())
  })
  setInline("Ecp5DataTristateSerdes.sv",
    """module Ecp5DataTristateSerdes(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire writeClock270, input wire outputEnable,
      |  output wire tristate
      |);
      |`ifdef SYNTHESIS
      |  TSHX2DQA u_primitive(
      |    .RST(reset), .SCLK(systemClock), .ECLK(edgeClock),
      |    .DQSW270(writeClock270), .T0(~outputEnable),
      |    .T1(~outputEnable), .Q(tristate)
      |  );
      |`else
      |  assign tristate = ~outputEnable;
      |`endif
      |endmodule
      |""".stripMargin)
}

class Ecp5DataInputSerdes extends BlackBox with HasBlackBoxInline {
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
  setInline("Ecp5DataInputSerdes.sv",
    """module Ecp5DataInputSerdes(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire readClock90, input wire [2:0] readPointer,
      |  input wire [2:0] writePointer, input wire serial,
      |  output wire [3:0] data
      |);
      |`ifdef SYNTHESIS
      |  wire delayed;
      |  DELAYG #(.DEL_MODE("DQS_ALIGNED_X2")) u_delay(.A(serial), .Z(delayed));
      |  IDDRX2DQA u_input(
      |    .RST(reset), .SCLK(systemClock), .ECLK(edgeClock),
      |    .DQSR90(readClock90), .RDPNTR0(readPointer[0]),
      |    .RDPNTR1(readPointer[1]), .RDPNTR2(readPointer[2]),
      |    .WRPNTR0(writePointer[0]), .WRPNTR1(writePointer[1]),
      |    .WRPNTR2(writePointer[2]), .D(delayed),
      |    .Q0(data[0]), .Q1(data[1]), .Q2(data[2]), .Q3(data[3])
      |  );
      |`else
      |  assign data = {4{serial}};
      |`endif
      |endmodule
      |""".stripMargin)
}

/** Inferred bidirectional pad buffer, matching Migen's Tristate lowering. */
class Ecp5TristateBuffer extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val outputData = Input(Bool())
    val tristate = Input(Bool())
    val inputData = Output(Bool())
    val pad = Analog(1.W)
  })
  setInline("Ecp5TristateBuffer.sv",
    """module Ecp5TristateBuffer(
      |  input wire outputData, input wire tristate,
      |  output wire inputData, inout wire pad
      |);
      |  assign pad = tristate ? 1'bz : outputData;
      |  assign inputData = pad;
      |endmodule
      |""".stripMargin)
}

class Ecp5DataSerdesLane extends RawModule {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val writeClock270 = Input(Clock())
    val readClock90 = Input(Clock())
    val readPointer = Input(UInt(3.W))
    val writePointer = Input(UInt(3.W))
    val parallelOut = Input(UInt(4.W))
    val outputEnable = Input(Bool())
    val parallelIn = Output(UInt(4.W))
    val pad = Analog(1.W)
  })
  private val output = Module(new Ecp5DataOutputSerdes)
  private val tristate = Module(new Ecp5DataTristateSerdes)
  private val buffer = Module(new Ecp5TristateBuffer)
  private val input = Module(new Ecp5DataInputSerdes)
  output.io.reset := io.reset
  output.io.systemClock := io.systemClock
  output.io.edgeClock := io.edgeClock
  output.io.writeClock270 := io.writeClock270
  output.io.data := io.parallelOut
  tristate.io.reset := io.reset
  tristate.io.systemClock := io.systemClock
  tristate.io.edgeClock := io.edgeClock
  tristate.io.writeClock270 := io.writeClock270
  tristate.io.outputEnable := io.outputEnable
  buffer.io.outputData := output.io.serial
  buffer.io.tristate := tristate.io.tristate
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
