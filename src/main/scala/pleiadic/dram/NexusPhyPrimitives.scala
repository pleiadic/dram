package pleiadic.dram

import chisel3._
import chisel3.experimental.{Analog, StringParam, attach}
import chisel3.util.HasBlackBoxInline
import scala.language.reflectiveCalls

class NexusOutputDdrX2 extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val data = Input(UInt(4.W))
    val serial = Output(Bool())
  })
  setInline("NexusOutputDdrX2.sv",
    """module NexusOutputDdrX2(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire [3:0] data, output wire serial
      |);
      |`ifdef SYNTHESIS
      |  ODDRX2 u_primitive(
      |    .RST(reset), .SCLK(systemClock), .ECLK(edgeClock),
      |    .D0(data[0]), .D1(data[1]), .D2(data[2]), .D3(data[3]), .Q(serial)
      |  );
      |`else
      |  assign serial = data[0];
      |`endif
      |endmodule
      |""".stripMargin)
}

class NexusCommandDelay(initialValue: Int = 0) extends BlackBox(Map(
    "INITIAL_VALUE" -> StringParam(initialValue.toString))) with HasBlackBoxInline {
  require(initialValue >= 0 && initialValue < 128)
  val io = IO(new Bundle {
    val dataIn = Input(Bool())
    val dataOut = Output(Bool())
  })
  setInline("NexusCommandDelay.sv",
    """module NexusCommandDelay #(
      |  parameter INITIAL_VALUE = "0"
      |) (input wire dataIn, output wire dataOut);
      |`ifdef SYNTHESIS
      |  DELAYB #(.DEL_VALUE(INITIAL_VALUE)) u_primitive(.A(dataIn), .Z(dataOut));
      |`else
      |  assign dataOut = dataIn;
      |`endif
      |endmodule
      |""".stripMargin)
}

class NexusDqsBuffer extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val dllCode = Input(UInt(9.W))
    val pause = Input(Bool())
    val loadN = Input(Bool())
    val move = Input(Bool())
    val readEnable = Input(Bool())
    val readDelay = Input(UInt(4.W))
    val dqsInput = Input(Bool())
    val readClock90 = Output(Clock())
    val writeClock270 = Output(Clock())
    val writeClock = Output(Clock())
    val readPointer = Output(UInt(3.W))
    val writePointer = Output(UInt(3.W))
    val dataValid = Output(Bool())
    val burstDetected = Output(Bool())
  })
  setInline("NexusDqsBuffer.sv",
    """module NexusDqsBuffer(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire [8:0] dllCode, input wire pause, input wire loadN,
      |  input wire move, input wire readEnable, input wire [3:0] readDelay,
      |  input wire dqsInput, output wire readClock90,
      |  output wire writeClock270, output wire writeClock,
      |  output wire [2:0] readPointer, output wire [2:0] writePointer,
      |  output wire dataValid, output wire burstDetected
      |);
      |`ifdef SYNTHESIS
      |  DQSBUF #(
      |    .SIGN_READ("POSITIVE"), .S_READ("0"),
      |    .SIGN_WRITE("POSITIVE"), .S_WRITE("0"),
      |    .ENABLE_FIFO("ENABLED"), .FORCE_READ("ENABLED"),
      |    .FREE_WHEEL("DDR"), .MT_EN_READ("ENABLED"),
      |    .MT_EN_WRITE("ENABLED"), .MT_EN_WRITE_LEVELING("ENABLED"),
      |    .READ_ENABLE("ENABLED"), .RX_CENTERED("ENABLED"),
      |    .MODX("MDDRX2"), .UPDATE_QU("UP1_AND_UP0_SAME"),
      |    .WRITE_ENABLE("ENABLED")
      |  ) u_primitive (
      |    .RST(reset), .RSTSMCNT(reset), .SCLK(systemClock),
      |    .ECLKIN(edgeClock), .SELCLK(1'b0), .DLLCODE(dllCode), .PAUSE(pause),
      |    .RDLOADN(loadN), .READMOVE(move), .RDDIR(1'b0),
      |    .WRLOAD_N(loadN), .WRMOVE(move), .WRDIR(1'b0),
      |    .WRLVLOAD_N(loadN), .WRLVMOVE(move), .WRLVDIR(1'b0),
      |    .READ({4{readEnable}}), .RDCLKSEL(readDelay), .DQSI(dqsInput),
      |    .DQSR90(readClock90), .DQSW270(writeClock270), .DQSW(writeClock),
      |    .RDPNTR(readPointer), .WRPNTR(writePointer),
      |    .DATAVALID(dataValid), .BURSTDETECT(burstDetected)
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

class NexusDqsOutputSerdes extends BlackBox with HasBlackBoxInline {
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
  setInline("NexusDqsOutputSerdes.sv",
    """module NexusDqsOutputSerdes(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire writeClock, input wire [3:0] data,
      |  input wire [1:0] tristateData, output wire serial,
      |  output wire tristate
      |);
      |`ifdef SYNTHESIS
      |  ODDRX2DQS u_data(
      |    .RST(reset), .SCLK(systemClock), .ECLK(edgeClock), .DQSW(writeClock),
      |    .D0(data[0]), .D1(data[1]), .D2(data[2]), .D3(data[3]), .Q(serial)
      |  );
      |  TSHX2DQS u_tristate(
      |    .RST(reset), .SCLK(systemClock), .ECLK(edgeClock), .DQSW(writeClock),
      |    .T0(tristateData[0]), .T1(tristateData[1]), .Q(tristate)
      |  );
      |`else
      |  assign serial = data[0];
      |  assign tristate = tristateData[0];
      |`endif
      |endmodule
      |""".stripMargin)
}

class NexusDataOutputSerdes extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val writeClock270 = Input(Clock())
    val data = Input(UInt(4.W))
    val serial = Output(Bool())
  })
  setInline("NexusDataOutputSerdes.sv",
    """module NexusDataOutputSerdes(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire writeClock270, input wire [3:0] data, output wire serial
      |);
      |`ifdef SYNTHESIS
      |  ODDRX2DQ u_primitive(
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

class NexusDataTristateSerdes extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val systemClock = Input(Clock())
    val edgeClock = Input(Clock())
    val writeClock270 = Input(Clock())
    val data = Input(UInt(2.W))
    val tristate = Output(Bool())
  })
  setInline("NexusDataTristateSerdes.sv",
    """module NexusDataTristateSerdes(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire writeClock270, input wire [1:0] data,
      |  output wire tristate
      |);
      |`ifdef SYNTHESIS
      |  TSHX2DQ u_primitive(
      |    .RST(reset), .SCLK(systemClock), .ECLK(edgeClock),
      |    .DQSW270(writeClock270), .T0(data[0]), .T1(data[1]), .Q(tristate)
      |  );
      |`else
      |  assign tristate = data[0];
      |`endif
      |endmodule
      |""".stripMargin)
}

class NexusDataInputSerdes extends BlackBox with HasBlackBoxInline {
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
  setInline("NexusDataInputSerdes.sv",
    """module NexusDataInputSerdes(
      |  input wire reset, input wire systemClock, input wire edgeClock,
      |  input wire readClock90, input wire [2:0] readPointer,
      |  input wire [2:0] writePointer, input wire serial,
      |  output wire [3:0] data
      |);
      |`ifdef SYNTHESIS
      |  wire delayed;
      |  DELAYB #(.DEL_VALUE("21"), .COARSE_DELAY("0NS"))
      |    u_delay(.A(serial), .Z(delayed));
      |  IDDRX2DQ u_input(
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

class NexusDataSerdesLane extends RawModule {
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
  private val output = Module(new NexusDataOutputSerdes)
  private val tristate = Module(new NexusDataTristateSerdes)
  private val buffer = Module(new Ecp5TristateBuffer)
  private val input = Module(new NexusDataInputSerdes)
  output.io.reset := io.reset
  output.io.systemClock := io.systemClock
  output.io.edgeClock := io.edgeClock
  output.io.writeClock270 := io.writeClock270
  output.io.data := io.parallelOut
  tristate.io.reset := io.reset
  tristate.io.systemClock := io.systemClock
  tristate.io.edgeClock := io.edgeClock
  tristate.io.writeClock270 := io.writeClock270
  tristate.io.data := io.tristateData
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
