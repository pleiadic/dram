package pleiadic.dram

import chisel3._
import chisel3.util.{Cat, HasBlackBoxInline, is, switch}
import scala.language.reflectiveCalls

class NexusDdrDll extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val clock = Input(Clock())
    val update = Input(Bool())
    val freeze = Input(Bool())
    val delayCode = Output(UInt(9.W))
    val lock = Output(Bool())
  })
  setInline("NexusDdrDll.sv",
    """module NexusDdrDll(
      |  input wire reset, input wire clock, input wire update,
      |  input wire freeze, output wire [8:0] delayCode, output wire lock
      |);
      |`ifdef SYNTHESIS
      |  DDRDLL u_primitive(
      |    .RST(reset), .CLKIN(clock), .UDDCNTL_N(~update), .FREEZE(freeze),
      |    .CODE(delayCode), .LOCK(lock)
      |  );
      |`else
      |  assign delayCode = 9'b0;
      |  assign lock = ~reset;
      |`endif
      |endmodule
      |""".stripMargin)
}

class NexusDdrPhyInitSequencer extends Module {
  val io = IO(new Bundle {
    val dllLock = Input(Bool())
    val pause = Output(Bool())
    val stop = Output(Bool())
    val resetDomain = Output(Bool())
    val freeze = Output(Bool())
    val update = Output(Bool())
    val loadN = Output(Bool())
    val move = Output(Bool())
    val busy = Output(Bool())
  })

  private val lockMeta = RegNext(io.dllLock, false.B)
  private val lockSync = RegNext(lockMeta, false.B)
  private val lockPrevious = RegNext(lockSync, false.B)
  private val newLock = lockSync && !lockPrevious
  private val running = RegInit(false.B)
  private val count = RegInit(0.U(7.W))
  private val pause = RegInit(false.B)
  private val stop = RegInit(false.B)
  private val resetDomain = RegInit(false.B)
  private val freeze = RegInit(false.B)
  private val update = RegInit(false.B)
  private val loadN = RegInit(true.B)
  private val move = RegInit(false.B)

  when(newLock) {
    running := true.B
    count := 0.U
    pause := false.B
    stop := false.B
    resetDomain := false.B
    freeze := false.B
    update := false.B
    loadN := true.B
    move := false.B
  }.elsewhen(running) {
    count := count + 1.U
    switch(count) {
      is(7.U)   { freeze := true.B }
      is(15.U)  { stop := true.B }
      is(23.U)  { resetDomain := true.B }
      is(31.U)  { resetDomain := false.B }
      is(39.U)  { stop := false.B }
      is(47.U)  { freeze := false.B }
      is(55.U)  { pause := true.B }
      is(63.U)  { update := true.B }
      is(71.U)  { update := false.B }
      is(79.U)  { loadN := false.B }
      is(87.U)  { move := true.B }
      is(95.U)  { move := false.B }
      is(103.U) { loadN := true.B }
      is(111.U) {
        pause := false.B
        running := false.B
      }
    }
  }

  io.pause := pause
  io.stop := stop
  io.resetDomain := resetDomain
  io.freeze := freeze
  io.update := update
  io.loadN := loadN
  io.move := move
  io.busy := running
}

class NexusDdrPhyInit extends Module {
  val io = IO(new Bundle {
    val sys2xClock = Input(Clock())
    val dllReset = Input(Bool())
    val pause = Output(Bool())
    val stop = Output(Bool())
    val resetDomain = Output(Bool())
    val delayCode = Output(UInt(9.W))
    val loadN = Output(Bool())
    val move = Output(Bool())
    val busy = Output(Bool())
  })
  private val dll = Module(new NexusDdrDll)
  private val sequencer = Module(new NexusDdrPhyInitSequencer)
  dll.io.reset := io.dllReset
  dll.io.clock := io.sys2xClock
  dll.io.update := sequencer.io.update
  dll.io.freeze := sequencer.io.freeze
  sequencer.io.dllLock := dll.io.lock
  io.pause := sequencer.io.pause
  io.stop := sequencer.io.stop
  io.resetDomain := sequencer.io.resetDomain
  io.delayCode := dll.io.delayCode
  io.loadN := sequencer.io.loadN
  io.move := sequencer.io.move
  io.busy := sequencer.io.busy
}

/** Fixed half-word slip used on every Nexus write and tristate stream. */
class NexusWriteBitSlip(width: Int) extends RawModule {
  require(width >= 2 && width % 2 == 0)
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
  io.output := (Cat(io.input, previous) >> (width / 2))(width - 1, 0)
}
