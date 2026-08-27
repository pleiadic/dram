package pleiadic.dram

import chisel3._
import chisel3.util.{HasBlackBoxInline, is, switch}
import scala.language.reflectiveCalls

sealed trait GowinFamily {
  private[dram] def suffix: String
  private[dram] def dllPrimitive: String
}

object GowinFamily {
  case object GW2A extends GowinFamily {
    private[dram] val suffix = "Gw2a"
    private[dram] val dllPrimitive = "DLL"
  }
  case object GW5A extends GowinFamily {
    private[dram] val suffix = "Gw5a"
    private[dram] val dllPrimitive = "DDRDLL"
  }
}

/** GW2A/GW5A DDR DLL. The two families only differ in the primitive name. */
class GowinDdrDll(family: GowinFamily) extends BlackBox with HasBlackBoxInline {
  override def desiredName: String = s"GowinDdrDll${family.suffix}"
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val clock = Input(Clock())
    val update = Input(Bool())
    val freeze = Input(Bool())
    val delayCode = Output(UInt(8.W))
    val lock = Output(Bool())
  })
  private val moduleName = desiredName
  setInline(s"$moduleName.sv",
    s"""module $moduleName(
       |  input wire reset, input wire clock, input wire update,
       |  input wire freeze, output wire [7:0] delayCode, output wire lock
       |);
       |`ifdef SYNTHESIS
       |  ${family.dllPrimitive} #(.SCAL_EN("false")) u_primitive(
       |    .RESET(reset), .CLKIN(clock), .UPDNCNTL(~update), .STOP(freeze),
       |    .STEP(delayCode), .LOCK(lock)
       |  );
       |`else
       |  assign delayCode = 8'b0;
       |  assign lock = ~reset;
       |`endif
       |endmodule
       |""".stripMargin)
}

/** Ten eight-cycle events copied from LiteDRAM's GW2/GW5 initialization timeline. */
class GowinDdrPhyInitSequencer extends Module {
  val io = IO(new Bundle {
    val dllLock = Input(Bool())
    val pause = Output(Bool())
    val stop = Output(Bool())
    val resetDomain = Output(Bool())
    val freeze = Output(Bool())
    val update = Output(Bool())
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

  when(newLock) {
    running := true.B
    count := 0.U
    pause := false.B
    stop := false.B
    resetDomain := false.B
    freeze := false.B
    update := false.B
  }.elsewhen(running) {
    count := count + 1.U
    switch(count) {
      is(7.U)  { freeze := true.B }
      is(15.U) { stop := true.B }
      is(23.U) { resetDomain := true.B }
      is(31.U) { resetDomain := false.B }
      is(39.U) { stop := false.B }
      is(47.U) { freeze := false.B }
      is(55.U) { pause := true.B }
      is(63.U) { update := true.B }
      is(71.U) { update := false.B }
      is(79.U) {
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
  io.busy := running
}

class GowinDdrPhyInit(family: GowinFamily) extends Module {
  val io = IO(new Bundle {
    val sys2xClock = Input(Clock())
    val dllReset = Input(Bool())
    val pause = Output(Bool())
    val stop = Output(Bool())
    val resetDomain = Output(Bool())
    val delayCode = Output(UInt(8.W))
    val busy = Output(Bool())
  })
  private val dll = Module(new GowinDdrDll(family))
  private val sequencer = Module(new GowinDdrPhyInitSequencer)
  dll.io.reset := io.dllReset
  dll.io.clock := io.sys2xClock
  dll.io.update := sequencer.io.update
  dll.io.freeze := sequencer.io.freeze
  sequencer.io.dllLock := dll.io.lock
  io.pause := sequencer.io.pause
  io.stop := sequencer.io.stop
  io.resetDomain := sequencer.io.resetDomain
  io.delayCode := dll.io.delayCode
  io.busy := sequencer.io.busy
}
