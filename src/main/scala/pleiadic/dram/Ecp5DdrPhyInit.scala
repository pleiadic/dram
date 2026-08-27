package pleiadic.dram

import chisel3._
import chisel3.util.{HasBlackBoxInline, is, switch}
import scala.language.reflectiveCalls

/** Lattice ECP5 DDRDLLA wrapper used by the DDR PHY initialization block. */
class Ecp5DdrDll extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val clock = Input(Clock())
    val update = Input(Bool())
    val freeze = Input(Bool())
    val delay = Output(Bool())
    val lock = Output(Bool())
  })

  setInline("Ecp5DdrDll.sv",
    """module Ecp5DdrDll(
      |  input wire reset, input wire clock, input wire update,
      |  input wire freeze, output wire delay, output wire lock
      |);
      |`ifdef SYNTHESIS
      |  DDRDLLA u_primitive(
      |    .RST(reset), .CLK(clock), .UDDCNTLN(~update), .FREEZE(freeze),
      |    .DDRDEL(delay), .LOCK(lock)
      |  );
      |`else
      |  assign delay = 1'b0;
      |  assign lock = ~reset;
      |`endif
      |endmodule
      |""".stripMargin)
}

/**
  * Cycle-exact ECP5 DDRDLLA/DDQBUFM/ECLK initialization timeline. A rising
  * synchronized DLL lock starts ten events spaced eight init-clock cycles
  * apart, matching LiteDRAM's `timeline(new_lock, ...)` sequence.
  */
class Ecp5DdrPhyInitSequencer extends Module {
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

/** DDRDLLA plus its init-domain sequence, ready for an ECP5 clocking wrapper. */
class Ecp5DdrPhyInit extends Module {
  val io = IO(new Bundle {
    val sys2xClock = Input(Clock())
    val dllReset = Input(Bool())
    val pause = Output(Bool())
    val stop = Output(Bool())
    val delay = Output(Bool())
    val resetDomain = Output(Bool())
    val busy = Output(Bool())
  })

  private val dll = Module(new Ecp5DdrDll)
  private val sequencer = Module(new Ecp5DdrPhyInitSequencer)
  dll.io.reset := io.dllReset
  dll.io.clock := io.sys2xClock
  dll.io.update := sequencer.io.update
  dll.io.freeze := sequencer.io.freeze
  sequencer.io.dllLock := dll.io.lock
  io.pause := sequencer.io.pause
  io.stop := sequencer.io.stop
  io.delay := dll.io.delay
  io.resetDomain := sequencer.io.resetDomain
  io.busy := sequencer.io.busy
}
