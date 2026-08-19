package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/** Counts accepted read/write commands over fixed 2^periodBits windows. */
class LiteDramBandwidth(dataWidth: Int, periodBits: Int = 24) extends Module {
  require(dataWidth >= 1 && periodBits >= 1)
  private val countWidth = periodBits + 1

  val io = IO(new Bundle {
    val commandAccepted = Input(Bool())
    val write = Input(Bool())
    val update = Input(Bool())
    val reads = Output(UInt(countWidth.W))
    val writes = Output(UInt(countWidth.W))
    val reportedDataWidth = Output(UInt(log2Ceil(dataWidth + 1).max(1).W))
  })

  private val periodCounter = RegInit(0.U(periodBits.W))
  private val currentReads = RegInit(0.U(countWidth.W))
  private val currentWrites = RegInit(0.U(countWidth.W))
  private val completedReads = RegInit(0.U(countWidth.W))
  private val completedWrites = RegInit(0.U(countWidth.W))
  private val visibleReads = RegInit(0.U(countWidth.W))
  private val visibleWrites = RegInit(0.U(countWidth.W))
  private val boundary = periodCounter.andR

  periodCounter := periodCounter + 1.U
  when(boundary) {
    completedReads := currentReads
    completedWrites := currentWrites
    currentReads := io.commandAccepted && !io.write
    currentWrites := io.commandAccepted && io.write
  }.elsewhen(io.commandAccepted) {
    when(io.write) { currentWrites := currentWrites + 1.U }
      .otherwise { currentReads := currentReads + 1.U }
  }

  when(io.update) {
    visibleReads := completedReads
    visibleWrites := completedWrites
  }
  io.reads := visibleReads
  io.writes := visibleWrites
  io.reportedDataWidth := dataWidth.U
}
