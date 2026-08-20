package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

object LpddrSpecialCommand {
  val mpc = 0
  val modeRegisterRead = 1
  val nop = 2
}

object Lpddr5WckSync {
  val none = 0.U(2.W)
  val write = 1.U(2.W)
  val read = 2.U(2.W)
  val fast = 3.U(2.W)
}

/** LiteDRAM-compatible translation of one DFI phase to four LPDDR4 SDR CA slots. */
class Lpddr4DfiPhaseAdapter(config: DramConfig, maskedWrite: Boolean = true) extends Module {
  require(config.nranks == 1, "LPDDR4 command adapter currently supports one rank")
  require(config.bankBits >= 6, "LPDDR4 mode-register addresses require six DFI bank bits")
  require(config.rowBits.max(config.columnBits).max(11) >= 17,
    "LPDDR4 ACTIVATE requires DFI address bit 16")

  val io = IO(new Bundle {
    val phase = Input(new DfiPhase(config))
    val cs = Output(UInt(4.W))
    val ca = Output(Vec(4, UInt(6.W)))
    val valid = Output(Bool())
  })

  private val Seq(des, mrw1, mrw2, mrr1, refresh, activate1, activate2,
    write1, maskedWrite1, read1, cas2, precharge, mpc) = Enum(13)

  private def pack(bits: Bool*): UInt = VecInit(bits).asUInt
  private val a = io.phase.address
  private val b = io.phase.bank

  private def command(command: UInt): (Bool, UInt, UInt) = {
    val entries = Seq(
      mrw1 -> Seq(pack(false.B, true.B, true.B, false.B, false.B, a(7)),
        pack(b(0), b(1), b(2), b(3), b(4), b(5))),
      mrw2 -> Seq(pack(false.B, true.B, true.B, false.B, true.B, a(6)),
        pack(a(0), a(1), a(2), a(3), a(4), a(5))),
      mrr1 -> Seq(pack(false.B, true.B, true.B, true.B, false.B, false.B),
        pack(a(0), a(1), a(2), a(3), a(4), a(5))),
      refresh -> Seq(pack(false.B, false.B, false.B, true.B, false.B, a(10)),
        pack(b(0), b(1), b(2), false.B, false.B, false.B)),
      activate1 -> Seq(pack(true.B, false.B, a(12), a(13), a(14), a(15)),
        pack(b(0), b(1), b(2), a(16), a(10), a(11))),
      activate2 -> Seq(pack(true.B, true.B, a(6), a(7), a(8), a(9)),
        pack(a(0), a(1), a(2), a(3), a(4), a(5))),
      write1 -> Seq(pack(false.B, false.B, true.B, false.B, false.B, false.B),
        pack(b(0), b(1), b(2), false.B, a(9), a(10))),
      maskedWrite1 -> Seq(pack(false.B, false.B, true.B, true.B, false.B, false.B),
        pack(b(0), b(1), b(2), false.B, a(9), a(10))),
      read1 -> Seq(pack(false.B, true.B, false.B, false.B, false.B, false.B),
        pack(b(0), b(1), b(2), false.B, a(9), a(10))),
      cas2 -> Seq(pack(false.B, true.B, false.B, false.B, true.B, a(8)),
        pack(a(2), a(3), a(4), a(5), a(6), a(7))),
      precharge -> Seq(pack(false.B, false.B, false.B, false.B, true.B, a(10)),
        pack(b(0), b(1), b(2), false.B, false.B, false.B)),
      mpc -> Seq(pack(false.B, false.B, false.B, false.B, false.B, a(6)),
        pack(a(0), a(1), a(2), a(3), a(4), a(5)))
    )
    val edge0 = MuxLookup(command, 0.U(6.W))(entries.map { case (id, edges) => id -> edges(0) })
    val edge1 = MuxLookup(command, 0.U(6.W))(entries.map { case (id, edges) => id -> edges(1) })
    (command =/= des, edge0, edge1)
  }

  val command1 = WireDefault(des)
  val command2 = WireDefault(des)
  val valid = WireDefault(false.B)
  val selected = !io.phase.csN.asUInt.andR
  when(selected) {
    when(!io.phase.rasN && io.phase.casN && io.phase.weN) {
      command1 := activate1; command2 := activate2; valid := true.B
    }.elsewhen(io.phase.rasN && !io.phase.casN && io.phase.weN) {
      command1 := read1; command2 := cas2; valid := true.B
    }.elsewhen(io.phase.rasN && !io.phase.casN && !io.phase.weN) {
      command1 := (if (maskedWrite) maskedWrite1 else write1)
      command2 := cas2; valid := true.B
    }.elsewhen(!io.phase.rasN && io.phase.casN && !io.phase.weN) {
      command1 := des; command2 := precharge; valid := true.B
    }.elsewhen(!io.phase.rasN && !io.phase.casN && io.phase.weN) {
      command1 := des; command2 := refresh; valid := true.B
    }.elsewhen(io.phase.rasN && io.phase.casN && !io.phase.weN) {
      when(io.phase.bank === LpddrSpecialCommand.mpc.U) {
        command1 := des; command2 := mpc; valid := true.B
      }.elsewhen(io.phase.bank === LpddrSpecialCommand.modeRegisterRead.U) {
        command1 := mrr1; command2 := cas2; valid := true.B
      }
    }.elsewhen(!io.phase.rasN && !io.phase.casN && !io.phase.weN) {
      command1 := mrw1; command2 := mrw2; valid := true.B
    }
  }

  private val first = command(command1)
  private val second = command(command2)
  io.cs := Cat(false.B, second._1, false.B, first._1)
  io.ca(0) := first._2
  io.ca(1) := first._3
  io.ca(2) := second._2
  io.ca(3) := second._3
  io.valid := valid
}

/** LiteDRAM-compatible translation of one DFI phase to four LPDDR5 DDR CA edges. */
class Lpddr5DfiPhaseAdapter(config: DramConfig, maskedWrite: Boolean = true) extends Module {
  require(config.nranks == 1, "LPDDR5 command adapter currently supports one rank")
  require(config.bankBits >= 7, "LPDDR5 mode-register addresses require seven DFI bank bits")
  require(config.rowBits.max(config.columnBits).max(11) >= 18,
    "LPDDR5 ACTIVATE requires DFI address bit 17")

  val io = IO(new Bundle {
    val phase = Input(new DfiPhase(config))
    val wckSyncDone = Input(Bool())
    val cs = Output(UInt(2.W))
    val ca = Output(Vec(4, UInt(7.W)))
    val valid = Output(Bool())
    val wckSync = Output(UInt(2.W))
  })

  private val Seq(des, nop, activate1, activate2, precharge, refresh, maskedWriteCmd,
    write16, read16, cas, mpc, mrw1, mrw2, mrr) = Enum(14)

  private def pack(bits: Bool*): UInt = VecInit(bits).asUInt
  private val a = io.phase.address
  private val b = io.phase.bank
  private val mpcOperand = Mux(a === 0.U, "h86".U(8.W), a(7, 0))

  private def command(command: UInt, sync: UInt): (Bool, UInt, UInt) = {
    val entries = Seq(
      nop -> Seq(pack(false.B, false.B, false.B, false.B, false.B, false.B, false.B), 0.U(7.W)),
      activate1 -> Seq(pack(true.B, true.B, true.B, a(14), a(15), a(16), a(17)),
        pack(b(0), b(1), b(2), b(3), a(11), a(12), a(13))),
      activate2 -> Seq(pack(true.B, true.B, false.B, a(7), a(8), a(9), a(10)),
        pack(a(0), a(1), a(2), a(3), a(4), a(5), a(6))),
      precharge -> Seq(pack(false.B, false.B, false.B, true.B, true.B, true.B, true.B),
        pack(b(0), b(1), b(2), b(3), false.B, false.B, a(10))),
      refresh -> Seq(pack(false.B, false.B, false.B, true.B, true.B, true.B, false.B),
        pack(b(0), b(1), b(2), false.B, false.B, false.B, a(10))),
      maskedWriteCmd -> Seq(pack(false.B, true.B, false.B, a(4), a(7), a(8), a(9)),
        pack(b(0), b(1), b(2), b(3), a(5), a(6), a(10))),
      write16 -> Seq(pack(false.B, true.B, true.B, a(4), a(7), a(8), a(9)),
        pack(b(0), b(1), b(2), b(3), a(5), a(6), a(10))),
      read16 -> Seq(pack(true.B, false.B, false.B, a(4), a(7), a(8), a(9)),
        pack(b(0), b(1), b(2), b(3), a(5), a(6), a(10))),
      cas -> Seq(pack(false.B, false.B, true.B, true.B,
        sync === Lpddr5WckSync.write, sync === Lpddr5WckSync.read,
        sync === Lpddr5WckSync.fast), 0.U(7.W)),
      mpc -> Seq(pack(false.B, false.B, false.B, false.B, true.B, true.B, mpcOperand(7)),
        pack((0 until 7).map(mpcOperand(_)): _*)),
      mrw1 -> Seq(pack(false.B, false.B, false.B, true.B, true.B, false.B, true.B),
        pack((0 until 7).map(b(_)): _*)),
      mrw2 -> Seq(pack(false.B, false.B, false.B, true.B, false.B, false.B, a(7)),
        pack((0 until 7).map(a(_)): _*)),
      mrr -> Seq(pack(false.B, false.B, false.B, true.B, true.B, false.B, false.B),
        pack((0 until 7).map(a(_)): _*))
    )
    val edge0 = MuxLookup(command, 0.U(7.W))(entries.map { case (id, edges) => id -> edges(0) })
    val edge1 = MuxLookup(command, 0.U(7.W))(entries.map { case (id, edges) => id -> edges(1) })
    (command =/= des, edge0, edge1)
  }

  val command1 = WireDefault(des)
  val command2 = WireDefault(des)
  val valid = WireDefault(false.B)
  val sync = WireDefault(Lpddr5WckSync.none)
  val selected = !io.phase.csN.asUInt.andR
  when(selected) {
    when(!io.phase.rasN && io.phase.casN && io.phase.weN) {
      command1 := activate1; command2 := activate2; valid := true.B
    }.elsewhen(io.phase.rasN && !io.phase.casN && io.phase.weN) {
      command1 := cas; command2 := read16; valid := true.B
      when(!io.wckSyncDone) { sync := Lpddr5WckSync.read }
    }.elsewhen(io.phase.rasN && !io.phase.casN && !io.phase.weN) {
      command1 := cas; command2 := (if (maskedWrite) maskedWriteCmd else write16); valid := true.B
      when(!io.wckSyncDone) { sync := Lpddr5WckSync.write }
    }.elsewhen(!io.phase.rasN && io.phase.casN && !io.phase.weN) {
      command1 := des; command2 := precharge; valid := true.B
    }.elsewhen(!io.phase.rasN && !io.phase.casN && io.phase.weN) {
      command1 := des; command2 := refresh; valid := true.B
    }.elsewhen(io.phase.rasN && io.phase.casN && !io.phase.weN) {
      when(io.phase.bank === LpddrSpecialCommand.mpc.U) {
        command1 := des; command2 := mpc; valid := true.B
      }.elsewhen(io.phase.bank === LpddrSpecialCommand.modeRegisterRead.U) {
        command1 := cas; command2 := mrr; valid := true.B
        when(!io.wckSyncDone) { sync := Lpddr5WckSync.read }
      }.elsewhen(io.phase.bank === LpddrSpecialCommand.nop.U) {
        command1 := des; command2 := nop; valid := true.B
      }
    }.elsewhen(!io.phase.rasN && !io.phase.casN && !io.phase.weN) {
      command1 := mrw1; command2 := mrw2; valid := true.B
    }
  }

  private val first = command(command1, sync)
  private val second = command(command2, sync)
  io.cs := Cat(second._1, first._1)
  io.ca(0) := first._2
  io.ca(1) := first._3
  io.ca(2) := second._2
  io.ca(3) := second._3
  io.valid := valid
  io.wckSync := sync
}

/** LiteDRAM-compatible DFI to RPC 16-bit parallel Request Packet encoder. */
class RpcDfiAdapter(config: DramConfig) extends Module {
  require(config.bankBits >= 2)
  require(config.rowBits.max(config.columnBits).max(11) >= 12)

  val io = IO(new Bundle {
    val phase = Input(new DfiPhase(config))
    val burstCount = Input(UInt(6.W))
    val refreshOperation = Input(UInt(2.W))
    val dbPositive = Output(UInt(16.W))
    val dbNegative = Output(UInt(16.W))
    val commandValid = Output(Bool())
    val utilityEnable = Output(Bool())
  })

  private val phase = io.phase
  private val special = !phase.resetN
  private val activate = !phase.rasN && phase.casN && phase.weN
  private val read = phase.rasN && !phase.casN && phase.weN
  private val write = phase.rasN && !phase.casN && !phase.weN
  private val precharge = !phase.rasN && phase.casN && !phase.weN
  private val refresh = !phase.rasN && !phase.casN && phase.weN
  private val zqc = phase.rasN && phase.casN && !phase.weN
  private val modeRegister = !phase.rasN && !phase.casN && !phase.weN
  private val resetCommand = special && activate
  private val utility = special && modeRegister
  private val allBanks = phase.address(10)
  private val bankMask = Mux(allBanks, "hf".U(4.W), UIntToOH(phase.bank(1, 0), 4))
  private val zqcOperation = Mux(special,
    Mux(allBanks, 0.U(2.W), 3.U(2.W)),
    Mux(allBanks, 1.U(2.W), 2.U(2.W)))

  val positive = WireDefault(0.U(16.W))
  val negative = WireDefault(0.U(16.W))
  val valid = WireDefault(false.B)

  when(resetCommand) {
    negative := 1.U
    valid := true.B
  }.elsewhen(utility) {
    positive := Cat(0.U(10.W), phase.address(2, 1), phase.address(0), "b111".U(3.W))
    valid := true.B
  }.elsewhen(activate) {
    positive := Cat(0.U(11.W), phase.bank(1, 0), "b101".U(3.W))
    negative := Cat(0.U(3.W), phase.address(11, 0), false.B)
    valid := true.B
  }.elsewhen(read || write) {
    positive := Cat(phase.address(6, 4), 0.U(2.W), io.burstCount,
      phase.bank(1, 0), Mux(write, "b001".U(3.W), 0.U(3.W)))
    negative := Cat(phase.address(9, 7), 0.U(12.W), false.B)
    valid := true.B
  }.elsewhen(precharge) {
    positive := Cat(0.U(6.W), bankMask, 0.U(3.W), "b100".U(3.W))
    valid := true.B
  }.elsewhen(refresh) {
    positive := Cat(0.U(6.W), bankMask, 0.U(3.W), "b110".U(3.W))
    negative := Cat(0.U(13.W), io.refreshOperation, false.B)
    valid := true.B
  }.elsewhen(zqc) {
    positive := Cat(zqcOperation, 0.U(11.W), "b001".U(3.W))
    negative := 1.U
    valid := true.B
  }.elsewhen(modeRegister) {
    val register = Cat(phase.address(9, 7), phase.address(6, 3),
      0.U(3.W), phase.address(2, 0))
    positive := Cat(register, "b010".U(3.W))
    negative := Cat(false.B, phase.bank(1), phase.address(10), phase.bank(0), 0.U(12.W))
    valid := true.B
  }

  io.dbPositive := positive
  io.dbNegative := negative
  io.commandValid := valid
  io.utilityEnable := phase.address(0)
}
