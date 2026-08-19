package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

/** Master-facing command subset of a LiteDRAM native port. */
class NativeCommandSlavePort(config: DramConfig) extends Bundle {
  val command = Flipped(Decoupled(new NativeCommand(config)))
  val lock = Output(Bool())
}

/**
  * Routes native-port commands to rank/bank request streams.
  *
  * Each bank arbitrates independently. Once a bank machine asserts lock, its
  * owner cannot issue to another bank until that bank drains. A held grant is
  * latched whenever downstream applies backpressure, satisfying Decoupled's
  * stable-bits contract even if another master becomes valid meanwhile.
  */
class LiteDramCommandCrossbar(config: DramConfig, masterCount: Int) extends Module {
  require(masterCount >= 1)

  private val masterWidth = log2Ceil(masterCount max 2)
  private val globalBankWidth = log2Ceil(config.rankBankCount max 2)

  val io = IO(new Bundle {
    val masters = Vec(masterCount, new NativeCommandSlavePort(config))
    val bankRequests = Vec(config.rankBankCount, Decoupled(new BankRequest(config)))
    val bankLocks = Input(Vec(config.rankBankCount, Bool()))
    val bankOwners = Output(Vec(config.rankBankCount, UInt(masterWidth.W)))
    val bankOwnerValid = Output(Vec(config.rankBankCount, Bool()))
  })

  private val mappers = Seq.fill(masterCount)(Module(new AddressMapper(config)))
  private val mappedBanks = Wire(Vec(masterCount, UInt(globalBankWidth.W)))
  private val mappedRows = Wire(Vec(masterCount, UInt(config.rowBits.W)))
  private val mappedColumns = Wire(Vec(masterCount, UInt(config.columnBits.W)))
  for (master <- 0 until masterCount) {
    mappers(master).io.address := Cat(io.masters(master).command.bits.address,
      0.U(config.byteOffsetBits.W))
    mappedBanks(master) := mappers(master).io.mapped.bank +
      mappers(master).io.mapped.rank * config.bankCount.U
    mappedRows(master) := mappers(master).io.mapped.row
    mappedColumns(master) := mappers(master).io.mapped.column
    io.masters(master).command.ready := false.B
  }

  private val owners = RegInit(VecInit(Seq.fill(config.rankBankCount)(0.U(masterWidth.W))))
  private val ownerValid = RegInit(VecInit(Seq.fill(config.rankBankCount)(false.B)))
  private val roundRobin = RegInit(VecInit(Seq.fill(config.rankBankCount)(0.U(masterWidth.W))))
  private val held = RegInit(VecInit(Seq.fill(config.rankBankCount)(false.B)))
  private val heldMaster = RegInit(VecInit(Seq.fill(config.rankBankCount)(0.U(masterWidth.W))))

  io.bankOwners := owners
  io.bankOwnerValid := ownerValid

  private val masterLocked = Wire(Vec(masterCount, Bool()))
  for (master <- 0 until masterCount) {
    masterLocked(master) := (0 until config.rankBankCount).map { bank =>
      io.bankLocks(bank) && ownerValid(bank) && owners(bank) === master.U
    }.reduce(_ || _)
    io.masters(master).lock := masterLocked(master)
  }

  for (bank <- 0 until config.rankBankCount) {
    val eligible = Wire(Vec(masterCount, Bool()))
    for (master <- 0 until masterCount) {
      val ownsThisBank = ownerValid(bank) && owners(bank) === master.U
      eligible(master) := io.masters(master).command.valid && mappedBanks(master) === bank.U &&
        (!masterLocked(master) || ownsThisBank)
    }

    val arbitrationChoice = Wire(UInt(masterWidth.W))
    val arbitrationValid = Wire(Bool())
    if (masterCount == 1) {
      arbitrationChoice := 0.U
      arbitrationValid := eligible(0)
    } else {
      val rotated = Wire(Vec(masterCount, Bool()))
      for (offset <- 0 until masterCount) {
        val sum = roundRobin(bank) +& offset.U
        val wrapped = Mux(sum >= masterCount.U, sum - masterCount.U, sum)
        rotated(offset) := eligible(wrapped(masterWidth - 1, 0))
      }
      val offset = PriorityEncoder(rotated)
      val sum = roundRobin(bank) +& offset
      val wrapped = Mux(sum >= masterCount.U, sum - masterCount.U, sum)
      arbitrationChoice := wrapped(masterWidth - 1, 0)
      arbitrationValid := rotated.asUInt.orR
    }

    val selected = Mux(io.bankLocks(bank) && ownerValid(bank), owners(bank),
      Mux(held(bank), heldMaster(bank), arbitrationChoice))
    val selectedValid = if (masterCount == 1) eligible(0) else
      Mux(io.bankLocks(bank) && ownerValid(bank), eligible(owners(bank)),
        Mux(held(bank), eligible(heldMaster(bank)), arbitrationValid))
    val selectedCommand = if (masterCount == 1) io.masters(0).command
      else io.masters(selected).command

    io.bankRequests(bank).valid := selectedValid
    io.bankRequests(bank).bits.write := selectedCommand.bits.write
    if (masterCount == 1) {
      io.bankRequests(bank).bits.row := mappedRows(0)
      io.bankRequests(bank).bits.column := mappedColumns(0)
    } else {
      io.bankRequests(bank).bits.row := mappedRows(selected)
      io.bankRequests(bank).bits.column := mappedColumns(selected)
    }
    for (master <- 0 until masterCount) {
      when(selectedValid && selected === master.U) {
        io.masters(master).command.ready := io.bankRequests(bank).ready
      }
    }

    when(io.bankRequests(bank).valid && !io.bankRequests(bank).ready &&
        !io.bankLocks(bank)) {
      held(bank) := true.B
      heldMaster(bank) := selected
    }
    when(io.bankRequests(bank).fire || !io.bankRequests(bank).valid) { held(bank) := false.B }

    when(!io.bankLocks(bank) && ownerValid(bank)) { ownerValid(bank) := false.B }
    when(io.bankRequests(bank).fire) {
      owners(bank) := selected
      ownerValid(bank) := true.B
      roundRobin(bank) := Mux(selected === (masterCount - 1).U, 0.U, selected + 1.U)
    }
  }
}

class NativeDataSlavePort(config: DramConfig) extends Bundle {
  val writeData = Flipped(Decoupled(new NativeWriteData(config)))
  val readData = Decoupled(new NativeReadData(config))
}

private class DataTag(masterWidth: Int) extends Bundle {
  val master = UInt(masterWidth.W)
  val write = Bool()
}

/** Routes write payloads and ordered read responses using bank completion tags. */
class LiteDramDataCrossbar(config: DramConfig, masterCount: Int) extends Module {
  require(masterCount >= 1)

  private val masterWidth = log2Ceil(masterCount max 2)
  private val tagDepth = (config.cmdBufferDepth * config.rankBankCount) max 2

  val io = IO(new Bundle {
    val masters = Vec(masterCount, new NativeDataSlavePort(config))
    val bankCompletions = Input(Vec(config.rankBankCount, Valid(new BankCompletion)))
    val bankOwners = Input(Vec(config.rankBankCount, UInt(masterWidth.W)))
    val bankOwnerValid = Input(Vec(config.rankBankCount, Bool()))
    val writeData = Decoupled(new NativeWriteData(config))
    val readData = Flipped(Decoupled(new NativeReadData(config)))
  })

  private val writeQueues = Seq.fill(masterCount) {
    Module(new Queue(new NativeWriteData(config), config.cmdBufferDepth))
  }
  for (master <- 0 until masterCount) {
    writeQueues(master).io.enq <> io.masters(master).writeData
  }

  private val completionValid = io.bankCompletions.map(_.valid).reduce(_ || _)
  private val completedBank = PriorityEncoder(io.bankCompletions.map(_.valid))
  private val tags = Module(new Queue(new DataTag(masterWidth), tagDepth))
  tags.io.enq.valid := completionValid
  tags.io.enq.bits.master := io.bankOwners(completedBank)
  tags.io.enq.bits.write := io.bankCompletions(completedBank).bits.write

  when(completionValid) {
    assert(PopCount(io.bankCompletions.map(_.valid)) === 1.U,
      "the controller may complete only one CAS command per cycle")
    assert(io.bankOwnerValid(completedBank), "completed bank has no crossbar owner")
    assert(tags.io.enq.ready, "data tag FIFO overflow")
  }

  private val tagValid = tags.io.deq.valid
  private val tag = tags.io.deq.bits
  private val selectedWriteValid = if (masterCount == 1) writeQueues(0).io.deq.valid
    else VecInit(writeQueues.map(_.io.deq.valid))(tag.master)
  private val selectedWriteBits = if (masterCount == 1) writeQueues(0).io.deq.bits
    else VecInit(writeQueues.map(_.io.deq.bits))(tag.master)

  io.writeData.valid := tagValid && tag.write && selectedWriteValid
  io.writeData.bits := selectedWriteBits
  for (master <- 0 until masterCount) {
    writeQueues(master).io.deq.ready := tagValid && tag.write && tag.master === master.U &&
      io.writeData.ready
  }

  for (master <- 0 until masterCount) {
    io.masters(master).readData.valid := tagValid && !tag.write && tag.master === master.U &&
      io.readData.valid
    io.masters(master).readData.bits := io.readData.bits
  }
  private val selectedReadReady = if (masterCount == 1) io.masters(0).readData.ready
    else VecInit(io.masters.map(_.readData.ready))(tag.master)
  io.readData.ready := tagValid && !tag.write && selectedReadReady

  tags.io.deq.ready := Mux(tag.write, io.writeData.fire, io.readData.fire)
}

/** Complete native-port crossbar: command routing plus ordered data routing. */
class NativeSlavePort(config: DramConfig) extends Bundle {
  val command = Flipped(Decoupled(new NativeCommand(config)))
  val writeData = Flipped(Decoupled(new NativeWriteData(config)))
  val readData = Decoupled(new NativeReadData(config))
  val flush = Input(Bool())
  val lock = Output(Bool())
}

class LiteDramCrossbar(config: DramConfig, masterCount: Int) extends Module {
  require(masterCount >= 1)

  private val masterWidth = log2Ceil(masterCount max 2)
  val io = IO(new Bundle {
    val masters = Vec(masterCount, new NativeSlavePort(config))
    val bankRequests = Vec(config.rankBankCount, Decoupled(new BankRequest(config)))
    val bankCompletions = Input(Vec(config.rankBankCount, Valid(new BankCompletion)))
    val bankLocks = Input(Vec(config.rankBankCount, Bool()))
    val writeData = Decoupled(new NativeWriteData(config))
    val readData = Flipped(Decoupled(new NativeReadData(config)))
  })

  private val commandCrossbar = Module(new LiteDramCommandCrossbar(config, masterCount))
  private val dataCrossbar = Module(new LiteDramDataCrossbar(config, masterCount))

  for (master <- 0 until masterCount) {
    commandCrossbar.io.masters(master).command.valid := io.masters(master).command.valid
    commandCrossbar.io.masters(master).command.bits := io.masters(master).command.bits
    io.masters(master).command.ready := commandCrossbar.io.masters(master).command.ready
    io.masters(master).lock := commandCrossbar.io.masters(master).lock

    dataCrossbar.io.masters(master).writeData.valid := io.masters(master).writeData.valid
    dataCrossbar.io.masters(master).writeData.bits := io.masters(master).writeData.bits
    io.masters(master).writeData.ready := dataCrossbar.io.masters(master).writeData.ready
    io.masters(master).readData.valid := dataCrossbar.io.masters(master).readData.valid
    io.masters(master).readData.bits := dataCrossbar.io.masters(master).readData.bits
    dataCrossbar.io.masters(master).readData.ready := io.masters(master).readData.ready
  }

  commandCrossbar.io.bankLocks := io.bankLocks
  for (bank <- 0 until config.rankBankCount) {
    io.bankRequests(bank) <> commandCrossbar.io.bankRequests(bank)
  }
  dataCrossbar.io.bankCompletions := io.bankCompletions
  dataCrossbar.io.bankOwners := commandCrossbar.io.bankOwners
  dataCrossbar.io.bankOwnerValid := commandCrossbar.io.bankOwnerValid
  io.writeData <> dataCrossbar.io.writeData
  dataCrossbar.io.readData <> io.readData
}
