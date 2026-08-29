package pleiadic.dram

import chisel3._
import chisel3.util.UIntToOH
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.util.Random

class LiteDramDfiCoreHarness(config: DramConfig) extends Module {
  val io = IO(new Bundle {
    val master = new NativeSlavePort(config)
    val protocolError = Output(Bool())
    val errors = Output(UInt(32.W))
    val refreshes = Output(UInt(16.W))
  })

  private val core = Module(new LiteDramDfiCore(config, masterCount = 1))
  private val memory = Module(new DfiMemoryModel(config,
    readLatency = config.readLatency, writeLatency = 0))

  core.io.masters(0).command.valid := io.master.command.valid
  core.io.masters(0).command.bits := io.master.command.bits
  io.master.command.ready := core.io.masters(0).command.ready
  core.io.masters(0).writeData.valid := io.master.writeData.valid
  core.io.masters(0).writeData.bits := io.master.writeData.bits
  io.master.writeData.ready := core.io.masters(0).writeData.ready
  io.master.readData.valid := core.io.masters(0).readData.valid
  io.master.readData.bits := core.io.masters(0).readData.bits
  core.io.masters(0).readData.ready := io.master.readData.ready
  core.io.masters(0).flush := io.master.flush
  io.master.lock := core.io.masters(0).lock

  memory.io.dfi := core.io.dfi
  memory.io.clearErrors := false.B
  core.io.phyRead := memory.io.read
  io.protocolError := memory.io.protocolError
  io.errors := memory.io.errors
  private val refreshCommand = core.io.dfi.phases.map { phase =>
    !phase.csN.asUInt.andR && phase.actN && !phase.rasN &&
      !phase.casN && phase.weN
  }.reduce(_ || _)
  private val refreshes = RegInit(0.U(16.W))
  when(refreshCommand && !refreshes.andR) { refreshes := refreshes + 1.U }
  io.refreshes := refreshes
}

class LiteDramDfiCoreMultiHarness(config: DramConfig, masterCount: Int)
    extends Module {
  val io = IO(new Bundle {
    val masters = Vec(masterCount, new NativeSlavePort(config))
    val protocolError = Output(Bool())
    val errors = Output(UInt(32.W))
    val refreshes = Output(UInt(16.W))
    val activates = Output(UInt(16.W))
    val precharges = Output(UInt(16.W))
    val reads = Output(UInt(16.W))
    val writes = Output(UInt(16.W))
    val rankSeen = Output(UInt(config.nranks.W))
    val bankSeen = Output(UInt(config.bankCount.W))
    val phaseSeen = Output(UInt(config.nPhases.W))
  })

  private val core = Module(new LiteDramDfiCore(config, masterCount))
  private val memory = Module(new DfiMemoryModel(config,
    readLatency = config.readLatency, writeLatency = 0))
  for (master <- 0 until masterCount) {
    core.io.masters(master).command.valid := io.masters(master).command.valid
    core.io.masters(master).command.bits := io.masters(master).command.bits
    io.masters(master).command.ready := core.io.masters(master).command.ready
    core.io.masters(master).writeData.valid := io.masters(master).writeData.valid
    core.io.masters(master).writeData.bits := io.masters(master).writeData.bits
    io.masters(master).writeData.ready := core.io.masters(master).writeData.ready
    io.masters(master).readData.valid := core.io.masters(master).readData.valid
    io.masters(master).readData.bits := core.io.masters(master).readData.bits
    core.io.masters(master).readData.ready := io.masters(master).readData.ready
    core.io.masters(master).flush := io.masters(master).flush
    io.masters(master).lock := core.io.masters(master).lock
  }
  memory.io.dfi := core.io.dfi
  memory.io.clearErrors := false.B
  core.io.phyRead := memory.io.read
  io.protocolError := memory.io.protocolError
  io.errors := memory.io.errors
  private val refresh = core.io.dfi.phases.map { phase =>
    !phase.csN.asUInt.andR && phase.actN && !phase.rasN &&
      !phase.casN && phase.weN
  }.reduce(_ || _)
  private val refreshes = RegInit(0.U(16.W))
  when(refresh && !refreshes.andR) { refreshes := refreshes + 1.U }
  io.refreshes := refreshes

  private def countWhen(condition: Bool): UInt = {
    val counter = RegInit(0.U(16.W))
    when(condition && !counter.andR) { counter := counter + 1.U }
    counter
  }
  private val selected = core.io.dfi.phases.map(phase => !phase.csN.asUInt.andR)
  private val activates = core.io.dfi.phases.zip(selected).map { case (phase, active) =>
    active && !phase.rasN && phase.casN && phase.weN
  }
  private val precharges = core.io.dfi.phases.zip(selected).map { case (phase, active) =>
    active && !phase.rasN && phase.casN && !phase.weN
  }
  private val reads = core.io.dfi.phases.zip(selected).map { case (phase, active) =>
    active && phase.rasN && !phase.casN && phase.weN
  }
  private val writes = core.io.dfi.phases.zip(selected).map { case (phase, active) =>
    active && phase.rasN && !phase.casN && !phase.weN
  }
  io.activates := countWhen(activates.reduce(_ || _))
  io.precharges := countWhen(precharges.reduce(_ || _))
  io.reads := countWhen(reads.reduce(_ || _))
  io.writes := countWhen(writes.reduce(_ || _))

  private val rankSeen = RegInit(0.U(config.nranks.W))
  private val bankSeen = RegInit(0.U(config.bankCount.W))
  private val phaseSeen = RegInit(0.U(config.nPhases.W))
  private val command = core.io.dfi.phases.zip(selected).map { case (phase, active) =>
    active && (!phase.rasN || !phase.casN || !phase.weN)
  }
  when(command.reduce(_ || _)) {
    val ranks = core.io.dfi.phases.zip(command).map { case (phase, active) =>
      Mux(active, (~phase.csN.asUInt)(config.nranks - 1, 0), 0.U)
    }.reduce(_ | _)
    val banks = core.io.dfi.phases.zip(command).map { case (phase, active) =>
      Mux(active, UIntToOH(phase.bank, config.bankCount), 0.U)
    }.reduce(_ | _)
    val phases = command.zipWithIndex.map { case (active, phaseIndex) =>
      Mux(active, (1.U(config.nPhases.W) << phaseIndex), 0.U)
    }.reduce(_ | _)
    rankSeen := rankSeen | ranks
    bankSeen := bankSeen | banks
    phaseSeen := phaseSeen | phases
  }
  io.rankSeen := rankSeen
  io.bankSeen := bankSeen
  io.phaseSeen := phaseSeen
}

class LiteDramDfiCoreSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val config = DramConfig(addressBits = 8, dataBits = 32,
    bankBits = 1, rowBits = 2, columnBits = 2, nPhases = 2, nranks = 2,
    phyDataBits = 32, readPhase = 0, writePhase = 1,
    cmdBufferDepth = 4, readTime = 4, writeTime = 4,
    readLatency = 1, writeLatency = 0,
    timing = DramTiming(tRcd = 1, tRp = 1, tRas = 2, tRc = 2,
      tCcd = 1, tWr = 1, tWtr = 1, tRtp = 1, tRrd = 1, tFaw = 8,
      tRefi = 32, tRfc = 2))

  private def address(rank: Int, row: Int, bank: Int, column: Int): Int =
    (((rank << config.rowBits | row) << config.bankBits | bank) <<
      config.columnBits) | column

  private def sendCommand(dut: LiteDramDfiCoreHarness, location: Int,
      write: Boolean): Unit = {
    val command = dut.io.master.command
    command.valid.poke(true.B)
    command.bits.address.poke(location.U)
    command.bits.write.poke(write.B)
    var timeout = 0
    while (!command.ready.peek().litToBoolean && timeout < 1000) {
      dut.clock.step()
      timeout += 1
    }
    assert(timeout < 1000, s"command $location timed out")
    dut.clock.step()
    command.valid.poke(false.B)
  }

  private def sendWriteData(dut: LiteDramDfiCoreHarness, data: BigInt,
      byteEnable: Int = 0xf): Unit = {
    val write = dut.io.master.writeData
    write.valid.poke(true.B)
    write.bits.data.poke(data.U)
    write.bits.byteEnable.poke(byteEnable.U)
    var timeout = 0
    while (!write.ready.peek().litToBoolean && timeout < 1000) {
      dut.clock.step()
      timeout += 1
    }
    assert(timeout < 1000, "write-data enqueue timed out")
    dut.clock.step()
    write.valid.poke(false.B)
  }

  behavior of "LiteDramDfiCore"

  it should "preserve multi-bank and multi-rank data through a DFI memory model" in {
    test(new LiteDramDfiCoreHarness(config)) { dut =>
      val master = dut.io.master
      master.flush.poke(false.B)
      master.command.valid.poke(false.B)
      master.writeData.valid.poke(false.B)
      master.readData.ready.poke(true.B)

      val rng = new Random(0x444649434f5245L)
      val expected = mutable.LinkedHashMap.empty[Int, BigInt]
      for (index <- 0 until 32) {
        val rank = rng.nextInt(config.nranks)
        val row = rng.nextInt(1 << config.rowBits)
        val bank = rng.nextInt(config.bankCount)
        val column = rng.nextInt(1 << config.columnBits)
        val location = address(rank, row, bank, column)
        val value = BigInt(32, rng)
        sendWriteData(dut, value)
        sendCommand(dut, location, write = true)
        expected(location) = value
      }

      // Exercise byte-enable polarity through the Native-to-DFI mask path.
      val maskedLocation = expected.head._1
      val replacement = BigInt("a1b2c3d4", 16)
      sendWriteData(dut, replacement, byteEnable = 0x5)
      sendCommand(dut, maskedLocation, write = true)
      expected(maskedLocation) =
        (expected(maskedLocation) & BigInt("ff00ff00", 16)) |
        (replacement & BigInt("00ff00ff", 16))

      for ((location, value) <- expected) {
        master.readData.ready.poke(false.B)
        sendCommand(dut, location, write = false)
        var timeout = 0
        var accepted = false
        var stalled = Option.empty[BigInt]
        while (!accepted && timeout < 2000) {
          val ready = rng.nextInt(100) < 58
          master.readData.ready.poke(ready.B)
          if (master.readData.valid.peek().litToBoolean) {
            val observed = master.readData.bits.data.peek().litValue
            stalled.foreach(previous => assert(observed == previous,
              s"read $location changed under backpressure"))
            assert(observed == value,
              s"read $location returned 0x${observed.toString(16)} instead of " +
                s"0x${value.toString(16)}")
            accepted = ready
            stalled = if (ready) None else Some(observed)
          }
          dut.clock.step()
          timeout += 1
        }
        assert(timeout < 2000, s"read $location timed out")
      }

      master.readData.ready.poke(true.B)
      assert(dut.io.refreshes.peek().litValue > 0, "random run issued no refreshes")
      dut.io.protocolError.expect(false.B)
      dut.io.errors.expect(0.U)
    }
  }

  it should "preserve three concurrent Native masters through the complete DFI path" in {
    val masterCount = 3
    val systemConfig = config.copy(timing = config.timing.copy(tRefi = 64))
    test(new LiteDramDfiCoreMultiHarness(systemConfig, masterCount)) { dut =>
      val random = new Random(0x53595354454dL)
      val coverage = new FunctionalCoverageBins("complete-dfi-system", Seq(
        "master_0_write", "master_1_write", "master_2_write",
        "master_0_read", "master_1_read", "master_2_read",
        "simultaneous_master_commands", "command_backpressure",
        "read_data_backpressure", "activate",
        "precharge", "dfi_read", "dfi_write", "refresh", "all_ranks",
        "all_banks", "all_phases", "scoreboards_drained", "protocol_clean"))
      val writes = Array.tabulate(masterCount) { master =>
        (0 until 48).filter(_ % masterCount == master).map { address =>
          address -> (BigInt(master + 1) << 28 | BigInt(address * 0x10201))
        }.toVector
      }
      for (master <- 0 until masterCount) {
        val port = dut.io.masters(master)
        port.flush.poke(false.B)
        port.command.valid.poke(false.B)
        port.writeData.valid.poke(false.B)
        port.readData.ready.poke(false.B)
      }

      val writeIndex = Array.fill(masterCount)(0)
      val commandAccepted = Array.fill(masterCount)(false)
      val dataAccepted = Array.fill(masterCount)(false)
      var cycles = 0
      while (writeIndex.exists(_ < 16) && cycles < 10000) {
        for (master <- 0 until masterCount) {
          val port = dut.io.masters(master)
          if (writeIndex(master) < writes(master).size) {
            val (address, value) = writes(master)(writeIndex(master))
            port.command.valid.poke((!commandAccepted(master)).B)
            port.command.bits.write.poke(true.B)
            port.command.bits.address.poke(address.U)
            port.writeData.valid.poke((!dataAccepted(master)).B)
            port.writeData.bits.data.poke(value.U)
            port.writeData.bits.byteEnable.poke("hf".U)
          } else {
            port.command.valid.poke(false.B)
            port.writeData.valid.poke(false.B)
          }
        }
        coverage.hitWhen("simultaneous_master_commands",
          dut.io.masters.count(_.command.valid.peek().litToBoolean) > 1)
        for (master <- 0 until masterCount) {
          val port = dut.io.masters(master)
          if (port.command.valid.peek().litToBoolean &&
              port.command.ready.peek().litToBoolean) {
            commandAccepted(master) = true
            coverage.hit(s"master_${master}_write")
          }
          if (port.writeData.valid.peek().litToBoolean &&
              port.writeData.ready.peek().litToBoolean) dataAccepted(master) = true
          coverage.hitWhen("command_backpressure",
            port.command.valid.peek().litToBoolean &&
              !port.command.ready.peek().litToBoolean)
        }
        dut.clock.step()
        for (master <- 0 until masterCount) {
          if (commandAccepted(master) && dataAccepted(master)) {
            writeIndex(master) += 1
            commandAccepted(master) = false
            dataAccepted(master) = false
          }
        }
        cycles += 1
      }
      assert(writeIndex.forall(_ == 16), s"concurrent writes timed out: ${writeIndex.mkString(",")}")

      dut.io.masters.foreach { port =>
        port.command.valid.poke(false.B)
        port.writeData.valid.poke(false.B)
      }
      dut.clock.step(200)
      dut.io.protocolError.expect(false.B)
      dut.io.errors.expect(0.U)

      val reads = writes.map(values => random.shuffle(values))
      val readCommandIndex = Array.fill(masterCount)(0)
      val expected = Array.fill(masterCount)(mutable.Queue.empty[BigInt])
      val completed = Array.fill(masterCount)(0)
      cycles = 0
      while (completed.exists(_ < 16) && cycles < 20000) {
        for (master <- 0 until masterCount) {
          val port = dut.io.masters(master)
          if (readCommandIndex(master) < reads(master).size) {
            val (address, _) = reads(master)(readCommandIndex(master))
            port.command.valid.poke(true.B)
            port.command.bits.write.poke(false.B)
            port.command.bits.address.poke(address.U)
          } else port.command.valid.poke(false.B)
          port.writeData.valid.poke(false.B)
          port.readData.ready.poke((random.nextInt(100) < 61).B)
        }

        for (master <- 0 until masterCount) {
          val port = dut.io.masters(master)
          if (port.command.valid.peek().litToBoolean &&
              port.command.ready.peek().litToBoolean) {
            expected(master).enqueue(reads(master)(readCommandIndex(master))._2)
            readCommandIndex(master) += 1
            coverage.hit(s"master_${master}_read")
          }
          if (port.readData.valid.peek().litToBoolean) {
            assert(expected(master).nonEmpty,
              s"master $master returned data without an accepted read")
            port.readData.bits.data.expect(expected(master).front.U)
            if (port.readData.ready.peek().litToBoolean) {
              expected(master).dequeue()
              completed(master) += 1
            }
          }
          coverage.hitWhen("read_data_backpressure",
            port.readData.valid.peek().litToBoolean &&
              !port.readData.ready.peek().litToBoolean)
        }
        dut.clock.step()
        cycles += 1
      }
      assert(completed.forall(_ == 16),
        s"concurrent reads timed out: ${completed.mkString(",")}")
      assert(expected.forall(_.isEmpty))
      assert(dut.io.refreshes.peek().litValue > 0, "system run issued no refreshes")
      dut.io.protocolError.expect(false.B)
      dut.io.errors.expect(0.U)
      coverage.hitWhen("activate", dut.io.activates.peek().litValue > 0)
      coverage.hitWhen("precharge", dut.io.precharges.peek().litValue > 0)
      coverage.hitWhen("dfi_read", dut.io.reads.peek().litValue > 0)
      coverage.hitWhen("dfi_write", dut.io.writes.peek().litValue > 0)
      coverage.hitWhen("refresh", dut.io.refreshes.peek().litValue > 0)
      coverage.hitWhen("all_ranks",
        dut.io.rankSeen.peek().litValue == (BigInt(1) << systemConfig.nranks) - 1)
      coverage.hitWhen("all_banks",
        dut.io.bankSeen.peek().litValue == (BigInt(1) << systemConfig.bankCount) - 1)
      coverage.hitWhen("all_phases",
        dut.io.phaseSeen.peek().litValue == (BigInt(1) << systemConfig.nPhases) - 1)
      coverage.hitWhen("scoreboards_drained",
        expected.forall(_.isEmpty) && completed.forall(_ == 16))
      coverage.hitWhen("protocol_clean",
        !dut.io.protocolError.peek().litToBoolean && dut.io.errors.peek().litValue == 0)
      coverage.requireComplete()
    }
  }
}
