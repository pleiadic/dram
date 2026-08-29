package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls
import scala.util.Random

class CrossbarSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 14, dataBits = 32, bankBits = 2,
    rowBits = 4, columnBits = 4, timing = DramTiming(tRefi = 100))

  private def address(row: Int, bank: Int, column: Int): BigInt =
    BigInt((((row << cfg.bankBits) | bank) << cfg.columnBits) | column)

  private def pokeMaster(dut: LiteDramCommandCrossbar, master: Int,
      row: Int, bank: Int, column: Int, write: Boolean): Unit = {
    val port = dut.io.masters(master).command
    port.valid.poke(true.B)
    port.bits.address.poke(address(row, bank, column).U)
    port.bits.write.poke(write.B)
  }

  behavior of "LiteDramDataCrossbar"

  it should "route write payloads and read responses by completion owner" in {
    test(new LiteDramDataCrossbar(cfg, 2)) { dut =>
      dut.io.bankCompletions.foreach(_.valid.poke(false.B))
      dut.io.bankOwnerValid.foreach(_.poke(false.B))
      dut.io.bankOwners.foreach(_.poke(0.U))
      dut.io.masters.foreach { master =>
        master.writeData.valid.poke(false.B)
        master.readData.ready.poke(true.B)
      }
      dut.io.writeData.ready.poke(true.B)
      dut.io.readData.valid.poke(false.B)
      dut.io.readData.bits.data.poke(0.U)

      dut.io.masters(1).writeData.valid.poke(true.B)
      dut.io.masters(1).writeData.bits.data.poke("hdeadbeef".U)
      dut.io.masters(1).writeData.bits.byteEnable.poke("hf".U)
      dut.clock.step()
      dut.io.masters(1).writeData.valid.poke(false.B)

      dut.io.bankOwnerValid(2).poke(true.B)
      dut.io.bankOwners(2).poke(1.U)
      dut.io.bankCompletions(2).valid.poke(true.B)
      dut.io.bankCompletions(2).bits.write.poke(true.B)
      dut.clock.step()
      dut.io.bankCompletions(2).valid.poke(false.B)
      dut.io.writeData.valid.expect(true.B)
      dut.io.writeData.bits.data.expect("hdeadbeef".U)
      dut.clock.step()

      dut.io.bankOwnerValid(0).poke(true.B)
      dut.io.bankOwners(0).poke(0.U)
      dut.io.bankCompletions(0).valid.poke(true.B)
      dut.io.bankCompletions(0).bits.write.poke(false.B)
      dut.clock.step()
      dut.io.bankCompletions(0).valid.poke(false.B)
      dut.io.readData.valid.poke(true.B)
      dut.io.readData.bits.data.poke("h12345678".U)
      dut.io.masters(0).readData.valid.expect(true.B)
      dut.io.masters(0).readData.bits.data.expect("h12345678".U)
      dut.io.masters(1).readData.valid.expect(false.B)
    }
  }

  it should "preserve global completion order across randomized multi-master backpressure" in {
    val stressCfg = cfg.copy(cmdBufferDepth = 4)
    test(new LiteDramDataCrossbar(stressCfg, 4)) { dut =>
      case class Operation(master: Int, write: Boolean, data: BigInt, bank: Int)
      val rng = new Random(0x43524f5353L)
      val operations = Seq.fill(240)(Operation(
        master = rng.nextInt(4),
        write = rng.nextBoolean(),
        data = BigInt(32, rng),
        bank = rng.nextInt(stressCfg.rankBankCount)))
      val writesByMaster = (0 until 4).map(master =>
        operations.filter(op => op.master == master && op.write))
      val reads = operations.filter(!_.write)
      val outstanding = ArrayBuffer.empty[Operation]
      val writeInputs = Array.fill(4)(0)
      var completion = 0
      var readInput = 0
      var consumed = 0
      var cycle = 0
      val coverage = new FunctionalCoverageBins("data-crossbar", Seq(
        "master_0", "master_1", "master_2", "master_3", "bank_0", "bank_1",
        "bank_2", "bank_3", "read_completion", "write_completion",
        "multiple_outstanding", "write_output_stall", "read_output_stall",
        "write_input_stall", "completion_and_consume", "queue_drained"))

      dut.io.bankCompletions.foreach(_.valid.poke(false.B))
      dut.io.bankOwnerValid.foreach(_.poke(true.B))
      dut.io.bankOwners.foreach(_.poke(0.U))
      while ((consumed < operations.size || completion < operations.size) && cycle < 30000) {
        for (master <- 0 until 4) {
          val writes = writesByMaster(master)
          dut.io.masters(master).writeData.valid.poke((writeInputs(master) < writes.size).B)
          if (writeInputs(master) < writes.size) {
            dut.io.masters(master).writeData.bits.data.poke(
              writes(writeInputs(master)).data.U)
            dut.io.masters(master).writeData.bits.byteEnable.poke("hf".U)
          }
          dut.io.masters(master).readData.ready.poke((rng.nextInt(100) < 57).B)
        }
        dut.io.writeData.ready.poke((rng.nextInt(100) < 61).B)
        dut.io.readData.valid.poke((readInput < reads.size).B)
        if (readInput < reads.size) dut.io.readData.bits.data.poke(reads(readInput).data.U)

        dut.io.bankCompletions.foreach(_.valid.poke(false.B))
        val issueCompletion = completion < operations.size && outstanding.size < 6 &&
          rng.nextInt(100) < 68
        if (issueCompletion) {
          val operation = operations(completion)
          dut.io.bankOwners(operation.bank).poke(operation.master.U)
          dut.io.bankCompletions(operation.bank).valid.poke(true.B)
          dut.io.bankCompletions(operation.bank).bits.write.poke(operation.write.B)
        }

        if (dut.io.writeData.valid.peek().litToBoolean) {
          assert(outstanding.nonEmpty && outstanding.head.write,
            s"cycle $cycle produced write data out of tag order")
          dut.io.writeData.bits.data.expect(outstanding.head.data.U)
          dut.io.writeData.bits.byteEnable.expect("hf".U)
        }
        coverage.hitWhen("multiple_outstanding", outstanding.size > 1)
        coverage.hitWhen("write_output_stall",
          dut.io.writeData.valid.peek().litToBoolean &&
            !dut.io.writeData.ready.peek().litToBoolean)
        coverage.hitWhen("read_output_stall", (0 until 4).exists { master =>
          dut.io.masters(master).readData.valid.peek().litToBoolean &&
            !dut.io.masters(master).readData.ready.peek().litToBoolean
        })
        coverage.hitWhen("write_input_stall", (0 until 4).exists { master =>
          dut.io.masters(master).writeData.valid.peek().litToBoolean &&
            !dut.io.masters(master).writeData.ready.peek().litToBoolean
        })
        for (master <- 0 until 4) {
          if (dut.io.masters(master).readData.valid.peek().litToBoolean) {
            assert(outstanding.nonEmpty && !outstanding.head.write &&
              outstanding.head.master == master,
              s"cycle $cycle routed a read to master $master out of order")
            dut.io.masters(master).readData.bits.data.expect(outstanding.head.data.U)
          }
        }

        val writeInputFires = (0 until 4).map { master =>
          writeInputs(master) < writesByMaster(master).size &&
            dut.io.masters(master).writeData.ready.peek().litToBoolean
        }
        val writeOutputFire = dut.io.writeData.valid.peek().litToBoolean &&
          dut.io.writeData.ready.peek().litToBoolean
        val readOutputFire = (0 until 4).exists { master =>
          dut.io.masters(master).readData.valid.peek().litToBoolean &&
            dut.io.masters(master).readData.ready.peek().litToBoolean
        }
        val readInputFire = readInput < reads.size &&
          dut.io.readData.ready.peek().litToBoolean
        coverage.hitWhen("completion_and_consume",
          issueCompletion && (writeOutputFire || readOutputFire))
        assert(readOutputFire == readInputFire,
          s"cycle $cycle read input/output handshakes diverged")

        dut.clock.step()
        for (master <- 0 until 4) if (writeInputFires(master)) writeInputs(master) += 1
        if (writeOutputFire || readOutputFire) {
          outstanding.remove(0)
          consumed += 1
        }
        if (readInputFire) readInput += 1
        if (issueCompletion) {
          val operation = operations(completion)
          coverage.hit(s"master_${operation.master}")
          coverage.hit(s"bank_${operation.bank}")
          coverage.hit(if (operation.write) "write_completion" else "read_completion")
          outstanding += operations(completion)
          completion += 1
        }
        cycle += 1
      }

      assert(cycle < 30000, s"crossbar stress timed out with ${outstanding.size} tags")
      assert(completion == operations.size && consumed == operations.size)
      assert(readInput == reads.size)
      for (master <- 0 until 4)
        assert(writeInputs(master) == writesByMaster(master).size)
      coverage.hitWhen("queue_drained", outstanding.isEmpty)
      coverage.requireComplete()
    }
  }

  behavior of "LiteDramCommandCrossbar"

  it should "map addresses and route concurrent masters to different banks" in {
    test(new LiteDramCommandCrossbar(cfg, 2)) { dut =>
      dut.io.bankLocks.foreach(_.poke(false.B))
      dut.io.bankRequests.foreach(_.ready.poke(true.B))
      pokeMaster(dut, 0, row = 3, bank = 1, column = 7, write = false)
      pokeMaster(dut, 1, row = 4, bank = 2, column = 5, write = true)
      dut.io.bankRequests(1).valid.expect(true.B)
      dut.io.bankRequests(1).bits.row.expect(3.U)
      dut.io.bankRequests(1).bits.column.expect(7.U)
      dut.io.bankRequests(2).valid.expect(true.B)
      dut.io.bankRequests(2).bits.row.expect(4.U)
      dut.io.bankRequests(2).bits.write.expect(true.B)
    }
  }

  it should "hold arbitration under backpressure and lock a master to its bank" in {
    test(new LiteDramCommandCrossbar(cfg, 2)) { dut =>
      dut.io.bankLocks.foreach(_.poke(false.B))
      dut.io.bankRequests.foreach(_.ready.poke(false.B))
      pokeMaster(dut, 0, row = 1, bank = 0, column = 0, write = false)
      dut.io.masters(1).command.valid.poke(false.B)
      dut.io.bankRequests(0).bits.row.expect(1.U)
      dut.clock.step()
      pokeMaster(dut, 1, row = 2, bank = 0, column = 0, write = false)
      dut.io.bankRequests(0).bits.row.expect(1.U)

      dut.io.bankRequests(0).ready.poke(true.B)
      dut.clock.step()
      dut.io.masters(0).command.valid.poke(false.B)
      dut.io.bankLocks(0).poke(true.B)
      pokeMaster(dut, 0, row = 1, bank = 1, column = 0, write = false)
      dut.io.masters(0).lock.expect(true.B)
      dut.io.bankRequests(1).valid.expect(false.B)
    }
  }
}
