package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.language.reflectiveCalls

class BankMachineSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 12, dataBits = 32, bankBits = 1,
    rowBits = 4, columnBits = 4, cmdBufferDepth = 4,
    timing = DramTiming(tRcd = 2, tRp = 2, tRas = 3, tRc = 4, tCcd = 1,
      tWr = 2, tRtp = 2, tRefi = 100))

  private def enqueue(dut: BankMachine, row: Int, column: Int, write: Boolean): Unit = {
    dut.io.request.valid.poke(true.B)
    dut.io.request.bits.row.poke(row.U)
    dut.io.request.bits.column.poke(column.U)
    dut.io.request.bits.write.poke(write.B)
    while (!dut.io.request.ready.peek().litToBoolean) dut.clock.step()
    dut.clock.step()
    dut.io.request.valid.poke(false.B)
  }

  behavior of "BankMachine"

  it should "open a row once and serve subsequent row hits" in {
    test(new BankMachine(cfg, bankIndex = 1)) { dut =>
      dut.io.command.ready.poke(false.B)
      dut.io.refreshRequest.poke(false.B)
      enqueue(dut, row = 3, column = 1, write = false)
      enqueue(dut, row = 3, column = 2, write = true)
      dut.io.command.ready.poke(true.B)

      val commands = ArrayBuffer.empty[BigInt]
      var completions = 0
      for (_ <- 0 until 20) {
        if (dut.io.command.valid.peek().litToBoolean)
          commands += dut.io.command.bits.command.peek().litValue
        if (dut.io.completion.valid.peek().litToBoolean) completions += 1
        dut.clock.step()
      }
      assert(commands == Seq(DramCommandType.activate.litValue,
        DramCommandType.read.litValue, DramCommandType.write.litValue))
      assert(completions == 2)
      dut.io.command.bits.bank.expect(1.U)
    }
  }

  it should "precharge before activating a conflicting row" in {
    test(new BankMachine(cfg, bankIndex = 0)) { dut =>
      dut.io.command.ready.poke(true.B)
      dut.io.refreshRequest.poke(false.B)
      enqueue(dut, row = 1, column = 0, write = false)
      dut.clock.step(10)
      enqueue(dut, row = 2, column = 0, write = false)
      val commands = ArrayBuffer.empty[BigInt]
      for (_ <- 0 until 16) {
        if (dut.io.command.valid.peek().litToBoolean)
          commands += dut.io.command.bits.command.peek().litValue
        dut.clock.step()
      }
      val precharge = commands.indexOf(DramCommandType.precharge.litValue)
      val activate = commands.indexOf(DramCommandType.activate.litValue)
      assert(precharge >= 0 && activate > precharge)
    }
  }

  it should "look ahead and set auto-precharge only across rows" in {
    val autoCfg = cfg.copy(withAutoPrecharge = true)
    test(new BankMachine(autoCfg, bankIndex = 0)) { dut =>
      dut.io.command.ready.poke(false.B)
      dut.io.refreshRequest.poke(false.B)
      enqueue(dut, row = 1, column = 0, write = false)
      enqueue(dut, row = 2, column = 0, write = false)
      dut.io.command.ready.poke(true.B)
      var sawAuto = false
      for (_ <- 0 until 24) {
        if (dut.io.command.valid.peek().litToBoolean &&
            dut.io.command.bits.command.peek().litValue == DramCommandType.read.litValue &&
            dut.io.command.bits.autoPrecharge.peek().litToBoolean) sawAuto = true
        dut.clock.step()
      }
      assert(sawAuto)
    }
  }

  it should "hold its command stable and grant refresh after draining timing" in {
    test(new BankMachine(cfg, bankIndex = 0)) { dut =>
      dut.io.command.ready.poke(false.B)
      dut.io.refreshRequest.poke(false.B)
      enqueue(dut, row = 5, column = 6, write = false)
      while (!dut.io.command.valid.peek().litToBoolean) dut.clock.step()
      val command = dut.io.command.bits.command.peek().litValue
      val row = dut.io.command.bits.row.peek().litValue
      dut.clock.step(3)
      assert(dut.io.command.bits.command.peek().litValue == command)
      assert(dut.io.command.bits.row.peek().litValue == row)
      dut.io.refreshRequest.poke(true.B)
      dut.io.command.ready.poke(true.B)
      while (!dut.io.refreshGrant.peek().litToBoolean) dut.clock.step()
      dut.io.rowOpen.expect(false.B)
    }
  }

  it should "keep a stalled column command irrevocable across lookahead and refresh changes" in {
    test(new BankMachine(cfg.copy(withAutoPrecharge = true), bankIndex = 0)) { dut =>
      dut.io.request.valid.poke(false.B)
      dut.io.refreshRequest.poke(false.B)
      dut.io.command.ready.poke(true.B)
      enqueue(dut, row = 1, column = 3, write = false)

      while (!(dut.io.command.valid.peek().litToBoolean &&
          dut.io.command.bits.command.peek().litValue == DramCommandType.activate.litValue)) {
        dut.clock.step()
      }
      dut.clock.step()
      dut.io.command.ready.poke(false.B)
      while (!(dut.io.command.valid.peek().litToBoolean &&
          dut.io.command.bits.command.peek().litValue == DramCommandType.read.litValue)) {
        dut.clock.step()
      }
      dut.io.command.bits.autoPrecharge.expect(false.B)
      dut.clock.step() // Capture the backpressured command.

      dut.io.request.valid.poke(true.B)
      dut.io.request.bits.row.poke(2.U)
      dut.io.request.bits.column.poke(7.U)
      dut.io.request.bits.write.poke(true.B)
      dut.io.refreshRequest.poke(true.B)
      dut.io.command.valid.expect(true.B)
      dut.io.command.bits.command.expect(DramCommandType.read)
      dut.io.command.bits.row.expect(1.U)
      dut.io.command.bits.column.expect(3.U)
      dut.io.command.bits.autoPrecharge.expect(false.B)
      dut.io.request.ready.expect(true.B)
      dut.clock.step()

      dut.io.request.valid.poke(false.B)
      dut.io.command.ready.poke(true.B)
      dut.io.command.valid.expect(true.B)
      dut.io.command.bits.command.expect(DramCommandType.read)
      dut.io.command.bits.row.expect(1.U)
      dut.io.command.bits.column.expect(3.U)
      dut.io.command.bits.autoPrecharge.expect(false.B)
      dut.io.completion.valid.expect(true.B)
      dut.clock.step()

      while (!dut.io.refreshGrant.peek().litToBoolean) dut.clock.step()
      dut.io.refreshRequest.poke(false.B)
      dut.clock.step()
      while (!(dut.io.command.valid.peek().litToBoolean &&
          dut.io.command.bits.command.peek().litValue == DramCommandType.activate.litValue)) {
        dut.clock.step()
      }
      dut.io.command.bits.row.expect(2.U)
    }
  }

  it should "match LiteDRAM local activate and write-to-precharge timing" in {
    val timing = DramTiming(tRcd = 3, tRp = 4, tRas = 2, tRc = 4,
      tCcd = 3, tWr = 5, tRtp = 2, tRefi = 100)
    val timingCfg = cfg.copy(timing = timing, writeLatency = 2,
      withAutoPrecharge = false)
    test(new BankMachine(timingCfg, bankIndex = 0)) { dut =>
      dut.io.command.ready.poke(false.B)
      dut.io.refreshRequest.poke(false.B)
      enqueue(dut, row = 1, column = 2, write = true)
      enqueue(dut, row = 2, column = 4, write = true)
      dut.io.command.ready.poke(true.B)

      val events = ArrayBuffer.empty[(Int, BigInt)]
      var cycle = 0
      while (events.size < 5 && cycle < 100) {
        if (dut.io.command.valid.peek().litToBoolean) {
          events += cycle -> dut.io.command.bits.command.peek().litValue
        }
        dut.clock.step()
        cycle += 1
      }
      assert(events.map(_._2) == Seq(
        DramCommandType.activate.litValue,
        DramCommandType.write.litValue,
        DramCommandType.precharge.litValue,
        DramCommandType.activate.litValue,
        DramCommandType.write.litValue))
      assert(events(1)._1 - events(0)._1 == timing.tRcd)
      assert(events(2)._1 - events(1)._1 ==
        timingCfg.writeLatency + timing.tWr + timing.tCcd)
      assert(events(3)._1 - events(2)._1 == timing.tRp)
    }
  }

  it should "enforce activate-to-precharge and activate-to-activate timing" in {
    val timing = DramTiming(tRcd = 1, tRp = 1, tRas = 8, tRc = 12,
      tCcd = 1, tWr = 1, tRtp = 1, tRefi = 100)
    test(new BankMachine(cfg.copy(timing = timing, withAutoPrecharge = false),
        bankIndex = 0)) { dut =>
      dut.io.command.ready.poke(false.B)
      dut.io.refreshRequest.poke(false.B)
      enqueue(dut, row = 1, column = 0, write = false)
      enqueue(dut, row = 2, column = 0, write = false)
      dut.io.command.ready.poke(true.B)

      val events = ArrayBuffer.empty[(Int, BigInt)]
      var cycle = 0
      while (events.size < 5 && cycle < 100) {
        if (dut.io.command.valid.peek().litToBoolean)
          events += cycle -> dut.io.command.bits.command.peek().litValue
        dut.clock.step()
        cycle += 1
      }
      val firstActivate = events(0)._1
      val precharge = events(2)._1
      val secondActivate = events(3)._1
      assert(precharge - firstActivate == timing.tRas)
      assert(secondActivate - firstActivate == timing.tRc)
    }
  }

  it should "lose no requests in same-row bursts at representative FIFO depths" in {
    for (depth <- Seq(1, 2, 8)) {
      test(new BankMachine(cfg.copy(cmdBufferDepth = depth), bankIndex = 0)) { dut =>
        dut.io.command.ready.poke(true.B)
        dut.io.refreshRequest.poke(false.B)
        val writes = ArrayBuffer.empty[Int]
        var activates = 0
        var sent = 0
        var cycles = 0
        while (writes.size < 32 && cycles < 500) {
          dut.io.request.valid.poke((sent < 32).B)
          dut.io.request.bits.row.poke(6.U)
          dut.io.request.bits.column.poke((sent & 0xf).U)
          dut.io.request.bits.write.poke(true.B)
          if (dut.io.command.valid.peek().litToBoolean) {
            dut.io.command.bits.command.peek().litValue match {
              case value if value == DramCommandType.activate.litValue => activates += 1
              case value if value == DramCommandType.write.litValue =>
                writes += dut.io.command.bits.column.peek().litValue.toInt
              case _ =>
            }
          }
          val requestFire = sent < 32 && dut.io.request.ready.peek().litToBoolean
          dut.clock.step()
          if (requestFire) sent += 1
          cycles += 1
        }
        dut.io.request.valid.poke(false.B)
        assert(sent == 32, s"depth=$depth accepted $sent requests")
        assert(activates == 1, s"depth=$depth activated $activates times")
        assert(writes == (0 until 32).map(_ & 0xf), s"depth=$depth writes=$writes")
        dut.io.lock.expect(false.B)
      }
    }
  }

  it should "preserve ordering and all local timing invariants in a long random run" in {
    val randomCfg = cfg.copy(
      cmdBufferDepth = 5,
      withAutoPrecharge = true,
      writeLatency = 2,
      timing = DramTiming(tRcd = 3, tRp = 3, tRas = 5, tRc = 7,
        tCcd = 2, tWr = 4, tRtp = 3, tRefi = 100))
    test(new BankMachine(randomCfg, bankIndex = 1)) { dut =>
      case class Request(row: Int, column: Int, write: Boolean)
      case class Payload(command: BigInt, row: BigInt, column: BigInt,
        autoPrecharge: Boolean, rank: BigInt, bank: BigInt)

      val rng = new Random(0x4b414e4bL)
      val expected = ArrayBuffer.empty[Request]
      val targetRequests = 400
      var pending = Option.empty[Request]
      var accepted = 0
      var completed = 0
      var openRow = Option.empty[Int]
      var lastActivate = Option.empty[Int]
      var lastPrecharge = Option.empty[Int]
      var lastColumn = Option.empty[(Int, Boolean)]
      var stalled = Option.empty[Payload]
      var refreshActive = false
      var refreshGranted = false
      var refreshHold = 0
      var cycle = 0

      dut.io.request.valid.poke(false.B)
      dut.io.refreshRequest.poke(false.B)
      dut.io.command.ready.poke(false.B)

      while ((accepted < targetRequests || expected.nonEmpty || pending.nonEmpty ||
          refreshActive || dut.io.command.valid.peek().litToBoolean) && cycle < 8000) {
        if (pending.isEmpty && accepted < targetRequests && rng.nextInt(100) < 82) {
          pending = Some(Request(rng.nextInt(1 << randomCfg.rowBits),
            rng.nextInt(1 << randomCfg.columnBits), rng.nextBoolean()))
        }
        pending match {
          case Some(request) =>
            dut.io.request.valid.poke(true.B)
            dut.io.request.bits.row.poke(request.row.U)
            dut.io.request.bits.column.poke(request.column.U)
            dut.io.request.bits.write.poke(request.write.B)
          case None => dut.io.request.valid.poke(false.B)
        }

        if (!refreshActive && accepted > 10 && accepted < targetRequests &&
            rng.nextInt(160) == 0) {
          refreshActive = true
          refreshGranted = false
        }
        dut.io.refreshRequest.poke(refreshActive.B)
        val ready = accepted == targetRequests || rng.nextInt(100) < 72
        dut.io.command.ready.poke(ready.B)

        dut.io.lock.expect(expected.nonEmpty.B)
        val commandValid = dut.io.command.valid.peek().litToBoolean
        val payload = if (commandValid) Some(Payload(
          dut.io.command.bits.command.peek().litValue,
          dut.io.command.bits.row.peek().litValue,
          dut.io.command.bits.column.peek().litValue,
          dut.io.command.bits.autoPrecharge.peek().litToBoolean,
          dut.io.command.bits.rank.peek().litValue,
          dut.io.command.bits.bank.peek().litValue)) else None

        stalled.foreach { previous =>
          assert(commandValid, s"cycle $cycle withdrew a stalled command")
          assert(payload.contains(previous),
            s"cycle $cycle changed stalled command from $previous to $payload")
        }

        payload.foreach { command =>
          assert(expected.nonEmpty, s"cycle $cycle emitted $command with no request")
          val head = expected.head
          assert(command.rank == 0 && command.bank == 1,
            s"cycle $cycle selected rank/bank ${command.rank}/${command.bank}")
          command.command match {
            case value if value == DramCommandType.activate.litValue =>
              assert(openRow.isEmpty, s"cycle $cycle activated with row $openRow open")
              assert(command.row == head.row, s"cycle $cycle activated wrong row")
            case value if value == DramCommandType.precharge.litValue =>
              assert(openRow.exists(_ != head.row),
                s"cycle $cycle issued unnecessary precharge for row $openRow")
            case value if value == DramCommandType.read.litValue ||
                value == DramCommandType.write.litValue =>
              assert(openRow.contains(head.row),
                s"cycle $cycle accessed row ${head.row} with $openRow open")
              assert(command.row == head.row && command.column == head.column,
                s"cycle $cycle reordered request $head into $command")
              assert((value == DramCommandType.write.litValue) == head.write,
                s"cycle $cycle changed read/write direction")
              if (stalled.isEmpty) {
                val expectedAuto = expected.size > 1 && expected(1).row != head.row
                assert(command.autoPrecharge == expectedAuto,
                  s"cycle $cycle auto-precharge=${command.autoPrecharge}, expected=$expectedAuto")
              }
            case other => fail(s"cycle $cycle emitted unknown command $other")
          }
        }

        val commandFire = commandValid && ready
        val columnFire = commandFire && payload.exists(command =>
          command.command == DramCommandType.read.litValue ||
            command.command == DramCommandType.write.litValue)
        dut.io.completion.valid.expect(columnFire.B)
        if (columnFire) {
          dut.io.completion.bits.write.expect(expected.head.write.B)
        }
        if (dut.io.refreshGrant.peek().litToBoolean) {
          assert(!commandValid, s"cycle $cycle granted refresh with a command valid")
          if (!refreshGranted) {
            refreshGranted = true
            refreshHold = 1 + rng.nextInt(4)
          }
        }

        if (commandFire) {
          val command = payload.get
          command.command match {
            case value if value == DramCommandType.activate.litValue =>
              lastActivate.foreach(previous => assert(cycle - previous >= randomCfg.timing.tRc,
                s"tRC violation at cycle $cycle"))
              lastPrecharge.foreach(previous => assert(cycle - previous >= randomCfg.timing.tRp,
                s"tRP violation at cycle $cycle"))
              openRow = Some(expected.head.row)
              lastActivate = Some(cycle)
            case value if value == DramCommandType.precharge.litValue =>
              lastActivate.foreach(previous => assert(cycle - previous >= randomCfg.timing.tRas,
                s"tRAS violation at cycle $cycle"))
              lastColumn.foreach { case (previous, write) =>
                val minimum = if (write)
                  randomCfg.writeLatency + randomCfg.timing.tWr + randomCfg.timing.tCcd
                else randomCfg.timing.tRtp
                assert(cycle - previous >= minimum,
                  s"column-to-precharge violation at cycle $cycle")
              }
              openRow = None
              lastPrecharge = Some(cycle)
            case value if value == DramCommandType.read.litValue ||
                value == DramCommandType.write.litValue =>
              lastActivate.foreach(previous => assert(cycle - previous >= randomCfg.timing.tRcd,
                s"tRCD violation at cycle $cycle"))
              val request = expected.remove(0)
              completed += 1
              lastColumn = Some(cycle -> request.write)
              if (command.autoPrecharge) openRow = None
            case _ =>
          }
        }

        val requestFire = pending.nonEmpty && dut.io.request.ready.peek().litToBoolean
        if (requestFire) {
          expected += pending.get
          pending = None
          accepted += 1
        }
        if (dut.io.refreshGrant.peek().litToBoolean) openRow = None
        stalled = if (commandValid && !ready) payload else None

        dut.clock.step()
        cycle += 1
        if (refreshGranted) {
          refreshHold -= 1
          if (refreshHold == 0) {
            refreshActive = false
            refreshGranted = false
          }
        }
      }

      assert(cycle < 8000, s"random run timed out with ${expected.size} queued requests")
      assert(accepted == targetRequests)
      assert(completed == targetRequests)
      assert(expected.isEmpty && pending.isEmpty)
      dut.io.lock.expect(false.B)
    }
  }
}
