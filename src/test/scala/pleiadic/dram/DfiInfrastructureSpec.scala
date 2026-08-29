package pleiadic.dram

import chisel3._
import chiseltest._
import chiseltest.simulator.{VerilatorBackendAnnotation, VerilatorCFlags}
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls
import scala.util.Random

class DfiRateConverterHarness extends Module {
  private val fastConfig = DramConfig(addressBits = 20, dataBits = 64, bankBits = 1,
    rowBits = 10, columnBits = 6, nPhases = 2, timing = DramTiming(tRefi = 100))
  private val converter = Module(new DfiRateConverter(fastConfig, ratio = 2,
    writeDelay = 1, readDelay = 0))
  val io = IO(new Bundle {
    val slow = Input(new DfiInterface(converter.slowConfig))
    val fast = Output(new DfiInterface(fastConfig))
    val fastRead = Input(Vec(fastConfig.nPhases, new DfiReadResponse(fastConfig)))
    val slowRead = Output(Vec(converter.slowConfig.nPhases,
      new DfiReadResponse(converter.slowConfig)))
    val slot = Output(UInt(1.W))
  })
  converter.io.fastClock := clock
  converter.io.fastReset := reset.asAsyncReset
  converter.io.slow := io.slow
  converter.io.fastRead := io.fastRead
  io.fast := converter.io.fast
  io.slowRead := converter.io.slowRead
  io.slot := converter.io.slot
}

class DfiRateConverterRatio4Harness extends Module {
  private val fastConfig = DramConfig(addressBits = 20, dataBits = 64, bankBits = 1,
    rowBits = 10, columnBits = 6, nPhases = 1, timing = DramTiming(tRefi = 100))
  private val converter = Module(new DfiRateConverter(fastConfig, ratio = 4,
    writeDelay = 2, readDelay = 3))
  val io = IO(new Bundle {
    val slow = Input(new DfiInterface(converter.slowConfig))
    val fast = Output(new DfiInterface(fastConfig))
    val fastRead = Input(Vec(1, new DfiReadResponse(fastConfig)))
    val slowRead = Output(Vec(4, new DfiReadResponse(converter.slowConfig)))
    val slot = Output(UInt(2.W))
  })
  converter.io.fastClock := clock
  converter.io.fastReset := reset.asAsyncReset
  converter.io.slow := io.slow
  converter.io.fastRead := io.fastRead
  io.fast := converter.io.fast
  io.slowRead := converter.io.slowRead
  io.slot := converter.io.slot
}

class DfiInfrastructureSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val cfg = DramConfig(addressBits = 24, dataBits = 32, bankBits = 1,
    rowBits = 17, columnBits = 4, nPhases = 2, timing = DramTiming(tRefi = 100))

  private def clearPhase(phase: DfiPhase): Unit = {
    phase.address.poke(0.U)
    phase.bank.poke(0.U)
    phase.csN.foreach(_.poke(true.B))
    phase.rasN.poke(true.B)
    phase.casN.poke(true.B)
    phase.weN.poke(true.B)
    phase.actN.poke(true.B)
    phase.cke.foreach(_.poke(false.B))
    phase.odt.foreach(_.poke(false.B))
    phase.resetN.poke(true.B)
    phase.rddataEn.poke(false.B)
    phase.wrdataEn.poke(false.B)
    phase.wrdata.poke(0.U)
    phase.wrdataMask.poke(0.U)
    phase.rddata.poke(0.U)
    phase.rddataValid.poke(false.B)
  }

  behavior of "Ddr4DfiMux"

  it should "move ACTIVATE address bits onto DDR4 RAS/CAS/WE and assert ACT_n" in {
    test(new Ddr4DfiMux(cfg)) { dut =>
      dut.io.input.phases.foreach(clearPhase)
      val activate = dut.io.input.phases(0)
      activate.rasN.poke(false.B)
      activate.casN.poke(true.B)
      activate.weN.poke(true.B)
      activate.address.poke(((BigInt(1) << 16) | (BigInt(1) << 14)).U)
      dut.io.output.phases(0).actN.expect(false.B)
      dut.io.output.phases(0).rasN.expect(true.B)
      dut.io.output.phases(0).casN.expect(false.B)
      dut.io.output.phases(0).weN.expect(true.B)

      activate.casN.poke(false.B)
      dut.io.output.phases(0).actN.expect(true.B)
      dut.io.output.phases(0).rasN.expect(false.B)
      dut.io.output.phases(0).casN.expect(false.B)
      dut.io.output.phases(0).weN.expect(true.B)
    }
  }

  behavior of "DfiInjector"

  it should "select hardware/external DFI and issue software commands for one cycle" in {
    test(new DfiInjector(cfg)) { dut =>
      dut.io.hardware.phases.foreach(clearPhase)
      dut.io.external.phases.foreach(clearPhase)
      dut.io.clockEnable.poke(true.B)
      dut.io.onDieTermination.poke(false.B)
      dut.io.resetN.poke(true.B)
      for (index <- 0 until cfg.nPhases) {
        val software = dut.io.software(index)
        software.issue.poke(false.B)
        software.chipSelect.poke(false.B)
        software.writeEnable.poke(false.B)
        software.columnStrobe.poke(false.B)
        software.rowStrobe.poke(false.B)
        software.writeDataEnable.poke(false.B)
        software.readDataEnable.poke(false.B)
        software.address.poke(0.U)
        software.bank.poke(0.U)
        software.writeData.poke(0.U)
        dut.io.phyRead(index).data.poke(0.U)
        dut.io.phyRead(index).valid.poke(false.B)
      }

      dut.io.hardware.phases(0).address.poke(3.U)
      dut.io.external.phases(0).address.poke(9.U)
      dut.io.hardwareControl.poke(true.B)
      dut.io.useExternal.poke(false.B)
      dut.io.master.phases(0).address.expect(3.U)
      dut.io.useExternal.poke(true.B)
      dut.io.master.phases(0).address.expect(9.U)

      dut.io.hardwareControl.poke(false.B)
      val software = dut.io.software(1)
      software.issue.poke(true.B)
      software.chipSelect.poke(true.B)
      software.writeEnable.poke(true.B)
      software.columnStrobe.poke(true.B)
      software.rowStrobe.poke(false.B)
      software.writeDataEnable.poke(true.B)
      software.address.poke(0x123.U)
      software.bank.poke(1.U)
      software.writeData.poke("hbabe".U)
      dut.io.master.phases(1).csN(0).expect(false.B)
      dut.io.master.phases(1).weN.expect(false.B)
      dut.io.master.phases(1).casN.expect(false.B)
      dut.io.master.phases(1).rasN.expect(true.B)
      dut.io.master.phases(1).wrdataEn.expect(true.B)
      dut.io.master.phases(1).wrdata.expect("hbabe".U)
      software.issue.poke(false.B)
      dut.io.master.phases(1).csN(0).expect(true.B)
      dut.io.master.phases(1).wrdataEn.expect(false.B)

      dut.io.phyRead(0).data.poke("h5678".U)
      dut.io.phyRead(0).valid.poke(true.B)
      dut.io.master.phases(0).rddata.expect("h5678".U)
      dut.clock.step()
      dut.io.phyRead(0).valid.poke(false.B)
      dut.io.capturedRead(0).expect("h5678".U)
    }
  }

  behavior of "DfiRateConverter"

  it should "serialize command phases and align packed write/read data windows" in {
    test(new DfiRateConverterHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.slow.phases.foreach(clearPhase)
      for (phase <- 0 until 4) {
        dut.io.slow.phases(phase).address.poke((10 + phase).U)
        dut.io.slow.phases(phase).bank.poke((phase & 1).U)
        dut.io.slow.phases(phase).wrdata.poke((0x1000 + phase).U)
        dut.io.slow.phases(phase).wrdataMask.poke((phase & 3).U)
      }
      dut.io.fastRead(0).data.poke("haabbccdd".U)
      dut.io.fastRead(0).valid.poke(true.B)
      dut.io.fastRead(1).data.poke("h11223344".U)
      dut.io.fastRead(1).valid.poke(true.B)

      while (dut.io.slot.peek().litValue != 0) dut.clock.step()
      dut.io.fast.phases(0).address.expect(10.U)
      dut.io.fast.phases(1).address.expect(11.U)
      dut.io.fast.phases(0).wrdata.expect(0.U)
      dut.clock.step()

      dut.io.slot.expect(1.U)
      dut.io.fast.phases(0).address.expect(12.U)
      dut.io.fast.phases(1).address.expect(13.U)
      dut.io.fast.phases(0).wrdata.expect("h10011000".U)
      dut.io.fast.phases(1).wrdata.expect("h10031002".U)
      dut.io.fast.phases(0).wrdataMask.expect("h4".U)
      dut.io.fast.phases(1).wrdataMask.expect("he".U)
      dut.io.slowRead(0).data.expect("hccdd".U)
      dut.io.slowRead(1).data.expect("haabb".U)
      dut.io.slowRead(2).data.expect("h3344".U)
      dut.io.slowRead(3).data.expect("h1122".U)
      dut.io.slowRead.foreach(_.valid.expect(true.B))

      dut.io.fastRead.foreach(_.valid.poke(false.B))
      dut.clock.step()
      dut.io.slot.expect(0.U)
      dut.clock.step()
      dut.io.slowRead.foreach(_.valid.expect(false.B))
    }
  }

  it should "match a ratio-four slot and data-packing oracle over random windows" in {
    test(new DfiRateConverterRatio4Harness).withAnnotations(Seq(
      VerilatorBackendAnnotation, VerilatorCFlags(Seq("-DWData=IData")))) { dut =>
      dut.io.slow.phases.foreach(clearPhase)
      dut.io.fastRead(0).data.poke(0.U)
      dut.io.fastRead(0).valid.poke(false.B)
      while (dut.io.slot.peek().litValue != 0) dut.clock.step()
      val random = new Random(0x52415445)

      for (window <- 0 until 80) {
        val addresses = Seq.fill(4)(random.nextInt(1024))
        val banks = Seq.fill(4)(random.nextInt(2))
        val writes = Seq.fill(4)(random.nextInt(1 << 16))
        val masks = Seq.fill(4)(random.nextInt(4))
        val readWord = BigInt(64, random)
        val readValid = random.nextBoolean()
        for (phase <- 0 until 4) {
          val slow = dut.io.slow.phases(phase)
          slow.address.poke(addresses(phase).U)
          slow.bank.poke(banks(phase).U)
          slow.csN(0).poke(false.B)
          slow.rasN.poke((phase % 2 == 0).B)
          slow.casN.poke((phase % 3 == 0).B)
          slow.weN.poke((phase % 4 == 0).B)
          slow.rddataEn.poke((phase == 1).B)
          slow.wrdataEn.poke((phase == 2).B)
          slow.wrdata.poke(writes(phase).U)
          slow.wrdataMask.poke(masks(phase).U)
        }
        dut.io.fastRead(0).data.poke(readWord.U)
        dut.io.fastRead(0).valid.poke(readValid.B)

        val packedWrite = writes.zipWithIndex.foldLeft(BigInt(0)) {
          case (value, (lane, index)) => value | (BigInt(lane) << (16 * index))
        }
        val packedMask = masks.zipWithIndex.foldLeft(BigInt(0)) {
          case (value, (lane, index)) => value | (BigInt(lane) << (2 * index))
        }
        for (slot <- 0 until 4) {
          dut.io.slot.expect(slot.U)
          val fast = dut.io.fast.phases(0)
          fast.address.expect(addresses(slot).U)
          fast.bank.expect(banks(slot).U)
          fast.rasN.expect((slot % 2 == 0).B)
          fast.casN.expect((slot % 3 == 0).B)
          fast.weN.expect((slot % 4 == 0).B)
          fast.rddataEn.expect((slot == 1).B)
          fast.wrdataEn.expect((slot == 2).B)
          fast.wrdata.expect((if (slot == 2) packedWrite else BigInt(0)).U)
          fast.wrdataMask.expect((if (slot == 2) packedMask else BigInt(0)).U)
          dut.clock.step()
        }
        for (lane <- 0 until 4) {
          val expected = (readWord >> (16 * lane)) & 0xffff
          dut.io.slowRead(lane).data.expect(expected.U)
          dut.io.slowRead(lane).valid.expect(readValid.B)
        }
      }
    }
  }
}
