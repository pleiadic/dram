package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

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
}
