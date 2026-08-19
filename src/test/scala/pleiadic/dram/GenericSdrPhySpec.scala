package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class GenericSdrPhySpec extends AnyFlatSpec with ChiselScalatestTester {
  private val config = DramConfig(addressBits = 18, dataBits = 16,
    bankBits = 2, rowBits = 11, columnBits = 3, memType = "SDR",
    nPhases = 1, timing = DramTiming(tRefi = 100))

  private def clear(dut: GenericSdrPhy): Unit = {
    val phase = dut.io.dfi.phases(0)
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
    dut.io.dqIn.poke(0.U)
  }

  behavior of "GenericSdrPhy"

  it should "forward commands and gate the SDR write datapath and mask" in {
    test(new GenericSdrPhy(config, casLatency = 2)) { dut =>
      clear(dut)
      val phase = dut.io.dfi.phases(0)
      phase.address.poke("h456".U)
      phase.bank.poke(2.U)
      phase.csN(0).poke(false.B)
      phase.rasN.poke(false.B)
      phase.casN.poke(true.B)
      phase.weN.poke(false.B)
      phase.cke(0).poke(true.B)
      phase.wrdata.poke("hbeef".U)
      phase.wrdataMask.poke("b10".U)

      dut.io.pads.address.expect("h456".U)
      dut.io.pads.bank.expect(2.U)
      dut.io.pads.csN(0).expect(false.B)
      dut.io.pads.rasN.expect(false.B)
      dut.io.pads.casN.expect(true.B)
      dut.io.pads.weN.expect(false.B)
      dut.io.pads.cke(0).expect(true.B)
      dut.io.pads.dqOut.expect("hbeef".U)
      dut.io.pads.dqOutputEnable.expect(false.B)
      dut.io.pads.dataMask.expect(0.U)

      phase.wrdataEn.poke(true.B)
      dut.io.pads.dqOutputEnable.expect(true.B)
      dut.io.pads.dataMask.expect("b10".U)
    }
  }

  it should "delay read-valid by CL plus one while exposing pad input data" in {
    test(new GenericSdrPhy(config, casLatency = 2)) { dut =>
      clear(dut)
      dut.io.dqIn.poke("h1234".U)
      dut.io.read.data.expect("h1234".U)
      dut.io.dfi.phases(0).rddataEn.poke(true.B)
      dut.io.read.valid.expect(false.B)
      dut.clock.step()
      dut.io.dfi.phases(0).rddataEn.poke(false.B)
      dut.io.read.valid.expect(false.B)
      dut.clock.step()
      dut.io.read.valid.expect(false.B)
      dut.clock.step()
      dut.io.read.valid.expect(true.B)
      dut.clock.step()
      dut.io.read.valid.expect(false.B)
    }
  }

  it should "tie the mask inactive when the pad has no mask pins" in {
    test(new GenericSdrPhy(config, casLatency = 2, withDataMask = false)) { dut =>
      clear(dut)
      dut.io.dfi.phases(0).wrdataEn.poke(true.B)
      dut.io.dfi.phases(0).wrdataMask.poke(3.U)
      dut.io.pads.dataMask.expect(0.U)
    }
  }
}
