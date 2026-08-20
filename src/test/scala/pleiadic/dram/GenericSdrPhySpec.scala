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

class HalfRateGenericSdrPhyHarness extends Module {
  private val config = DramConfig(addressBits = 20, dataBits = 32,
    bankBits = 2, rowBits = 11, columnBits = 3, memType = "SDR",
    nPhases = 2, padDataBits = 16, timing = DramTiming(tRefi = 100))
  private val slowClockLevel = RegInit(false.B)
  slowClockLevel := !slowClockLevel
  private val phy = Module(new HalfRateGenericSdrPhy(config, casLatency = 2))
  val io = IO(new Bundle {
    val dfi = Input(new DfiInterface(config))
    val dqIn = Input(UInt(16.W))
    val pads = Output(new GenericSdrPads(config))
    val read = Output(Vec(2, new DfiReadResponse(config)))
    val slot = Output(Bool())
    val slowClockLevel = Output(Bool())
  })
  phy.io.fastClock := clock
  phy.io.slowClock := slowClockLevel.asClock
  phy.io.reset := reset.asAsyncReset
  phy.io.dfi := io.dfi
  phy.io.dqIn := io.dqIn
  io.pads := phy.io.pads
  io.read := phy.io.read
  io.slot := phy.io.slot
  io.slowClockLevel := slowClockLevel
}

class HalfRateGenericSdrPhySpec extends AnyFlatSpec with ChiselScalatestTester {
  private val verilator = Seq(
    chiseltest.simulator.VerilatorBackendAnnotation,
    chiseltest.simulator.VerilatorCFlags(Seq("-DWData=IData")))

  private def clear(dut: HalfRateGenericSdrPhyHarness): Unit = {
    for (phase <- dut.io.dfi.phases) {
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
    dut.io.dqIn.poke(0.U)
  }

  behavior of "HalfRateGenericSdrPhy"

  it should "serialize both command/data phases and capture both read samples" in {
    test(new HalfRateGenericSdrPhyHarness).withAnnotations(verilator) { dut =>
      clear(dut)
      dut.reset.poke(true.B)
      dut.clock.step(3)
      dut.reset.poke(false.B)
      for (index <- 0 until 2) {
        dut.io.dfi.phases(index).address.poke((0x120 + index).U)
        dut.io.dfi.phases(index).bank.poke(index.U)
        dut.io.dfi.phases(index).csN(0).poke(false.B)
        dut.io.dfi.phases(index).wrdata.poke((0x1111 * (index + 1)).U)
        dut.io.dfi.phases(index).wrdataMask.poke(index.U)
      }
      dut.io.dfi.phases(0).wrdataEn.poke(true.B)

      while (dut.io.slot.peek().litToBoolean) dut.clock.step()
      dut.io.pads.address.expect("h120".U)
      dut.io.pads.dqOut.expect("h1111".U)
      dut.io.pads.dqOutputEnable.expect(true.B)
      dut.io.dqIn.poke("haaaa".U)
      dut.clock.step()

      dut.io.slot.expect(true.B)
      dut.io.pads.address.expect("h121".U)
      dut.io.pads.dqOut.expect("h2222".U)
      dut.io.pads.dataMask.expect(1.U)
      dut.io.dqIn.poke("hbbbb".U)
      dut.clock.step()
      dut.io.read(0).data.expect("haaaa".U)
      dut.io.read(1).data.expect("hbbbb".U)
    }
  }

  it should "delay read-valid by the half-rate PHY latency" in {
    test(new HalfRateGenericSdrPhyHarness).withAnnotations(verilator) { dut =>
      clear(dut)
      dut.reset.poke(true.B)
      dut.clock.step(3)
      dut.reset.poke(false.B)

      dut.io.dfi.phases(0).rddataEn.poke(true.B)
      dut.io.read.foreach(_.valid.expect(false.B))
      dut.clock.step() // First rising slow-clock edge.
      dut.io.dfi.phases(0).rddataEn.poke(false.B)
      dut.io.read.foreach(_.valid.expect(false.B))
      dut.clock.step() // Falling slow-clock edge.
      dut.io.read.foreach(_.valid.expect(false.B))
      dut.clock.step() // Second rising slow-clock edge.
      dut.io.read.foreach(_.valid.expect(true.B))
      dut.clock.step()
      dut.io.read.foreach(_.valid.expect(true.B))
      dut.clock.step() // Next rising slow-clock edge shifts the pulse out.
      dut.io.read.foreach(_.valid.expect(false.B))
    }
  }
}
