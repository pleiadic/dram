package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class InterfaceSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "layered DRAM configuration"

  it should "round-trip Native geometry PHY and controller layers" in {
    val native = DramNativeConfig(addressBits = 40, dataBits = 64)
    val geometry = DramGeometryConfig(bankBits = 3, rowBits = 15,
      columnBits = 10, nranks = 2)
    val phy = DramPhyConfig(memType = "DDR3", nPhases = 2,
      phyDataBits = 64, padDataBits = 8, burstLength = 8,
      readLatency = 6, writeLatency = 2)
    val controller = DramControllerConfig(
      timing = DramTiming(tZqcs = Some(8)), withAutoPrecharge = true,
      addressMapping = DramAddressMapping.BankRowColumn,
      refreshPostponing = 4, cmdBufferDepth = 16,
      zqCalibrationPeriodCycles = Some(1024))
    val config = DramConfig.fromLayers(native, geometry, phy, controller)

    assert(config.native == native)
    assert(config.geometry == geometry)
    assert(config.phy == phy)
    assert(config.controller == controller)
    assert(config.dfiDataBits == 32)
    assert(config.effectivePadDataBits == 8)
    assert(config.rankBankCount == 16)
  }

  it should "validate both individual layers and their combined width contract" in {
    assertThrows[IllegalArgumentException](DramNativeConfig(dataBits = 12))
    assertThrows[IllegalArgumentException](DramGeometryConfig(nranks = 3))
    assertThrows[IllegalArgumentException](DramPhyConfig(memType = "GDDR6"))
    assertThrows[IllegalArgumentException](DramControllerConfig(refreshPostponing = 9))
    assertThrows[IllegalArgumentException](DramConfig.fromLayers(
      DramNativeConfig(addressBits = 32, dataBits = 64),
      DramGeometryConfig(),
      DramPhyConfig(memType = "DDR3", nPhases = 4, phyDataBits = 48)))
  }

  it should "cover SDR DDR3 and DDR4 across one two and four DFI phases" in {
    val matrix = Seq(
      ("SDR", 1, 1, 32, 32),
      ("DDR3", 2, 1, 64, 32),
      ("DDR4", 4, 2, 128, 32))
    for ((memoryType, phases, ranks, phyBits, expectedPhaseBits) <- matrix) {
      val config = DramConfig.fromLayers(
        DramNativeConfig(addressBits = 40, dataBits = 128),
        DramGeometryConfig(bankBits = 3, rowBits = 15,
          columnBits = 10, nranks = ranks),
        DramPhyConfig(memType = memoryType, nPhases = phases,
          phyDataBits = phyBits, padDataBits = 8,
          burstLength = if (memoryType == "SDR") 1 else 8))
      assert(config.dfiDataBits == expectedPhaseBits)
      assert(config.nPhases == phases)
      assert(config.nranks == ranks)
      assert(config.rankBankCount == ranks * 8)
    }
  }

  behavior of "AddressMapper"

  it should "decode row-bank-column addresses" in {
    val cfg = DramConfig(addressBits = 16, dataBits = 32, bankBits = 2, rowBits = 5, columnBits = 4)
    test(new AddressMapper(cfg)) { dut =>
      val row = 0x12
      val bank = 2
      val column = 9
      val address = (((row << cfg.bankBits) | bank) << cfg.columnBits | column) << cfg.byteOffsetBits
      dut.io.address.poke(address.U)
      dut.io.mapped.row.expect(row.U)
      dut.io.mapped.bank.expect(bank.U)
      dut.io.mapped.column.expect(column.U)
    }
  }

  behavior of "DfiSteerer"

  it should "encode an activate as active-low DFI signals" in {
    val cfg = DramConfig(addressBits = 16, dataBits = 32, bankBits = 2, rowBits = 5, columnBits = 4)
    test(new DfiSteerer(cfg)) { dut =>
      dut.io.command.valid.poke(true.B)
      dut.io.command.bits.command.poke(DramCommandType.activate)
      dut.io.command.bits.allBanks.poke(false.B)
      dut.io.command.bits.autoPrecharge.poke(false.B)
      dut.io.command.bits.rank.poke(0.U)
      dut.io.command.bits.bank.poke(2.U)
      dut.io.command.bits.row.poke(17.U)
      dut.io.command.bits.column.poke(0.U)
      dut.io.command.bits.data.poke(0.U)
      dut.io.command.bits.mask.poke(0.U)
      dut.io.dfi.phases(0).rasN.expect(false.B)
      dut.io.dfi.phases(0).casN.expect(true.B)
      dut.io.dfi.phases(0).weN.expect(true.B)
      dut.io.dfi.phases(0).bank.expect(2.U)
      dut.io.dfi.phases(0).address.expect(17.U)

      dut.io.command.bits.command.poke(DramCommandType.zqCalibration)
      dut.io.dfi.phases(0).rasN.expect(true.B)
      dut.io.dfi.phases(0).casN.expect(true.B)
      dut.io.dfi.phases(0).weN.expect(false.B)
    }
  }
}
