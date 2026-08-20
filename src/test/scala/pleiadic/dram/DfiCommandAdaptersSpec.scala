package pleiadic.dram

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.language.reflectiveCalls

class DfiCommandAdaptersSpec extends AnyFlatSpec with ChiselScalatestTester {
  private case class CaVector(name: String, rasN: Boolean, casN: Boolean, weN: Boolean,
    address: Int, bank: Int, cs: Int, ca: Seq[Int], valid: Boolean = true,
    wckSync: Int = 0, selected: Boolean = true, wckSyncDone: Boolean = false)

  private case class RpcVector(name: String, rasN: Boolean, casN: Boolean, weN: Boolean,
    address: Int, bank: Int, dbPositive: Int, dbNegative: Int, valid: Boolean = true,
    burstCount: Int = 0, refreshOperation: Int = 0, resetN: Boolean = true)

  private def pokePhase(phase: DfiPhase, address: Int, bank: Int, rasN: Boolean,
      casN: Boolean, weN: Boolean, selected: Boolean = true, resetN: Boolean = true): Unit = {
    phase.address.poke(address.U)
    phase.bank.poke(bank.U)
    phase.csN.foreach(_.poke(true.B))
    if (selected) phase.csN(0).poke(false.B)
    phase.rasN.poke(rasN.B)
    phase.casN.poke(casN.B)
    phase.weN.poke(weN.B)
    phase.actN.poke(true.B)
    phase.cke.foreach(_.poke(true.B))
    phase.odt.foreach(_.poke(false.B))
    phase.resetN.poke(resetN.B)
    phase.rddataEn.poke(false.B)
    phase.wrdataEn.poke(false.B)
    phase.wrdata.poke(0.U)
    phase.wrdataMask.poke(0.U)
    phase.rddata.poke(0.U)
    phase.rddataValid.poke(false.B)
  }

  behavior of "Lpddr4DfiPhaseAdapter"

  it should "match the LiteDRAM Migen command encoder" in {
    val config = DramConfig(addressBits = 40, dataBits = 16, bankBits = 6,
      rowBits = 17, columnBits = 10, memType = "LPDDR4")
    val vectors = Seq(
      CaVector("ACT", false, true, true, 0x15555, 0x2d, 5, Seq(21, 29, 23, 21)),
      CaVector("READ", true, false, true, 0x6d4, 0x2d, 5, Seq(2, 53, 18, 53)),
      CaVector("MASK WRITE", true, false, false, 0x6d4, 0x2d, 5, Seq(12, 53, 18, 53)),
      CaVector("PRECHARGE ALL", false, true, false, 0x400, 0x2d, 4, Seq(0, 0, 48, 5)),
      CaVector("REFRESH", false, false, true, 0x400, 0x2d, 4, Seq(0, 0, 40, 5)),
      CaVector("MPC", true, true, false, 0x51, 0, 4, Seq(0, 0, 32, 17)),
      CaVector("MRR", true, true, false, 0x55, 1, 5, Seq(14, 21, 18, 21)),
      CaVector("MRW", false, false, false, 0xd5, 0x2d, 5, Seq(38, 45, 54, 21)),
      CaVector("deselect", false, false, false, 0xd5, 0x2d, 0, Seq(0, 0, 0, 0),
        valid = false, selected = false)
    )
    test(new Lpddr4DfiPhaseAdapter(config)) { dut =>
      for (vector <- vectors) {
        pokePhase(dut.io.phase, vector.address, vector.bank, vector.rasN, vector.casN,
          vector.weN, vector.selected)
        dut.io.valid.expect(vector.valid.B, vector.name)
        dut.io.cs.expect(vector.cs.U, vector.name)
        vector.ca.zipWithIndex.foreach { case (value, edge) =>
          dut.io.ca(edge).expect(value.U, s"${vector.name} CA edge $edge")
        }
      }
    }
  }

  it should "select unmasked WRITE when requested" in {
    val config = DramConfig(addressBits = 40, dataBits = 16, bankBits = 6,
      rowBits = 17, columnBits = 10, memType = "LPDDR4")
    test(new Lpddr4DfiPhaseAdapter(config, maskedWrite = false)) { dut =>
      pokePhase(dut.io.phase, 0x6d4, 0x2d, rasN = true, casN = false, weN = false)
      dut.io.ca(0).expect(4.U)
      dut.io.ca(1).expect(53.U)
    }
  }

  behavior of "Lpddr5DfiPhaseAdapter"

  it should "match the LiteDRAM Migen command encoder and WCK sync flags" in {
    val config = DramConfig(addressBits = 40, dataBits = 16, bankBits = 7,
      rowBits = 18, columnBits = 10, memType = "LPDDR5")
    val vectors = Seq(
      CaVector("ACT", false, true, true, 0x15555, 0x2d, 3, Seq(47, 45, 83, 85)),
      CaVector("READ sync", true, false, true, 0x6d4, 0x2d, 3, Seq(44, 0, 89, 109), wckSync = 2),
      CaVector("MASK WRITE sync", true, false, false, 0x6d4, 0x2d, 3, Seq(28, 0, 90, 109), wckSync = 1),
      CaVector("PRECHARGE ALL", false, true, false, 0x400, 0x2d, 2, Seq(0, 0, 120, 77)),
      CaVector("REFRESH", false, false, true, 0x400, 0x2d, 2, Seq(0, 0, 56, 69)),
      CaVector("MPC", true, true, false, 0x51, 0, 2, Seq(0, 0, 48, 81)),
      CaVector("MRR sync", true, true, false, 0x55, 1, 3, Seq(44, 0, 24, 85), wckSync = 2),
      CaVector("MRW", false, false, false, 0xd5, 0x2d, 3, Seq(88, 45, 72, 85)),
      CaVector("special NOP", true, true, false, 0, 2, 2, Seq(0, 0, 0, 0)),
      CaVector("MPC zero aliases ZQC latch", true, true, false, 0, 0, 2, Seq(0, 0, 112, 6)),
      CaVector("already synced READ", true, false, true, 0x6d4, 0x2d, 3,
        Seq(12, 0, 89, 109), wckSyncDone = true),
      CaVector("deselect", false, false, false, 0xd5, 0x2d, 0, Seq(0, 0, 0, 0),
        valid = false, selected = false)
    )
    test(new Lpddr5DfiPhaseAdapter(config)) { dut =>
      for (vector <- vectors) {
        pokePhase(dut.io.phase, vector.address, vector.bank, vector.rasN, vector.casN,
          vector.weN, vector.selected)
        dut.io.wckSyncDone.poke(vector.wckSyncDone.B)
        dut.io.valid.expect(vector.valid.B, vector.name)
        dut.io.cs.expect(vector.cs.U, vector.name)
        dut.io.wckSync.expect(vector.wckSync.U, vector.name)
        vector.ca.zipWithIndex.foreach { case (value, edge) =>
          dut.io.ca(edge).expect(value.U, s"${vector.name} CA edge $edge")
        }
      }
    }
  }

  it should "select unmasked WRITE16 when requested" in {
    val config = DramConfig(addressBits = 40, dataBits = 16, bankBits = 7,
      rowBits = 18, columnBits = 10, memType = "LPDDR5")
    test(new Lpddr5DfiPhaseAdapter(config, maskedWrite = false)) { dut =>
      pokePhase(dut.io.phase, 0x6d4, 0x2d, rasN = true, casN = false, weN = false)
      dut.io.wckSyncDone.poke(false.B)
      dut.io.ca(2).expect(94.U)
      dut.io.ca(3).expect(109.U)
    }
  }

  behavior of "RpcDfiAdapter"

  it should "match LiteDRAM Request Packets including special reset and utility commands" in {
    val config = DramConfig(addressBits = 28, dataBits = 16, bankBits = 2,
      rowBits = 12, columnBits = 10, memType = "RPC")
    val vectors = Seq(
      RpcVector("ACT", false, true, true, 0xa55, 2, 21, 5290),
      RpcVector("READ", true, false, true, 0x3d0, 1, 42152, 57344, burstCount = 0x25),
      RpcVector("WRITE", true, false, false, 0x3d0, 1, 42153, 57344, burstCount = 0x25),
      RpcVector("PRE bank", false, true, false, 0, 2, 260, 0),
      RpcVector("PRE all", false, true, false, 0x400, 2, 964, 0),
      RpcVector("REF", false, false, true, 0x400, 0, 966, 4, refreshOperation = 2),
      RpcVector("ZQC short", true, true, false, 0, 0, 32769, 1),
      RpcVector("ZQC init", true, true, false, 0x400, 0, 1, 1, resetN = false),
      RpcVector("MRS", false, false, false, 0x6ad, 3, 43562, 28672),
      RpcVector("RESET", false, true, true, 0, 0, 0, 1, resetN = false),
      RpcVector("UTR", false, false, false, 5, 0, 47, 0, resetN = false),
      RpcVector("NOP", true, true, true, 0, 0, 0, 0, valid = false)
    )
    test(new RpcDfiAdapter(config)) { dut =>
      for (vector <- vectors) {
        pokePhase(dut.io.phase, vector.address, vector.bank, vector.rasN, vector.casN,
          vector.weN, resetN = vector.resetN)
        dut.io.burstCount.poke(vector.burstCount.U)
        dut.io.refreshOperation.poke(vector.refreshOperation.U)
        dut.io.commandValid.expect(vector.valid.B, vector.name)
        dut.io.dbPositive.expect(vector.dbPositive.U, vector.name)
        dut.io.dbNegative.expect(vector.dbNegative.U, vector.name)
        dut.io.utilityEnable.expect((vector.address & 1).B, vector.name)
      }
    }
  }
}
