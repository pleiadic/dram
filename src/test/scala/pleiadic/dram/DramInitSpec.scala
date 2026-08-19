package pleiadic.dram

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DramInitSpec extends AnyFlatSpec with Matchers {
  import DfiiEncoding._

  private val timing = DramTiming(tWtr = 2)

  behavior of "DramInit"

  it should "match the LiteDRAM SDR initialization golden" in {
    val result = DramInit.generate(DramInitSettings("SDR", casLatency = 2), timing)
    result.modeRegisters shouldBe empty
    result.steps shouldBe Seq(
      DramInitStep("Bring CKE high", 0, 0, clockEnable, 20000),
      DramInitStep("Precharge All", 1024, 0, prechargeAll, 0),
      DramInitStep("Load Mode Register / Reset DLL, CL=2, BL=1", 288, 0, modeRegister, 200),
      DramInitStep("Precharge All", 1024, 0, prechargeAll, 0),
      DramInitStep("Auto Refresh", 0, 0, autoRefresh, 4),
      DramInitStep("Auto Refresh", 0, 0, autoRefresh, 4),
      DramInitStep("Load Mode Register / CL=2, BL=1", 32, 0, modeRegister, 200))
  }

  it should "match the LiteDRAM DDR3 initialization golden and exported MR1" in {
    val settings = DramInitSettings("DDR3", casLatency = 7,
      writeLatency = 6, nPhases = 4)
    val result = DramInit.generate(settings, timing)
    result.steps shouldBe Seq(
      DramInitStep("Release reset", 0, 0, unreset, 50000),
      DramInitStep("Bring CKE high", 0, 0, clockEnable, 10000),
      DramInitStep("Load Mode Register 2, CWL=6", 520, 2, modeRegister, 0),
      DramInitStep("Load Mode Register 3", 0, 3, modeRegister, 0),
      DramInitStep("Load Mode Register 1", 6, 1, modeRegister, 0),
      DramInitStep("Load Mode Register 0, CL=7, BL=8", 2352, 0, modeRegister, 200),
      DramInitStep("ZQ Calibration", 1024, 0, zqCalibration, 200))
    result.modeRegisters shouldBe Map(1 -> BigInt(6))
  }

  it should "match the LiteDRAM DDR4 initialization golden and exported MR1" in {
    val settings = DramInitSettings("DDR4", casLatency = 9,
      writeLatency = 9, nPhases = 4)
    val result = DramInit.generate(settings, timing)
    result.steps shouldBe Seq(
      DramInitStep("Release reset", 0, 0, unreset, 50000),
      DramInitStep("Bring CKE high", 0, 0, clockEnable, 10000),
      DramInitStep("Load Mode Register 3", 0, 3, modeRegister, 0),
      DramInitStep("Load Mode Register 6", 0, 6, modeRegister, 0),
      DramInitStep("Load Mode Register 5", 1024, 5, modeRegister, 0),
      DramInitStep("Load Mode Register 4", 0, 4, modeRegister, 0),
      DramInitStep("Load Mode Register 2, CWL=9", 512, 2, modeRegister, 0),
      DramInitStep("Load Mode Register 1", 769, 1, modeRegister, 0),
      DramInitStep("Load Mode Register 0, CL=9, BL=8", 256, 0, modeRegister, 200),
      DramInitStep("ZQ Calibration", 1024, 0, zqCalibration, 200))
    result.modeRegisters shouldBe Map(1 -> BigInt(769))
  }

  it should "cover DDR, LPDDR, and DDR2 sequence differences" in {
    val ddr = DramInit.generate(DramInitSettings("DDR", 3), timing)
    val lpddr = DramInit.generate(DramInitSettings("LPDDR", 3), timing)
    ddr.steps(2).bank shouldBe 1
    lpddr.steps(2).bank shouldBe 2

    val ddr2 = DramInit.generate(DramInitSettings("DDR2", 3), timing)
    ddr2.steps.map(_.bank).slice(2, 5) shouldBe Seq(3, 2, 1)
    ddr2.steps.takeRight(2).map(_.address) shouldBe Seq(BigInt(896), BigInt(0))
  }

  it should "match the LiteDRAM RPC initialization sequence" in {
    val result = DramInit.generate(DramInitSettings("RPC", casLatency = 9,
      writeLatency = 9), timing)
    result.steps shouldBe Seq(
      DramInitStep("Stabilize clocks", 0, 0, unreset, 40000),
      DramInitStep("Hold CS# low", 0, 0, commandChipSelect, 20),
      DramInitStep("RPC special commands: ON", 0, 0, controlOdt, 20),
      DramInitStep("PU RESET sequence (ACT)", 0, 0,
        commandRowStrobe | commandChipSelect, 1001),
      DramInitStep("RPC special commands: OFF", 0, 0, unreset, 20),
      DramInitStep("Precharge ALL", 1024, 0, prechargeAll, 20),
      DramInitStep("Load Mode Register: CL=8", 576, 1, modeRegister, 20),
      DramInitStep("RPC special commands: ON", 0, 0, controlOdt, 20),
      DramInitStep("ZQ Init Calibration", 1024, 0, zqCalibration, 200),
      DramInitStep("RPC special commands: OFF", 0, 0, unreset, 20))
    result.modeRegisters shouldBe empty
  }

  it should "match the LiteDRAM LPDDR4 initialization sequence and mode registers" in {
    val result = DramInit.generate(DramInitSettings("LPDDR4", casLatency = 14,
      writeLatency = 8), timing)
    result.modeRegisters shouldBe Map(
      1 -> BigInt(36), 2 -> BigInt(18), 3 -> BigInt(17), 11 -> BigInt(34),
      12 -> BigInt(85), 13 -> BigInt(0), 14 -> BigInt(85))
    result.steps shouldBe Seq(
      DramInitStep("Assert reset", 0, 0, controlOdt, 20),
      DramInitStep("Release reset", 0, 0, unreset, 400000),
      DramInitStep("Bring CKE high", 0, 0, clockEnable, 400),
      DramInitStep("Load More Register 1", 36, 1, modeRegister, 200),
      DramInitStep("Load More Register 2", 18, 2, modeRegister, 200),
      DramInitStep("Load More Register 3", 17, 3, modeRegister, 200),
      DramInitStep("Load More Register 11", 34, 11, modeRegister, 200),
      DramInitStep("Load More Register 12", 85, 12, modeRegister, 200),
      DramInitStep("Load More Register 13", 0, 13, modeRegister, 200),
      DramInitStep("Load More Register 14", 85, 14, modeRegister, 200),
      DramInitStep("ZQ Calibration start", 79, 0, zqCalibration, 200),
      DramInitStep("ZQ Calibration latch", 81, 0, zqCalibration, 8))
  }

  it should "match the LiteDRAM LPDDR5 initialization sequence and mode registers" in {
    val result = DramInit.generate(DramInitSettings("LPDDR5", casLatency = 10,
      writeLatency = 6, wckCkRatio = 2, vrefCaPercent = 34.0,
      vrefDqPercent = 34.0), timing)
    result.modeRegisters shouldBe Map(
      1 -> BigInt(32), 2 -> BigInt(34), 3 -> BigInt(18), 10 -> BigInt(0),
      11 -> BigInt(34), 12 -> BigInt(48), 13 -> BigInt(0), 14 -> BigInt(48),
      15 -> BigInt(48), 17 -> BigInt(56), 18 -> BigInt(128), 20 -> BigInt(1),
      22 -> BigInt(0), 28 -> BigInt(4))
    result.steps shouldBe Seq(
      DramInitStep("Assert reset", 0, 0, controlOdt, 40000),
      DramInitStep("Release reset", 0, 0, unreset, 400005),
      DramInitStep("Toggle CS", 0, 2, zqCalibration, 400),
      DramInitStep("Load More Register 1", 32, 1, modeRegister, 200),
      DramInitStep("Load More Register 2", 34, 2, modeRegister, 200),
      DramInitStep("Load More Register 3", 18, 3, modeRegister, 200),
      DramInitStep("Load More Register 10", 0, 10, modeRegister, 200),
      DramInitStep("Load More Register 11", 34, 11, modeRegister, 200),
      DramInitStep("Load More Register 12", 48, 12, modeRegister, 200),
      DramInitStep("Load More Register 13", 0, 13, modeRegister, 200),
      DramInitStep("Load More Register 14", 48, 14, modeRegister, 200),
      DramInitStep("Load More Register 15", 48, 15, modeRegister, 200),
      DramInitStep("Load More Register 17", 56, 17, modeRegister, 200),
      DramInitStep("Load More Register 18", 128, 18, modeRegister, 200),
      DramInitStep("Load More Register 20", 1, 20, modeRegister, 200),
      DramInitStep("Load More Register 22", 0, 22, modeRegister, 200),
      DramInitStep("Load More Register 28", 4, 28, modeRegister, 200),
      DramInitStep("ZQ Calibration latch", 134, 0, zqCalibration, 6))
  }

  it should "cover alternate LPDDR4 VREF and LPDDR5 WCK ratio encodings" in {
    val lpddr4 = DramInit.generate(DramInitSettings("LPDDR4", 6,
      writeLatency = 4, vrefCaRange = 0, vrefCaPercent = 10.0), timing)
    lpddr4.modeRegisters(1) shouldBe BigInt(4)
    lpddr4.modeRegisters(2) shouldBe BigInt(0)
    lpddr4.modeRegisters(12) shouldBe BigInt(0)

    val lpddr5 = DramInit.generate(DramInitSettings("LPDDR5", 17,
      writeLatency = 9, wckCkRatio = 4, vrefCaPercent = 34.25), timing)
    lpddr5.modeRegisters(1) shouldBe BigInt(176)
    lpddr5.modeRegisters(2) shouldBe BigInt(187)
    lpddr5.modeRegisters(12) shouldBe BigInt(48)
    lpddr5.modeRegisters(18) shouldBe BigInt(0)
  }

  it should "reject mode-register encodings outside the JEDEC tables" in {
    an[IllegalArgumentException] should be thrownBy
      DramInit.generate(DramInitSettings("DDR4", 8, writeLatency = 9), timing)
    an[IllegalArgumentException] should be thrownBy DramInit.generate(
      DramInitSettings("LPDDR4", 14, writeLatency = 10), timing)
    an[IllegalArgumentException] should be thrownBy
      DramInit.generate(DramInitSettings("RPC", 9, writeLatency = 8), timing)
    an[IllegalArgumentException] should be thrownBy DramInit.generate(
      DramInitSettings("LPDDR5", 10, writeLatency = 8, wckCkRatio = 2), timing)
    an[IllegalArgumentException] should be thrownBy DramInit.generate(
      DramInitSettings("LPDDR5", 10, writeLatency = 6,
        wckCkRatio = 2, vrefCaPercent = 14.0), timing)
  }

  it should "export deterministic C and Scala initialization tables" in {
    val result = DramInit.generate(DramInitSettings("DDR3", 7,
      writeLatency = 6, nPhases = 4), timing)
    val c = DramInitExport.toC(result, "ddr3-init")
    c should include("#define DDR3_INIT_MR1 0x6")
    c should include("static const dram_init_step_t ddr3_init[]")
    c should include("{ 0x208, 2, 0xf, 0 }, /* Load Mode Register 2, CWL=6 */")
    c should include("ddr3_init_count = 7")

    val scala = DramInitExport.toScala(result, "Ddr3Generated")
    scala should include("object Ddr3Generated")
    scala should include("DramInitStep(\"ZQ Calibration\", BigInt(\"1024\"), 0, 3, 200)")
    scala should include("modeRegisters = Map(1 -> BigInt(\"6\"))")
  }
}
