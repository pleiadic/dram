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

  it should "reject mode-register encodings outside the JEDEC tables" in {
    an[IllegalArgumentException] should be thrownBy
      DramInit.generate(DramInitSettings("DDR4", 8, writeLatency = 9), timing)
    an[IllegalArgumentException] should be thrownBy
      DramInit.generate(DramInitSettings("LPDDR4", 14, writeLatency = 8), timing)
  }
}
