package pleiadic.dram

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DramDeviceSpec extends AnyFlatSpec with Matchers {
  behavior of "DramDeviceCatalog"

  it should "contain every concrete module in the pinned LiteDRAM catalog" in {
    DramDeviceCatalog.devices.size shouldBe 74
    DramDeviceCatalog.byName.size shouldBe 74
    DramDeviceCatalog.devices.groupBy(_.memType).view.mapValues(_.size).toMap shouldBe Map(
      "SDR" -> 22, "DDR" -> 1, "LPDDR" -> 4, "DDR2" -> 5,
      "DDR3" -> 25, "RPC" -> 1, "DDR4" -> 15, "LPDDR4" -> 1)
  }

  it should "preserve geometry registration speedgrades and timing variants" in {
    val ddr4 = DramDeviceCatalog("MTA4ATF51264HZ")
    ddr4.geometry shouldBe DramSpdGeometry(3, 16, 10)
    ddr4.registered shouldBe false
    ddr4.speedgrades shouldBe Seq("2133")
    ddr4.timing("tREFI", fineRefreshMode = "4x") shouldBe
      Some(SpdTiming(0, 1953.125))
    ddr4.timing("tRFC", speedgrade = "2133", fineRefreshMode = "2x") shouldBe
      Some(SpdTiming(0, 260.0))

    val rdimm = DramDeviceCatalog("MTA18ASF2G72PZ")
    rdimm.registered shouldBe true
    rdimm.geometry.bankBits shouldBe 4
    rdimm.speedgrades shouldBe Seq("2400", "2666", "2933", "3200")

    val lpddr4 = DramDeviceCatalog("MT53E256M16D1")
    lpddr4.memType shouldBe "LPDDR4"
    lpddr4.timing("tCCD") shouldBe Some(SpdTiming(32, 0.0))
  }

  it should "retain unknown timings as absent and reject unknown lookup keys" in {
    DramDeviceCatalog("IS42S16160").timing("tFAW") shouldBe None
    an[IllegalArgumentException] should be thrownBy DramDeviceCatalog("not-a-device")
    an[IllegalArgumentException] should be thrownBy
      DramDeviceCatalog("MTA4ATF51264HZ").timing("tRP", speedgrade = "9999")
  }

  it should "convert a catalog speedgrade into controller timing cycles" in {
    val device = DramDeviceCatalog("MTA4ATF51264HZ")
    device.toDramTiming(controllerClockHz = 100e6, nPhases = 4,
      speedgrade = "2133") shouldBe
      DramTiming(tRcd = 3, tRp = 3, tRas = 5, tRc = 6, tCcd = 1,
        tWr = 3, tWtr = 2, tRtp = 1, tRrd = 2, tFaw = 7,
        tRefi = 782, tRfc = 36, tZqcs = Some(32))
    val refresh2x = device.toDramTiming(controllerClockHz = 100e6,
      nPhases = 4, speedgrade = "2133", fineRefreshMode = "2x")
    refresh2x.tRefi shouldBe 391
    refresh2x.tRfc shouldBe 27
  }
}
