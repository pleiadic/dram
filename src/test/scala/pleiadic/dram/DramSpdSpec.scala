package pleiadic.dram

import java.nio.file.{Files, Paths}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.jdk.CollectionConverters._

class DramSpdSpec extends AnyFlatSpec with Matchers {
  private def reference(name: String): Seq[String] = {
    val path = Paths.get("../../litex/litedram/test/spd_data", name)
    Files.readAllLines(path).asScala.toSeq
  }

  behavior of "DramSpd"

  it should "match the LiteDRAM DDR3 Micron SPD golden" in {
    val spd = DramSpd.parseMicronCsv(reference("MT16KTF1G64HZ-1G6P1.csv"))
    spd.memType shouldBe "DDR3"
    spd.geometry shouldBe DramSpdGeometry(bankBits = 3, rowBits = 16, columnBits = 10)
    spd.speedgrade shouldBe 1600
    spd.speedgradeTimings("tRP").nanoseconds shouldBe 13.125 +- 1e-9
    spd.speedgradeTimings("tRCD").nanoseconds shouldBe 13.125 +- 1e-9
    (spd.speedgradeTimings("tRP").nanoseconds +
      spd.speedgradeTimings("tRAS").nanoseconds) shouldBe 48.125 +- 1e-9
    spd.technologyTimings("tRRD") shouldBe SpdTiming(4, 6.0)
  }

  it should "match LiteDRAM DDR4 geometry speedgrade and fine-refresh timings" in {
    val lines = reference("MTA4ATF51264HZ-2G3B1.csv")
    val spd = DramSpd.parseMicronCsv(lines)
    spd.memType shouldBe "DDR4"
    spd.geometry shouldBe DramSpdGeometry(bankBits = 3, rowBits = 16,
      columnBits = 10, bankGroupBits = 1)
    spd.speedgrade shouldBe 2400
    spd.speedgradeTimings("tRP").nanoseconds shouldBe 13.75 +- 1e-9
    spd.speedgradeTimings("tRCD").nanoseconds shouldBe 13.75 +- 1e-9
    (spd.speedgradeTimings("tRP").nanoseconds +
      spd.speedgradeTimings("tRAS").nanoseconds) shouldBe 45.75 +- 1e-9

    val refresh2x = DramSpd.parseMicronCsv(lines, fineRefreshMode = "2x")
    refresh2x.technologyTimings("tREFI").nanoseconds shouldBe
      spd.technologyTimings("tREFI").nanoseconds / 2 +- 1e-9
    refresh2x.speedgradeTimings("tRFC").nanoseconds should be <
      spd.speedgradeTimings("tRFC").nanoseconds
  }

  it should "parse LiteX BIOS hexadecimal dumps and validate input" in {
    val bytes = Array.fill(48)(0)
    bytes(2) = 0x0b
    bytes(4) = 0x00
    bytes(5) = 0x00
    bytes(9) = 0x11
    bytes(10) = 1
    bytes(11) = 8
    bytes(12) = 10 // 1.25ns -> DDR3-1600
    val rows = bytes.grouped(16).zipWithIndex.map { case (row, index) =>
      f"0x${index * 16}%08x  ${row.map(value => f"$value%02x").mkString("  ")}"
    }.toSeq
    DramSpd.parseHexdump(rows).speedgrade shouldBe 1600

    an[IllegalArgumentException] should be thrownBy DramSpd.parse(Seq(0, 0, 0xff))
    an[IllegalArgumentException] should be thrownBy DramSpd.parseMicronCsv(Seq("header"))
  }

  it should "match LiteDRAM controller-cycle conversion for DDR3 and DDR4" in {
    val ddr3 = DramSpd.parseMicronCsv(reference("MT16KTF1G64HZ-1G6P1.csv"))
    DramSpd.toDramTiming(ddr3, controllerClockHz = 125e6, nPhases = 4) shouldBe
      DramTiming(tRcd = 3, tRp = 3, tRas = 6, tRc = 7, tCcd = 1,
        tWr = 3, tWtr = 2, tRtp = 1, tRrd = 2, tFaw = 5,
        tRefi = 977, tRfc = 34, tZqcs = Some(16))

    val ddr4Lines = reference("MTA4ATF51264HZ-2G3B1.csv")
    val ddr4 = DramSpd.parseMicronCsv(ddr4Lines)
    DramSpd.toDramTiming(ddr4, controllerClockHz = 100e6, nPhases = 4) shouldBe
      DramTiming(tRcd = 3, tRp = 3, tRas = 4, tRc = 6, tCcd = 2,
        tWr = 3, tWtr = 2, tRtp = 1, tRrd = 2, tFaw = 7,
        tRefi = 782, tRfc = 36, tZqcs = Some(32))

    val ddr4Refresh2x = DramSpd.parseMicronCsv(ddr4Lines, fineRefreshMode = "2x")
    val refresh2x = DramSpd.toDramTiming(ddr4Refresh2x,
      controllerClockHz = 100e6, nPhases = 4)
    refresh2x.tRefi shouldBe 391
    refresh2x.tRfc shouldBe 27
  }
}
