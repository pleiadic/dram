package pleiadic.dram

/** A JEDEC timing limit expressed as cycles, nanoseconds, or both. */
case class SpdTiming(cycles: Int = 0, nanoseconds: Double = 0.0)

case class DramSpdGeometry(
  bankBits: Int,
  rowBits: Int,
  columnBits: Int,
  bankGroupBits: Int = 0) {
  val banks: Int = 1 << bankBits
  val rows: Int = 1 << rowBits
  val columns: Int = 1 << columnBits
}

case class DramSpdData(
  memType: String,
  geometry: DramSpdGeometry,
  speedgrade: Int,
  technologyTimings: Map[String, SpdTiming],
  speedgradeTimings: Map[String, SpdTiming])

/** DDR3/DDR4 Serial Presence Detect decoder matching LiteDRAM modules.py. */
object DramSpd {
  private def field(byte: Int, bits: Int, shift: Int): Int =
    (byte >> shift) & ((1 << bits) - 1)
  private def upperNibble(byte: Int): Int = field(byte, 4, 4)
  private def lowerNibble(byte: Int): Int = field(byte, 4, 0)
  private def word(msb: Int, lsb: Int): Int = (msb << 8) | lsb
  private def signedByte(value: Int): Int = if ((value & 0x80) != 0) value - 256 else value

  private def select(name: String, value: Int, values: Map[Int, Int]): Int =
    values.getOrElse(value,
      throw new IllegalArgumentException(s"unsupported SPD $name encoding: $value"))

  private def speedgrade(tckNs: Double, supported: Seq[Int]): Int = {
    val transferRate = 2000.0 / tckNs
    supported.find(value => math.abs(transferRate - value) < 2.0).getOrElse(
      throw new IllegalArgumentException(
        f"SPD transfer rate $transferRate%.2f MT/s has no supported speedgrade"))
  }

  def parse(bytes: Seq[Int], fineRefreshMode: String = "1x"): DramSpdData = {
    require(bytes.length >= 3, "SPD data must contain the memory-type byte")
    require(bytes.forall(value => value >= 0 && value <= 0xff),
      "SPD bytes must be in the range 0..255")
    bytes(2) match {
      case 0x0b => parseDdr3(bytes)
      case 0x0c => parseDdr4(bytes, fineRefreshMode)
      case value => throw new IllegalArgumentException(f"unsupported SPD memory type: 0x$value%02x")
    }
  }

  /**
    * Convert SPD CK/ns limits to controller-clock cycles with LiteDRAM's
    * safety margin for a 1:nPhases controller/DRAM clock ratio.
    */
  def toDramTiming(data: DramSpdData, controllerClockHz: Double, nPhases: Int,
      readToPrecharge: SpdTiming = SpdTiming(cycles = 4)): DramTiming = {
    require(controllerClockHz > 0, "controller clock frequency must be positive")
    require(nPhases > 0, "DFI phase count must be positive")
    val controllerPeriodNs = 1e9 / controllerClockHz
    val marginNs = controllerPeriodNs * (1.0 - 1.0 / nPhases)

    def convert(timing: SpdTiming, margin: Boolean = true): Int = {
      val ckCycles = math.ceil(timing.cycles.toDouble / nPhases).toInt
      val ns = timing.nanoseconds + (if (margin) marginNs else 0.0)
      val nsCycles = math.ceil(ns / controllerPeriodNs).toInt
      math.max(ckCycles, nsCycles)
    }
    def technology(name: String): SpdTiming = data.technologyTimings.getOrElse(name,
      throw new IllegalArgumentException(s"SPD data is missing technology timing $name"))
    def speed(name: String): SpdTiming = data.speedgradeTimings.getOrElse(name,
      throw new IllegalArgumentException(s"SPD data is missing speedgrade timing $name"))
    def sum(left: SpdTiming, right: SpdTiming): SpdTiming =
      SpdTiming(left.cycles + right.cycles, left.nanoseconds + right.nanoseconds)

    val rp = speed("tRP")
    val ras = speed("tRAS")
    DramTiming(
      tRcd = convert(speed("tRCD")),
      tRp = convert(rp),
      tRas = convert(ras),
      tRc = convert(sum(rp, ras)),
      tCcd = convert(technology("tCCD")),
      tWr = convert(speed("tWR")),
      tWtr = convert(technology("tWTR")),
      tRtp = convert(readToPrecharge),
      tRrd = convert(technology("tRRD")),
      tFaw = convert(speed("tFAW")),
      tRefi = convert(technology("tREFI"), margin = false),
      tRfc = convert(speed("tRFC")),
      tZqcs = data.technologyTimings.get("tZQCS").map(convert(_)))
  }

  /** Parse Micron's four-column SPD CSV reference format. */
  def parseMicronCsv(lines: Seq[String], fineRefreshMode: String = "1x"): DramSpdData = {
    val values = Array.fill(512)(0)
    var found = false
    lines.drop(1).filter(_.trim.nonEmpty).foreach { line =>
      val columns = line.split(",", -1)
      require(columns.length >= 4, s"invalid SPD CSV row: $line")
      val address = columns(1).trim
      if (!address.contains("-")) {
        val index = address.toInt
        require(index >= 0 && index < values.length, s"SPD CSV address out of range: $index")
        values(index) = Integer.parseInt(columns(3).trim, 16)
        found = true
      }
    }
    require(found, "SPD CSV did not contain any individual byte rows")
    parse(values.toIndexedSeq, fineRefreshMode)
  }

  /** Parse the hexadecimal rows printed by LiteX BIOS `spdread`. */
  def parseHexdump(lines: Seq[String], fineRefreshMode: String = "1x"): DramSpdData = {
    val data = collection.mutable.ArrayBuffer.empty[Int]
    var lastAddress = -1
    lines.iterator.map(_.trim).filter(_.startsWith("0x")).foreach { line =>
      val tokens = line.split("\\s+")
      require(tokens.length >= 17, s"invalid SPD hexdump row: $line")
      val address = java.lang.Long.parseLong(tokens(0).drop(2), 16).toInt
      require(address > lastAddress, "SPD hexdump addresses must increase")
      data ++= tokens.slice(1, 17).map(Integer.parseInt(_, 16))
      lastAddress = address
    }
    require(data.nonEmpty, "SPD hexdump did not contain data rows")
    parse(data.toIndexedSeq, fineRefreshMode)
  }

  private def parseDdr3(bytes: Seq[Int]): DramSpdData = {
    require(bytes.length >= 39, s"DDR3 SPD requires at least 39 bytes, got ${bytes.length}")
    val bankBits = select("DDR3 bank count", field(bytes(4), 3, 4),
      Map(0 -> 3, 1 -> 4, 2 -> 5, 3 -> 6))
    val rowBits = select("DDR3 row count", field(bytes(5), 3, 3),
      Map(0 -> 12, 1 -> 13, 2 -> 14, 3 -> 15, 4 -> 16))
    val columnBits = select("DDR3 column count", field(bytes(5), 3, 0),
      Map(0 -> 9, 1 -> 10, 2 -> 11, 3 -> 12))
    val fineDividend = field(bytes(9), 4, 4)
    val fineDivisor = field(bytes(9), 4, 0)
    require(fineDivisor != 0 && bytes(11) != 0, "invalid DDR3 SPD timebase divisor")
    val fineNs = fineDividend.toDouble / fineDivisor * 1e-3
    val mediumNs = bytes(10).toDouble / bytes(11)
    def timing(mtb: Int, ftb: Int = 0): Double =
      mtb * mediumNs + signedByte(ftb) * fineNs

    val tck = timing(bytes(12), bytes(34))
    val trp = timing(bytes(20), bytes(37))
    val trcd = timing(bytes(18), bytes(36))
    val twr = timing(bytes(17))
    val trfc = timing(word(bytes(25), bytes(24)))
    val tfaw = timing(word(lowerNibble(bytes(28)), bytes(29)))
    val tras = timing(word(lowerNibble(bytes(21)), bytes(22)))
    val technology = Map(
      "tREFI" -> SpdTiming(nanoseconds = 64e6 / 8192),
      "tWTR" -> SpdTiming(4, timing(bytes(26))),
      "tCCD" -> SpdTiming(cycles = 4),
      "tRRD" -> SpdTiming(4, timing(bytes(19))),
      "tZQCS" -> SpdTiming(64, 80))
    val speed = Map(
      "tRP" -> SpdTiming(nanoseconds = trp),
      "tRCD" -> SpdTiming(nanoseconds = trcd),
      "tWR" -> SpdTiming(nanoseconds = twr),
      "tRFC" -> SpdTiming(nanoseconds = trfc),
      "tFAW" -> SpdTiming(nanoseconds = tfaw),
      "tRAS" -> SpdTiming(nanoseconds = tras))
    DramSpdData("DDR3", DramSpdGeometry(bankBits, rowBits, columnBits),
      speedgrade(tck, Seq(800, 1066, 1333, 1600, 1866, 2133)), technology, speed)
  }

  private def parseDdr4(bytes: Seq[Int], fineRefreshMode: String): DramSpdData = {
    require(bytes.length >= 126, s"DDR4 SPD requires at least 126 bytes, got ${bytes.length}")
    require(Set("1x", "2x", "4x").contains(fineRefreshMode),
      s"unsupported DDR4 fine refresh mode: $fineRefreshMode")
    val bankGroupBits = select("DDR4 bank-group count", field(bytes(4), 2, 6),
      Map(0 -> 0, 1 -> 1, 2 -> 2))
    val bankWithinGroupBits = select("DDR4 bank count", field(bytes(4), 2, 4),
      Map(0 -> 2, 1 -> 3))
    val rowBits = select("DDR4 row count", field(bytes(5), 3, 3),
      Map(0 -> 12, 1 -> 13, 2 -> 14, 3 -> 15, 4 -> 16, 5 -> 17, 6 -> 18))
    val columnBits = select("DDR4 column count", field(bytes(5), 3, 0),
      Map(0 -> 9, 1 -> 10, 2 -> 11, 3 -> 12))
    require(field(bytes(17), 2, 2) == 0 && field(bytes(17), 2, 0) == 0,
      "unsupported DDR4 SPD timebase")
    def timing(mtb: Int, ftb: Int = 0): Double =
      mtb * 0.125 + signedByte(ftb) * 0.001

    val tck = timing(bytes(18), bytes(125))
    val trp = timing(bytes(26), bytes(121))
    val trcd = timing(bytes(25), bytes(122))
    val tras = timing(word(lowerNibble(bytes(27)), bytes(28)))
    val twr = timing(word(lowerNibble(bytes(41)), bytes(42)))
    val trfcBytes = fineRefreshMode match {
      case "1x" => (bytes(31), bytes(30))
      case "2x" => (bytes(33), bytes(32))
      case "4x" => (bytes(35), bytes(34))
    }
    val trfc = timing(word(trfcBytes._1, trfcBytes._2))
    val tfaw = timing(word(lowerNibble(bytes(36)), bytes(37)))
    val trrdLong = timing(bytes(39), bytes(118))
    val tccdLong = timing(bytes(40), bytes(117))
    val twtrLong = timing(word(upperNibble(bytes(43)), bytes(45)))
    val deviceWidth = select("DDR4 SDRAM device width", field(bytes(12), 3, 0),
      Map(0 -> 4, 1 -> 8, 2 -> 16, 3 -> 32))
    val pageBytes = (1 << columnBits) * deviceWidth / 8
    val tfawCycles = select("DDR4 page size", pageBytes,
      Map(512 -> 16, 1024 -> 20, 2048 -> 28))
    val refreshDivisor = fineRefreshMode match {
      case "1x" => 1
      case "2x" => 2
      case "4x" => 4
    }
    val technology = Map(
      "tREFI" -> SpdTiming(nanoseconds = (64e6 / 8192) / refreshDivisor),
      "tWTR" -> SpdTiming(4, twtrLong),
      "tCCD" -> SpdTiming(4, tccdLong),
      "tRRD" -> SpdTiming(4, trrdLong),
      "tZQCS" -> SpdTiming(128, 80))
    val speed = Map(
      "tRP" -> SpdTiming(nanoseconds = trp),
      "tRCD" -> SpdTiming(nanoseconds = trcd),
      "tWR" -> SpdTiming(nanoseconds = twr),
      "tRFC" -> SpdTiming(nanoseconds = trfc),
      "tFAW" -> SpdTiming(tfawCycles, tfaw),
      "tRAS" -> SpdTiming(nanoseconds = tras))
    val geometry = DramSpdGeometry(bankGroupBits + bankWithinGroupBits,
      rowBits, columnBits, bankGroupBits)
    DramSpdData("DDR4", geometry,
      speedgrade(tck, Seq(1600, 1866, 2133, 2400, 2666, 2933, 3200)), technology, speed)
  }
}
