package pleiadic.dram

/** DFI injector control/command bits used by LiteDRAM initialization tables. */
object DfiiEncoding {
  val controlSelect = 0x01
  val controlClockEnable = 0x02
  val controlOdt = 0x04
  val controlResetN = 0x08

  val commandChipSelect = 0x01
  val commandWriteEnable = 0x02
  val commandColumnStrobe = 0x04
  val commandRowStrobe = 0x08
  val commandWriteData = 0x10
  val commandReadData = 0x20

  val prechargeAll = commandRowStrobe | commandWriteEnable | commandChipSelect
  val modeRegister = commandRowStrobe | commandColumnStrobe |
    commandWriteEnable | commandChipSelect
  val autoRefresh = commandRowStrobe | commandColumnStrobe | commandChipSelect
  val unreset = controlOdt | controlResetN
  val clockEnable = controlClockEnable | controlOdt | controlResetN
  val zqCalibration = commandWriteEnable | commandChipSelect
}

case class DramInitStep(
  description: String,
  address: BigInt,
  bank: Int,
  command: Int,
  delayCycles: Int)

case class DramInitResult(
  steps: Seq[DramInitStep],
  modeRegisters: Map[Int, BigInt] = Map.empty)

/** PHY parameters that affect JEDEC mode-register programming. */
case class DramInitSettings(
  memType: String,
  casLatency: Int,
  writeLatency: Int = 0,
  nPhases: Int = 1,
  rttNom: Option[String] = None,
  rttWr: Option[String] = None,
  ron: Option[String] = None,
  tdqs: Int = 0,
  fineRefreshMode: String = "1x") {
  require(casLatency > 0 && writeLatency >= 0 && nPhases > 0)
  require(tdqs == 0 || tdqs == 1)
}

/**
  * Generates the same portable initialization tables as LiteDRAM's init.py.
  * The initial implementation covers SDR through DDR4; LPDDR4/5 and RPC use
  * different mode-register command formats and are intentionally rejected.
  */
object DramInit {
  import DfiiEncoding._

  private def step(description: String, address: BigInt = 0, bank: Int = 0,
      command: Int, delay: Int = 0): DramInitStep =
    DramInitStep(description, address, bank, command, delay)

  private def log2Exact(value: Int): Int = {
    require(value > 0 && (value & (value - 1)) == 0,
      s"burst length must be a power of two, got $value")
    Integer.numberOfTrailingZeros(value)
  }

  private def lookup[A](name: String, value: A, values: Map[A, Int]): Int =
    values.getOrElse(value,
      throw new IllegalArgumentException(s"unsupported $name value: $value"))

  def generate(settings: DramInitSettings, timing: DramTiming): DramInitResult =
    settings.memType.toUpperCase match {
      case "SDR" => sdr(settings)
      case "DDR" => ddrLike(settings, lpddr = false)
      case "LPDDR" => ddrLike(settings, lpddr = true)
      case "DDR2" => ddr2(settings)
      case "DDR3" => ddr3(settings, timing)
      case "RPC" => rpc(settings)
      case "DDR4" => ddr4(settings, timing)
      case other => throw new IllegalArgumentException(
        s"initialization sequence for $other is not implemented")
    }

  private def sdr(settings: DramInitSettings): DramInitResult = {
    val cl = settings.casLatency
    val bl = settings.nPhases
    val mr = log2Exact(bl) | (cl << 4)
    DramInitResult(Seq(
      step("Bring CKE high", command = clockEnable, delay = 20000),
      step("Precharge All", 0x400, command = prechargeAll),
      step(s"Load Mode Register / Reset DLL, CL=$cl, BL=$bl", mr | 0x100,
        command = modeRegister, delay = 200),
      step("Precharge All", 0x400, command = prechargeAll),
      step("Auto Refresh", command = autoRefresh, delay = 4),
      step("Auto Refresh", command = autoRefresh, delay = 4),
      step(s"Load Mode Register / CL=$cl, BL=$bl", mr,
        command = modeRegister, delay = 200)))
  }

  private def ddrLike(settings: DramInitSettings, lpddr: Boolean): DramInitResult = {
    val cl = settings.casLatency
    val bl = 4
    val mr = log2Exact(bl) | (cl << 4)
    val extendedBank = if (lpddr) 2 else 1
    DramInitResult(Seq(
      step("Bring CKE high", command = clockEnable, delay = 20000),
      step("Precharge All", 0x400, command = prechargeAll),
      step("Load Extended Mode Register", bank = extendedBank, command = modeRegister),
      step(s"Load Mode Register / Reset DLL, CL=$cl, BL=$bl", mr | 0x100,
        command = modeRegister, delay = 200),
      step("Precharge All", 0x400, command = prechargeAll),
      step("Auto Refresh", command = autoRefresh, delay = 4),
      step("Auto Refresh", command = autoRefresh, delay = 4),
      step(s"Load Mode Register / CL=$cl, BL=$bl", mr,
        command = modeRegister, delay = 200)))
  }

  private def ddr2(settings: DramInitSettings): DramInitResult = {
    val cl = settings.casLatency
    val bl = 4
    val mr = log2Exact(bl) | (cl << 4) | (2 << 9)
    val ocdDefault = 7 << 7
    DramInitResult(Seq(
      step("Bring CKE high", command = clockEnable, delay = 20000),
      step("Precharge All", 0x400, command = prechargeAll),
      step("Load Extended Mode Register 3", bank = 3, command = modeRegister),
      step("Load Extended Mode Register 2", bank = 2, command = modeRegister),
      step("Load Extended Mode Register", bank = 1, command = modeRegister),
      step(s"Load Mode Register / Reset DLL, CL=$cl, BL=$bl", mr | 0x100,
        command = modeRegister, delay = 200),
      step("Precharge All", 0x400, command = prechargeAll),
      step("Auto Refresh", command = autoRefresh, delay = 4),
      step("Auto Refresh", command = autoRefresh, delay = 4),
      step(s"Load Mode Register / CL=$cl, BL=$bl", mr,
        command = modeRegister, delay = 200),
      step("Load Extended Mode Register / OCD Default", ocdDefault, bank = 1,
        command = modeRegister),
      step("Load Extended Mode Register / OCD Exit", bank = 1,
        command = modeRegister)))
  }

  private val ddr3Cl = Map(5 -> 2, 6 -> 4, 7 -> 6, 8 -> 8, 9 -> 10,
    10 -> 12, 11 -> 14, 12 -> 1, 13 -> 3, 14 -> 5)
  private val ddr3Wr = Map(16 -> 0, 5 -> 1, 6 -> 2, 7 -> 3, 8 -> 4,
    10 -> 5, 12 -> 6, 14 -> 7)
  private val ddr3RttNom = Map("disabled" -> 0, "60ohm" -> 1,
    "120ohm" -> 2, "40ohm" -> 3, "20ohm" -> 4, "30ohm" -> 5)
  private val ddr3RttWr = Map("disabled" -> 0, "60ohm" -> 1, "120ohm" -> 2)
  private val ddr3Ron = Map("40ohm" -> 0, "34ohm" -> 1)

  private def ddr3(settings: DramInitSettings, timing: DramTiming): DramInitResult = {
    val cl = settings.casLatency
    val cwl = settings.writeLatency
    val wr = math.max(timing.tWtr * settings.nPhases, 5)
    val encodedCl = lookup("DDR3 CL", cl, ddr3Cl)
    val encodedWr = lookup("DDR3 WR", wr, ddr3Wr)
    val mr0 = ((encodedCl & 1) << 2) | (((encodedCl >> 1) & 7) << 4) |
      0x100 | (encodedWr << 9)
    val ron = lookup("DDR3 RON", settings.ron.getOrElse("34ohm"), ddr3Ron)
    val rttNom = lookup("DDR3 RTT_NOM", settings.rttNom.getOrElse("60ohm"), ddr3RttNom)
    val rttWr = lookup("DDR3 RTT_WR", settings.rttWr.getOrElse("60ohm"), ddr3RttWr)
    val mr1 = ((ron & 1) << 1) | (((ron >> 1) & 1) << 5) |
      ((rttNom & 1) << 2) | (((rttNom >> 1) & 1) << 6) |
      (((rttNom >> 2) & 1) << 9) | ((settings.tdqs & 1) << 11)
    require(cwl >= 5 && cwl <= 12, s"unsupported DDR3 CWL value: $cwl")
    val mr2 = ((cwl - 5) << 3) | (rttWr << 9)
    val steps = Seq(
      step("Release reset", command = unreset, delay = 50000),
      step("Bring CKE high", command = clockEnable, delay = 10000),
      step(s"Load Mode Register 2, CWL=$cwl", mr2, bank = 2, command = modeRegister),
      step("Load Mode Register 3", bank = 3, command = modeRegister),
      step("Load Mode Register 1", mr1, bank = 1, command = modeRegister),
      step(s"Load Mode Register 0, CL=$cl, BL=8", mr0,
        command = modeRegister, delay = 200),
      step("ZQ Calibration", 0x400, command = zqCalibration, delay = 200))
    DramInitResult(steps, Map(1 -> BigInt(mr1)))
  }

  private val rpcCl = Map(8 -> 0, 10 -> 1, 11 -> 2, 13 -> 3, 3 -> 6)
  private val rpcNwr = Map(4 -> 0, 6 -> 1, 7 -> 2, 8 -> 3, 10 -> 4,
    12 -> 5, 14 -> 6, 16 -> 7)
  private val rpcZout = Map("120ohm" -> 2, "90ohm" -> 4, "51.4ohm" -> 6,
    "60ohm" -> 8, "40ohm" -> 10, "36ohm" -> 12, "27.7ohm" -> 14,
    "short" -> 1, "open" -> 0)
  private val rpcOdt = Map("60ohm" -> 1, "45ohm" -> 2, "25.7ohm" -> 3,
    "30ohm" -> 4, "20ohm" -> 5, "18ohm" -> 6, "13.85ohm" -> 7,
    "open" -> 0)

  private def rpc(settings: DramInitSettings): DramInitResult = {
    require(settings.casLatency == settings.writeLatency,
      "RPC requires equal read and write latency")
    // LiteDRAM's RPC PHY adds one cycle of additive latency.
    val cl = settings.casLatency - 1
    val encodedCl = lookup("RPC CL", cl, rpcCl)
    lookup("RPC nWR", 8, rpcNwr) // Encoded in the packet, not in DFI address bits.
    val zout = lookup("RPC ZOUT", settings.ron.getOrElse("60ohm"), rpcZout)
    val odt = lookup("RPC ODT", settings.rttNom.getOrElse("30ohm"), rpcOdt)
    val modeAddress = encodedCl | (zout << 3) | (odt << 7)
    val modeBank = 1 // ODT_STB=1; CSR_FX=0 and ODT_PD=0.
    def delay(seconds: Double): Int = math.ceil(seconds * 200e6).toInt
    DramInitResult(Seq(
      step("Stabilize clocks", command = unreset, delay = delay(200e-6)),
      step("Hold CS# low", command = commandChipSelect, delay = delay(100e-9)),
      step("RPC special commands: ON", command = controlOdt, delay = delay(100e-9)),
      step("PU RESET sequence (ACT)", command = commandRowStrobe | commandChipSelect,
        delay = delay(5e-6)),
      step("RPC special commands: OFF", command = unreset, delay = delay(100e-9)),
      step("Precharge ALL", 0x400, command = prechargeAll, delay = delay(100e-9)),
      step(s"Load Mode Register: CL=$cl", modeAddress, modeBank,
        modeRegister, delay(100e-9)),
      step("RPC special commands: ON", command = controlOdt, delay = delay(100e-9)),
      step("ZQ Init Calibration", 0x400, command = zqCalibration, delay = delay(1e-6)),
      step("RPC special commands: OFF", command = unreset, delay = delay(100e-9))))
  }

  private val ddr4Cl = Map(9 -> 0, 10 -> 1, 11 -> 2, 12 -> 3,
    13 -> 4, 14 -> 5, 15 -> 6, 16 -> 7, 18 -> 8, 20 -> 9,
    22 -> 10, 24 -> 11, 23 -> 12, 17 -> 13, 19 -> 14, 21 -> 15,
    25 -> 16, 26 -> 17, 27 -> 18, 28 -> 19, 29 -> 20, 30 -> 21,
    31 -> 22, 32 -> 23)
  private val ddr4Wr = Map(10 -> 0, 12 -> 1, 14 -> 2, 16 -> 3,
    18 -> 4, 20 -> 5, 24 -> 6, 22 -> 7, 26 -> 8, 28 -> 9)
  private val ddr4Cwl = Map(9 -> 0, 10 -> 1, 11 -> 2, 12 -> 3,
    14 -> 4, 16 -> 5, 18 -> 6, 20 -> 7)
  private val ddr4RttNom = Map("disabled" -> 0, "60ohm" -> 1,
    "120ohm" -> 2, "40ohm" -> 3, "240ohm" -> 4, "48ohm" -> 5,
    "80ohm" -> 6, "34ohm" -> 7)
  private val ddr4RttWr = Map("disabled" -> 0, "120ohm" -> 1,
    "240ohm" -> 2, "high-z" -> 3, "80ohm" -> 4)
  private val ddr4Ron = Map("34ohm" -> 0, "48ohm" -> 1)
  private val ddr4Refresh = Map("1x" -> 0, "2x" -> 1, "4x" -> 2)

  private def ddr4(settings: DramInitSettings, timing: DramTiming): DramInitResult = {
    val cl = settings.casLatency
    val cwl = settings.writeLatency
    val wr = math.max(timing.tWtr * settings.nPhases, 10)
    val encodedCl = lookup("DDR4 CL", cl, ddr4Cl)
    val encodedWr = lookup("DDR4 WR", wr, ddr4Wr)
    val mr0 = ((encodedCl & 1) << 2) | (((encodedCl >> 1) & 7) << 4) |
      (((encodedCl >> 4) & 1) << 12) | 0x100 |
      ((encodedWr & 7) << 9) | ((encodedWr >> 3) << 13)
    val ron = lookup("DDR4 RON", settings.ron.getOrElse("34ohm"), ddr4Ron)
    val rttNom = lookup("DDR4 RTT_NOM", settings.rttNom.getOrElse("40ohm"), ddr4RttNom)
    val rttWr = lookup("DDR4 RTT_WR", settings.rttWr.getOrElse("120ohm"), ddr4RttWr)
    val mr1 = 1 | ((ron & 1) << 1) | (((ron >> 1) & 1) << 2) |
      ((rttNom & 1) << 8) | (((rttNom >> 1) & 1) << 9) |
      (((rttNom >> 2) & 1) << 10) | ((settings.tdqs & 1) << 11)
    require(settings.tdqs == 0, "DDR4 data-mask mode is incompatible with TDQS")
    val mr2 = (lookup("DDR4 CWL", cwl, ddr4Cwl) << 3) | (rttWr << 9)
    val mr3 = lookup("DDR4 fine refresh mode", settings.fineRefreshMode, ddr4Refresh) << 6
    val modeRegisters = Seq(mr3, 0, 1 << 10, 0, mr2, mr1, mr0)
    val banks = Seq(3, 6, 5, 4, 2, 1, 0)
    val descriptions = Seq(
      "Load Mode Register 3",
      "Load Mode Register 6",
      "Load Mode Register 5",
      "Load Mode Register 4",
      s"Load Mode Register 2, CWL=$cwl",
      "Load Mode Register 1",
      s"Load Mode Register 0, CL=$cl, BL=8")
    val steps = Seq(
      step("Release reset", command = unreset, delay = 50000),
      step("Bring CKE high", command = clockEnable, delay = 10000)) ++
      modeRegisters.indices.map { index =>
        step(descriptions(index), modeRegisters(index), banks(index), modeRegister,
          if (banks(index) == 0) 200 else 0)
      } ++ Seq(step("ZQ Calibration", 0x400,
        command = zqCalibration, delay = 200))
    DramInitResult(steps, Map(1 -> BigInt(mr1)))
  }
}

/** Deterministic firmware/source representations of a generated init table. */
object DramInitExport {
  private def identifier(value: String): String = {
    val sanitized = value.map(character =>
      if (character.isLetterOrDigit || character == '_') character else '_')
    val nonEmpty = if (sanitized.nonEmpty) sanitized else "dram"
    if (nonEmpty.head.isDigit) s"dram_$nonEmpty" else nonEmpty
  }

  def toC(result: DramInitResult, name: String = "dram_init"): String = {
    val symbol = identifier(name)
    val rows = result.steps.map { value =>
      f"  { ${value.address}%#x, ${value.bank}%d, ${value.command}%#x, ${value.delayCycles}%d }, /* ${value.description} */"
    }.mkString("\n")
    val modeRegisters = result.modeRegisters.toSeq.sortBy(_._1).map { case (index, value) =>
      f"#define ${symbol.toUpperCase}_MR$index ${value}%#x"
    }.mkString("\n")
    val prefix = if (modeRegisters.isEmpty) "" else modeRegisters + "\n\n"
    s"""${prefix}typedef struct {
       |  unsigned int address;
       |  unsigned int bank;
       |  unsigned int command;
       |  unsigned int delay_cycles;
       |} dram_init_step_t;
       |
       |static const dram_init_step_t ${symbol}[] = {
       |$rows
       |};
       |static const unsigned int ${symbol}_count = ${result.steps.size};
       |""".stripMargin
  }

  def toScala(result: DramInitResult, name: String = "DramInitTable"): String = {
    val symbol = identifier(name)
    val rows = result.steps.map { value =>
      val escaped = value.description.replace("\\", "\\\\").replace("\"", "\\\"")
      s"    DramInitStep(\"$escaped\", BigInt(\"${value.address}\"), ${value.bank}, ${value.command}, ${value.delayCycles})"
    }.mkString(",\n")
    val modeRegisters = result.modeRegisters.toSeq.sortBy(_._1)
      .map { case (index, value) => s"$index -> BigInt(\"$value\")" }.mkString(", ")
    s"""object $symbol {
       |  val result: DramInitResult = DramInitResult(
       |    steps = Seq(
       |$rows),
       |    modeRegisters = Map($modeRegisters))
       |}
       |""".stripMargin
  }
}
