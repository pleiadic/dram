package pleiadic.dram

import chisel3._
import chisel3.util._
import scala.language.reflectiveCalls

object DfiTimingRule {
  val none = 0.U(5.W)
  val tRp = 1.U(5.W)
  val tRcd = 2.U(5.W)
  val tRas = 3.U(5.W)
  val tRfc = 4.U(5.W)
  val tCcd = 5.U(5.W)
  val tRc = 6.U(5.W)
  val tWr = 7.U(5.W)
  val tWtr = 8.U(5.W)
  val tRrd = 9.U(5.W)
  val tFaw = 10.U(5.W)
  val tRefi = 11.U(5.W)
  val tZqcs = 12.U(5.W)
}

/** Synthesizable DFI command timing monitor using phase-granular timestamps. */
class DfiTimingChecker(config: DramConfig) extends Module {
  private val bankCount = config.rankBankCount
  private val commandCount = 6
  private val pre = 0
  private val refresh = 1
  private val activate = 2
  private val read = 3
  private val write = 4
  private val zqCalibration = 5

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val clear = Input(Bool())
    val dfi = Input(new DfiInterface(config))
    val violation = Output(Bool())
    val rule = Output(UInt(5.W))
    val violations = Output(UInt(32.W))
    val refreshLate = Output(Bool())
  })

  private val time = RegInit(0.U(64.W))
  time := time + config.nPhases.U
  private val timestamps = RegInit(VecInit(Seq.fill(bankCount)(
    VecInit(Seq.fill(commandCount)(0.U(64.W))))))
  private val seen = RegInit(VecInit(Seq.fill(bankCount)(
    VecInit(Seq.fill(commandCount)(false.B)))))
  private val violation = WireDefault(false.B)
  private val rule = WireDefault(DfiTimingRule.none)

  private case class Rule(previous: Int, current: Int, delay: Int, code: UInt)
  private val baseRules = Seq(
    Rule(pre, activate, config.timing.tRp, DfiTimingRule.tRp),
    Rule(pre, refresh, config.timing.tRp, DfiTimingRule.tRp),
    Rule(activate, write, config.timing.tRcd, DfiTimingRule.tRcd),
    Rule(activate, read, config.timing.tRcd, DfiTimingRule.tRcd),
    Rule(activate, pre, config.timing.tRas, DfiTimingRule.tRas),
    Rule(refresh, pre, config.timing.tRfc, DfiTimingRule.tRfc),
    Rule(refresh, activate, config.timing.tRfc, DfiTimingRule.tRfc),
    Rule(write, read, config.timing.tCcd, DfiTimingRule.tCcd),
    Rule(write, write, config.timing.tCcd, DfiTimingRule.tCcd),
    Rule(read, read, config.timing.tCcd, DfiTimingRule.tCcd),
    Rule(read, write, config.timing.tCcd, DfiTimingRule.tCcd),
    Rule(activate, activate, config.timing.tRc, DfiTimingRule.tRc),
    Rule(write, pre, config.timing.tWr, DfiTimingRule.tWr),
    Rule(write, read, config.timing.tWtr, DfiTimingRule.tWtr))
  private val rules = baseRules ++ config.timing.tZqcs.toSeq.map(delay =>
    Rule(zqCalibration, activate, delay, DfiTimingRule.tZqcs))

  // Newest ACT is at index 0 and the fourth-newest at index 3. Build a
  // combinational next-state chain below so multiple ACTs in different DFI
  // phases of one controller cycle are all retained.
  private val actHistory = RegInit(VecInit(Seq.fill(4)(0.U(64.W))))
  private val actCount = RegInit(0.U(3.W))
  private val lastAct = RegInit(0.U(64.W))
  private val lastActSeen = RegInit(false.B)
  private val lastRefresh = RegInit(0.U(64.W))
  private val refreshSeen = RegInit(false.B)

  private val phaseCommands = (0 until config.nPhases).map { phaseIndex =>
    val phase = io.dfi.phases(phaseIndex)
    val selected = !phase.csN.asUInt.andR
    val isActivate = selected && ((!phase.actN) || (!phase.rasN && phase.casN && phase.weN))
    val isPrecharge = selected && phase.actN && !phase.rasN && phase.casN && !phase.weN
    val isRefresh = selected && phase.actN && !phase.rasN && !phase.casN && phase.weN
    val isRead = selected && phase.actN && phase.rasN && !phase.casN && phase.weN
    val isWrite = selected && phase.actN && phase.rasN && !phase.casN && !phase.weN
    val isZqCalibration = selected && phase.actN && phase.rasN && phase.casN && !phase.weN
    Seq(isPrecharge, isRefresh, isActivate, isRead, isWrite, isZqCalibration)
  }
  private val phaseAllBanks = (0 until config.nPhases).map { phaseIndex =>
    val phase = io.dfi.phases(phaseIndex)
    phaseCommands(phaseIndex)(refresh) ||
      (phaseCommands(phaseIndex)(pre) && phase.address(10))
  }
  private val phaseBanks = (0 until config.nPhases).map { phaseIndex =>
    val phase = io.dfi.phases(phaseIndex)
    if (config.nranks == 1) phase.bank else
      Cat(PriorityEncoder(~phase.csN.asUInt), phase.bank)
  }

  private var rollingActHistory: Vec[UInt] = actHistory
  private var rollingActCount: UInt = actCount
  private var rollingLastAct: UInt = lastAct
  private var rollingLastActSeen: Bool = lastActSeen

  for (phaseIndex <- 0 until config.nPhases) {
    val phase = io.dfi.phases(phaseIndex)
    val now = time + phaseIndex.U
    val commands = phaseCommands(phaseIndex)
    val allBanks = phaseAllBanks(phaseIndex)
    val selectedBank = phaseBanks(phaseIndex)

    for (bank <- 0 until bankCount; current <- 0 until commandCount) {
      val targetsBank = allBanks || selectedBank === bank.U
      for (timingRule <- rules.filter(_.current == current)) {
        when(io.enable && commands(current) && targetsBank &&
            seen(bank)(timingRule.previous) &&
            now - timestamps(bank)(timingRule.previous) <
              (timingRule.delay * config.nPhases).U) {
          violation := true.B
          rule := timingRule.code
        }
      }
      when(commands(current) && targetsBank) {
        timestamps(bank)(current) := now
        seen(bank)(current) := true.B
      }
    }

    val nextActHistory = WireDefault(rollingActHistory)
    val nextActCount = WireDefault(rollingActCount)
    val nextLastAct = WireDefault(rollingLastAct)
    val nextLastActSeen = WireDefault(rollingLastActSeen)
    when(commands(activate)) {
      when(io.enable && rollingLastActSeen && now - rollingLastAct <
          (config.timing.tRrd * config.nPhases).U) {
        violation := true.B
        rule := DfiTimingRule.tRrd
      }
      when(io.enable && rollingActCount >= 4.U && now - rollingActHistory(3) <
          (config.timing.tFaw * config.nPhases).U) {
        violation := true.B
        rule := DfiTimingRule.tFaw
      }
      nextActHistory(0) := now
      for (index <- 1 until 4) {
        nextActHistory(index) := rollingActHistory(index - 1)
      }
      nextLastAct := now
      nextLastActSeen := true.B
      when(rollingActCount < 4.U) { nextActCount := rollingActCount + 1.U }
    }
    rollingActHistory = nextActHistory
    rollingActCount = nextActCount
    rollingLastAct = nextLastAct
    rollingLastActSeen = nextLastActSeen
    when(commands(refresh)) {
      lastRefresh := now
      refreshSeen := true.B
    }
  }
  actHistory := rollingActHistory
  actCount := rollingActCount
  lastAct := rollingLastAct
  lastActSeen := rollingLastActSeen

  // Register timestamps cannot expose a command from an earlier phase in the
  // same controller cycle, so check those ordered phase pairs explicitly.
  for {
    previousPhase <- 0 until config.nPhases
    currentPhase <- previousPhase + 1 until config.nPhases
    bank <- 0 until bankCount
    timingRule <- rules
  } {
    val previousTargets = phaseAllBanks(previousPhase) || phaseBanks(previousPhase) === bank.U
    val currentTargets = phaseAllBanks(currentPhase) || phaseBanks(currentPhase) === bank.U
    when(io.enable && phaseCommands(previousPhase)(timingRule.previous) && previousTargets &&
        phaseCommands(currentPhase)(timingRule.current) && currentTargets &&
        (currentPhase - previousPhase).U < (timingRule.delay * config.nPhases).U) {
      violation := true.B
      rule := timingRule.code
    }
  }
  for {
    previousPhase <- 0 until config.nPhases
    currentPhase <- previousPhase + 1 until config.nPhases
  } {
    when(io.enable && phaseCommands(previousPhase)(activate) &&
        phaseCommands(currentPhase)(activate) &&
        (currentPhase - previousPhase).U < (config.timing.tRrd * config.nPhases).U) {
      violation := true.B
      rule := DfiTimingRule.tRrd
    }
  }

  private val violationCount = RegInit(0.U(32.W))
  private val lastRule = RegInit(DfiTimingRule.none)
  when(io.clear) {
    violationCount := 0.U
    lastRule := DfiTimingRule.none
  }.elsewhen(violation) {
    when(!violationCount.andR) { violationCount := violationCount + 1.U }
    lastRule := rule
  }
  io.violation := violation
  io.rule := Mux(violation, rule, lastRule)
  io.violations := violationCount
  io.refreshLate := io.enable && refreshSeen &&
    time - lastRefresh > (config.timing.tRefi * config.nPhases).U
}
