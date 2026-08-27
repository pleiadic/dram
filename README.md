# Pleiadic Chisel DRAM

This directory is an in-progress Chisel reconstruction of
[LiteDRAM](../../litex/litedram). The detailed parity matrix and staged
acceptance criteria are in [DEVELOPMENT_PLAN.md](DEVELOPMENT_PLAN.md).

The new synthesizable control path is decomposed into `BankMachine`,
`Refresher`, `CommandChooser`, `Multiplexer`, `LiteDramController`, and
`LiteDramCrossbar`, with `LiteDramCore` as the integrated abstract command/data
top level. The original single-request `DramController` is temporarily retained
as a compatibility/reference model while the simulation PHY is built.

## LiteDRAM analysis

LiteDRAM is organized into four functional layers:

1. **User frontends** (`frontend/`) adapt Wishbone, AXI, Avalon, DMA, FIFO and
   BIST requests to a common memory interface.
2. **Controller core** (`core/`) contains one `BankMachine` per bank. A bank
   machine tracks the currently open row and inserts `ACTIVATE` and
   `PRECHARGE` around `READ`/`WRITE` commands. `Multiplexer` arbitrates bank
   machines, enforces turnaround and timing constraints, and chooses the
   command/data phase.
3. **Refresh** (`core/refresher.py`) requests periodic refresh at `tREFI` and
   executes `PRECHARGE-ALL`, `tRP`, `AUTO-REFRESH`, `tRFC`, with optional
   postponement and periodic ZQ short calibration.
4. **PHY/DFI** (`phy/`) translates abstract commands into SDR/DDR/LPDDR
   vendor-specific signals and initialization sequences.

The Chisel core maps these responsibilities as follows:

| LiteDRAM behavior | Chisel implementation |
| --- | --- |
| BankMachine open-row tracking | `BankMachine` per rank/bank |
| ACT/PRE insertion and local timings | `BankMachine` |
| Command arbitration/global timings | `CommandChooser` + `Multiplexer` |
| Refresh/ZQ timers and sequencers | `Refresher` + `RefreshSequencer` + `ZqCalibrationSequencer` |
| Multi-master command routing | `LiteDramCommandCrossbar` |
| Ordered native data routing | `LiteDramDataCrossbar` |
| Controller composition | `LiteDramController` |
| Integrated native core | `LiteDramCore` |
| Compatibility simulation model | `DramController` |
| DFI command encoding | `DfiSteerer` |

This is not yet a drop-in replacement for LiteDRAM. In particular, the full
init/SPD catalog, ECC-integrated Native wrappers, and PHY families remain
tracked work in the development plan.

The frontend layer currently includes Native up/down width conversion, a
Gray-pointer asynchronous Native CDC, Wishbone B4 with width conversion,
burst-capable Avalon-MM, and an AXI4 bridge. The AXI bridge buffers AW/W/AR
independently, preserves IDs, implements FIXED/INCR/WRAP address generation,
and never releases write data before a matching Native command. Optional AXI
sidebands/exclusive accesses, ECC read-modify-write, and narrow-Wishbone burst
coalescing remain compatibility work.

The asynchronous FIFO regression uses Verilator and derived clocks with
different periods. ChiselTest 6 requires a local `WData` compatibility define
when used with Verilator 5.050 or newer; the define is scoped to the test C++
harness and does not alter generated RTL.

Functional frontends now also include ordered Native DMA reader/writer engines,
an equal-width DRAM-backed ring FIFO, incremental/PRBS BIST generation and
checking, LiteX-compatible per-lane SEC-DED codecs, saturating ECC status
counters, and a fixed-window bandwidth monitor. Their remaining control-plane,
bypass, and read-modify-write variants are tracked in P8.

The DFI layer exposes the standard DDR4 `act_n` signal and includes a DDR4
command mux, a hardware/external/software injector with per-phase read-data
capture, a related-clock rate converter, and a phase-granular timing checker
including tZQCS. The refresher can schedule periodic ZQ short calibration after
a refresh while retaining exclusive ownership through both sequences.
The pure Scala initialization generator currently matches the repository's
SDR, DDR3, and DDR4 golden tables and also covers DDR, LPDDR, and DDR2. C/Scala
text export is available. The DDR3/DDR4 SPD decoder accepts Micron reference
CSV or LiteX BIOS hexdumps and extracts geometry, speedgrade, and timing data.
It also converts mixed CK/ns constraints into controller cycles using the same
rate margin and refresh exception as LiteDRAM. RPC mode-register encoding and
its reset/special-command/ZQ initialization sequence are covered. LPDDR4/5
initialization includes latency/frequency-set selection, ODT/drive strength,
VREF, complete mode-register tables, and reset/ZQ sequencing. Initialization
now covers every memory type handled by LiteDRAM `init.py`. The generated
device catalog contains all 74 concrete modules from the pinned `modules.py`,
including geometry, registered-module flags, speedgrades, and fine-refresh
timing variants, and can convert a selected entry directly to `DramTiming`.

PHY work has started with a portable full-rate Generic SDR implementation. It
forwards the single DFI phase to pad-side command/address signals, gates DQ/DM
on write enable, and aligns read-valid to `CL + 1`; FPGA-specific I/O cells are
left to thin outer wrappers. The half-rate variant serializes two DFI phases
over a related 2x clock and captures both read samples. Physical pad width is
now independent from Native and aggregate/per-phase DFI widths.

An independent DFI memory model decodes SDR through DDR4 command encodings,
tracks open rows per rank/bank, combines and splits multi-phase data, applies
byte masks, delays reads/writes, and reports invalid command sequencing. Large
production geometries should be reduced when instantiating its explicit test
memory.

Portable command adapters translate legacy DFI encodings into LPDDR4 and
LPDDR5 CA edge sequences or RPC parallel Request Packets. They cover mode
register and special commands, masked writes, LPDDR5 WCK synchronization, and
RPC reset/utility/ZQ encodings; directed vectors are golden outputs from the
pinned LiteDRAM Migen implementation.

The LPDDR4 command pipeline places each four-slot CA sequence at its source DFI
phase, carries late slots into the next controller cycle, and supports both
LiteDRAM overlap-filter modes. The LPDDR5 scheduler buffers the second small
command, delays one-part commands consistently, and reports an illegal adjacent
command that collides with the buffer.

The portable LPDDR4 PHY presents complete 16-edge parallel vectors for an
outer I/O serializer wrapper. It includes 8-phase DFI command integration,
bidirectional DQ transposition, DMI masks, DQS preamble/postamble and write
leveling, latency control, delayed CKE/ODT/reset, and independently selectable
read/write bitslip per byte lane.

The portable LPDDR5 PHY supports WCK:CK ratios of 2:1 and 4:1. Its WCK state
machine implements synchronization delay, static preamble, half/full-rate
toggle, postamble, and leveling patterns. A BL16 DFI word is split into two or
four serialized chunks on writes and reassembled on reads, with DMI, output
enable, per-byte bitslip, and the buffered CA command path kept aligned.

The portable RPC PHY implements the three-cycle DFI history used for Request
Packets, the two-cycle STB preamble and optional burst-stop sequence, latched
CS#, command/data-mask/data DB muxing, phase-specific DQS waveforms, and BL16
write/read conversion across four 64-bit DFI phases. Its power-up controller
enforces parallel reset, four serial-reset cycles, the reset wait, initial ZQ
calibration, READY, and restricted Utility Register mode. Board wrappers supply
the frequency-derived reset/ZQ cycle counts and physical serializers/tristates.

The portable standard DDR PHY covers the common 1:4 DDR2, DDR3, and DDR4
boundary used by 8:1 vendor SerDes. It expands each DFI command phase across
both memory-clock edges, transposes rising/falling DQ and DM data, applies the
DDR4 ACT command mux and active-low DM polarity, generates DQS/write-enable
windows, supports per-byte read/write bitslip and write leveling, and maps
deserialized DQ back into all four DFI read phases. Family-specific delay and
I/O cells remain in the outer pad assembly. The 7-series assembly supplies the
8:1 OSERDESE2/ISERDESE2, IOBUF/IOBUFDS, and per-byte IDELAY control. Artix-7
uses an externally phase-shifted DQS clock; Kintex-7 and Virtex-7 use the
OSERDES feedback path and independently controlled ODELAYE2 cells for commands,
DQ/DM, and DQS. DDR4's combined bank vector is split into BA/BG by board-level
pin mapping.

A common simulation SerDes boundary converts these portable parallel PHY
signals to explicit serial input/output/enable pads. It supports back-to-back
LSB-first words without an idle edge, captures output enables atomically with
each word, expands LPDDR4 SDR controls across pairs of DQ edges, and expands
LPDDR5 CK/CA according to the WCK:CK ratio. LPDDR4 16-edge, LPDDR5 2:1/4:1,
and RPC 8-edge data inputs are reassembled for their respective PHY cores.
Analog phase shifts and vendor I/O primitives remain outer-wrapper concerns.

The Xilinx 7-series primitive layer now provides parameterized 4:1 SDR and
8:1 DDR OSERDESE2/ISERDESE2 wrappers, variable IDELAYE2/ODELAYE2 controls,
IOBUF/IOBUFDS/OBUFDS wrappers, and a composed bidirectional SerDes lane. Inline
SystemVerilog selects real primitives for synthesis and deterministic portable
models otherwise. Artix-7-style LPDDR5 and RPC pad assemblies connect those
lanes to their complete differential and bidirectional pad sets. The LPDDR5
assembly supports both WCK:CK ratios by expanding 4-edge streams to the common
8:1 DDR boundary, implements the fixed cross-word CS/CA phase shifts, and uses
an externally shifted clock for RDQS on Artix-7. Kintex-7 and Virtex-7 variants
route CK/CA/WCK and DQ/DMI through separately controlled ODELAY groups and use
an independently calibrated differential ODELAY+IDELAY RDQS lane. A
phase-aligned LPDDR4 16:8 gearbox maps the portable 16-edge boundary onto the
same primitives and reconstructs input halves with the reference two-cycle
latency. Its Artix-7 variant uses an external sys8x_90 DQS clock; Kintex-7 and
Virtex-7 variants instead place CK/command, DQ/DMI, and DQS in independently
controlled ODELAY groups while retaining separate DQ/DQS IDELAY calibration.
Both LPDDR pad assemblies reproduce LiteDRAM's data/DQS output-enable
pipelines. The RPC assembly exposes the distinct clock, command/write-data,
read-data, and DQS clock phases and registers tristate control across the two
relevant domains. Tests compile portable and synthesis branches with Verilator.

The UltraScale primitive layer provides fixed 8:1 OSERDESE3/ISERDESE3,
nine-bit TIME-mode IDELAYE3/ODELAYE3 with voltage/temperature compensation,
and IOBUFDSE3 differential lanes for both UltraScale and UltraScale+. Its
DDR3/DDR4 pad assembly places CK/commands, DQ/DM, and DQS in independently
trainable delay groups; DQS accepts the quarter-cycle phase offset in
picoseconds. Use the portable standard PHY's one-cycle output-enable setting
for this E3 boundary. The current pad bundle models one DQS pair per byte;
x4 DIMM duplication and registered/clam-shell board topologies remain wrapper
extensions.

The ECP5 DDR3 PHY provides a half-rate two-phase DFI boundary with four DQ
edges per phase. It emits each BL8 write over two x2 primitive words, joins two
IDDRX2 input halves, implements read bitslip and command/read/write latency,
and exposes DQS preamble/postamble control. Its pad assembly wraps ODDRX2F,
ODDRX2DQA/B, IDDRX2DQA, TSHX2DQA, DQSBUFM, and DELAYG, including per-byte read
delay, burst detection, and the cycle-exact DDRDLLA/ECLK initialization
timeline. Portable and synthesis primitive branches are covered by Verilator.

A Vivado device-library smoke test remains outstanding.

## Build and test

Requirements: JDK 11+ and sbt 2.0.6 (the project launcher version).

```sh
sbt test
```

The tests verify row-miss command generation, write/readback through the
internal model, a row conflict's `PRECHARGE` then `ACTIVATE` ordering, and
periodic refresh generation.

## Address mapping

Addresses are byte addresses. The low bits select the byte within a word,
followed by column, bank, and row bits. This is equivalent to LiteDRAM's
row-bank-column mapping for the default configuration and is kept explicit so
an adapter can implement another mapping if needed.
