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
   postponement.
4. **PHY/DFI** (`phy/`) translates abstract commands into SDR/DDR/LPDDR
   vendor-specific signals and initialization sequences.

The Chisel core maps these responsibilities as follows:

| LiteDRAM behavior | Chisel implementation |
| --- | --- |
| BankMachine open-row tracking | `BankMachine` per rank/bank |
| ACT/PRE insertion and local timings | `BankMachine` |
| Command arbitration/global timings | `CommandChooser` + `Multiplexer` |
| RefreshTimer/Sequencer | `Refresher` + `RefreshSequencer` |
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
capture, a related-clock rate converter, and a phase-granular timing checker.
The pure Scala initialization generator currently matches the repository's
SDR, DDR3, and DDR4 golden tables and also covers DDR, LPDDR, and DDR2. C/Scala
text export is available. The DDR3/DDR4 SPD decoder accepts Micron reference
CSV or LiteX BIOS hexdumps and extracts geometry, speedgrade, and timing data.
It also converts mixed CK/ns constraints into controller cycles using the same
rate margin and refresh exception as LiteDRAM. RPC mode-register encoding and
its reset/special-command/ZQ initialization sequence are covered. LPDDR4/5
initialization includes latency/frequency-set selection, ODT/drive strength,
VREF, complete mode-register tables, and reset/ZQ sequencing. Initialization
now covers every memory type handled by LiteDRAM `init.py`; the static device
catalog remains P9 work.

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
