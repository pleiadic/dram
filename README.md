# Pleiadic Chisel DRAM

This directory is a standalone Chisel implementation of the portable part of
[LiteDRAM](../../litex/litedram). It is deliberately independent of FPGA
vendor primitives: `DramController` emits a simple command stream that can be
connected to a DFI/PHY adapter, while its internal memory array provides a
useful simulation reference model.

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
| BankMachine open-row tracking | `openValid`/`openRow` and row-miss FSM |
| ACT/PRE insertion | `stateActivate`, `statePrecharge` |
| Column arbitration interface | Decoupled `command` stream |
| RefreshTimer/RefreshExecuter | `refreshTimer` and refresh FSM states |
| User LiteDRAM interface | Decoupled `request`/`response` |
| DFI and vendor PHY | Intentionally left as an adapter boundary |

This is a portable behavioral/synthesizable core, not a drop-in replacement
for every LiteDRAM PHY. Multi-bank scheduling is represented by the command
stream and can be extended with a round-robin queue when a frontend needs
multiple outstanding requests.

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
