#!/usr/bin/env python3
"""Run live cycle-by-cycle checks against the pinned LiteDRAM/Migen source."""

import argparse
import os
import pathlib
import random
import shutil
import subprocess
import sys
import tempfile
import types


PINNED_REVISION = "3cf585a60a37113f18a9f6c6f3ee774be521623e"


def find_python(explicit, reference):
    candidates = [explicit, sys.executable, shutil.which("python"), shutil.which("python3")]
    checked = []
    for candidate in candidates:
        if not candidate:
            continue
        candidate = os.path.realpath(candidate)
        if candidate in checked:
            continue
        checked.append(candidate)
        result = subprocess.run(
            [candidate, "-c", "import migen"], cwd=reference,
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        if result.returncode == 0:
            return candidate
    raise SystemExit("no Python interpreter can import migen; checked: " + ", ".join(checked))


def install_litex_stream_stub():
    """common.py only needs the stream name while timing controllers are imported."""
    litex = types.ModuleType("litex")
    litex.__path__ = []
    soc = types.ModuleType("litex.soc")
    soc.__path__ = []
    interconnect = types.ModuleType("litex.soc.interconnect")
    interconnect.__path__ = []
    stream = types.ModuleType("litex.soc.interconnect.stream")
    interconnect.stream = stream
    sys.modules.update({
        "litex": litex,
        "litex.soc": soc,
        "litex.soc.interconnect": interconnect,
        "litex.soc.interconnect.stream": stream,
    })


def emit_oracle(reference, output, seed, cycles):
    install_litex_stream_stub()
    sys.path.insert(0, str(reference))
    from migen import Module, Signal
    from migen.fhdl import verilog
    from litedram.common import tFAWController, tXXDController

    generator = random.Random(seed)
    parameters = [("TXXD", value) for value in range(1, 9)] + [
        ("TFAW", value) for value in (4, 5, 6, 8, 10, 12, 16)]
    stimuli = []
    for kind, _ in parameters:
        probability = 0.23 if kind == "TXXD" else 0.38
        stimuli.append([int(generator.random() < probability) for _ in range(cycles)])

    class TimingOracle(Module):
        def __init__(self):
            self.valid = Signal(len(parameters), name="valid")
            self.ready = Signal(len(parameters), name="ready")
            for index, (kind, parameter) in enumerate(parameters):
                controller = (tXXDController(parameter) if kind == "TXXD"
                    else tFAWController(parameter))
                self.submodules += controller
                self.comb += [
                    controller.valid.eq(self.valid[index]),
                    self.ready[index].eq(controller.ready),
                ]

    top = TimingOracle()
    generated = verilog.convert(top, ios={top.valid, top.ready}, name="TimingOracle")
    with tempfile.TemporaryDirectory(prefix="litedram-migen-verilog-") as directory:
        directory = pathlib.Path(directory)
        rtl = directory / "TimingOracle.v"
        testbench = directory / "TimingOracleTb.sv"
        rtl.write_text(str(generated))
        statements = []
        for cycle in range(cycles):
            value = sum(stimuli[index][cycle] << index
                for index in range(len(parameters)))
            statements.append(
                f"    valid = {len(parameters)}'h{value:x}; @(posedge sys_clk); "
                f"#1; $display(\"READY %0{len(parameters)}b\", ready);")
        testbench.write_text(
            "module TimingOracleTb;\n"
            "  reg sys_clk = 0; reg sys_rst = 1;\n"
            f"  reg [{len(parameters) - 1}:0] valid = 0;\n"
            f"  wire [{len(parameters) - 1}:0] ready;\n"
            "  TimingOracle dut(.sys_clk(sys_clk), .sys_rst(sys_rst), "
            ".valid(valid), .ready(ready));\n"
            "  always #5 sys_clk = ~sys_clk;\n"
            "  initial begin\n"
            "    repeat (2) @(posedge sys_clk); @(negedge sys_clk); sys_rst = 0;\n"
            + "\n".join(statements) + "\n"
            "    $finish;\n"
            "  end\n"
            "endmodule\n")
        model_dir = directory / "obj_dir"
        subprocess.run([
            "verilator", "--binary", "--timing", "-Wno-fatal",
            "-Wno-WIDTHEXPAND", "-Wno-WIDTHTRUNC", "-Wno-COMBDLY",
            "--top-module", "TimingOracleTb", "--Mdir", str(model_dir),
            "-o", "timing_oracle_sim", str(rtl), str(testbench),
        ], cwd=directory, check=True, stdout=subprocess.DEVNULL)
        result = subprocess.run([str(model_dir / "timing_oracle_sim")],
            cwd=directory, check=True, text=True, capture_output=True)
    ready_words = [line.split()[1] for line in result.stdout.splitlines()
        if line.startswith("READY ")]
    if len(ready_words) != cycles:
        raise SystemExit(f"Verilator oracle produced {len(ready_words)} of {cycles} cycles")

    lines = [f"# seed={seed} cycles={cycles} revision={PINNED_REVISION}"]
    for index, (kind, parameter) in enumerate(parameters):
        readys = [word[-1 - index] for word in ready_words]
        lines.append("{}\t{}\t{}\t{}".format(kind, parameter,
            "".join(map(str, stimuli[index])), "".join(readys)))
    output.write_text("\n".join(lines) + "\n")


def verify_reference(reference):
    result = subprocess.run(
        ["git", "rev-parse", "HEAD"], cwd=reference,
        check=True, text=True, capture_output=True)
    revision = result.stdout.strip()
    if revision != PINNED_REVISION:
        raise SystemExit(
            f"LiteDRAM reference is {revision}, expected {PINNED_REVISION}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--python", help="Python interpreter containing Migen")
    parser.add_argument("--reference", type=pathlib.Path)
    parser.add_argument("--seed", type=lambda value: int(value, 0), default=0x4C445241)
    parser.add_argument("--cycles", type=int, default=256)
    parser.add_argument("--emit-oracle", type=pathlib.Path,
        help=argparse.SUPPRESS)
    args = parser.parse_args()
    root = pathlib.Path(__file__).resolve().parent.parent
    reference = (args.reference or root.parents[1] / "litex/litedram").resolve()
    if args.cycles < 16:
        raise SystemExit("--cycles must be at least 16")

    if args.emit_oracle:
        emit_oracle(reference, args.emit_oracle.resolve(), args.seed, args.cycles)
        return

    verify_reference(reference)
    python = find_python(args.python, reference)
    with tempfile.TemporaryDirectory(prefix="litedram-differential-") as directory:
        oracle = pathlib.Path(directory) / "timing-oracle.tsv"
        subprocess.run([
            python, str(pathlib.Path(__file__).resolve()),
            "--reference", str(reference), "--seed", str(args.seed),
            "--cycles", str(args.cycles), "--emit-oracle", str(oracle),
        ], cwd=root, check=True)
        subprocess.run([
            "sbt", f"Test / runMain pleiadic.dram.TimingDifferential {oracle}"
        ], cwd=root, check=True)
    print(f"differential timing checks passed with {python}")


if __name__ == "__main__":
    main()
