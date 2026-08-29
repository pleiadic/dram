#!/usr/bin/env python3
"""Run and report backend-independent LiteDRAM functional coverage.

This deliberately does not set PYTHONPATH and does not require an RTL coverage,
formal, or synthesis tool.  ScalaTest JUnit results provide executed-test
evidence, while FunctionalCoverageBins artifacts provide randomized event-bin
evidence.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
from pathlib import Path
import shutil
import subprocess
import sys
import xml.etree.ElementTree as ET


ROOT = Path(__file__).resolve().parents[1]
MANIFEST = ROOT / "functional_coverage.json"
DEFAULT_RESULTS = ROOT / "target" / "functional-coverage-results"
DEFAULT_EVENTS = ROOT / "target" / "functional-coverage-events"
DEFAULT_OUTPUT = ROOT / "target" / "functional-coverage"


def target_path(value: str, default: Path) -> Path:
    path = Path(value).resolve() if value else default.resolve()
    target = (ROOT / "target").resolve()
    if path != target and target not in path.parents:
        raise ValueError(f"coverage output must remain below {target}: {path}")
    return path


def clean_directory(path: Path) -> None:
    if path.exists():
        shutil.rmtree(path)
    path.mkdir(parents=True)


def test_status(testcase: ET.Element) -> str:
    if testcase.find("failure") is not None or testcase.find("error") is not None:
        return "failed"
    if testcase.find("skipped") is not None:
        return "skipped"
    return "passed"


def load_tests(results: Path) -> list[dict[str, str]]:
    tests: list[dict[str, str]] = []
    for path in sorted(results.glob("TEST-*.xml")):
        root = ET.parse(path).getroot()
        for testcase in root.findall("testcase"):
            tests.append({
                "suite": testcase.attrib.get("classname", root.attrib.get("name", "")),
                "name": testcase.attrib.get("name", ""),
                "status": test_status(testcase),
            })
    return tests


def evaluate_requirements(manifest: dict, tests: list[dict[str, str]]) -> list[dict]:
    evaluated = []
    for requirement in manifest["requirements"]:
        evidence_results = []
        for evidence in requirement["evidence"]:
            matches = [
                test for test in tests
                if test["suite"] == evidence["suite"]
                and evidence["test_contains"] in test["name"]
            ]
            evidence_results.append({
                **evidence,
                "matches": len(matches),
                "passed": bool(matches) and all(test["status"] == "passed" for test in matches),
                "tests": matches,
            })
        evaluated.append({
            **requirement,
            "covered": all(item["passed"] for item in evidence_results),
            "evidence_results": evidence_results,
        })
    return evaluated


def evaluate_events(manifest: dict, events: Path) -> list[dict]:
    artifacts = {}
    for path in sorted(events.glob("*.json")):
        artifact = json.loads(path.read_text(encoding="utf-8"))
        artifacts[artifact["scope"]] = artifact
    evaluated = []
    for expected in manifest["event_scopes"]:
        artifact = artifacts.get(expected["scope"])
        hits = artifact.get("hits", {}) if artifact else {}
        covered = bool(artifact) and artifact.get("total", 0) >= expected["minimum_bins"]
        covered = covered and artifact.get("covered") == artifact.get("total")
        covered = covered and all(value > 0 for value in hits.values())
        evaluated.append({**expected, "covered": covered, "artifact": artifact})
    return evaluated


def markdown_report(manifest: dict, tests: list[dict[str, str]],
                    requirements: list[dict], events: list[dict]) -> str:
    domains: dict[str, list[dict]] = {}
    for requirement in requirements:
        domains.setdefault(requirement["domain"], []).append(requirement)
    req_covered = sum(item["covered"] for item in requirements)
    event_covered = sum(item["covered"] for item in events)
    passed_tests = sum(test["status"] == "passed" for test in tests)
    overall = bool(tests) and passed_tests == len(tests)
    overall = overall and req_covered == len(requirements)
    overall = overall and event_covered == len(events)
    lines = [
        "# Functional coverage report",
        "",
        f"Scope: {manifest['scope']}.",
        "",
        f"Result: **{'PASS' if overall else 'FAIL'}**",
        "",
        f"- Executed ScalaTest cases: {passed_tests}/{len(tests)} passed",
        f"- Functional requirements: {req_covered}/{len(requirements)} covered",
        f"- Random event scopes: {event_covered}/{len(events)} closed",
        "",
        "## Requirement coverage",
        "",
        "| Domain | Covered | Total |",
        "| --- | ---: | ---: |",
    ]
    for domain, items in domains.items():
        lines.append(f"| {domain} | {sum(item['covered'] for item in items)} | {len(items)} |")
    lines.extend(["", "## Random event coverage", "", "| Scope | Covered bins | Status |", "| --- | ---: | --- |"])
    for event in events:
        artifact = event["artifact"] or {}
        lines.append(
            f"| {event['scope']} | {artifact.get('covered', 0)}/{artifact.get('total', event['minimum_bins'])} | "
            f"{'PASS' if event['covered'] else 'FAIL'} |"
        )
    uncovered = [item for item in requirements if not item["covered"]]
    if uncovered:
        lines.extend(["", "## Uncovered requirements", ""])
        for item in uncovered:
            missing = [
                f"{e['suite']} / {e['test_contains']}"
                for e in item["evidence_results"] if not e["passed"]
            ]
            lines.append(f"- `{item['id']}` {item['description']}: {'; '.join(missing)}")
    lines.extend(["", "## Excluded from this report", ""])
    lines.extend(f"- {item}" for item in manifest["excluded"])
    lines.append("")
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--report-only", action="store_true",
                        help="do not run sbt; report existing result artifacts")
    parser.add_argument("--results-dir", default="")
    parser.add_argument("--events-dir", default="")
    parser.add_argument("--output-dir", default="")
    parser.add_argument("--sbt", default="sbt")
    args = parser.parse_args()

    results = target_path(args.results_dir, DEFAULT_RESULTS)
    events = target_path(args.events_dir, DEFAULT_EVENTS)
    output = target_path(args.output_dir, DEFAULT_OUTPUT)
    if not args.report_only:
        clean_directory(results)
        clean_directory(events)
        if results != DEFAULT_RESULTS.resolve():
            raise ValueError(
                "running tests uses the build.sbt JUnit path; use the default "
                "--results-dir or combine a custom directory with --report-only"
            )
        command = [args.sbt, "testFull"]
        completed = subprocess.run(command, cwd=ROOT, check=False)
        if completed.returncode != 0:
            return completed.returncode

    manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
    tests = load_tests(results)
    requirements = evaluate_requirements(manifest, tests)
    event_results = evaluate_events(manifest, events)
    output.mkdir(parents=True, exist_ok=True)
    passed = bool(tests) and all(test["status"] == "passed" for test in tests)
    passed = passed and all(item["covered"] for item in requirements + event_results)
    report = {
        "schema": 1,
        "generated_utc": dt.datetime.now(dt.timezone.utc).isoformat(),
        "passed": passed,
        "test_count": len(tests),
        "passed_test_count": sum(test["status"] == "passed" for test in tests),
        "requirements": requirements,
        "event_scopes": event_results,
        "excluded": manifest["excluded"],
    }
    (output / "report.json").write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    summary = markdown_report(manifest, tests, requirements, event_results)
    (output / "report.md").write_text(summary, encoding="utf-8")
    print(summary)
    return 0 if passed else 1


if __name__ == "__main__":
    sys.exit(main())
