#!/usr/bin/env python3
"""Flag tracked decks no suite runs.

A deck under an inputs/ directory that a suite config references, but which no
`input_files:` entry names, is an orphan: it ships, it drifts, and nothing ever
runs it.  A directory no config references at all is left alone -- that is an
experiment in progress, not a gap in the suite.

Usage:
    uv run tools/check_orphan_decks.py    # exit 1 and list the orphans
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path

import yaml

REPO = Path(__file__).resolve().parent.parent
CONFIGS = (
    REPO / "test" / "regression" / "regression_config.yaml",
    REPO / "test" / "validation" / "validation_config.yaml",
    REPO / "examples" / "examples_config.yaml",
)
# decks a suite cannot run by design: the deliberate-abort demo and the
# preview of unimplemented keys
EXEMPT = {"examples/rip_2d/02_error.yaml", "examples/rip_2d/05_future_preview.yaml"}


def main() -> int:
    """Report decks no config references; 1 when any."""
    referenced: set[Path] = set()
    for config in CONFIGS:
        for sim in yaml.safe_load(config.read_text())["simulations"]:
            for name in sim.get("input_files", []):
                referenced.add((REPO / sim["input"] / name).resolve())
    run_dirs = {p.parent for p in referenced}

    tracked = subprocess.run(
        ["git", "ls-files", "test/*/inputs/*.yaml", "examples/*.yaml"], cwd=REPO, capture_output=True, text=True, check=True
    )
    orphans = sorted(
        p
        for line in tracked.stdout.splitlines()
        if line not in EXEMPT and (p := (REPO / line).resolve()).parent in run_dirs and p not in referenced
    )
    for p in orphans:
        print(f"ORPHAN: {p.relative_to(REPO)} -- its directory is run by a suite, this deck is not")
    if not orphans:
        print(f"OK: every deck in a suite-run directory is referenced ({len(referenced)} decks, {len(run_dirs)} directories)")
    return 1 if orphans else 0


if __name__ == "__main__":
    sys.exit(main())
