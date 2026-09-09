#!/usr/bin/env python3
"""Generate per-case validation pages from test/validation/validation_config.yaml.

The config is the single source of truth for what a validation case IS -- its
decks, rank count, oracle module and tolerances -- so the page describing it is
generated rather than written; a hand-kept page drifts the first time a deck
changes.  Generated pages are COMMITTED, like config_reference.md, so the site
builds from a clean clone with no Python step (see .private_docs/STANDARDS.md).

Measured numbers are NOT invented here.  A case renders its metrics only when a
results record exists at test/validation/results/<case>.json, written by the
board that ran it; until then the page states plainly that no run is recorded.
Figures follow the same rule, and their files are untracked artifacts under
docs/_generated/ pulled from a board bundle -- the page carries the reference,
never the image.

Usage:
    uv run tools/gen_validation_docs.py            # regenerate in place
    uv run tools/gen_validation_docs.py --check    # verify committed pages
                                                     # (exit 1 + diff if stale)
"""

from __future__ import annotations

import argparse
import difflib
import json
import sys
from pathlib import Path

import yaml

REPO = Path(__file__).resolve().parent.parent
CONFIG = REPO / "test" / "validation" / "validation_config.yaml"
RESULTS = REPO / "test" / "validation" / "results"
DOCS = REPO / "docs" / "validation"
# artifact root, relative to docs/ -- untracked, filled from a board bundle
GENERATED = "_generated"

# Cases with a reviewed page.  Expand deliberately: every entry adds a tracked
# page and a nav row, so a case lands here once its oracle output is worth
# publishing, not merely because it exists in the config.
CASES = (
    "dispersion_2d",
    "lab_synolakis_brk_visc",
)

# Deck keys worth showing in Setup, as (dotted path, label).  Absent keys are
# skipped rather than rendered empty -- presence IS the configuration in this
# schema, so a missing row means the block is off.
SETUP_KEYS = (
    ("grid.n_cells", "Grid"),
    ("grid.cell_size", "Cell size (m)"),
    ("grid.bathymetry.type", "Bathymetry"),
    ("simulation.total_time", "Duration (s)"),
    ("numerics.cfl", "CFL"),
    ("numerics.min_depth", "Minimum depth (m)"),
    ("breaking.model", "Breaking model"),
    ("breaking.solver", "Breaker solver"),
    ("wavemaker.spectrum.type", "Wavemaker"),
)


def group_of(tags: list[str]) -> str:
    """Lab cases carry a published experiment; everything else checks mathematics."""
    return "lab" if "lab" in tags else "analytic"


def dig(deck: dict, dotted: str):
    node = deck
    for key in dotted.split("."):
        if not isinstance(node, dict) or key not in node:
            return None
        node = node[key]
    return node


def load_decks(sim: dict) -> list[tuple[str, dict]]:
    """Return (filename, parsed deck) for every input file the case runs."""
    base = REPO / sim["input"]
    out = []
    for name in sim.get("input_files", []):
        path = base / name
        if path.exists():
            out.append((name, yaml.safe_load(path.read_text()) or {}))
    return out


def load_results(case: str) -> dict | None:
    path = RESULTS / f"{case}.json"
    return json.loads(path.read_text()) if path.exists() else None


def fmt(value) -> str:
    if isinstance(value, list):
        return " x ".join(str(v) for v in value)
    return str(value)


def render(sim: dict, results: dict | None, depth: int, diagram: str | None = None) -> str:
    """Render one case page.  `depth` = parent directories between docs/ and the
    page source, the prefix an artifact link needs to climb back to docs/."""
    name = sim["name"]
    decks = load_decks(sim)
    up = "../" * depth
    lines: list[str] = []

    lines.append(f"# {name}")
    lines.append("")
    lines.append(f"Runs `{len(sim.get('input_files', []))}` deck(s) on {sim.get('np', 1)} rank(s) from `{sim['input']}`.")
    lines.append("")

    # ── setup, read out of the decks themselves ──
    lines.append("## Setup")
    lines.append("")
    if decks:
        rows = []
        for dotted, label in SETUP_KEYS:
            values = {fmt(dig(deck, dotted)) for _, deck in decks if dig(deck, dotted) is not None}
            if values:
                rows.append((label, ", ".join(sorted(values))))
        if rows:
            lines.append("| Setting | Value |")
            lines.append("| --- | --- |")
            lines.extend(f"| {label} | {value} |" for label, value in rows)
        else:
            lines.append("The decks declare none of the summarised keys.")
        lines.append("")
        lines.append("Decks: " + ", ".join(f"`{n}`" for n, _ in decks))
    else:
        lines.append("Deck files are not present in this checkout.")
    lines.append("")

    # a diagram script sharing the page's stem is hand-authored content the
    # hook renders; generated pages could otherwise never carry one
    if diagram:
        lines.append(f"![Domain sketch]({up}{GENERATED}/{diagram})")
        lines.append("")

    # ── oracle ──
    lines.append("## Oracle")
    lines.append("")
    for key, module in (sim.get("postprocess") or {}).items():
        lines.append(f"`{key}` &mdash; `{module}`")
        lines.append("")
        tol = (sim.get("tolerances") or {}).get(key) or {}
        scalar = {k: v for k, v in tol.items() if not isinstance(v, list)}
        if scalar:
            lines.append("| Tolerance | Value |")
            lines.append("| --- | --- |")
            lines.extend(f"| `{k}` | {v} |" for k, v in scalar.items())
            lines.append("")

    # ── results, only when a board recorded them ──
    lines.append("## Results")
    lines.append("")
    if results:
        lines.append(
            f"Engine `{results.get('engine_sha', '?')}`, board `{results.get('board', '?')}`, "
            f"{results.get('date', '?')} &mdash; **{results.get('verdict', '?')}**"
        )
        lines.append("")
        metrics = results.get("metrics") or []
        if metrics:
            lines.append("| Metric | Value | Tolerance | |")
            lines.append("| --- | --- | --- | --- |")
            for m in metrics:
                mark = "PASS" if m.get("passed") else "FAIL"
                lines.append(f"| {m.get('name', '?')} | {m.get('value', '?')} | {m.get('tolerance', '?')} | {mark} |")
            lines.append("")
        for figure in results.get("figures") or []:
            lines.append(f"![{figure.get('caption', '')}]({up}{GENERATED}/{figure['path']})")
            lines.append("")
    else:
        lines.append('!!! note "No run recorded"')
        lines.append("")
        lines.append(f"    No results record at `test/validation/results/{name}.json`. Numbers appear")
        lines.append("    here once a board writes one; nothing is inferred from the config.")
        lines.append("")

    lines.append("<!-- generated by tools/gen_validation_docs.py -- do not edit;")
    lines.append("     sync test: uv run tools/gen_validation_docs.py --check -->")
    return "\n".join(lines) + "\n"


def page_path(sim: dict) -> Path:
    return DOCS / group_of(sim.get("tags", [])) / f"{sim['name']}.md"


def build() -> dict[Path, str]:
    config = yaml.safe_load(CONFIG.read_text())
    by_name = {s["name"]: s for s in config["simulations"]}
    missing = [c for c in CASES if c not in by_name]
    if missing:
        print(f"unknown case(s) in CASES: {', '.join(missing)}", file=sys.stderr)
        raise SystemExit(2)

    pages = {}
    for case in CASES:
        sim = by_name[case]
        path = page_path(sim)
        # links resolve from the SOURCE directory, not the rendered URL -- mkdocs
        # rewrites for directory URLs itself, so counting the page's own segment
        # yields a path one level too high (caught by --strict, 404 without it)
        depth = len(path.relative_to(REPO / "docs").parts) - 1
        script = path.with_suffix(".py")
        diagram = str(script.relative_to(REPO / "docs").with_suffix(".svg")) if script.exists() else None
        pages[path] = render(sim, load_results(case), depth=depth, diagram=diagram)
    return pages


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--check", action="store_true", help="verify committed pages match the config")
    args = ap.parse_args()

    pages = build()
    stale = False
    for path, new in sorted(pages.items()):
        rel = path.relative_to(REPO)
        if args.check:
            old = path.read_text() if path.exists() else ""
            if old == new:
                print(f"OK: {rel} is in sync with validation_config.yaml")
                continue
            stale = True
            print(f"STALE: {rel} does not match validation_config.yaml — rerun: uv run tools/gen_validation_docs.py")
            sys.stdout.writelines(
                difflib.unified_diff(
                    old.splitlines(keepends=True),
                    new.splitlines(keepends=True),
                    fromfile="committed",
                    tofile="generated",
                )
            )
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(new)
            print(f"wrote {rel}")

    return 1 if stale else 0


if __name__ == "__main__":
    sys.exit(main())
