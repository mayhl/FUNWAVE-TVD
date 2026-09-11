"""Validation case pages, generated at docs build from the suite config and the board's record.

The config is the single source of truth for what a validation case IS -- its
decks, rank count, oracle module and tolerances -- so the page describing it is
generated rather than written; a hand-kept page drifts the first time a deck
changes.  Pages are built in memory by the mkdocs hook (docs/hooks.py) and are
never committed: authored pages are tracked, derived pages are built.

Measured numbers are NOT invented here.  A case renders its metrics only from
the results record a board wrote (workspaces/validation_report.json, the
tier-named twin of the HTML report); until then the page states plainly that no
run is recorded.  Figures follow the same rule: the record names them relative
to itself, and the hook copies them under docs/_generated/ at build.
"""

from __future__ import annotations

import json
import re
from pathlib import Path

import yaml

REPO = Path(__file__).resolve().parent.parent
CONFIG = REPO / "test" / "validation" / "validation_config.yaml"
REPORT = REPO / "workspaces" / "validation_report.json"
SECTION = "validation"
# artifact root, relative to docs/ -- untracked, filled at build
GENERATED = "_generated"

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

GROUPS = {
    "analytic": "Cases whose reference is a closed-form result, a conservation law or a convergence rate.",
    "lab": "Cases whose reference is a published laboratory experiment.",
}


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


def case_notes(config_path: Path = CONFIG) -> dict[str, tuple[str, str]]:
    """The comment block above each case entry: (lead line, body) per case name.

    The config comments are the case descriptions -- written once, beside the
    keys they explain -- so the page harvests them rather than carrying a copy.
    A `# -- title --` rule line becomes the lead; the rest is the body.
    """
    notes: dict[str, tuple[str, str]] = {}
    block: list[str] = []
    for raw in config_path.read_text().splitlines():
        line = raw.strip()
        if line.startswith("#"):
            block.append(line.lstrip("#").strip())
        elif line.startswith("- name:"):
            name = line.split(":", 1)[1].strip()
            lead, body = "", " ".join(block)
            if block and block[0].startswith("\u2500"):
                lead = block[0].strip("\u2500 ").rstrip(":")
                body = " ".join(block[1:])
            notes[name] = (lead, body)
            block = []
        elif line and not line.startswith("-"):
            block = [] if not block or not raw.startswith("  #") else block
    return notes


def load_report(path: Path = REPORT) -> dict | None:
    """The board record, or None; a record of another tier is ignored, not mis-rendered."""
    if not path.exists():
        return None
    record = json.loads(path.read_text())
    return record if isinstance(record, dict) and record.get("tier") == SECTION else None


def fmt(value) -> str:
    if isinstance(value, list):
        return " x ".join(str(v) for v in value)
    return str(value)


def figure_target(case: str, variant: str | None, fig: dict) -> str:
    """Where a record figure lands under docs/, relative to docs/ (variants
    write same-named files, so each gets its own folder)."""
    return f"{GENERATED}/{SECTION}/{case}/{variant or 'run'}/{Path(fig['path']).name}"


def render(
    sim: dict, rows: list[dict], report: dict | None, depth: int, diagram: str | None = None, note: tuple[str, str] = ("", "")
) -> str:
    """Render one case page.  `rows` are the record rows for this case (one per
    variant); `depth` = parent directories between docs/ and the page source,
    the prefix an artifact link needs to climb back to docs/."""
    name = sim["name"]
    decks = load_decks(sim)
    up = "../" * depth
    lines: list[str] = []

    lines.append(f"# {name}")
    lines.append("")
    lead, body = note
    if lead:
        lines.append(f"**{lead[0].upper() + lead[1:]}.** {body}".rstrip())
    elif body:
        lines.append(body)
    else:
        lines.append(f"Runs `{len(sim.get('input_files', []))}` deck(s) from `{sim['input']}`.")
    lines.append("")

    # ── setup, read out of the decks themselves ──
    lines.append("## Setup")
    lines.append("")
    if decks:
        table = []
        for dotted, label in SETUP_KEYS:
            values = {fmt(dig(deck, dotted)) for _, deck in decks if dig(deck, dotted) is not None}
            if values:
                table.append((label, ", ".join(sorted(values))))
        if table:
            lines.append("| Setting | Value |")
            lines.append("| --- | --- |")
            lines.extend(f"| {label} | {value} |" for label, value in table)
        else:
            lines.append("The decks declare none of the summarised keys.")
        lines.append("")
        # the real decks, transcluded -- the page cannot drift from them; one
        # collapsed block, a tab per deck
        lines.append('??? example "Decks"')
        lines.append("")
        for deck_name, _ in decks:
            lines.append(f'    === "{deck_name}"')
            lines.append("")
            lines.append("        ```yaml")
            lines.append(f'        --8<-- "{sim["input"]}/{deck_name}"')
            lines.append("        ```")
            lines.append("")
    else:
        lines.append("Deck files are not present in this checkout.")
    lines.append("")

    # a diagram script sharing the page's stem is hand-authored content the
    # hook renders; generated pages could otherwise never carry one
    if diagram:
        lines.append(f"![Domain sketch]({up}{GENERATED}/{diagram})")
        lines.append("")

    # ── results: what a board recorded ──
    lines.append("## Results")
    lines.append("")
    if rows:
        record = report or {}
        engine = record.get("engine") or {}
        when = (record.get("generated_at") or "")[:10]
        sha = (engine.get("sha") or "").replace("-dirty", "")[:8]
        lines.append(f"Run on {when} with FUNWAVE `{sha}` (`{engine.get('branch', '?')}`).")
        lines.append("")
        labels = sim.get("labels") or {}
        lines.extend(_pivot_table(rows, labels))
        for row in rows:
            if row.get("notes"):
                lines.append(f"> **{run_label(row.get('variant'), labels)}:** {row['notes']}")
                lines.append("")
        # a tab per run selects its figures
        with_figs = [row for row in rows if row.get("figures")]
        for row in with_figs:
            tabbed = len(with_figs) > 1 or row.get("variant")
            if tabbed:
                lines.append(f'=== "{run_label(row.get("variant"), labels)}"')
                lines.append("")
            pad = "    " if tabbed else ""
            for fig in row["figures"]:
                lines.append(f"{pad}![{fig.get('title', '')}]({up}{figure_target(name, row.get('variant'), fig)})")
                lines.append("")
    else:
        lines.append('!!! note "No run recorded"')
        lines.append("")
        lines.append("    No board has written a validation record for this case; numbers appear")
        lines.append("    here once one does.  Nothing is inferred from the config.")
        lines.append("")

    return "\n".join(lines) + "\n"


def run_label(variant: str | None, labels: dict[str, str]) -> str:
    """Human name for a variant: the config's `labels:` entry, else the deck stem
    with a rank pin spelled out."""
    if not variant:
        return "run"
    if variant in labels:
        return labels[variant]
    m = re.fullmatch(r"(?:(.*)_)?np(\d+)", variant)
    if m:
        deck, n = m.group(1), int(m.group(2))
        ranks = f"{n} rank" + ("s" if n > 1 else "")
        return f"{labels.get(deck, deck)}, {ranks}" if deck else ranks
    if variant.endswith("_sweep"):
        deck = variant[: -len("_sweep")]
        return f"{labels.get(deck, deck)}, rank sweep"
    return "rank sweep" if variant == "sweep" else variant


def _pivot_table(rows: list[dict], labels: dict[str, str]) -> list[str]:
    """One row per variant, one column per (variable, stat) the oracle reported;
    gated cells carry the verdict, so a sweep reads top to bottom."""
    columns: list[tuple[str, str]] = []
    for row in rows:
        for m in row.get("metrics") or []:
            key = (m["variable"], m["stat"])
            if key not in columns:
                columns.append(key)
    variables = {v for v, _ in columns}
    # a gated column carries its bound in the header; the cell keeps the verdict
    bounds = {
        (m["variable"], m["stat"]): m["tolerance"]
        for row in rows
        for m in row.get("metrics") or []
        if m.get("tolerance") not in (None, float("inf"))
    }
    names = {(m["variable"], m["stat"]): m.get("label") for row in rows for m in row.get("metrics") or []}
    head = [
        (names.get((v, s)) or (s if len(variables) == 1 else f"{v} {s}"))
        + (f" (< {bounds[(v, s)]:.4g})" if (v, s) in bounds else "")
        for v, s in columns
    ]
    lines = ["| Run | " + " | ".join(head) + " | Result |", "| --- |" + " --- |" * len(columns) + " --- |"]
    for row in rows:
        by_key = {(m["variable"], m["stat"]): m for m in row.get("metrics") or []}
        cells = []
        for key in columns:
            m = by_key.get(key)
            if m is None:
                cells.append("&mdash;")
                continue
            gated = m.get("tolerance") not in (None, float("inf"))
            value = f"{m['value']:.4g}"
            if gated:
                value = f"**{value}** " + ("✓" if m.get("passed") else "✗")
            cells.append(value)
        lines.append(f"| {run_label(row.get('variant'), labels)} | " + " | ".join(cells) + f" | {row['status']} |")
    lines.append("")
    return lines


def render_index(group: str, sims: list[dict], by_case: dict[str, list[dict]]) -> str:
    """The group overview: one row per case, verdict from the record when present."""
    lines = [f"# {group.capitalize()} Cases", "", GROUPS[group], ""]
    lines.append("| Case | Check | Tags | Result |")
    lines.append("| --- | --- | --- | --- |")
    for sim in sims:
        rows = by_case.get(sim["name"], [])
        labels = sim.get("labels") or {}
        verdict = ", ".join(f"{run_label(r['variant'], labels)}: {r['status']}" for r in rows) if rows else "&mdash;"
        oracle = ", ".join(f"`{k}`" for k in (sim.get("postprocess") or {}))
        tags = ", ".join(t for t in sim.get("tags", []) if t != group)
        lines.append(f"| [{sim['name']}]({sim['name']}.md) | {oracle} | {tags} | {verdict} |")
    lines.append("")
    return "\n".join(lines) + "\n"


def build(
    config_path: Path = CONFIG, report_path: Path = REPORT, docs_dir: Path = REPO / "docs"
) -> tuple[dict[str, str], dict, list[tuple[str, dict]]]:
    """Return (pages keyed by docs-relative path, nav subtree for the section,
    figures to copy as (case, variant, record figure))."""
    config = yaml.safe_load(config_path.read_text())
    report = load_report(report_path)
    by_case: dict[str, list[dict]] = {}
    for row in (report or {}).get("results", []):
        by_case.setdefault(row["case"], []).append(row)

    notes = case_notes(config_path)
    pages: dict[str, str] = {}
    nav: dict[str, list] = {g: [] for g in GROUPS}
    figures: list[tuple[str, str | None, dict]] = []
    grouped: dict[str, list[dict]] = {g: [] for g in GROUPS}
    for sim in config["simulations"]:
        group = group_of(sim.get("tags", []))
        grouped[group].append(sim)
        src = f"{SECTION}/{group}/{sim['name']}.md"
        script = docs_dir / Path(src).with_suffix(".py")
        diagram = str(Path(src).with_suffix(".svg")) if script.exists() else None
        rows = by_case.get(sim["name"], [])
        pages[src] = render(sim, rows, report, depth=2, diagram=diagram, note=notes.get(sim["name"], ("", "")))
        nav[group].append({sim["name"]: src})
        figures.extend((sim["name"], r.get("variant"), f) for r in rows for f in r.get("figures") or [])

    for group, sims in grouped.items():
        pages[f"{SECTION}/{group}/index.md"] = render_index(group, sims, by_case)

    section_nav = [
        {"Overview": f"{SECTION}/index.md"},
        *({group.capitalize(): [{"Overview": f"{SECTION}/{group}/index.md"}, *nav[group]]} for group in GROUPS),
    ]
    return pages, {"Validation": section_nav}, figures
