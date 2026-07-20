"""
Solitary-wave runup validation oracle (Synolakis 1987 plane beach).

Ref-less validation postproc: a solitary wave of height H over depth h runs up
the cot(beta) plane beach; for nonbreaking waves the maximum vertical runup
obeys the asymptotic NSWE runup law

    R/h = 2.831 sqrt(cot beta) (H/h)^{5/4}.

The oracle tracks the wet/dry front through the MASK frames and takes the
runup as the maximum over frames of the surface elevation at the front-most
wet cell (sub-cell granularity ~ dx * slope, a few % here).  It also reports
the measured incident crest height just seaward of the beach toe, since the
law's H is the toe value, not the release value.

Note 1: the model lands ~10 % BELOW the law (R/h ~ 0.080 vs 0.089 at
H/h = 0.019, cot beta = 19.85), robust across dispersion scheme, dx,
min_depth, and the breaking path.  ADJUDICATED INHERITED (2026-07-20): the
legacy FUNWAVE-TVD binary on the same bathymetry/IC gives R = 0.0797 vs
modern 0.0794 — 0.3 % apart — so the distance from the asymptotic law is
standing FUNWAVE-TVD behaviour (lab data scatter below the law as well),
not a modern-side swash regression.  The 15 % law-referenced gate is the
drift tripwire around that documented behaviour.

Deck params read from the run YAML: initial.solitary {amplitude, depth} and
grid.bathymetry {slope, x0}.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) ->
SubsectionResult (ref_dir is None in oracle mode and unused).

Tolerance keys in validation_config.yaml (under tolerances: runup:):
  runup_error_pct   (default 15 %)
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
import yaml
from rich import box
from rich.console import Console
from rich.table import Table

from test.framework.results import MetricResult, SubsectionResult
from test.regression.postproc.utils import read_run_metadata

_console = Console()


def _deck_params(run_dir: Path) -> tuple[float, float, float, float]:
    """Return (H, h, slope, toe_x) from the run deck."""
    deck = sorted(run_dir.glob("*.yaml"))[0]
    with open(deck) as f:
        cfg = yaml.safe_load(f)
    sol = cfg["initial"]["solitary"]
    bathy = cfg["grid"]["bathymetry"]
    return (float(sol["amplitude"]), float(sol["depth"]),
            float(bathy["slope"]), float(bathy["x0"]))


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    meta = read_run_metadata(dev_dir)

    dep_files = meta.output_files("DEPTH_OUT")
    mask_files = meta.output_files("MASK")
    eta_files = meta.output_files("ETA")
    if not dep_files or len(mask_files) < 5 or len(eta_files) < 5:
        _console.print("[yellow]runup:[/yellow] need dep.out + MASK + ETA frames — skipping")
        return SubsectionResult(kind="statistics", label="Runup", metrics=[])

    height, h, slope, toe_x = _deck_params(dev_dir)
    law = 2.831 * math.sqrt(1.0 / slope) * (height / h) ** 1.25 * h

    mid = meta.ny // 2
    i_toe = max(0, int(toe_x / meta.dx) - int(2.0 / meta.dx))  # 2 m seaward of the toe

    runup = 0.0
    h_toe = 0.0
    for pm, pe in zip(mask_files, eta_files):
        m = meta.read_field(pm).astype(int)[mid]
        e = meta.read_field(pe).astype(float)[mid]
        wet = np.where(m == 1)[0]
        if len(wet):
            runup = max(runup, float(e[wet.max()]))
        h_toe = max(h_toe, float(e[i_toe]))

    runup_error = abs(runup / law - 1.0) * 100.0
    tol = float(tolerances.get("runup_error_pct", 15.0))
    ok = math.isfinite(runup_error) and runup_error < tol

    metrics = [
        MetricResult("runup", "n_frames", float(len(mask_files)), True, math.inf),
        MetricResult("runup", "runup_error_pct", runup_error, ok, tol),
    ]

    if verbose or not ok:
        table = Table(box=box.SIMPLE_HEAD, header_style="bold cyan", show_edge=False,
                      pad_edge=True, title="[bold]Solitary Runup (Synolakis)[/bold]",
                      title_justify="left")
        table.add_column("Metric", min_width=22)
        table.add_column("Value", justify="right", min_width=14)
        table.add_column("Tolerance", justify="right", min_width=12)
        table.add_column("", min_width=10)
        table.add_row("Frames", f"{len(mask_files)}", "—", "")
        table.add_row("Incident crest (toe)", f"{h_toe:.5f} m", "—", "")
        table.add_row("Runup (model)", f"{runup:.5f} m", "—", "")
        table.add_row("Runup (law)", f"{law:.5f} m", "—", "")
        status = "[bold green]✓ PASS[/bold green]" if ok else "[bold red]✗ FAIL[/bold red]"
        c = "green" if ok else "red"
        table.add_row("Runup error", f"[{c}]{runup_error:.4g} %[/{c}]", f"{tol:.3g} %", status)
        _console.print(table)

    return SubsectionResult(kind="statistics", label="Runup", metrics=metrics)
