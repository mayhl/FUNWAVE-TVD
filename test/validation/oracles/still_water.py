"""
Still-water-level validation oracle (2D Boussinesq).

Ref-less validation postproc for grid.water_level.  A flat, unforced basin with a
nonzero still-water level must (1) bake the level into the depth datum
(depth = d + W at init) and (2) leave the surface quiescent, since output eta is
referenced to that level (registry water_level, main.f90 apply_water_level).  A
level that leaked into the eta frame would read eta ~ W; a datum that was not
shifted would read depth ~ d.  Both are caught here.

The stronger claim -- that W adds identically to the depth AND the wavemaker
reference depth -- is the (d, W) vs (d + W, 0) bit-identical cross-check, run
separately (see the task report); a single-deck oracle cannot compare two runs.

Metrics:
  max_eta_m         -- max_t max_ij |eta| over the run.  Zero to roundoff for a
                       correct still run; the numerical flat-state transient sets
                       the floor.  The gate sits far below a leaked level (~W) but
                       above that floor.
  depth_datum_err_m -- |max(depth) - (d + W)| from the deck's grid.bathymetry.depth
                       and grid.water_level.  Gates that the level reached the depth
                       datum; tight (near machine precision).

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) -> SubsectionResult
(ref_dir is None in oracle mode and unused.)

Tolerance keys (under tolerances: still_water: in validation_config.yaml):
  max_eta_m          -- max allowed surface excursion (default 1e-3 m)
  depth_datum_err_m  -- max allowed datum error (default 1e-6 m)
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
from rich.console import Console

from test.framework.results import MetricResult, SubsectionResult
from test.framework.run_output import read_run_metadata
from test.framework.tolerances import check_keys
from test.validation.oracles._lab import load_deck

_console = Console()
ACCEPTED_KEYS = ("max_eta_m", "depth_datum_err_m")


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    check_keys(tolerances, ACCEPTED_KEYS, "still_water")
    meta = read_run_metadata(dev_dir)
    deck = load_deck(dev_dir)

    d = float(deck["grid"]["bathymetry"]["depth"])
    w = float(deck["grid"].get("water_level", 0.0))

    eta_files = meta.output_files("ETA")
    dep_files = meta.output_files("DEPTH_OUT")
    if not dep_files or len(eta_files) < 2:
        _console.print("[yellow]still_water:[/yellow] need dep.out + >=2 ETA frames — skipping")
        return SubsectionResult(kind="statistics", label="Still water level", metrics=[])

    max_eta = 0.0
    for ep in eta_files:
        max_eta = max(max_eta, float(np.max(np.abs(meta.read_field(ep).astype(float)))))

    depth = meta.read_field(dep_files[0]).astype(float)
    depth_err = abs(float(np.max(depth)) - (d + w))

    tol_eta = float(tolerances.get("max_eta_m", 1e-3))
    tol_dat = float(tolerances.get("depth_datum_err_m", 1e-6))
    eta_ok = math.isfinite(max_eta) and max_eta < tol_eta
    dat_ok = math.isfinite(depth_err) and depth_err < tol_dat

    metrics = [
        MetricResult("still_water", "n_frames", float(len(eta_files)), True, math.inf),
        MetricResult("still_water", "max_eta_m", max_eta, eta_ok, tol_eta),
        MetricResult("still_water", "depth_datum_err_m", depth_err, dat_ok, tol_dat),
    ]

    if verbose or not (eta_ok and dat_ok):
        _console.print(f"still_water: d={d} W={w}  max|eta|={max_eta:.3e} (<{tol_eta}) datum_err={depth_err:.3e} (<{tol_dat})")

    return SubsectionResult(kind="statistics", label="Still water level", metrics=metrics)
