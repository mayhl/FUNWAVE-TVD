"""
Flather open-boundary transmission oracle (2D Boussinesq).

Ref-less validation postproc for the characteristic/Flather BC (forcing without
a relaxation strip).  A slow tide (period >> basin transit) is forced on the
open face(s); a faithful radiation boundary imposes it with no strip attenuation,
so the interior tracks the prescribed range.  The metric is the interior-centre
transmission ratio = (interior peak-to-peak) / (reference peak-to-peak).

The reference is the prescribed eta range read from the west forcing file, unless
the config supplies reference_pp_m (needed for the u/v-only arm, whose eta column
is zero -- there the reference is the velocity-equivalent elevation range).

Expected ratios (measured + understood; see the cases):
  * co-oscillation, eta both ends              -> ~1.0  (standing tide, both ends set)
  * progressive, eta + u (full R+ invariant)   -> ~1.0
  * progressive, eta only OR u/v only          -> ~0.5  (half the Riemann invariant)
The relaxation strip gives 0.2-0.4 for all of these; Flather is the fix.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) -> SubsectionResult

Tolerance keys (under tolerances: flather_transmission:):
  ratio_min, ratio_max  -- gated window on the transmission ratio
  reference_pp_m        -- optional; overrides the eta range read from the file
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
from rich.console import Console

from test.framework.results import MetricResult, SubsectionResult
from test.framework.tolerances import check_keys
from test.regression.postproc.utils import read_run_metadata
from test.validation.oracles._lab import load_deck

_console = Console()
ACCEPTED_KEYS = ("ratio_min", "ratio_max", "reference_pp_m")


def _file_eta_pp(run_dir: Path, deck: dict) -> float:
    """Peak-to-peak of the eta column in the west forcing file (0 if none)."""
    try:
        fname = deck["boundaries"]["west"]["forcing"]["file"]
    except (KeyError, TypeError):
        return 0.0
    path = run_dir / fname
    if not path.exists():
        return 0.0
    eta = []
    for k, line in enumerate(path.read_text().splitlines()):
        if k == 0 or not line.strip():
            continue
        parts = line.split()
        if len(parts) >= 2:
            eta.append(float(parts[1]))
    return (max(eta) - min(eta)) if eta else 0.0


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    check_keys(tolerances, ACCEPTED_KEYS, "flather_transmission")
    meta = read_run_metadata(dev_dir)
    deck = load_deck(dev_dir)

    eta_files = meta.output_files("ETA")
    if len(eta_files) < 4:
        _console.print("[yellow]flather_transmission:[/yellow] need >=4 ETA frames — skipping")
        return SubsectionResult(kind="statistics", label="Flather transmission", metrics=[])

    # interior-centre time series over the back half of the run (skip startup)
    n = len(eta_files)
    i0 = n // 2
    centre = []
    for ep in eta_files[i0:]:
        a = meta.read_field(ep).astype(float)
        centre.append(a[a.shape[0] // 2, a.shape[1] // 2])
    interior_pp = float(np.max(centre) - np.min(centre))

    ref_pp = float(tolerances.get("reference_pp_m", 0.0)) or _file_eta_pp(dev_dir, deck)
    ratio = interior_pp / ref_pp if ref_pp > 0 else float("nan")

    rmin = float(tolerances.get("ratio_min", 0.9))
    rmax = float(tolerances.get("ratio_max", 1.1))
    ok = math.isfinite(ratio) and rmin <= ratio <= rmax

    metrics = [
        MetricResult("flather_transmission", "n_frames", float(n), True, math.inf),
        MetricResult("flather_transmission", "interior_pp_m", interior_pp, True, math.inf),
        MetricResult("flather_transmission", "ratio", ratio, ok, rmax),
    ]
    if verbose or not ok:
        _console.print(
            f"flather_transmission: interior_pp={interior_pp:.4f} ref_pp={ref_pp:.4f} "
            f"ratio={ratio:.3f} (window [{rmin}, {rmax}])"
        )
    return SubsectionResult(kind="statistics", label="Flather transmission", metrics=metrics)
