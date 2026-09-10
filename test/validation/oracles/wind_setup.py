"""
Wind setup validation oracle (2D Boussinesq).

Analytic ref-less postproc for meteo.wind.  A closed rectangular basin under a
steady uniform wind reaches a static balance between the wind stress and the
surface pressure gradient, so the equilibrium setup slope is

    d(eta)/dx = tau / (rho g h),   tau/rho = (rho_a/rho) Cd_w W^2

with rho_a/rho = RHO_AW = 0.0012041 (constants.f90), Cd_w = meteo.wind.cd, W the
steady wind speed, h the still depth.  The wind is ramped on over ~2 seiche
periods to suppress the onset seiche; the small residual seiche is removed by
time-averaging the fitted slope over the settled (back) half of the run.

Metric:
  slope_ratio  -- (time-mean fitted d(eta)/dx) / (analytic d(eta)/dx).  Gated
                  near 1.0; the window absorbs the residual seiche.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) -> SubsectionResult

Tolerance keys (under tolerances: wind_setup:):
  ratio_min, ratio_max  -- gated window on slope_ratio
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
ACCEPTED_KEYS = ("ratio_min", "ratio_max")
RHO_AW = 0.0012041  # rho_air / rho_water (core_constants_mod)
G = 9.81


def _steady_wind_speed(run_dir: Path, deck: dict) -> float:
    """Peak |(WU, WV)| in the wind file — the held steady value after the ramp."""
    fname = deck["meteo"]["wind"]["file"]
    speeds = []
    for k, line in enumerate((run_dir / fname).read_text().splitlines()):
        if k < 2 or not line.strip():  # skip title + count
            continue
        p = line.split()
        if len(p) >= 3:
            speeds.append(math.hypot(float(p[1]), float(p[2])))
    return max(speeds) if speeds else 0.0


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    check_keys(tolerances, ACCEPTED_KEYS, "wind_setup")
    meta = read_run_metadata(dev_dir)
    deck = load_deck(dev_dir)

    eta_files = meta.output_files("ETA")
    if len(eta_files) < 4:
        _console.print("[yellow]wind_setup:[/yellow] need >=4 ETA frames — skipping")
        return SubsectionResult(kind="statistics", label="Wind setup", metrics=[])

    cd = float(deck["meteo"]["wind"].get("cd", 0.002))
    w = _steady_wind_speed(dev_dir, deck)
    h = float(deck["grid"]["bathymetry"]["depth"])
    slope_th = RHO_AW * cd * w * w / (G * h)

    # time-mean fitted slope over the settled back half (absorbs residual seiche)
    slopes = []
    for ep in eta_files[len(eta_files) // 2 :]:
        row = meta.read_field(ep).astype(float)
        row = row[row.shape[0] // 2, :]
        x = np.arange(row.size) * meta.dx
        slopes.append(np.polyfit(x, row, 1)[0])
    slope_meas = float(np.mean(slopes))
    ratio = slope_meas / slope_th if slope_th != 0 else float("nan")

    rmin = float(tolerances.get("ratio_min", 0.9))
    rmax = float(tolerances.get("ratio_max", 1.1))
    ok = math.isfinite(ratio) and rmin <= ratio <= rmax

    metrics = [
        MetricResult("wind_setup", "slope_analytic", slope_th, True, math.inf),
        MetricResult("wind_setup", "slope_measured", slope_meas, True, math.inf),
        MetricResult("wind_setup", "slope_ratio", ratio, ok, rmax),
    ]
    if verbose or not ok:
        _console.print(
            f"wind_setup: W={w:.3f} cd={cd} h={h}  slope theory={slope_th:.3e} "
            f"meas={slope_meas:.3e} ratio={ratio:.3f} (window [{rmin}, {rmax}])"
        )
    return SubsectionResult(kind="statistics", label="Wind setup", metrics=metrics)
