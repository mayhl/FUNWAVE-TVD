"""
Synolakis (1987) runup: measured eta(x) at published instants, model
nondimensionalized by the deck depth and interpolated in time and space.
One case-wide norm scale (per-instant blows up on drawdown); dry land is
clamped to -depth (the NTHMP archives record the beach face).

Keys: profiles (file names; headers carry t_star).
Gate: profile_nrmse_pct on the worst instant.
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np

from test.framework.results import MetricResult, SubsectionResult
from test.regression.postproc.utils import read_run_metadata
from test.validation.oracles._lab import frame_times, load_deck, new_figure, read_table, save_figure

G = 9.81  # m s-2

_LABEL = "Synolakis runup"


def _skip(msg: str) -> SubsectionResult:
    print(f"synolakis: {msg} — skipping")
    return SubsectionResult(kind="statistics", label=_LABEL, metrics=[])


def _model_profile(meta, eta_files, times, t_want, dep):
    """Return the mid-row eta profile at t_want, linearly interpolated in time."""
    if t_want < times[0] or t_want > times[-1]:
        return None
    k = int(np.searchsorted(times, t_want))
    k0 = max(0, k - 1)
    k1 = min(len(times) - 1, k)
    mid = meta.ny // 2
    e0 = meta.read_field(eta_files[k0]).astype(float)[mid]
    e1 = meta.read_field(eta_files[k1]).astype(float)[mid]
    w = 0.0 if k1 == k0 else (t_want - times[k0]) / (times[k1] - times[k0])
    eta = (1.0 - w) * e0 + w * e1
    # eta below ground is the sturdier wetness test than the MASK frame
    return np.maximum(eta, -dep)


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    names = tolerances.get("profiles") or []
    if not names:
        return _skip("tolerances block needs profiles:")

    meta = read_run_metadata(dev_dir)
    eta_files = meta.output_files("ETA")
    dep_files = meta.output_files("DEPTH_OUT")
    if len(eta_files) < 5 or not dep_files:
        return _skip("need eta frames + dep.out")
    times = frame_times(eta_files)

    deck = load_deck(dev_dir)
    bathy = deck["grid"]["bathymetry"]
    h = float(deck["initial"]["solitary"]["depth"])
    x_shore = float(bathy["x0"]) + h / float(bathy["slope"])

    dep = meta.read_field(dep_files[0]).astype(float)[meta.ny // 2]
    x_cell = (np.arange(meta.nx) + 0.5) * meta.dx  # cell centers, matching dep.out
    xprime = (x_shore - x_cell) / h  # decreasing along i

    loaded = []
    for name in names:
        data, head = read_table(dev_dir / name)
        loaded.append((float(head["t_star"]), data[:, 0], data[:, 1]))
    scale = max(float(np.max(np.abs(em))) for _, _, em in loaded)

    tol = float(tolerances.get("profile_nrmse_pct", 15.0))
    metrics, panels, worst = [], [], 0.0
    for t_star, xm, em in loaded:
        eta = _model_profile(meta, eta_files, times, t_star * math.sqrt(h / G), dep)
        if eta is None:
            metrics.append(MetricResult("synolakis", f"nrmse_t{t_star:g}_pct", float("nan"), True, math.inf))
            worst = float("inf")
            continue
        e_mod = np.interp(xm, xprime[::-1], (eta / h)[::-1])  # np.interp wants ascending xp
        err = 100.0 * float(np.sqrt(np.mean((e_mod - em) ** 2))) / scale
        worst = max(worst, err)
        metrics.append(MetricResult("synolakis", f"nrmse_t{t_star:g}_pct", err, True, math.inf))
        panels.append((t_star, xm, em, e_mod))

    ok = math.isfinite(worst) and worst < tol
    metrics.append(MetricResult("synolakis", "profile_nrmse_pct", worst, ok, tol))

    figures = []
    if panels:
        fig, axes = new_figure(nrows=len(panels))
        for ax, (t_star, xm, em, e_mod) in zip(axes, panels):
            ax.plot(xm, em, "o", ms=3, color="#2563eb", label="measured")
            ax.plot(xm, e_mod, "-", color="#dc2626", label="modern")
            ax.set_ylabel(f"eta/h  (t*={t_star:g})")
            ax.grid(alpha=0.3)
            if ax is axes[0]:
                ax.legend(loc="upper right", fontsize=8)
        axes[-1].set_xlabel("x/h (seaward from initial shoreline)")
        figures.append(save_figure(fig, plots_dir, "synolakis", "eta(x) at published instants"))

    return SubsectionResult(kind="statistics", label=_LABEL, metrics=metrics, figures=figures)
