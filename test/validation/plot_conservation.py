#!/usr/bin/env python
"""
Mass / energy conservation diagram (2D Boussinesq).

Diagnostic companion to the conservation gate (test/validation/oracles/conservation.py).
Reads the ETA/U/V field frames a validation run already wrote and plots the two
conserved budgets over time, so the gate's single numbers get a visual story:

  * mass  — total excess volume V = integral eta dA, drift |V(t) - V(0)| / V_still
            on a log axis; a flux-form scheme with wall BC pins this at machine
            precision (a flat ~1e-9 % floor), which a plot makes obvious.
  * energy — PE = 1/2 g integral eta^2 dA, KE = 1/2 integral H |u|^2 dA, E = PE + KE.
            The PE<->KE exchange is the fast oscillation; the dispersive cases (kh ~ 1.57)
            also show the large 2*omega swing where leading-order E is NOT the invariant,
            while the low-kh case is a near-flat envelope whose windowed-mean decay
            (drawn) is the gated numerical dissipation.

Unlike the isotropy/dispersion plotters this does NOT re-run the model — it consumes
the runs left in workspaces/dev/validation-2d/runs/ by the validation suite.  Run the
suite first if they are absent:
  FUNWAVE_REGRESSION_CONFIG=test/validation/validation_config.yaml \
    uv run python -m test.framework.cli regression -t conservation

Run from the repo root:
  uv run python -m test.validation.plot_conservation
  uv run python -m test.validation.plot_conservation --runs-root <path> --out <png>
"""

from __future__ import annotations

import argparse
import sys
from dataclasses import dataclass
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

from test.regression.postproc.utils import read_run_metadata
from test.validation.oracles.conservation import G, _windowed_decay_pct

REPO_ROOT = Path(__file__).resolve().parents[2]
RUNS_ROOT = REPO_ROOT / "workspaces" / "dev" / "validation-2d" / "runs"
OUT_PNG = REPO_ROOT / "workspaces" / "dev" / "validation-2d" / "conservation_diagram.png"

# (run-dir name, short label, kh, whether energy is the gated quantity)
_CASES = [
    ("conservation_2d_cons_diag",        "diag (4,3)",  1.57, False),
    ("conservation_2d_cons_axis",        "axis (5,0)",  1.57, False),
    ("conservation_2d_lowkh_cons_lowkh", "lowkh (2,0)", 0.16, True),
]


@dataclass
class Series:
    label: str
    kh: float
    gated: bool
    t: np.ndarray
    v_drift_pct: np.ndarray   # |V(t) - V0| / V_still * 100
    pe: np.ndarray
    ke: np.ndarray

    @property
    def energy(self) -> np.ndarray:
        return self.pe + self.ke


def _load(run_dir: Path, label: str, kh: float, gated: bool) -> Series | None:
    """Integrate the ETA/U/V frames of one run into a time series, or None if absent."""
    if not run_dir.exists():
        return None
    meta = read_run_metadata(run_dir)
    dep = meta.output_files("DEPTH_OUT")
    eta_files = meta.output_files("ETA")
    if not dep or len(eta_files) < 3:
        return None
    h = meta.read_field(dep[0]).astype(float)
    u_files = meta.output_files("U")
    v_files = meta.output_files("V")
    da = meta.dx * meta.dy
    v_still = float(np.sum(h)) * da

    vols, pe, ke = [], [], []
    for i, ep in enumerate(eta_files):
        eta = meta.read_field(ep).astype(float)
        hh = eta + h
        u = meta.read_field(u_files[i]).astype(float) if i < len(u_files) else np.zeros_like(eta)
        v = meta.read_field(v_files[i]).astype(float) if i < len(v_files) else np.zeros_like(eta)
        vols.append(float(np.sum(eta)) * da)
        pe.append(0.5 * G * float(np.sum(eta * eta)) * da)
        ke.append(0.5 * float(np.sum(hh * (u * u + v * v))) * da)
    vols = np.asarray(vols)

    # Frame times from time_dt.out col 0 (written at the output cadence); fall back
    # to the frame index if the file is missing.
    n = len(eta_files)
    tfile = run_dir / "time_dt.out"
    if tfile.exists():
        t = np.loadtxt(tfile)[:, 0][:n]
        if len(t) < n:
            t = np.arange(n, dtype=float)
    else:
        t = np.arange(n, dtype=float)

    # A perfectly flat floor would be log(0); clip to a tiny epsilon for the log axis.
    v_drift = np.abs(vols - vols[0]) / v_still * 100.0
    v_drift = np.maximum(v_drift, 1e-14)
    return Series(label, kh, gated, t, v_drift, np.asarray(pe), np.asarray(ke))


def _plot(series: list[Series], out_png: Path) -> None:
    # Row 0: mass drift for all cases on one log axis (the machine-precision story).
    # Rows 1..N: per-case PE / KE / E, so the PE<->KE exchange and (low-kh) the flat
    # energy envelope with its windowed-mean decay are each legible.
    n = len(series)
    fig, axes = plt.subplots(1 + n, 1, figsize=(9.0, 2.6 * (1 + n)),
                             gridspec_kw={"height_ratios": [1.15] + [1.0] * n})
    ax_mass = axes[0]
    cmap = plt.get_cmap("viridis")
    shades = np.linspace(0.0, 0.75, n)

    for s, sh in zip(series, shades):
        ax_mass.semilogy(s.t, s.v_drift_pct, lw=1.4, color=cmap(sh),
                         label=f"{s.label}  kh={s.kh:.2f}")
    ax_mass.set_ylabel("mass drift\n|ΔV|/V₀  [%]")
    ax_mass.set_title("Mass conservation — flux-form + wall BC pins volume at machine precision",
                      fontsize=10, loc="left")
    ax_mass.legend(fontsize=8, ncol=n, loc="upper left", framealpha=0.9)
    ax_mass.grid(True, which="both", alpha=0.25)

    for ax, s in zip(axes[1:], series):
        ax.plot(s.t, s.pe, lw=1.1, color="tab:blue", label="PE = ½g∫η²")
        ax.plot(s.t, s.ke, lw=1.1, color="tab:red", label="KE = ½∫H|u|²")
        ax.plot(s.t, s.energy, lw=1.7, color="k", label="E = PE + KE")

        drift = _windowed_decay_pct(s.energy)
        half = len(s.energy) // 2
        m1 = float(np.mean(s.energy[:half]))
        m2 = float(np.mean(s.energy[half:]))
        ax.hlines([m1, m2], [s.t[0], s.t[half]], [s.t[half], s.t[-1]],
                  color="tab:green", lw=1.6, ls="--", alpha=0.9)
        gate = "gated" if s.gated else "diagnostic"
        ax.set_title(f"{s.label}  kh={s.kh:.2f}   energy windowed-mean drift "
                     f"{drift:+.2f} %  ({gate})", fontsize=10, loc="left")
        ax.set_ylabel("energy  [m³·(m/s²)]")
        ax.grid(True, alpha=0.25)
        ax.legend(fontsize=8, ncol=3, loc="upper right", framealpha=0.9)

    axes[-1].set_xlabel("time  [s]")
    fig.tight_layout()
    out_png.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_png, dpi=140)
    print(f"wrote {out_png}")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--runs-root", type=Path, default=RUNS_ROOT,
                    help="directory holding the conservation_2d_* run folders")
    ap.add_argument("--out", type=Path, default=OUT_PNG, help="output PNG path")
    args = ap.parse_args()

    series: list[Series] = []
    for name, label, kh, gated in _CASES:
        s = _load(args.runs_root / name, label, kh, gated)
        if s is None:
            print(f"skip {name}: run output not found under {args.runs_root}")
            continue
        print(f"{label:14s} kh={kh:.2f}  frames={len(s.t)}  "
              f"max mass drift={s.v_drift_pct.max():.2e} %  "
              f"energy drift={_windowed_decay_pct(s.energy):+.2f} %")
        series.append(s)

    if not series:
        print("no conservation runs found — run the validation suite first (see module docstring)")
        return 1
    _plot(series, args.out)
    return 0


if __name__ == "__main__":
    sys.exit(main())
