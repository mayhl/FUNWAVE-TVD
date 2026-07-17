#!/usr/bin/env python
"""
Linear-dispersion diagram generator (2D Boussinesq).

Sweeps the closed-basin seiche (INI_SINE) across two axes and plots the measured
phase speed against the analytic curves:

A FAMILY of measured curves, one per grid resolution.  Each curve fixes the
wavenumber k (hence the points-per-wavelength ppw = 2 pi / (k dx)) and sweeps the
depth h so kh runs up to 4 pi.  This fuses two readings into one plot:

  * Along a curve (physics) — the DISPERSION shape: every curve tracks Nwogu and
    peels away from Airy above kh ~ 3.

  * Between curves at fixed kh (numerics) — the grid error ~ (k dx)^2: the finest
    grid hugs the Nwogu curve, coarser grids ride above it, converging as ppw grows.

Diagram: C / sqrt(g h) vs kh, one measured line per resolution (coloured by k)
over the analytic Nwogu and Airy curves.  This is an analysis artifact, not a gate
— the pass/fail dispersion rung lives in validation_config.yaml /
oracles/dispersion.py, whose physics we reuse.

Run from the repo root:
  uv run python -m test.validation.plot_dispersion            # full sweep + plot
  uv run python -m test.validation.plot_dispersion --quick    # coarse, fast check
"""

from __future__ import annotations

import argparse
import csv
import math
import sys
from dataclasses import dataclass
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

from test.validation._seiche import (
    BINARY,
    REPO_ROOT,
    _cg_airy,
    _cg_nwogu,
    run_length,
    run_seiche,
)
from test.validation.oracles.dispersion import (
    G,
    BETA_REF_DEFAULT,
    _boussinesq_period,
)

RUN_ROOT = REPO_ROOT / "workspaces" / "dev" / "validation-2d" / "plot_runs"
OUT_PNG = REPO_ROOT / "workspaces" / "dev" / "validation-2d" / "dispersion_diagram.png"
OUT_CSV = REPO_ROOT / "workspaces" / "dev" / "validation-2d" / "dispersion_data.csv"

DY = 0.2  # cross-basin cell size (basin is 1-D in x; Nglob held at 3)


# ---------------------------------------------------------------------------
# Case matrix
# ---------------------------------------------------------------------------


@dataclass
class Case:
    tag: str
    m: int  # interior cells across the basin (Mglob) — sets the resolution
    dx: float
    h: float  # still-water depth

    @property
    def lx(self) -> float:
        return self.m * self.dx

    @property
    def k(self) -> float:
        return math.pi / self.lx  # fundamental seiche: half a wavelength

    @property
    def kh(self) -> float:
        return self.k * self.h

    @property
    def ppw(self) -> float:
        return 2.0 * self.lx / self.dx  # grid points per wavelength (= 2 M)


DX = 0.2  # cell size (fixed across the family so M alone sets the resolution)

# Sweep density per tier: (resolution curves M, kh sample points).  light = CI
# smoke, medium = dev default, heavy = dense publication run (~10 min; adds finer
# grids M up to 160 that are slower per case and resolve the convergence tail).
_TIERS = {
    "light": ([12, 100], [0.5, 1.5, 3.0, 6.0, 4.0 * math.pi]),
    "medium": ([12, 40, 100], [0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 6.0, 8.0, 10.0, 4.0 * math.pi]),
    "heavy": (
        [8, 12, 20, 40, 80, 160],
        [
            0.3,
            0.5,
            0.7,
            0.9,
            1.1,
            1.3,
            1.6,
            1.9,
            2.2,
            2.6,
            3.0,
            3.5,
            4.0,
            4.6,
            5.3,
            6.0,
            7.0,
            8.0,
            9.0,
            10.0,
            11.0,
            12.0,
            4.0 * math.pi,
        ],
    ),
}


def _build_cases(tier: str) -> list[Case]:
    """One curve per resolution M; each sweeps kh (via depth) up to 4 pi."""
    m_levels, kh_targets = _TIERS[tier]
    cases: list[Case] = []
    for m in m_levels:
        k = math.pi / (m * DX)  # fundamental seiche wavenumber, fixed per curve
        for kh in kh_targets:
            h = kh / k  # invert kh = k h at this resolution
            cases.append(Case(f"M{m:03d}_kh{kh:05.2f}", m, DX, h))
    return cases


# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------


def _run_case(case: Case, verbose: bool) -> float | None:
    """Run one 1D seiche case and return its measured period (s), or None."""
    # Near-wall antinode "2 2" is safe for the coarsest grid (i = 2 is interior
    # for M >= 8).  Nglob = 3 makes it a 1D strip; mode (1, 0) = fundamental.
    total_time, dt_sta = run_length(case.h, case.k)
    return run_seiche(
        RUN_ROOT / case.tag,
        title=case.tag,
        h=case.h,
        mglob=case.m,
        nglob=3,
        dx=case.dx,
        dy=DY,
        mode_x=1,
        mode_y=0,
        total_time=total_time,
        dt_sta=dt_sta,
        verbose=verbose,
    )


# ---------------------------------------------------------------------------
# Plot
# ---------------------------------------------------------------------------


@dataclass
class Point:
    m: int  # resolution group
    k: float
    ppw: float
    kh: float
    y_meas: float  # measured  C / sqrt(g h)


# CSV columns: the raw case + the derived speeds and ratios, so the table is
# self-contained for later postprocessing (the plot only needs the first five).
_CSV_FIELDS = [
    "M",
    "ppw",
    "k",
    "kh",
    "h",
    "C_over_sqrtgh_meas",
    "C_over_sqrtgh_nwogu",
    "C_over_sqrtgh_airy",
    "model_over_nwogu",
    "model_over_airy",
    "err_pct_vs_nwogu",
]


def _write_csv(pts: list[Point], path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(_CSV_FIELDS)
        for p in sorted(pts, key=lambda q: (-q.m, q.kh)):
            yn = float(_cg_nwogu(p.kh))
            ya = float(_cg_airy(p.kh))
            w.writerow(
                [
                    p.m,
                    f"{p.ppw:.0f}",
                    f"{p.k:.6f}",
                    f"{p.kh:.6f}",
                    f"{p.kh / p.k:.6f}",
                    f"{p.y_meas:.6f}",
                    f"{yn:.6f}",
                    f"{ya:.6f}",
                    f"{p.y_meas / yn:.6f}",
                    f"{p.y_meas / ya:.6f}",
                    f"{abs(p.y_meas - yn) / yn * 100.0:.4f}",
                ]
            )
    print(f"wrote {path}")


def _read_csv(path: Path) -> list[Point]:
    with open(path, newline="") as fh:
        return [
            Point(int(r["M"]), float(r["k"]), float(r["ppw"]), float(r["kh"]), float(r["C_over_sqrtgh_meas"]))
            for r in csv.DictReader(fh)
        ]


def _measure(cases: list[Case], verbose: bool) -> list[Point]:
    pts: list[Point] = []
    for i, c in enumerate(cases, 1):
        t_meas = _run_case(c, verbose)
        tag = f"[{i:2d}/{len(cases)}] {c.tag}  kh={c.kh:5.2f}  M={c.m:3d}  ppw={c.ppw:5.0f}"
        if t_meas is None or not math.isfinite(t_meas) or t_meas <= 0.0:
            print(f"{tag}  ->  FAILED")
            continue
        # measured phase speed:  C = omega / k = (2 pi / T) / k
        y_meas = ((2.0 * math.pi / t_meas) / c.k) / math.sqrt(G * c.h)
        _, t_nwogu = _boussinesq_period(c.h, 2.0 * c.lx, BETA_REF_DEFAULT)
        y_nwogu = ((2.0 * math.pi / t_nwogu) / c.k) / math.sqrt(G * c.h)
        err = abs(y_meas - y_nwogu) / y_nwogu * 100.0
        print(f"{tag}  ->  C/sqrt(gh)={y_meas:.4f}  (Nwogu {y_nwogu:.4f}, {err:4.1f}%)")
        pts.append(Point(c.m, c.k, c.ppw, c.kh, y_meas))
    return pts


def _plot(pts: list[Point], out_png: Path) -> None:
    kh_max = max(p.kh for p in pts) * 1.02
    kh = np.linspace(1e-3, kh_max, 600)
    nwogu = _cg_nwogu(kh)
    airy = _cg_airy(kh)

    # Three stacked panels sharing kh:  absolute speed, then the model measured
    # against its OWN theory (numerics only), then against exact Airy (vs truth).
    fig, (ax0, ax1, ax2) = plt.subplots(
        3,
        1,
        figsize=(9.0, 11.0),
        sharex=True,
        gridspec_kw={"height_ratios": [2.2, 1.0, 1.0], "hspace": 0.08},
    )

    ax0.plot(kh, nwogu, "-", color="#1f4e79", lw=2.4, zorder=6, label=r"Nwogu (model theory, $\beta_{ref}=-0.531$)")
    ax0.plot(kh, airy, "--", color="#888888", lw=1.8, zorder=2, label=r"Airy (exact linear)")

    # One measured curve per resolution.  Fine grids (low k, high ppw) hug Nwogu;
    # coarse grids ride above it — colour dark -> bright with k (coarse = bright).
    groups = sorted({p.m for p in pts}, reverse=True)  # fine -> coarse
    cmap = plt.get_cmap("viridis")
    shades = np.linspace(0.0, 0.88, len(groups))
    for shade, m in zip(shades, groups):
        gp = sorted((p for p in pts if p.m == m), key=lambda p: p.kh)
        if not gp:
            continue
        color = cmap(shade)
        khs = np.array([p.kh for p in gp])
        ys = np.array([p.y_meas for p in gp])
        label = rf"$M={m}$  ({gp[0].ppw:.0f} pts/$\lambda$, $k={gp[0].k:.2f}$)"
        style = dict(marker="o", ms=4.5, lw=1.4, color=color, markeredgecolor="k", markeredgewidth=0.4, zorder=4)
        ax0.plot(khs, ys, **style, label=label)
        ax1.plot(khs, (ys / _cg_nwogu(khs) - 1.0) * 100.0, **style)  # numerics: -> 0 % with resolution
        ax2.plot(khs, (ys / _cg_airy(khs) - 1.0) * 100.0, **style)  # % departure from exact dispersion

    # Ratio-panel references: zero error, and the irreducible model-vs-truth gap.
    for ax in (ax1, ax2):
        ax.axhline(0.0, color="k", lw=1.0, alpha=0.5, zorder=1)
    ax2.plot(
        kh,
        (nwogu / airy - 1.0) * 100.0,
        "--",
        color="#1f4e79",
        lw=1.8,
        zorder=5,
        label=r"Nwogu / Airy (model limit, $\infty$ resolution)",
    )

    for ax in (ax0, ax1, ax2):
        for x in (math.pi, 2 * math.pi, 3 * math.pi, 4 * math.pi):
            if x <= kh_max:
                ax.axvline(x, color="k", ls=":", lw=0.6, alpha=0.35, zorder=1)
        ax.grid(True, alpha=0.25)
        ax.set_xlim(0, kh_max)
    for x in (math.pi, 2 * math.pi, 3 * math.pi, 4 * math.pi):
        if x <= kh_max:
            ax2.text(
                x,
                0.015,
                rf"${int(round(x / math.pi))}\pi$",
                ha="center",
                va="bottom",
                fontsize=8,
                alpha=0.6,
                transform=ax2.get_xaxis_transform(),
            )

    ax0.set_ylabel(r"$C / \sqrt{gh}$")
    ax0.set_ylim(0, 1.05)
    ax0.set_title("Linear dispersion — closed-basin seiche vs Boussinesq theory")
    ax0.legend(loc="upper right", framealpha=0.95, fontsize=9)

    ax1.set_ylabel(r"$C_{\rm model}/C_{\rm Nwogu}-1$  (%)")
    ax1.set_title("numerical error — measured vs the model's own theory (grid convergence)", fontsize=9, loc="left", pad=3)
    ax2.set_ylabel(r"$C_{\rm model}/C_{\rm Airy}-1$  (%)")
    ax2.set_title("departure from exact dispersion (physics + numerics)", fontsize=9, loc="left", pad=3)
    ax2.legend(loc="upper left", framealpha=0.95, fontsize=8)
    ax2.set_xlabel(r"$kh$")

    out_png.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(out_png, dpi=140, bbox_inches="tight")
    print(f"\nwrote {out_png}")


# ---------------------------------------------------------------------------


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument(
        "--tier",
        choices=["light", "medium", "heavy"],
        default="medium",
        help="sweep density: light=smoke, medium=default, heavy=dense (~10 min)",
    )
    ap.add_argument("--quick", action="store_true", help="alias for --tier light")
    ap.add_argument("--out", type=Path, default=OUT_PNG, help="output PNG path")
    ap.add_argument("--csv", type=Path, default=OUT_CSV, help="write the measured sweep table here")
    ap.add_argument("--from-csv", type=Path, default=None, help="skip the sims and re-plot from a saved table")
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    if args.from_csv is not None:
        pts = _read_csv(args.from_csv)
        print(f"replotting {len(pts)} cases from {args.from_csv}")
        _plot(pts, args.out)
        return 0

    if not BINARY.exists():
        print(f"validation binary not found: {BINARY}\nbuild it first (validation-2d executable).", file=sys.stderr)
        return 1

    tier = "light" if args.quick else args.tier
    cases = _build_cases(tier)
    n_curves = len({c.m for c in cases})
    print(f"[{tier}] running {len(cases)} seiche cases ({n_curves} resolution curves x {len(cases) // n_curves} kh)\n")
    pts = _measure(cases, args.verbose)
    if not pts:
        print("no cases succeeded — nothing to plot", file=sys.stderr)
        return 1
    _write_csv(pts, args.csv)
    _plot(pts, args.out)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
