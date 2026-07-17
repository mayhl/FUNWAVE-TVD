#!/usr/bin/env python
"""
Dispersion-isotropy diagram (2D Boussinesq).

Tests whether the model's phase speed depends only on the wavenumber magnitude
|k| and not on propagation direction.  In a closed SQUARE basin the mode
(n_x, n_y) is an oblique standing wave

    eta = A cos(n_x pi x / L) cos(n_y pi y / L),
    |k| = (pi / L) sqrt(n_x^2 + n_y^2),   theta = atan2(n_y, n_x),

so Pythagorean sets with n_x^2 + n_y^2 held FIXED give one |k| (one grid
resolution, one kh) sampled across a spread of angles.  A perfectly isotropic
scheme returns the same period at every angle; the residual angular variation is
the grid's dispersion anisotropy (the (k dx)^2 error differs for axis-aligned vs
diagonal propagation).

Diagram: (C_measured / C_Nwogu - 1) in %, versus angle, one series per |k| set.
Flat within a series = isotropic; the offset from 0 is the (isotropic part of the)
grid dispersion error, which grows for the coarser / higher-|k| sets.

Run from the repo root:
  uv run python -m test.validation.plot_isotropy            # full sweep + plot
  uv run python -m test.validation.plot_isotropy --from-csv <path>
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

from test.validation._seiche import BINARY, REPO_ROOT, _cg_nwogu, run_length, run_seiche
from test.validation.oracles.dispersion import G

RUN_ROOT = REPO_ROOT / "workspaces" / "dev" / "validation-2d" / "iso_runs"
OUT_PNG = REPO_ROOT / "workspaces" / "dev" / "validation-2d" / "isotropy_diagram.png"
OUT_CSV = REPO_ROOT / "workspaces" / "dev" / "validation-2d" / "isotropy_data.csv"

DX = 0.2
H = 2.0

# Each series is a target |k| (hence kh, resolution).  We densify its angle
# sampling by pooling several sum-of-squares S = n_x^2 + n_y^2: every S with a
# rich set of integer representations contributes its angles, and the grid size
# M is chosen PER S so that |k| = (pi/(M dx)) sqrt(S) lands on the series target.
# So |k| (and kh, ppw) stays matched while the angle count multiplies, and the
# fixed-|k| rigor is exact (no rounding jitter in |k| beyond integer M).
# Sweep density per tier: (target |k| series, sum-of-squares pool, grid-size cap).
# Bigger S = more angle representations; a higher M cap admits the biggest S at
# fine |k| (slower square grids), so heavy is a dense ~10 min publication run.
_TIERS = {
    "light": ([1.2], [25, 65], 260),
    "medium": ([0.8, 1.6], [25, 65, 325], 260),
    "heavy": ([0.6, 0.9, 1.2, 1.6], [25, 65, 325, 1105], 340),
}


def _sos_reps(s: int) -> list[tuple[int, int]]:
    """All (n_x, n_y) with n_x, n_y >= 0 and n_x^2 + n_y^2 == s (both orders)."""
    out = []
    nx = 0
    while nx * nx <= s:
        ny = math.isqrt(s - nx * nx)
        if ny * ny == s - nx * nx:
            out.append((nx, ny))
        nx += 1
    return out


@dataclass
class Case:
    nx: int
    ny: int
    m: int  # square-basin cells (chosen per S to fix |k|)
    level: float  # target |k| this case belongs to (series key)

    @property
    def k(self) -> float:
        return math.hypot(self.nx, self.ny) * math.pi / (self.m * DX)

    @property
    def theta_deg(self) -> float:
        return math.degrees(math.atan2(self.ny, self.nx))

    @property
    def kh(self) -> float:
        return self.k * H

    @property
    def ppw(self) -> float:
        return 2.0 * math.pi / (self.k * DX)


def _build_cases(tier: str) -> list[Case]:
    targets, s_pool, m_max = _TIERS[tier]
    cases: list[Case] = []
    for kt in targets:
        for s in s_pool:
            m = round(math.pi * math.sqrt(s) / (DX * kt))  # -> |k| ~ kt
            if not (20 <= m <= m_max):
                continue
            for nx, ny in _sos_reps(s):
                cases.append(Case(nx, ny, m, kt))
    return cases


# ---------------------------------------------------------------------------


def _run_case(case: Case, verbose: bool) -> float | None:
    """Run one oblique-mode case and return its measured period (s), or None."""
    # Square basin (Nglob = Mglob), mode (n_x, n_y); station "2 2" sits near the
    # (0,0) corner antinode, an antinode of every mode.
    total_time, dt_sta = run_length(H, case.k)
    return run_seiche(
        RUN_ROOT / f"iso_M{case.m}_{case.nx}_{case.ny}",
        title=f"iso_{case.nx}_{case.ny}",
        h=H,
        mglob=case.m,
        nglob=case.m,
        dx=DX,
        dy=DX,
        mode_x=case.nx,
        mode_y=case.ny,
        total_time=total_time,
        dt_sta=dt_sta,
        verbose=verbose,
    )


# ---------------------------------------------------------------------------


@dataclass
class Point:
    level: float
    nx: int
    ny: int
    k: float
    kh: float
    ppw: float
    theta_deg: float
    y_meas: float  # measured C / sqrt(g h)


def _measure(cases: list[Case], verbose: bool) -> list[Point]:
    pts: list[Point] = []
    for i, c in enumerate(cases, 1):
        t_meas = _run_case(c, verbose)
        tag = (
            f"[{i:2d}/{len(cases)}] M{c.m:3d} ({c.nx},{c.ny})  th={c.theta_deg:5.1f}deg  "
            f"|k|={c.k:5.3f}  kh={c.kh:4.2f}  ppw={c.ppw:4.0f}"
        )
        if t_meas is None or not math.isfinite(t_meas) or t_meas <= 0.0:
            print(f"{tag}  ->  FAILED")
            continue
        y_meas = ((2.0 * math.pi / t_meas) / c.k) / math.sqrt(G * H)
        y_nwogu = float(_cg_nwogu(c.kh))
        dev = (y_meas / y_nwogu - 1.0) * 100.0
        print(f"{tag}  ->  C/sqrt(gh)={y_meas:.4f}  ({dev:+.2f} % vs Nwogu)")
        pts.append(Point(c.level, c.nx, c.ny, c.k, c.kh, c.ppw, c.theta_deg, y_meas))
    return pts


_CSV_FIELDS = ["level", "nx", "ny", "k", "kh", "ppw", "theta_deg", "C_over_sqrtgh_meas", "C_over_sqrtgh_nwogu", "dev_pct"]


def _write_csv(pts: list[Point], path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(_CSV_FIELDS)
        for p in sorted(pts, key=lambda q: (q.level, q.theta_deg)):
            yn = float(_cg_nwogu(p.kh))
            w.writerow(
                [
                    f"{p.level:.2f}",
                    p.nx,
                    p.ny,
                    f"{p.k:.6f}",
                    f"{p.kh:.6f}",
                    f"{p.ppw:.1f}",
                    f"{p.theta_deg:.3f}",
                    f"{p.y_meas:.6f}",
                    f"{yn:.6f}",
                    f"{(p.y_meas / yn - 1.0) * 100.0:.4f}",
                ]
            )
    print(f"wrote {path}")


def _read_csv(path: Path) -> list[Point]:
    with open(path, newline="") as fh:
        return [
            Point(
                float(r["level"]),
                int(r["nx"]),
                int(r["ny"]),
                float(r["k"]),
                float(r["kh"]),
                float(r["ppw"]),
                float(r["theta_deg"]),
                float(r["C_over_sqrtgh_meas"]),
            )
            for r in csv.DictReader(fh)
        ]


def _plot(pts: list[Point], out_png: Path) -> None:
    # Two panels sharing the angle axis: (top) absolute deviation from Nwogu, which
    # mixes the isotropic grid-dispersion offset with the anisotropy; (bottom) the
    # same series each normalized by its most axis-aligned angle, which cancels the
    # common offset and leaves the PURE anisotropy (C(theta)/C(theta->0) - 1).
    fig, (ax0, ax1) = plt.subplots(2, 1, figsize=(9.0, 8.4), sharex=True, gridspec_kw={"height_ratios": [1.0, 1.0]})
    levels = sorted({p.level for p in pts})
    cmap = plt.get_cmap("viridis")
    shades = np.linspace(0.0, 0.82, len(levels))

    ax0.axhline(0.0, color="k", lw=1.0, alpha=0.5, zorder=1)
    ax1.axhline(0.0, color="k", lw=1.0, alpha=0.5, zorder=1)
    for shade, lv in zip(shades, levels):
        gp = sorted((p for p in pts if p.level == lv), key=lambda p: p.theta_deg)
        if not gp:
            continue
        kh0 = float(np.mean([p.kh for p in gp]))
        ppw0 = float(np.mean([p.ppw for p in gp]))
        theta = [p.theta_deg for p in gp]
        lbl = rf"$|k|\approx{lv:.1f}$  ($kh\approx{kh0:.2f}$, {ppw0:.0f} pts/$\lambda$)"
        style = dict(marker="o", ms=5, lw=1.3, color=cmap(shade), markeredgecolor="k", markeredgewidth=0.3, zorder=4)

        dev = [(p.y_meas / float(_cg_nwogu(p.kh)) - 1.0) * 100.0 for p in gp]
        ax0.plot(theta, dev, label=lbl, **style)

        # Reference = the most axis-aligned angle available in this set (theta = 0
        # when an (n, 0) mode is present, else the smallest theta of the pool).
        c_ref = gp[0].y_meas
        aniso = [(p.y_meas / c_ref - 1.0) * 100.0 for p in gp]
        ax1.plot(theta, aniso, label=lbl, **style)

    ax0.set_ylabel(r"$C_{\rm model}/C_{\rm Nwogu}-1$  (%)")
    ax0.set_title("Dispersion isotropy — oblique standing wave in a square basin")
    ax1.set_xlabel(r"propagation angle  $\theta = \arctan(k_y/k_x)$  (deg)")
    ax1.set_ylabel(r"$C_{\rm model}(\theta)/C_{\rm model}(\theta\!\to\!0)-1$  (%)")
    ax1.set_title("pure anisotropy — offset from the axis-aligned direction removed")
    for ax in (ax0, ax1):
        ax.set_xlim(-3, 93)
        ax.set_xticks([0, 15, 30, 45, 60, 75, 90])
        ax.grid(True, alpha=0.25)
        ax.axvline(45, color="k", ls=":", lw=0.6, alpha=0.35, zorder=1)
    ax0.legend(loc="best", framealpha=0.95, fontsize=9, title="fixed-$|k|$ sets (axis-aligned $\\to$ diagonal)")
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
    ap.add_argument("--csv", type=Path, default=OUT_CSV, help="output table path")
    ap.add_argument("--from-csv", type=Path, default=None, help="skip the sims and re-plot from a saved table")
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    if args.from_csv is not None:
        pts = _read_csv(args.from_csv)
        print(f"replotting {len(pts)} cases from {args.from_csv}")
        _plot(pts, args.out)
        return 0

    if not BINARY.exists():
        print(f"validation binary not found: {BINARY}", file=sys.stderr)
        return 1

    tier = "light" if args.quick else args.tier
    cases = _build_cases(tier)
    print(f"[{tier}] running {len(cases)} oblique-mode cases ({len({c.level for c in cases})} fixed-|k| levels)\n")
    pts = _measure(cases, args.verbose)
    if not pts:
        print("no cases succeeded — nothing to plot", file=sys.stderr)
        return 1
    _write_csv(pts, args.csv)
    _plot(pts, args.out)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
