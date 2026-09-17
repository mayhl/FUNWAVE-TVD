"""
Morphological change vs measured: final bed change dz(x) from the cumulative
sediment_dchgs + sediment_dchgb fields (deposition positive), centerline row.
Two gate families, picked by the tolerances keys:

  bedchange: <file>   measured dz(x) profile (x offset by gate_x onto model x);
                      gates bedchange_nrmse_pct (peak norm) + trough_offset_m
                      (half-maximum scour-centroid offset; the deposit one reports).
  growth: <file>      per-wave emax/dmax growth table normalized by h0; wave
                      `wave` (default 1) gates emax_rel_err_pct/dmax_rel_err_pct.

Note 1: dchg_s + dchg_b is the unclamped bed change; the hard-bottom clamp
and avalanching move zb only, so on a clamped cell the sum can overstate the
erosion.  Both lab cases erode sand well inside the clamp.
Note 2: measured profiles settle fully; a short model window still carries
suspended load (CACR-16-02 p.69) — amplitude gates stay inherited-loose.
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np

from test.framework.results import MetricResult, SubsectionResult
from test.framework.run_output import read_run_metadata
from test.framework.tolerances import check_keys
from test.validation.oracles._lab import new_figure, nrmse_pct, read_table, save_figure

_LABEL = "Morphology"
ACCEPTED_KEYS = (
    "bedchange",
    "gate_x",
    "bedchange_nrmse_pct",
    "trough_offset_m",
    "growth",
    "wave",
    "h0",
    "emax_rel_err_pct",
    "dmax_rel_err_pct",
)


def _centroid(x: np.ndarray, w: np.ndarray, frac: float = 0.5) -> float:
    """Centroid of the part of a non-negative profile at or above frac of its
    maximum; nan when it is all zero.  The threshold keeps the locator on the
    main feature: the low-level checkerboard away from it would otherwise
    drag a full-profile centroid by metres on a long flume."""
    top = float(w.max())
    if top <= 0:
        return math.nan
    m = w >= frac * top
    return float((x[m] * w[m]).sum() / w[m].sum())


def _skip(msg: str) -> SubsectionResult:
    print(f"morphology: {msg} — skipping")
    return SubsectionResult(kind="statistics", label=_LABEL, metrics=[])


def _final_dz(meta) -> tuple[np.ndarray, np.ndarray]:
    """Centerline bed change (m) and cell-centre x from the last dchg frames."""
    total = None
    for prefix in ("sediment_dchgs", "sediment_dchgb"):
        files = meta.output_files(prefix)
        if not files:
            raise FileNotFoundError(f"no {prefix} output files")
        arr = meta.read_field(files[-1]).astype(float)
        total = arr if total is None else total + arr
    row = total[total.shape[0] // 2, :]
    x = np.arange(len(row)) * meta.dx  # cell i at (i-1) dx, the engine's registration
    return x, row


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    check_keys(tolerances, ACCEPTED_KEYS, "morphology")
    meta = read_run_metadata(dev_dir)
    try:
        x_mod, dz_mod = _final_dz(meta)
    except FileNotFoundError as exc:
        return _skip(str(exc))

    metrics = []
    fig, axes = new_figure(nrows=1, height_per_row=2.8)
    ax = axes[0]

    if "bedchange" in tolerances:
        data, head = read_table(dev_dir / tolerances["bedchange"])
        gate_x = float(tolerances.get("gate_x", 0.0))
        xm, dzm = data[:, 0] + gate_x, data[:, 1]
        fit = np.interp(xm, x_mod, dz_mod)
        err = nrmse_pct(dzm, fit, norm="max")
        tol = float(tolerances.get("bedchange_nrmse_pct", 60.0))
        ok = err < tol
        metrics.append(MetricResult("morphology", "bedchange_nrmse_pct", err, ok, tol))
        # the erosion trough is the robust locator: measured profiles settle
        # fully, so the deposition peak carries the suspended-load lag
        # (CACR-16-02 p.69) and reports ungated.  Both are half-maximum
        # scour/deposit centroids, not argmin/argmax: the modelled bed
        # change carries a 2 dx bed-feedback checkerboard, and a point
        # locator lands on whichever spike wins
        tr_off = _centroid(xm, np.maximum(-fit, 0)) - _centroid(xm, np.maximum(-dzm, 0))
        tr_tol = float(tolerances.get("trough_offset_m", math.inf))
        metrics.append(MetricResult("morphology", "trough_offset_m", tr_off, abs(tr_off) < tr_tol, tr_tol))
        pk_off = _centroid(xm, np.maximum(fit, 0)) - _centroid(xm, np.maximum(dzm, 0))
        metrics.append(MetricResult("morphology", "peak_offset_m", pk_off, True, math.inf))
        ax.plot(xm, dzm, "-", color="#2563eb", label="measured")
        ax.plot(xm, fit, "-", color="#dc2626", label="modern")

    if "growth" in tolerances:
        data, _ = read_table(dev_dir / tolerances["growth"])
        wave = int(tolerances.get("wave", 1))
        h0 = float(tolerances["h0"])
        row = data[data[:, 0] == wave][0]
        emax_meas, dmax_meas = float(row[1]), float(row[2])
        emax_mod = float(max(0.0, -dz_mod.min())) / h0
        dmax_mod = float(max(0.0, dz_mod.max())) / h0
        for name, mod, meas in (("emax", emax_mod, emax_meas), ("dmax", dmax_mod, dmax_meas)):
            rel = abs(mod - meas) / meas * 100.0
            tol = float(tolerances.get(f"{name}_rel_err_pct", 60.0))
            metrics.append(MetricResult("morphology", f"{name}_over_h", mod, True, math.inf))
            metrics.append(MetricResult("morphology", f"{name}_rel_err_pct", rel, rel < tol, tol))
        ax.plot(x_mod, dz_mod, "-", color="#dc2626", label="modern dz")
        ax.axhline(-emax_meas * h0, color="#2563eb", ls="--", lw=0.8, label="measured emax/dmax")
        ax.axhline(dmax_meas * h0, color="#2563eb", ls="--", lw=0.8)

    # mass closure across the row — diagnostic only (porosity-scaled fields)
    net = float(np.trapezoid(dz_mod, x_mod))
    metrics.append(MetricResult("morphology", "net_dz_int_m2", net, True, math.inf))

    ax.set_xlabel("x (m)")
    ax.set_ylabel("bed change (m)")
    ax.grid(alpha=0.3)
    ax.legend(loc="best", fontsize=8)
    sub = SubsectionResult(kind="statistics", label=_LABEL, metrics=metrics)
    sub.figures.append(save_figure(fig, plots_dir, "morphology", "Bed change vs measured"))
    return sub
