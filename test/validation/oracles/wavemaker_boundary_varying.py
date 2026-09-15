"""Spatially-varying boundary-feed oracle (2D Boussinesq).

Ref-less validation postproc for the along-face spectrum interpolation: the
west feed reads a two-anchor manifest (locations:), so the fed spectrum varies
along the boundary between anchor A at y_A and anchor B at y_B.  With
single-direction (theta = 0) scaled anchors the component amplitudes — hence
Hm0 — interpolate LINEARLY in y and clamp past the anchors, giving an exact
analytic profile with no lab data.

The case runs a long alongshore domain (periodic-y, case A) with an alongshore
station line at a fixed downwave x, buffered off the periodic wrap so the seam's
local diffraction stays clear of the gauges.  For each station we measure the
variance Hm0 (4 sqrt(m0)) over the record's last T_rec and compare it to the
interpolated target derived from the manifest and the anchor tables — an
end-to-end check of the blend through the solver, not just the mode build.

Metrics (only the finite-tolerance ones gate):
  * ``hm0_profile_err_pct`` (gated) — max over stations of the percent error
    between the measured Hm0 and the interpolated target.
  * ``hm0_gradient_ok`` (diagnostic) — 1 if the measured Hm0 rises monotonically
    from the low anchor to the high one (the spatial variation is realized).

Note 1: gauge series are re-sampled onto a uniform grid by linear interpolation
before the variance estimate; the channel writer follows the adaptive step, so
raw sample times are only nominally uniform.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) -> SubsectionResult
(ref_dir is None in oracle mode and unused.)

Tolerance keys (under tolerances: wavemaker_boundary_varying:):
  hm0_profile_err_pct — max allowed % error of measured vs interpolated Hm0
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
from rich import box
from rich.console import Console
from rich.table import Table

from test.framework.results import MetricResult, SubsectionResult
from test.framework.tolerances import check_keys
from test.framework.run_output import read_run_metadata
from test.validation.oracles import wave_stats
from test.validation.oracles._lab import load_deck

_console = Console()
ACCEPTED_KEYS = ("hm0_profile_err_pct",)

DT_SAMPLE = 0.1  # uniform re-sample step (matches the channel interval)
T_REC = 200.0  # equal-df ladder recurrence period (df = 0.005 Hz over 41 lines)


def _table_hm0(path: Path) -> float:
    """Component-sum Hm0 = 4 sqrt(sum a^2 / 2) from a WK_DATA2D table."""
    toks = path.read_text().split()
    nfreq, ndir = int(toks[0]), int(toks[1])
    # layout: nfreq ndir / peak / nfreq freqs / ndir dirs / ndir*nfreq amps
    off = 2 + 1 + nfreq + ndir
    amp = np.array(toks[off : off + nfreq * ndir], dtype=float)
    return 4.0 * math.sqrt(0.5 * float(np.sum(amp**2)))


def _anchor_profile(run_dir: Path, deck: dict) -> tuple[np.ndarray, np.ndarray] | None:
    """Sorted (y_anchor, Hm0_anchor) parsed from the deck manifest + tables."""
    wm = deck.get("wavemaker")
    if isinstance(wm, list):
        wm = wm[0] if wm else {}
    manifest = (wm or {}).get("spectrum", {}).get("locations")
    if manifest is None:
        return None
    man_path = run_dir / manifest
    base = man_path.parent
    ys, hm0s = [], []
    for line in man_path.read_text().splitlines():
        s = line.strip()
        if not s or s.startswith("#"):
            continue
        y_tok, f_tok = s.split()[:2]
        ys.append(float(y_tok))
        hm0s.append(_table_hm0(base / f_tok))
    order = np.argsort(ys)
    return np.array(ys)[order], np.array(hm0s)[order]


def _station_y(deck: dict) -> np.ndarray | None:
    """Alongshore coordinates of the station gauges, in gauge-column order."""
    for ch in deck.get("output", {}).get("channels", []):
        if ch.get("type") == "station":
            return np.array(ch["y"], dtype=float)
    return None


def _load_gauges(output_dir: Path) -> tuple[np.ndarray, np.ndarray] | None:
    """Point-channel eta series -> (t, eta[n_t, n_gauge])."""
    try:
        t, v = wave_stats.read_point_channel(output_dir, "eta")
    except FileNotFoundError:
        return None
    if t.shape[0] < 16:
        return None
    sta = np.column_stack([t, v])
    keep = np.concatenate([[True], np.diff(t) > 0.0])
    return t[keep], sta[keep, 1:]


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    """Oracle entry point (ref_dir is None in oracle mode and unused)."""
    dev_dir = Path(dev_dir)
    check_keys(tolerances, ACCEPTED_KEYS, "wavemaker_boundary_varying")
    label = "Wavemaker Boundary (spatially varying)"
    output_dir = read_run_metadata(dev_dir).output_dir

    try:
        deck = load_deck(dev_dir)
    except FileNotFoundError:
        _console.print("[yellow]wavemaker_boundary_varying:[/yellow] deck not found — skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])

    prof = _anchor_profile(dev_dir, deck)
    sta_y = _station_y(deck)
    if prof is None or sta_y is None:
        _console.print("[yellow]wavemaker_boundary_varying:[/yellow] manifest/stations missing — skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    y_anchor, hm0_anchor = prof

    gauges = _load_gauges(output_dir)
    if gauges is None:
        _console.print("[yellow]wavemaker_boundary_varying:[/yellow] gauge records missing/short — skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    t_raw, eta_raw = gauges
    if eta_raw.shape[1] != len(sta_y):
        _console.print("[yellow]wavemaker_boundary_varying:[/yellow] gauge count != station count — skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])

    # stats window: the record's last T_rec (anchored at the end so it stays
    # exact under deck total_time changes)
    n_win = int(round(T_REC / DT_SAMPLE))
    t_start = DT_SAMPLE * math.floor((t_raw[-1] - T_REC) / DT_SAMPLE)
    t_uni = t_start + DT_SAMPLE * np.arange(n_win)
    if t_start < 0.0 or t_raw[-1] < t_uni[-1]:
        _console.print("[yellow]wavemaker_boundary_varying:[/yellow] record ends before the stats window — skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    eta = np.stack([np.interp(t_uni, t_raw, eta_raw[:, g]) for g in range(eta_raw.shape[1])], axis=1)
    eta -= eta.mean(axis=0)

    # measured vs interpolated (np.interp clamps to the endpoints past anchors)
    hm0_meas = 4.0 * np.sqrt(np.mean(eta**2, axis=0))
    hm0_tgt = np.interp(sta_y, y_anchor, hm0_anchor)
    err = np.abs(hm0_meas - hm0_tgt) / hm0_tgt * 100.0
    err_max = float(np.max(err))
    # gradient realized: measured rises from the low-y anchor toward the high-y
    order = np.argsort(sta_y)
    gradient_ok = 1.0 if np.all(np.diff(hm0_meas[order]) > 0.0) else 0.0

    tol = float(tolerances["hm0_profile_err_pct"]) if "hm0_profile_err_pct" in tolerances else math.inf
    prof_pass = err_max < tol

    metrics = [
        MetricResult("wavemaker_boundary_varying", "hm0_profile_err_pct", err_max, prof_pass, tol),
        MetricResult("wavemaker_boundary_varying", "hm0_gradient_ok", gradient_ok, True, math.inf),
    ]

    if verbose or not prof_pass:
        table = Table(
            box=box.SIMPLE_HEAD,
            header_style="bold cyan",
            show_edge=False,
            title="[bold]Spatially-Varying Boundary Feed[/bold]",
            title_justify="left",
        )
        for col, just in (("y (m)", "right"), ("Hm0 target", "right"), ("Hm0 measured", "right"), ("err %", "right")):
            table.add_column(col, justify=just)
        for k in order:
            table.add_row(f"{sta_y[k]:.0f}", f"{hm0_tgt[k]:.3f}", f"{hm0_meas[k]:.3f}", f"{err[k]:.1f}")
        _console.print(table)
        _console.print(
            f"max profile error {err_max:.1f}% (tol {tol:.1f}%), gradient {'monotonic' if gradient_ok else 'NON-monotonic'}"
        )

    return SubsectionResult(kind="statistics", label=label, metrics=metrics)
