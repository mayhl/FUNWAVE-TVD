"""Generating-absorbing (GEN_ABS) combined-forcing oracle (2D Boussinesq).

Ref-less validation postproc for combined boundary forcing: the west face
carries BOTH a wavemaker spectrum and a still-water-level (SWL) file series on
one forcing block.  The tide reads the file as the relaxation target while the
wavemaker generates the waves through the same strip, so the two compose -- the
interior mean rides up to the SWL while the spectrum's Hm0 still transmits.
This is the mechanism a rising-SWL match (e.g. an XBeach zs0file) needs.

The near-west station line (just past the relaxation strip) is sampled over the
record's last T_rec.  Two gated checks confirm both forcings took effect:
  * ``swl_err_m`` (gated) -- x-line-mean water level vs the SWL file value in
    the window (the file forcing raised the mean).
  * ``hm0_err_pct`` (gated) -- x-line-mean variance Hm0 (4 sqrt(m0)) about that
    mean vs the table target (the wavemaker still transmits under the SWL).

Note 1: gauge series are re-sampled onto a uniform grid by linear interpolation
before the statistics; the channel writer follows the adaptive step.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) -> SubsectionResult
(ref_dir is None in oracle mode and unused.)

Tolerance keys (under tolerances: wavemaker_gen_abs:):
  swl_err_m     -- max allowed |mean level - SWL file value| [m]
  hm0_err_pct   -- max allowed % error of Hm0 vs the table target
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
from rich.console import Console

from test.framework.results import MetricResult, SubsectionResult
from test.framework.run_output import read_run_metadata
from test.framework.tolerances import check_keys
from test.validation.oracles import wave_stats
from test.validation.oracles._lab import load_deck

_console = Console()
ACCEPTED_KEYS = ("swl_err_m", "hm0_err_pct")

DT_SAMPLE = 0.1  # uniform re-sample step (matches the channel interval)
T_REC = 200.0  # equal-df ladder recurrence period (41 lines, df = 0.005 Hz)


def _table_hm0(path: Path) -> float:
    """Component-sum Hm0 = 4 sqrt(sum a^2 / 2) from a WK_DATA2D table."""
    toks = path.read_text().split()
    nfreq, ndir = int(toks[0]), int(toks[1])
    off = 2 + 1 + nfreq + ndir  # nfreq ndir / peak / freqs / dirs / amps
    amp = np.array(toks[off : off + nfreq * ndir], dtype=float)
    return 4.0 * math.sqrt(0.5 * float(np.sum(amp**2)))


def _swl_series(path: Path) -> tuple[np.ndarray, np.ndarray]:
    """(t, eta) from the tide file series (one header line, then t eta u v)."""
    d = np.loadtxt(path, skiprows=1)
    return d[:, 0], d[:, 1]


def _forcing_paths(run_dir: Path, deck: dict) -> tuple[Path, Path] | None:
    """(spectrum table, SWL file) resolved from the deck's west forcing."""
    wm = deck.get("wavemaker")
    if isinstance(wm, list):
        wm = wm[0] if wm else {}
    tbl = (wm or {}).get("spectrum", {}).get("file")
    swl = deck.get("boundaries", {}).get("west", {}).get("forcing", {}).get("file")
    if tbl is None or swl is None:
        return None
    return run_dir / tbl, run_dir / swl


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
    check_keys(tolerances, ACCEPTED_KEYS, "wavemaker_gen_abs")
    label = "Wavemaker Combined Forcing (GEN_ABS)"
    output_dir = read_run_metadata(dev_dir).output_dir

    try:
        deck = load_deck(dev_dir)
    except FileNotFoundError:
        _console.print("[yellow]wavemaker_gen_abs:[/yellow] deck not found -- skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    paths = _forcing_paths(dev_dir, deck)
    if paths is None:
        _console.print("[yellow]wavemaker_gen_abs:[/yellow] spectrum/SWL file missing -- skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    tbl_path, swl_path = paths
    hm0_target = _table_hm0(tbl_path)
    swl_t, swl_eta = _swl_series(swl_path)

    gauges = _load_gauges(output_dir)
    if gauges is None:
        _console.print("[yellow]wavemaker_gen_abs:[/yellow] gauge records missing/short -- skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    t_raw, eta_raw = gauges

    n_win = int(round(T_REC / DT_SAMPLE))
    t_start = DT_SAMPLE * math.floor((t_raw[-1] - T_REC) / DT_SAMPLE)
    t_uni = t_start + DT_SAMPLE * np.arange(n_win)
    if t_start < 0.0 or t_raw[-1] < t_uni[-1]:
        _console.print("[yellow]wavemaker_gen_abs:[/yellow] record ends before the stats window -- skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    eta = np.stack([np.interp(t_uni, t_raw, eta_raw[:, g]) for g in range(eta_raw.shape[1])], axis=1)

    # mean water level (x-line average) vs the SWL file value in the window,
    # and Hm0 about that mean vs the table target
    mean_level = float(np.mean(eta))
    swl_target = float(np.interp(0.5 * (t_uni[0] + t_uni[-1]), swl_t, swl_eta))
    swl_err = abs(mean_level - swl_target)
    hm0 = float(np.mean(4.0 * np.sqrt(np.mean((eta - eta.mean(axis=0)) ** 2, axis=0))))
    hm0_err = abs(hm0 - hm0_target) / hm0_target * 100.0

    def tol(key: str) -> float:
        return float(tolerances[key]) if key in tolerances else math.inf

    swl_tol, hm0_tol = tol("swl_err_m"), tol("hm0_err_pct")
    swl_pass = swl_err < swl_tol
    hm0_pass = hm0_err < hm0_tol

    metrics = [
        MetricResult("wavemaker_gen_abs", "mean_level_m", mean_level, True, math.inf),
        MetricResult("wavemaker_gen_abs", "swl_target_m", swl_target, True, math.inf),
        MetricResult("wavemaker_gen_abs", "swl_err_m", swl_err, swl_pass, swl_tol),
        MetricResult("wavemaker_gen_abs", "hm0_measured_m", hm0, True, math.inf),
        MetricResult("wavemaker_gen_abs", "hm0_err_pct", hm0_err, hm0_pass, hm0_tol),
    ]
    if verbose or not (swl_pass and hm0_pass):
        _console.print(
            f"GEN_ABS: mean level {mean_level:.3f} m vs SWL {swl_target:.3f} m "
            f"(err {swl_err:.3f}, tol {swl_tol:.3f}); "
            f"Hm0 {hm0:.3f} vs {hm0_target:.3f} m (err {hm0_err:.1f}%, tol {hm0_tol:.1f}%)"
        )

    return SubsectionResult(kind="statistics", label=label, metrics=metrics)
