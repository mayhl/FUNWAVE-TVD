"""
Phase-averaged surf profiles: H(x) + setup(x) vs measured (Hansen-Svendsen,
Ting-Kirby).  Zero-crossing stats per station over window_s, interpolated
onto the measured x; model_x = data_x + (bathymetry x0 - toe_x), never
fitted.  Setup is anchored at the seaward-most station (lab datum); the
removed offset reports ungated as mean_level_offset_m.

Keys: heights/setup (file names), channel, height_stat, window_s.
Gates: height_nrmse_pct (peak norm), setup_nrmse_pct (range norm).
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np

from test.framework.results import MetricResult, SubsectionResult
from test.regression.postproc.utils import read_run_metadata
from test.validation.oracles import wave_stats
from test.validation.oracles._lab import check_keys, load_deck, new_figure, nrmse_pct, read_table, save_figure

MIN_WAVES = 5  # stations with fewer zero-crossing waves are swash/dry — excluded

_LABEL = "Surf profiles"
ACCEPTED_KEYS = ("heights", "setup", "window_s", "channel", "height_stat", "height_nrmse_pct", "setup_nrmse_pct")


def _skip(msg: str) -> SubsectionResult:
    print(f"surf: {msg} — skipping")
    return SubsectionResult(kind="statistics", label=_LABEL, metrics=[])


def _station_x(deck: dict, channel: str | None) -> np.ndarray:
    """Return the point x-coordinates of the deck's station/transect channel."""
    out = deck.get("output") or {}
    geoms = {g.get("name"): g for g in out.get("geometries") or []}
    for ch in out.get("channels") or []:
        if channel is not None and ch.get("name") != channel:
            continue
        entry = geoms.get(ch.get("geometry"), ch)
        if "x" in entry:
            return np.asarray(entry["x"], dtype=float)
        if "start" in entry and "n_points" in entry:
            return np.linspace(float(entry["start"][0]), float(entry["end"][0]), int(entry["n_points"]))
    raise KeyError(f"no station/transect geometry resolvable for channel {channel!r}")


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    check_keys(tolerances, ACCEPTED_KEYS, "surf")
    h_name = tolerances.get("heights")
    s_name = tolerances.get("setup")
    if not (h_name or s_name):
        return _skip("tolerances block needs heights: or setup:")
    window = tolerances.get("window_s")
    if not window:
        return _skip("no steady window: set window_s in tolerances")

    def _profile(name: str, ycol: str) -> tuple[np.ndarray, np.ndarray, dict]:
        data, head = read_table(dev_dir / name)
        cols = head["columns"].split()
        return data[:, cols.index("x")], data[:, cols.index(ycol)], head

    xm_h = h_meas = xm_s = s_meas = None
    if h_name:
        xm_h, h_meas, head = _profile(h_name, "h")
    if s_name:
        xm_s, s_meas, head = _profile(s_name, "setup")

    meta = read_run_metadata(dev_dir)
    try:
        t_mod, v_mod = wave_stats.read_point_channel(meta.output_dir, "eta", tolerances.get("channel"))
    except FileNotFoundError as exc:
        return _skip(str(exc))
    deck = load_deck(dev_dir)
    x_sta = _station_x(deck, tolerances.get("channel"))
    if v_mod.shape[1] < len(x_sta):
        return _skip(f"model channel has {v_mod.shape[1]} points, deck geometry lists {len(x_sta)}")

    t0, t1 = float(window[0]), float(window[1])
    sel = (t_mod >= t0) & (t_mod <= t1)
    if sel.sum() < 64:
        return _skip(f"steady window [{t0}, {t1}] holds too few model samples")

    stat = tolerances.get("height_stat", "h_mean")
    h_mod = np.full(len(x_sta), np.nan)
    setup_mod = np.full(len(x_sta), np.nan)
    for k in range(len(x_sta)):
        e = v_mod[sel, k]
        stats = wave_stats.height_stats(t_mod[sel], e)
        if stats["n_waves"] >= MIN_WAVES:
            h_mod[k] = stats[stat]
        setup_mod[k] = float(np.mean(e))

    x_off = float(deck["grid"]["bathymetry"]["x0"]) - float(head["toe_x"])
    order = np.argsort(x_sta)
    xs = x_sta[order]

    def _onto(x_meas: np.ndarray, vals: np.ndarray) -> np.ndarray:
        good = np.isfinite(vals[order])
        if good.sum() < 2:
            return np.full(len(x_meas), np.nan)
        return np.interp(x_meas + x_off, xs[good], vals[order][good])

    ok = True
    metrics = []
    h_fit = None
    if h_name:
        h_fit = _onto(xm_h, h_mod)
        h_err = nrmse_pct(h_meas, h_fit, norm="max") if np.isfinite(h_fit).all() else float("inf")
        h_tol = float(tolerances.get("height_nrmse_pct", 15.0))
        h_ok = math.isfinite(h_err) and h_err < h_tol
        ok = h_ok
        if np.isfinite(h_fit).any():
            bp_off = float(xm_h[int(np.nanargmax(h_fit))] - xm_h[int(np.argmax(h_meas))])
            metrics.append(MetricResult("surf", "breakpoint_offset_m", bp_off, True, math.inf))
        metrics.append(MetricResult("surf", "height_nrmse_pct", h_err, h_ok, h_tol))

    s_fit = None
    if s_name:
        s_fit = _onto(xm_s, setup_mod)
        k0 = int(np.argmin(xm_s))
        metrics.append(MetricResult("surf", "mean_level_offset_m", float(s_fit[k0] - s_meas[k0]), True, math.inf))
        s_fit = s_fit - s_fit[k0]
        s_meas = s_meas - s_meas[k0]
        s_err = nrmse_pct(s_meas, s_fit, norm="range") if np.isfinite(s_fit).all() else float("inf")
        s_tol = float(tolerances.get("setup_nrmse_pct", 20.0))
        s_ok = math.isfinite(s_err) and s_err < s_tol
        ok = ok and s_ok
        metrics.append(MetricResult("surf", "setup_nrmse_pct", s_err, s_ok, s_tol))

    fig, axes = new_figure(nrows=(1 if h_name else 0) + (1 if s_name else 0), height_per_row=2.6)
    ax_iter = iter(axes)
    if h_name:
        ax = next(ax_iter)
        ax.plot(xm_h, h_meas, "o", ms=4, color="#2563eb", label="measured")
        ax.plot(xm_h, h_fit, "-", color="#dc2626", label="modern")
        ax.set_ylabel(f"H (m), {stat}")
        ax.grid(alpha=0.3)
        ax.legend(loc="upper right", fontsize=8)
    if s_name:
        ax = next(ax_iter)
        ax.plot(xm_s, s_meas, "o", ms=4, color="#2563eb", label="measured")
        ax.plot(xm_s, s_fit, "-", color="#dc2626", label="modern")
        ax.axhline(0.0, color="#94a3b8", lw=0.8)
        ax.set_ylabel("setup (m)")
        ax.grid(alpha=0.3)
        if not h_name:
            ax.legend(loc="upper right", fontsize=8)
    axes[-1].set_xlabel("x (m, lab frame)")
    figures = [save_figure(fig, plots_dir, "surf", f"H(x) + setup(x), window [{t0:g}, {t1:g}] s")]

    return SubsectionResult(kind="statistics", label=_LABEL, metrics=metrics, figures=figures)
