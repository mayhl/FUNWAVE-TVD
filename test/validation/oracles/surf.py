"""
Phase-averaged surf profiles: H(x) + setup(x) vs measured (Hansen-Svendsen,
Ting-Kirby).  Zero-crossing stats per station over window_s, interpolated
onto the measured x; model_x = data_x + (bathymetry x0 - toe_x), never
fitted.  Setup is anchored at the seaward-most station (lab datum); the
removed offset reports ungated as mean_level_offset_m.

Keys: heights/setup (file names), channel, height_stat, window_s.
Gates: height_nrmse_pct (peak norm), setup_rms_pct (rms setup error over
the seaward-most measured height H0 -- setup scales with H, and a range
norm turns a few mm into tens of percent).  Report-only:
breakpoint_offset_m, mean_level_offset_m, setup_slope_error_pct (linear
setup gradient shoreward of the measured breakpoint, model vs measured --
the radiation-stress balance, blind to the datum), and the breaker class
at the measured breakpoint when the deck writes the breaker fields
(xi_0/xi_b/gamma_b medians over the 2 m shoreward of it; Battjes 1974:
xi_b < 0.4 spilling, 0.4 to 2 plunging, above surging).
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np

from test.framework.results import MetricResult, SubsectionResult
from test.framework.tolerances import check_keys
from test.framework.run_output import read_run_metadata
from test.validation.oracles import wave_stats
from test.validation.oracles._lab import load_deck, new_figure, nrmse_pct, read_table, save_figure

MIN_WAVES = 5  # stations with fewer zero-crossing waves are swash/dry — excluded
MIN_SLOPE_POINTS = 3  # setup stations shoreward of the breakpoint needed for a gradient
BREAKER_SPAN_M = 2.0  # breaker-field median window shoreward of the measured breakpoint
FILL = -9999.0

_LABEL = "Surf profiles"
ACCEPTED_KEYS = ("heights", "setup", "window_s", "channel", "height_stat", "height_nrmse_pct", "setup_rms_pct")


def _skip(msg: str) -> SubsectionResult:
    print(f"surf: {msg} — skipping")
    return SubsectionResult(kind="statistics", label=_LABEL, metrics=[])


def _breaker_class(meta, deck: dict, x_bp: float) -> tuple[dict[str, float], str] | None:
    """Breaker-field medians over the span shoreward of x_bp, and the class.

    Reads the last breaker frame; None when the deck writes no breaker fields.
    """
    files = meta.output_files("xi_b")
    if not files:
        return None
    dx = float(deck["grid"]["cell_size"][0])
    out: dict[str, float] = {}
    for name in ("xi_0", "xi_b", "gamma_b"):
        frames = meta.output_files(name)
        if not frames:
            return None
        fld = meta.read_field(frames[-1])
        row = fld[fld.shape[0] // 2]
        x = np.arange(len(row)) * dx
        sel = (x >= x_bp) & (x <= x_bp + BREAKER_SPAN_M) & (row > FILL + 1.0)
        out[name] = float(np.median(row[sel])) if sel.any() else float("nan")
    xb = out["xi_b"]
    label = "unknown" if not np.isfinite(xb) else "spilling" if xb < 0.4 else "plunging" if xb < 2.0 else "surging"
    return out, label


def _event_log_rows(meta, deck: dict) -> list[tuple[str, int, int]]:
    """(channel, rows, count sum) per logged event channel: the log rides the
    same filters as the count statistic, so its rows must equal the count
    summed over cells at the last frame."""
    out = []
    for ch in deck.get("output", {}).get("channels", []):
        if not ch.get("log"):
            continue
        for var in ch.get("variables", []):
            path = Path(meta.output_dir) / ch["name"] / f"events_{var}.dat"
            frames = meta.output_files(f"{var}_count")
            if not path.exists() or not frames:
                continue
            rows = sum(1 for line in path.read_text().splitlines() if line and not line.startswith("#"))
            fld = meta.read_field(frames[-1])
            out.append((ch["name"], rows, int(round(float(fld[fld > FILL + 1.0].sum())))))
    return out


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
    if s_name and not h_name:
        return _skip("setup: needs heights: for the H0 norm and the breakpoint")
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
        breaker = _breaker_class(meta, deck, float(xm_h[int(np.argmax(h_meas))]) + x_off)
        if breaker is not None:
            vals, label = breaker
            for name, v in vals.items():
                metrics.append(MetricResult("surf", f"{name}_breakpoint", v, True, math.inf))
            print(
                f"surf: breaker class at the measured breakpoint: {label} (xi_b {vals['xi_b']:.2f}, gamma_b {vals['gamma_b']:.2f}, xi_0 {vals['xi_0']:.2f})"
            )
        metrics.append(MetricResult("surf", "height_nrmse_pct", h_err, h_ok, h_tol))

    s_fit = None
    if s_name:
        s_fit = _onto(xm_s, setup_mod)
        k0 = int(np.argmin(xm_s))
        metrics.append(MetricResult("surf", "mean_level_offset_m", float(s_fit[k0] - s_meas[k0]), True, math.inf))
        s_fit = s_fit - s_fit[k0]
        s_meas = s_meas - s_meas[k0]
        h0 = float(h_meas[int(np.argmin(xm_h))])
        s_err = 100.0 * float(np.sqrt(np.mean((s_fit - s_meas) ** 2))) / h0 if np.isfinite(s_fit).all() else float("inf")
        s_tol = float(tolerances.get("setup_rms_pct", 5.0))
        s_ok = math.isfinite(s_err) and s_err < s_tol
        ok = ok and s_ok
        metrics.append(MetricResult("surf", "setup_rms_pct", s_err, s_ok, s_tol))
        inner = xm_s > float(xm_h[int(np.argmax(h_meas))])
        if inner.sum() >= MIN_SLOPE_POINTS and np.isfinite(s_fit[inner]).all():
            slope_meas = float(np.polyfit(xm_s[inner], s_meas[inner], 1)[0])
            slope_fit = float(np.polyfit(xm_s[inner], s_fit[inner], 1)[0])
            if slope_meas != 0.0:
                slope_err = 100.0 * (slope_fit - slope_meas) / abs(slope_meas)
                metrics.append(MetricResult("surf", "setup_slope_error_pct", slope_err, True, math.inf))

    for name, rows, total in _event_log_rows(meta, deck):
        metrics.append(MetricResult("surf", f"{name}_event_rows_minus_count", rows - total, rows == total, 0.0))

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
