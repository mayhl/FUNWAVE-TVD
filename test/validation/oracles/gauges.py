"""
Measured gauge records (Mase-Kirby): model point channel vs a
[t, g1, g2, ...] table.  One documented lag per run (align gauge or
align: none), never per-gauge.  Modes: timeseries (worst-gauge NRMSE) or
heights (H_rms error + the Shi et al. 2012 Sec. 4.2 sigma/skew/asym set;
aggregates gate only when named, stat_exclude trims them).

Keys: file, gauges, align, max_lag_s, channel, variable (the point-channel
variable the table holds, default eta), window_s, mode, stat_exclude.
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np

from test.framework.results import MetricResult, SubsectionResult
from test.framework.tolerances import check_keys
from test.framework.run_output import read_run_metadata
from test.validation.oracles import wave_stats
from test.validation.oracles._lab import new_figure, nrmse_pct, read_table, save_figure

_LABEL = "Lab gauges"
ACCEPTED_KEYS = (
    "file",
    "gauges",
    "channel",
    "variable",
    "align",
    "max_lag_s",
    "window_s",
    "mode",
    "stat_exclude",
    "height_error_pct",
    "sigma_error_pct",
    "sigma_rmse_pct",
    "skew_rmse_pct",
    "asym_rmse_pct",
    "gauge_nrmse_pct",
)


def _skip(msg: str) -> SubsectionResult:
    print(f"gauges: {msg} — skipping")
    return SubsectionResult(kind="statistics", label=_LABEL, metrics=[])


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    check_keys(tolerances, ACCEPTED_KEYS, "gauges")
    file_name = tolerances.get("file")
    if not file_name:
        return _skip("tolerances block needs file:")

    data, head = read_table(dev_dir / file_name)
    cols = head["columns"].split()
    if cols[0] != "t":
        return _skip(f"measured file '{file_name}' must lead with a t column")
    gauge_names = list(tolerances.get("gauges") or cols[1:])

    meta = read_run_metadata(dev_dir)
    try:
        t_mod, v_mod = wave_stats.read_point_channel(meta.output_dir, tolerances.get("variable", "eta"), tolerances.get("channel"))
    except FileNotFoundError as exc:
        return _skip(str(exc))
    if v_mod.shape[1] < len(gauge_names):
        return _skip(f"model channel has {v_mod.shape[1]} points, deck expects {len(gauge_names)}")

    t_meas = data[:, 0]
    meas = {name: data[:, cols.index(name)] for name in gauge_names}
    model = {name: v_mod[:, k] for k, name in enumerate(gauge_names)}

    # single documented lag: model shifted onto the measured time base
    align = tolerances.get("align", gauge_names[0])
    lag = 0.0
    if align not in (None, "none"):
        if align not in meas:
            return _skip(f"align gauge '{align}' not among {gauge_names}")
        lag = wave_stats.lag_align(t_meas, meas[align], t_mod, model[align])
        max_lag = tolerances.get("max_lag_s")
        if max_lag is not None and abs(lag) > float(max_lag):
            lag = 0.0  # implausible peak (e.g. reflection): fall back, report raw

    window = tolerances.get("window_s")
    t0, t1 = (float(window[0]), float(window[1])) if window else (float(t_meas[0]), float(t_meas[-1]))
    sel = (t_meas >= t0) & (t_meas <= t1)
    if sel.sum() < 16:
        return _skip(f"analysis window [{t0}, {t1}] holds too few measured samples")

    mode = tolerances.get("mode", "timeseries")
    metrics = [MetricResult("gauges", "lag_s", lag, True, math.inf)]
    figures = []

    if mode == "heights":
        rows = []  # (name, hrms_err, sigma_err, sig_m, sig_d, skew_m, skew_d, asym_m, asym_d)
        for name in gauge_names:
            m = meas[name][sel]
            d = np.interp(t_meas[sel], t_mod + lag, model[name])
            sm = wave_stats.height_stats(t_meas[sel], m)
            sd = wave_stats.height_stats(t_meas[sel], d)
            h_err = abs(sd["h_rms"] / sm["h_rms"] - 1.0) * 100.0 if sm["h_rms"] else float("inf")
            sig_m, sig_d = float(np.std(m - m.mean())), float(np.std(d - d.mean()))
            sig_err = abs(sig_d / sig_m - 1.0) * 100.0 if sig_m else float("inf")
            sk_m, as_m = wave_stats.skewness_asymmetry(m)
            sk_d, as_d = wave_stats.skewness_asymmetry(d)
            rows.append((name, h_err, sig_err, sig_m, sig_d, sk_m, sk_d, as_m, as_d))
            metrics.append(MetricResult("gauges", f"hrms_err_{name}_pct", h_err, True, math.inf))

        tol = float(tolerances.get("height_error_pct", 20.0))
        worst = max(r[1] for r in rows)
        ok = math.isfinite(worst) and worst < tol
        metrics.append(MetricResult("gauges", "height_error_pct", worst, ok, tol))

        # sigma / skewness / asymmetry aggregates (Shi et al. 2012 Sec. 4.2),
        # gated only when the tolerances block names them
        excl = set(tolerances.get("stat_exclude") or [])
        agg = [r for r in rows if r[0] not in excl]
        sig_errs = np.array([r[2] for r in agg])
        sk_m = np.array([r[5] for r in agg])
        sk_d = np.array([r[6] for r in agg])
        as_m = np.array([r[7] for r in agg])
        as_d = np.array([r[8] for r in agg])
        aggregates = {
            "sigma_error_pct": float(sig_errs.max()),
            "sigma_rmse_pct": float(np.sqrt(np.mean(sig_errs**2))),
            "skew_rmse_pct": nrmse_pct(sk_m, sk_d, norm="max"),
            "asym_rmse_pct": nrmse_pct(as_m, as_d, norm="max"),
        }
        for key, value in aggregates.items():
            key_tol = tolerances.get(key)
            if key_tol is None:
                metrics.append(MetricResult("gauges", key, value, True, math.inf))
            else:
                metrics.append(MetricResult("gauges", key, value, math.isfinite(value) and value < float(key_tol), float(key_tol)))

        fig, axes = new_figure(nrows=3)
        idx = np.arange(len(gauge_names))
        # figure plots every gauge (stat_exclude only trims the gates)
        panels = tuple(
            (label, [r[km] for r in rows], [r[kd] for r in rows])
            for label, km, kd in (("sigma (m)", 3, 4), ("skewness", 5, 6), ("asymmetry", 7, 8))
        )
        for ax, (label, vm, vd) in zip(axes, panels):
            ax.plot(idx, vm, "o-", color="#2563eb", lw=1.0, ms=4, label="measured")
            ax.plot(idx, vd, "s--", color="#dc2626", lw=1.0, ms=4, label="modern")
            ax.set_ylabel(label)
            ax.grid(alpha=0.3)
            if ax is axes[0]:
                ax.legend(loc="upper left", fontsize=8)
        axes[-1].set_xticks(idx, gauge_names, rotation=45, fontsize=8)
        figures.append(save_figure(fig, plots_dir, "gauges", "gauge wave statistics"))
    else:
        worst = 0.0
        curves = []
        for name in gauge_names:
            m = meas[name][sel]
            d = np.interp(t_meas[sel], t_mod + lag, model[name])
            err = nrmse_pct(m, d, norm="max")
            worst = max(worst, err)
            metrics.append(MetricResult("gauges", f"nrmse_{name}_pct", err, True, math.inf))
            curves.append(name)
        tol = float(tolerances.get("gauge_nrmse_pct", 20.0))
        ok = math.isfinite(worst) and worst < tol
        metrics.append(MetricResult("gauges", "gauge_nrmse_pct", worst, ok, tol))

        fig, axes = new_figure(nrows=len(curves))
        for ax, name in zip(axes, curves):
            ax.plot(t_meas, meas[name], "-", color="#2563eb", lw=1.0, label="measured")
            ax.plot(t_mod + lag, model[name], "--", color="#dc2626", lw=1.0, label="modern")
            ax.axvspan(t0, t1, color="#94a3b8", alpha=0.15, lw=0)
            ax.set_xlim(t0 - 0.1 * (t1 - t0), t1 + 0.1 * (t1 - t0))
            ax.set_ylabel(f"{name} (m)")
            ax.grid(alpha=0.3)
            if ax is axes[0]:
                ax.legend(loc="upper right", fontsize=8)
        axes[-1].set_xlabel("t (s), measured base; model shifted by the documented lag")
        figures.append(save_figure(fig, plots_dir, "gauges", "gauge eta(t)"))

    return SubsectionResult(kind="statistics", label=_LABEL, metrics=metrics, figures=figures)
