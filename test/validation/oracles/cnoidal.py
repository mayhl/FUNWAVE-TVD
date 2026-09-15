"""
Cnoidal-wave generation oracle: a flat flume fed by the cnoidal spectrum
type, gauges read back against first-order cnoidal theory (the same solve
the wavemaker uses).  Ref-less.

The modulus m solves the L = cT closure of L = 4Kh sqrt(mh/3H) with Mei's
celerity; the surface is y_t + H cn^2(2K(x - ct)/L | m), zero-mean.  Over the
last window_periods periods at every gauge:

  height_error_pct    — |mean crest-to-trough height / H - 1|
  celerity_error_pct  — |c_1 - c| / c, c_1 from the fundamental's phase slope
                        along the transect (k_1 = -d phi_1/dx)
  harmonic_lock_pct   — max over n = 2, 3 of |c_n - c_1| / c_1: a permanent
                        form carries every harmonic at one speed; a free
                        harmonic would run at its own linear speed.  Harmonics
                        under 10 % of the fundamental are left out (no usable
                        phase at low Ursell number)
  shape_error_pct     — RMS of the record (mean removed) against the theory
                        profile over one period, crest-aligned, per H

Reported ungated: modulus, wavelength, theory celerity, crest fraction and
setdown (mean level), the a_2/a_1 ladder, the first-to-last-gauge height change.
The model runs 3-4 % slower than first-order theory at this steepness (part
resolution, part the theory's own O((H/h)^2) error), so the celerity gate
tracks generation, not the theory's accuracy.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) ->
SubsectionResult (ref_dir is None in oracle mode and unused).
Keys: channel, window_periods; gates height_error_pct, celerity_error_pct,
harmonic_lock_pct, shape_error_pct.
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np

from test.framework.results import MetricResult, SubsectionResult
from test.framework.tolerances import check_keys
from test.framework.run_output import read_run_metadata
from test.validation.oracles import wave_stats
from test.validation.oracles._lab import load_deck, new_figure, save_figure
from test.validation.oracles.surf import _station_x

G = 9.81
WEAK_HARMONIC = 0.1  # fraction of the fundamental below which a harmonic has no usable phase
_LABEL = "Cnoidal generation"
ACCEPTED_KEYS = (
    "channel",
    "window_periods",
    "height_error_pct",
    "celerity_error_pct",
    "harmonic_lock_pct",
    "shape_error_pct",
)


def _skip(msg: str) -> SubsectionResult:
    print(f"cnoidal: {msg} — skipping")
    return SubsectionResult(kind="statistics", label=_LABEL, metrics=[])


# ---------------------------------------------------------------------------
# First-order cnoidal theory (mirrors src/model/2d/wavemaker.f90 cnoidal_harmonics)
# ---------------------------------------------------------------------------


def elliptic_ke(m: float) -> tuple[float, float]:
    """Complete elliptic integrals K(m), E(m) by the arithmetic-geometric mean."""
    a, b, c = 1.0, math.sqrt(1.0 - m), math.sqrt(m)
    series, pow2 = 0.5 * c * c, 1.0
    for _ in range(60):
        a, b, c = 0.5 * (a + b), math.sqrt(a * b), 0.5 * (a - b)
        series += pow2 * c * c
        pow2 *= 2.0
        if abs(c) < 1e-15:
            break
    k = 0.5 * math.pi / a
    return k, k * (1.0 - series)


def cn(u: np.ndarray, m: float) -> np.ndarray:
    """Jacobi cn(u | m) by the descending AGM (Abramowitz & Stegun 16.4)."""
    a, b, c = [1.0], [math.sqrt(1.0 - m)], [math.sqrt(m)]
    while abs(c[-1]) > 1e-15 and len(a) < 60:
        a.append(0.5 * (a[-1] + b[-1]))
        c.append(0.5 * (a[-2] - b[-1]))
        b.append(math.sqrt(a[-2] * b[-1]))
    n = len(a) - 1
    phi = 2.0**n * a[n] * np.asarray(u, dtype=float)
    for i in range(n, 0, -1):
        phi = 0.5 * (phi + np.arcsin(c[i] / a[i] * np.sin(phi)))
    return np.cos(phi)


def solve(height: float, depth: float, period: float) -> dict | None:
    """Modulus, wavelength, celerity, trough level and harmonic ladder; None outside the cnoidal range."""

    def residual(m: float) -> float:
        k, e = elliptic_ke(m)
        return (
            m * depth
            + 2.0 * height
            - m * height
            - 3.0 * height * e / k
            - 16.0 * depth**3 * m**2 * k**2 / (3.0 * G * height * period**2)
        )

    ms = 1.0 - np.exp(-np.linspace(0.05, 30.0, 600))
    fs = np.array([residual(m) for m in ms])
    if fs.max() <= 0.0:
        return None
    lo, hi = float(ms[np.argmax(fs)]), 1.0 - 1e-12
    for _ in range(200):
        mid = 0.5 * (lo + hi)
        if residual(mid) > 0.0:
            lo = mid
        else:
            hi = mid
        if hi - lo < 1e-13:
            break
    m = 0.5 * (lo + hi)
    k, e = elliptic_ke(m)
    kp, _ = elliptic_ke(1.0 - m)
    wave_length = 4.0 * k * depth * math.sqrt(m * depth / (3.0 * height))
    q = math.exp(-math.pi * kp / k)
    n = np.arange(1, 65)
    amps = height * 2.0 * math.pi**2 / (m * k * k) * n * q**n / (1.0 - q ** (2 * n))
    return {
        "m": m,
        "K": k,
        "L": wave_length,
        "c": wave_length / period,
        "y_t": height / m * (1.0 - m - e / k),
        "amps": amps[amps > 1e-6 * height],
    }


def profile(t: np.ndarray, th: dict, height: float, period: float) -> np.ndarray:
    """Zero-mean theory surface at a fixed gauge, crest at t = 0."""
    return th["y_t"] + height * cn(2.0 * th["K"] * (-t / period), th["m"]) ** 2


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    check_keys(tolerances, ACCEPTED_KEYS, "cnoidal")
    deck = load_deck(dev_dir)
    spec = deck["wavemaker"]["spectrum"]
    height, period = 2.0 * float(spec["amplitude"]), float(spec["period"])
    depth = float(deck["wavemaker"].get("source", {}).get("depth") or deck["grid"]["bathymetry"]["depth"])
    th = solve(height, period=period, depth=depth)
    if th is None:
        return _skip("deck wave lies outside the cnoidal range")

    meta = read_run_metadata(dev_dir)
    try:
        t, v = wave_stats.read_point_channel(meta.output_dir, "eta", tolerances.get("channel"))
    except FileNotFoundError as exc:
        return _skip(str(exc))
    x = _station_x(deck, tolerances.get("channel"))
    if v.shape[1] < len(x):
        return _skip(f"model channel has {v.shape[1]} points, deck geometry lists {len(x)}")
    n_per = int(tolerances.get("window_periods", 4))
    dt = float(np.median(np.diff(t)))
    npts = int(round(n_per * period / dt))
    if npts > len(t) or t[-1] < 3.0 * period + x[-1] / th["c"]:
        return _skip("record too short for the window")
    e = v[-npts:, : len(x)]
    tw = t[-npts:]

    # height, crest fraction, mean level per gauge
    crest, trough = e.max(axis=0), e.min(axis=0)
    h_gauge = crest - trough
    mean_level = e.mean(axis=0)
    crest_frac = float(np.mean((crest - mean_level) / h_gauge))
    height_error = abs(float(np.mean(h_gauge)) / height - 1.0) * 100.0
    # along-flume change of the height, first to last gauge: the harmonics
    # breathe while the first-order shape settles into the model's own form
    height_change = (float(h_gauge[-1]) / float(h_gauge[0]) - 1.0) * 100.0

    # per-harmonic celerity from the phase slope along the transect
    spec_c = np.fft.rfft(e - mean_level, axis=0)
    f = np.fft.rfftfreq(npts, dt)
    # a harmonic below WEAK_HARMONIC of the fundamental carries no usable phase
    # (near-sinusoidal waves at low Ursell number): its speed is reported but
    # left out of the lock, which then reads 0 when nothing qualifies
    c_n, strong = [], []
    for nh in (1, 2, 3):
        i = int(np.argmin(np.abs(f - nh / period)))
        ph = np.unwrap(np.angle(spec_c[i, :]))
        k_n = -float(np.polyfit(x, ph, 1)[0])
        c_n.append(2.0 * math.pi * nh / period / k_n)
        strong.append(
            float(np.mean(np.abs(spec_c[i, :])))
            >= WEAK_HARMONIC * float(np.mean(np.abs(spec_c[int(np.argmin(np.abs(f - 1.0 / period))), :])))
        )
    celerity_error = abs(c_n[0] - th["c"]) / th["c"] * 100.0
    harmonic_lock = max([abs(c_n[n] - c_n[0]) / c_n[0] * 100.0 for n in (1, 2) if strong[n]], default=0.0)
    i1 = int(np.argmin(np.abs(f - 1.0 / period)))
    i2 = int(np.argmin(np.abs(f - 2.0 / period)))
    a2_a1 = float(np.mean(np.abs(spec_c[i2, :]) / np.abs(spec_c[i1, :])))

    # shape: last period at each gauge, crest-aligned, mean removed
    errs = []
    n1 = int(round(period / dt))
    for k in range(len(x)):
        seg = e[-n1:, k] - mean_level[k]
        ic = int(np.argmax(seg))
        tt = (np.arange(n1) - ic) * dt
        errs.append(float(np.sqrt(np.mean((seg - profile(tt, th, height, period)) ** 2))) / height * 100.0)
    shape_error = float(np.mean(errs))

    tol_h = float(tolerances.get("height_error_pct", 10.0))
    tol_c = float(tolerances.get("celerity_error_pct", 5.0))
    tol_l = float(tolerances.get("harmonic_lock_pct", 3.0))
    tol_s = float(tolerances.get("shape_error_pct", 10.0))
    metrics = [
        MetricResult("cnoidal", "modulus", th["m"], True, math.inf),
        MetricResult("cnoidal", "wave_length_m", th["L"], True, math.inf),
        MetricResult("cnoidal", "celerity_theory_mps", th["c"], True, math.inf),
        MetricResult("cnoidal", "celerity_model_mps", c_n[0], True, math.inf),
        MetricResult("cnoidal", "crest_fraction", crest_frac, True, math.inf),
        MetricResult("cnoidal", "crest_fraction_theory", (height + th["y_t"]) / height, True, math.inf),
        MetricResult("cnoidal", "mean_level_m", float(np.mean(mean_level)), True, math.inf),
        MetricResult("cnoidal", "height_change_pct", height_change, True, math.inf),
        MetricResult("cnoidal", "a2_a1", a2_a1, True, math.inf),
        MetricResult("cnoidal", "a2_a1_theory", float(th["amps"][1] / th["amps"][0]), True, math.inf),
        MetricResult("cnoidal", "height_error_pct", height_error, height_error < tol_h, tol_h),
        MetricResult("cnoidal", "celerity_error_pct", celerity_error, celerity_error < tol_c, tol_c),
        MetricResult("cnoidal", "harmonic_lock_pct", harmonic_lock, harmonic_lock < tol_l, tol_l),
        MetricResult("cnoidal", "shape_error_pct", shape_error, shape_error < tol_s, tol_s),
    ]

    fig, (ax1, ax2) = new_figure(1, 2)
    for k, ls in ((0, "-"), (len(x) - 1, "--")):
        seg = e[-2 * n1 :, k] - mean_level[k]
        ax1.plot(tw[-2 * n1 :] - tw[-2 * n1], seg, ls, lw=1.0, label=f"model x = {x[k]:g} m")
    ic = int(np.argmax(e[-2 * n1 :, 0]))
    tt = (np.arange(2 * n1) - ic) * dt
    ax1.plot(tw[-2 * n1 :] - tw[-2 * n1], profile(tt, th, height, period), "k:", lw=1.2, label="theory")
    ax1.set_xlabel("t [s]")
    ax1.set_ylabel("eta [m]")
    ax1.legend(fontsize=7)
    kk = []
    for nh in (1, 2, 3):
        w = 2.0 * math.pi * nh / period
        kx = w / math.sqrt(G * depth)
        for _ in range(60):
            kx = w * w / (G * math.tanh(kx * depth))
        kk.append(w / kx)
    ax2.plot([1, 2, 3], c_n, "o-", label="model harmonic speed")
    ax2.plot([1, 2, 3], kk, "s--", label="free linear speed")
    ax2.axhline(th["c"], color="k", ls=":", label="cnoidal c")
    ax2.set_xlabel("harmonic n")
    ax2.set_ylabel("c [m/s]")
    ax2.legend(fontsize=7)
    figures = [save_figure(fig, plots_dir, "cnoidal", f"cnoidal generation, H {height:g} m, h {depth:g} m, T {period:g} s")]
    return SubsectionResult(kind="statistics", label=_LABEL, metrics=metrics, figures=figures)
