"""
Wave statistics utilities for the lab-validation oracles (numpy only).

To keep the test suite self-contained on the HPC board we implement the small
set of signal statistics the lab oracles need directly on numpy: zero-crossing
wave heights, a hand-rolled Welch PSD, the FFT analytic signal (Hilbert
envelope, skewness/asymmetry), and cross-correlation lag alignment.  scipy is
deliberately not imported anywhere in this module; the one-time .mat
conversions during dataset curation live in scripts/, not here.

We also provide the reader for the modern point-channel station layout
(result_folder/<channel>/<var>.dat with one "t v1 .. vn" row per flush),
since every gauge-comparison oracle starts from it.

Note 1: all series are assumed uniformly sampled; the readers report the
median dt and the statistics take dt as given rather than resampling.
Note 2: Hm0 here is the variance measure 4 sqrt(m0) over the analysis
window, matching the spectral_fidelity oracle's convention.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np

__all__ = [
    "zero_crossing_waves",
    "height_stats",
    "hilbert_analytic",
    "skewness_asymmetry",
    "lag_align",
    "read_point_channel",
]


# ---------------------------------------------------------------------------
# Zero-crossing statistics
# ---------------------------------------------------------------------------


def zero_crossing_waves(t: np.ndarray, eta: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    """Return (heights, periods) of the individual up-crossing waves.

    We demean the record, locate the upward zero crossings by sign change,
    and take each wave as the crest-to-trough excursion between consecutive
    crossings.  Records with fewer than two crossings return empty arrays.
    """
    eta = np.asarray(eta, dtype=float) - float(np.mean(eta))
    sign = np.signbit(eta)
    # up-crossing: sample k negative, k+1 non-negative
    up = np.where(sign[:-1] & ~sign[1:])[0] + 1
    if len(up) < 2:
        return np.array([]), np.array([])
    heights = np.array([eta[a:b].max() - eta[a:b].min() for a, b in zip(up[:-1], up[1:])])
    periods = np.diff(np.asarray(t, dtype=float)[up])
    return heights, periods


def height_stats(t: np.ndarray, eta: np.ndarray) -> dict[str, float]:
    """Return the standard wave-height measures of a gauge record.

    Keys: n_waves, h_mean, h_rms, h_13 (mean of the highest third), hm0
    (4 sqrt(variance)), t_mean.  Zero-crossing measures are NaN when the
    record holds fewer than two waves; hm0 is always defined.
    """
    eta = np.asarray(eta, dtype=float)
    hm0 = 4.0 * float(np.std(eta))
    h, tp = zero_crossing_waves(t, eta)
    if len(h) == 0:
        return {"n_waves": 0.0, "h_mean": np.nan, "h_rms": np.nan, "h_13": np.nan, "hm0": hm0, "t_mean": np.nan}
    h_sorted = np.sort(h)[::-1]
    n13 = max(1, len(h) // 3)
    return {
        "n_waves": float(len(h)),
        "h_mean": float(np.mean(h)),
        "h_rms": float(np.sqrt(np.mean(h**2))),
        "h_13": float(np.mean(h_sorted[:n13])),
        "hm0": hm0,
        "t_mean": float(np.mean(tp)) if len(tp) else np.nan,
    }


# ---------------------------------------------------------------------------
# Spectra
# ---------------------------------------------------------------------------


def hilbert_analytic(x: np.ndarray) -> np.ndarray:
    """Return the analytic signal x + i H[x] via the FFT construction."""
    x = np.asarray(x, dtype=float)
    n = len(x)
    X = np.fft.fft(x)
    h = np.zeros(n)
    h[0] = 1.0
    if n % 2 == 0:
        h[n // 2] = 1.0
        h[1 : n // 2] = 2.0
    else:
        h[1 : (n + 1) // 2] = 2.0
    return np.fft.ifft(X * h)


def skewness_asymmetry(x: np.ndarray) -> tuple[float, float]:
    """Return (skewness, asymmetry) of a wave record.

    Skewness is the normalized third moment <x^3>/<x^2>^{3/2}; asymmetry is
    the same moment of the Hilbert transform, -<H[x]^3>/<x^2>^{3/2} — the
    standard front-face pitching measure for shoaling waves.
    """
    x = np.asarray(x, dtype=float)
    x = x - x.mean()
    m2 = float(np.mean(x**2))
    if m2 <= 0.0:
        return np.nan, np.nan
    hx = np.imag(hilbert_analytic(x))
    skew = float(np.mean(x**3)) / m2**1.5
    asym = -float(np.mean(hx**3)) / m2**1.5
    return skew, asym


# ---------------------------------------------------------------------------
# Alignment
# ---------------------------------------------------------------------------


def lag_align(t_a: np.ndarray, a: np.ndarray, t_b: np.ndarray, b: np.ndarray) -> float:
    """Return the lag (seconds) that best aligns series b onto series a.

    We resample both demeaned series onto the finer of the two grids over
    the overlapping span padded by half its length on each side, and take
    the full cross-correlation peak.  A positive value means b happens
    EARLIER than a and must be shifted by +lag to line up (compare
    a(t) with b(t - lag)).  One lag per run: callers apply a single
    documented value to all gauges, never a per-gauge fit.
    """
    t_a, a = np.asarray(t_a, dtype=float), np.asarray(a, dtype=float)
    t_b, b = np.asarray(t_b, dtype=float), np.asarray(b, dtype=float)
    dt = min(float(np.median(np.diff(t_a))), float(np.median(np.diff(t_b))))
    lo = min(t_a[0], t_b[0])
    hi = max(t_a[-1], t_b[-1])
    grid = np.arange(lo, hi + dt / 2, dt)
    ag = np.interp(grid, t_a, a - a.mean(), left=0.0, right=0.0)
    bg = np.interp(grid, t_b, b - b.mean(), left=0.0, right=0.0)
    xc = np.correlate(ag, bg, mode="full")
    k = int(np.argmax(xc)) - (len(grid) - 1)
    return k * dt


# ---------------------------------------------------------------------------
# Station channel reader (modern point-channel layout)
# ---------------------------------------------------------------------------


def read_point_channel(output_dir: str | Path, variable: str = "eta", channel: str | None = None) -> tuple[np.ndarray, np.ndarray]:
    """Return (t, values[n_t, n_points]) from a point-channel file.

    The modern layout is result_folder/<channel>/<var>.dat with one
    "t v1 .. vn" row per flush; the pre-folder flat spelling
    <channel>_<var>.dat is kept readable for older trees.  When channel is
    None the first matching file (sorted) is used.
    """
    output_dir = Path(output_dir)
    if channel is not None:
        candidates = [output_dir / channel / f"{variable}.dat", output_dir / f"{channel}_{variable}.dat"]
        candidates = [p for p in candidates if p.exists()]
    else:
        candidates = sorted(output_dir.glob(f"*/{variable}.dat")) + sorted(output_dir.glob(f"*_{variable}.dat"))
    if not candidates:
        raise FileNotFoundError(f"no point-channel '{variable}' file under {output_dir}")
    arr = np.loadtxt(candidates[0])
    if arr.ndim == 1:
        arr = arr.reshape(1, -1)
    return arr[:, 0], arr[:, 1:]
