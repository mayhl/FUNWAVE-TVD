"""Wavemaker spectrum self-consistency oracle (2D Boussinesq).

Ref-less validation postproc for the synthetic parametric wavemaker rung: the
deck asks a spectra-type wavemaker (TMA / JONSWAP / spectrum_2d file table)
for a known target, and the oracle measures what the FIELD realized at gauges
well downwave of the source — target-vs-realized self-consistency, no lab data.

All five lanes share one rig (see inputs/wavemaker_spectrum/tma_grid.yaml):
flat bottom, periodic-y, a 41-line ENDPOINT-INCLUSIVE equal-df ladder
(df = band/(nf-1) = 0.005 Hz) whose recurrence period is T_rec = 200 s, and a
stats window of exactly the record's LAST T_rec (W = k T_rec with k = 1) —
an exact multiple of T_rec, so ladder lines land on periodogram bins and,
over a full period, cross-frequency variance terms integrate out exactly.
k = 1 trades line/background bin separation for laptop wall-clock; the comb
this metric trips (a dead-every-Nth-line pattern, ~14 % pre-fix) sits far
above the residual bound-harmonic leakage into line bins.  Gauge convention
(fixed across the lanes): columns 1-5 are the downwave x-line (Hm0 average),
columns 6-7 the lateral pair that brackets the x-line center gauge
(homogeneity diagnostic).

Metrics (only the finite-tolerance ones gate):
  * ``hm0_err_pct`` (gated) — x-line-mean variance Hm0 (4 sqrt(m0)) vs the
    analytic target: the deck hm0 for TMA/JONSWAP (band renormalization makes
    it exact), or 4 sqrt(sum(a^2)/2) over the file table for spectrum_2d.
  * ``fp_err_pct`` (gated, TMA/JONSWAP only) — Welch peak location vs the deck
    peak; the coarse peak-bin window [0.10, 0.14] Hz expressed as a percent
    tolerance (0.02/0.12 = 16.7 %).  A flat spectrum_2d table has no peak, so
    the lane's config carries no tolerance and the metric stays diagnostic.
  * ``band_frac_pct`` (gated, FLOOR) — percent of gauge variance inside
    [f_min, f_max] vs total; passes when it EXCEEDS the tolerance (a floor on
    in-band containment, not a ceiling on error — the one inverted gate here).
  * ``dead_line_pct`` (gated on the uniform-ladder single_dir lane) — the comb
    tripwire: exact-2-T_rec periodogram at the x-line center gauge, percent of
    in-band ladder lines carrying < 1 % of the median line power.  Regresses
    the f0a8375 equal-spreading-mass fix (the legacy ntheta-cyclic draw left
    ~14 % of lines dead).  Skipped under equal_energy: freq — those bins are
    non-uniform, so there is no T_rec and no comb for the lines to land on.

Note 1: gauge series are re-sampled onto a uniform grid by linear
interpolation before any spectral estimate; the channel writer follows the
adaptive step, so raw sample times are only nominally uniform.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) -> SubsectionResult
(ref_dir is None in oracle mode and unused.)

Tolerance keys (under tolerances: wavemaker_spectrum:):
  hm0_err_pct    — max allowed % error of x-line-mean Hm0 vs the target
  fp_err_pct     — max allowed % offset of the Welch peak vs the deck peak
  band_frac_pct  — MIN allowed % of variance inside the generated band
  dead_line_pct  — max allowed % of dead ladder lines (uniform ladders only)
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import yaml
from rich import box
from rich.console import Console
from rich.table import Table

from test.framework.results import MetricResult, SubsectionResult
from test.regression.postproc.utils import read_run_metadata
from test.validation.oracles._lab import check_keys

_console = Console()
ACCEPTED_KEYS = ("hm0_err_pct", "fp_err_pct", "band_frac_pct", "dead_line_pct", "hm0_target_scale")

DT_SAMPLE = 0.1  # uniform re-sample step (matches the channel interval)
N_XLINE = 5  # gauge columns 1..5 = downwave x-line, 6..7 = lateral pair
DEAD_LINE_POWER_FRAC = 0.01  # a line is dead below this fraction of the median


@dataclass
class CaseSpec:
    """Analytic target parsed from the run deck (and file table, if any)."""

    stype: str  # tma | jonswap | spectrum_2d
    hm0: float
    fp: float | None  # None: no peak defined (flat file table)
    fmin: float
    fmax: float
    nfreq: int
    uniform_ladder: bool  # equal-df lines -> T_rec exists


# ---------------------------------------------------------------------------
# I/O
# ---------------------------------------------------------------------------


def _read_data2d_table(path: Path) -> tuple[float, np.ndarray, np.ndarray]:
    """Parse a WK_DATA2D file -> (peak_period, freqs, amp[nfreq, ndir]).

    Mirrors data2d_init_compute: nf nd / PeakPeriod / freq per line / dir per
    line / amp(1:nf) row per dir, with the engine's |dir| >= 60 deg drop
    applied so the analytic Hm0 matches what the source actually radiates.
    """
    tok = path.read_text().split()
    nf, nd = int(tok[0]), int(tok[1])
    peak_period = float(tok[2])
    vals = np.array(tok[3:], dtype=float)
    freqs = vals[:nf]
    dirs = vals[nf : nf + nd]
    amp = vals[nf + nd : nf + nd + nf * nd].reshape(nd, nf).T
    return peak_period, freqs, amp[:, np.abs(dirs) < 60.0]


def _read_case(run_dir: Path) -> CaseSpec | None:
    """Build the analytic target spec from the run deck (single *.yaml)."""
    yaml_files = sorted(run_dir.glob("*.yaml"))
    if not yaml_files:
        return None
    with open(yaml_files[0]) as fh:
        cfg = yaml.safe_load(fh)

    spec = cfg.get("wavemaker", {}).get("spectrum", {})
    stype = str(spec.get("type", "")).lower()

    if stype == "spectrum_2d":
        table = run_dir / spec["file"]
        peak_period, freqs, amp = _read_data2d_table(table)
        # component amplitudes carry variance a^2/2 -> Hm0 = 4 sqrt(sum/2);
        # a flat table has no spectral peak, so fp stays None (ungated)
        hm0 = 4.0 * math.sqrt(0.5 * float(np.sum(amp**2)))
        df = np.diff(freqs)
        return CaseSpec(
            stype=stype,
            hm0=hm0,
            fp=None,
            fmin=float(freqs.min()),
            fmax=float(freqs.max()),
            nfreq=len(freqs),
            uniform_ladder=bool(np.allclose(df, df[0])),
        )

    if stype not in ("tma", "jonswap"):
        return None
    freq = spec.get("freq", {})
    disc = spec.get("discretization", {})
    eqe = str(disc.get("equal_energy", "none")).lower()
    return CaseSpec(
        stype=stype,
        hm0=float(spec["hm0"]),
        fp=float(freq["peak"]),
        fmin=float(freq["min"]),
        fmax=float(freq["max"]),
        nfreq=int(disc.get("freq_bins", 45)),
        uniform_ladder=eqe not in ("freq", "both", "true", "yes"),
    )


def _load_gauges(output_dir: Path) -> tuple[np.ndarray, np.ndarray] | None:
    """Point-channel eta series -> (t, eta[n_t, n_gauge]).

    Current writer layout is one directory per channel (<channel>/eta.dat with
    t, eta(1..n) per row); the flat <name>_eta.dat spelling is kept as a
    fallback for older run dirs.
    """
    sta_files = sorted(output_dir.glob("*/eta.dat")) or sorted(output_dir.glob("*_eta.dat"))
    if not sta_files:
        return None
    sta = np.loadtxt(sta_files[0])
    if sta.ndim == 1 or sta.shape[0] < 16:
        return None
    t = sta[:, 0]
    keep = np.concatenate([[True], np.diff(t) > 0.0])  # drop any pad rows
    return t[keep], sta[keep, 1:]


# ---------------------------------------------------------------------------
# Spectral estimators (numpy-only)
# ---------------------------------------------------------------------------


def _welch_psd(x: np.ndarray, dt: float, nseg: int = 1024) -> tuple[np.ndarray, np.ndarray]:
    """Hann-windowed, 50 %-overlap segment-averaged periodogram."""
    nseg = min(nseg, len(x))
    win = np.hanning(nseg)
    step = nseg // 2
    psd = np.zeros(nseg // 2 + 1)
    count = 0
    for start in range(0, len(x) - nseg + 1, step):
        seg = x[start : start + nseg]
        seg = (seg - seg.mean()) * win
        psd += np.abs(np.fft.rfft(seg)) ** 2
        count += 1
    return np.fft.rfftfreq(nseg, d=dt), psd / max(count, 1)


def _line_powers(x: np.ndarray, dt: float, lines: np.ndarray) -> np.ndarray:
    """Exact-bin ladder line powers.

    The window is an exact multiple of the ladder recurrence period, so each
    ladder line falls on an rFFT bin.
    """
    freqs = np.fft.rfftfreq(len(x), d=dt)
    spec = np.abs(np.fft.rfft(x - x.mean())) ** 2
    idx = np.rint(lines / (freqs[1] - freqs[0])).astype(int)
    return spec[idx]


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    """Oracle entry point (ref_dir is None in oracle mode and unused)."""
    dev_dir = Path(dev_dir)
    check_keys(tolerances, ACCEPTED_KEYS, "wavemaker_spectrum")
    label = "Wavemaker Spectrum"
    output_dir = read_run_metadata(dev_dir).output_dir

    case = _read_case(dev_dir)
    if case is None:
        _console.print("[yellow]wavemaker_spectrum:[/yellow] spectra-type wavemaker not found — skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    gauges = _load_gauges(output_dir)
    if gauges is None:
        _console.print("[yellow]wavemaker_spectrum:[/yellow] gauge records missing/short — skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    t_raw, eta_raw = gauges

    # stats window: the record's last T_rec (spin-up = everything before it);
    # anchoring at the end keeps the window exact under deck total_time changes
    t_rec = (case.nfreq - 1) / (case.fmax - case.fmin)
    n_win = int(round(t_rec / DT_SAMPLE))
    t_start = DT_SAMPLE * math.floor((t_raw[-1] - t_rec) / DT_SAMPLE)
    t_uni = t_start + DT_SAMPLE * np.arange(n_win)
    if t_start < 0.0 or t_raw[-1] < t_uni[-1]:
        _console.print("[yellow]wavemaker_spectrum:[/yellow] record ends before the stats window — skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    eta = np.stack([np.interp(t_uni, t_raw, eta_raw[:, g]) for g in range(eta_raw.shape[1])], axis=1)
    eta -= eta.mean(axis=0)

    # ── realized Hm0: variance-based, averaged over the x-line ────────────
    hm0_g = 4.0 * np.sqrt(np.mean(eta**2, axis=0))
    hm0_mean = float(np.mean(hm0_g[:N_XLINE]))
    # a boundary feed injects one-way, so it delivers ~2x the internal-source
    # calibration (which splits energy both ways); hm0_target_scale (default 1)
    # carries that documented factor
    hm0_target = case.hm0 * float(tolerances.get("hm0_target_scale", 1.0))
    hm0_err = abs(hm0_mean - hm0_target) / hm0_target * 100.0
    # lateral pair vs the x-line center gauge (homogeneity diagnostic)
    lat = np.append(hm0_g[N_XLINE:], hm0_g[N_XLINE // 2])
    lat_spread = float((lat.max() - lat.min()) / lat.mean() * 100.0) if len(lat) > 1 else 0.0

    # ── Welch peak + band containment (x-line-mean spectra) ───────────────
    psd_sum = None
    for g in range(N_XLINE):
        f_w, psd = _welch_psd(eta[:, g], DT_SAMPLE)
        psd_sum = psd if psd_sum is None else psd_sum + psd
    psd_sum[0] = 0.0
    fp_meas = float(f_w[int(np.argmax(psd_sum))])
    fp_err = abs(fp_meas - case.fp) / case.fp * 100.0 if case.fp else float("nan")

    freqs_full = np.fft.rfftfreq(n_win, d=DT_SAMPLE)
    pow_full = np.zeros(len(freqs_full))
    for g in range(N_XLINE):
        pow_full += np.abs(np.fft.rfft(eta[:, g])) ** 2
    df_bin = freqs_full[1]
    in_band = (freqs_full >= case.fmin - 0.5 * df_bin) & (freqs_full <= case.fmax + 0.5 * df_bin)
    band_frac = float(pow_full[in_band].sum() / pow_full.sum() * 100.0)

    # ── line completeness on the equal-df comb (center x-line gauge) ──────
    dead_pct = float("nan")
    if case.uniform_ladder:
        lines = np.linspace(case.fmin, case.fmax, case.nfreq)
        p_line = _line_powers(eta[:, N_XLINE // 2], DT_SAMPLE, lines)
        dead_pct = float(np.sum(p_line < DEAD_LINE_POWER_FRAC * np.median(p_line)) / len(lines) * 100.0)

    # ── metrics: only the finite-tolerance ones gate ──────────────────────
    def tol(key: str) -> float:
        return float(tolerances[key]) if key in tolerances else math.inf

    hm0_tol, fp_tol, band_tol, dead_tol = (tol(k) for k in ("hm0_err_pct", "fp_err_pct", "band_frac_pct", "dead_line_pct"))
    hm0_pass = hm0_err < hm0_tol
    fp_pass = (not math.isfinite(fp_tol)) or (math.isfinite(fp_err) and fp_err < fp_tol)
    band_pass = band_frac > band_tol if math.isfinite(band_tol) else True  # floor gate
    dead_pass = (not math.isfinite(dead_tol)) or (math.isfinite(dead_pct) and dead_pct < dead_tol)

    metrics = [
        MetricResult("wavemaker_spectrum", "hm0_target_m", hm0_target, True, math.inf),
        MetricResult("wavemaker_spectrum", "hm0_measured_m", hm0_mean, True, math.inf),
        MetricResult("wavemaker_spectrum", "lateral_spread_pct", lat_spread, True, math.inf),
        MetricResult("wavemaker_spectrum", "fp_measured_hz", fp_meas, True, math.inf),
        MetricResult("wavemaker_spectrum", "hm0_err_pct", hm0_err, hm0_pass, hm0_tol),
        MetricResult("wavemaker_spectrum", "fp_err_pct", fp_err, fp_pass, fp_tol),
        MetricResult("wavemaker_spectrum", "band_frac_pct", band_frac, band_pass, band_tol),
    ]
    if case.uniform_ladder:
        metrics.append(MetricResult("wavemaker_spectrum", "dead_line_pct", dead_pct, dead_pass, dead_tol))

    all_passed = hm0_pass and fp_pass and band_pass and dead_pass
    if verbose or not all_passed:
        info = [
            ("Spectrum", case.stype),
            ("Hm0 target", f"{hm0_target:.4f} m"),
            ("Hm0 measured", f"{hm0_mean:.4f} m"),
            ("Lateral spread", f"{lat_spread:.2f} %"),
            ("fp measured", f"{fp_meas:.4f} Hz"),
        ]
        gated = [("Hm0 error", hm0_err, hm0_tol, hm0_pass, False)]
        if math.isfinite(fp_err):
            gated.append(("fp offset", fp_err, fp_tol, fp_pass, False))
        gated.append(("Band containment", band_frac, band_tol, band_pass, True))
        if math.isfinite(dead_pct):
            gated.append(("Dead ladder lines", dead_pct, dead_tol, dead_pass, False))
        _print_table(info, gated)

    return SubsectionResult(kind="statistics", label=label, metrics=metrics)


def _print_table(info: list[tuple[str, str]], gated: list[tuple[str, float, float, bool, bool]]) -> None:
    table = Table(
        box=box.SIMPLE_HEAD,
        header_style="bold cyan",
        show_edge=False,
        pad_edge=True,
        title="[bold]Wavemaker Spectrum Self-Consistency[/bold]",
        title_justify="left",
    )
    table.add_column("Metric", min_width=22)
    table.add_column("Value", justify="right", min_width=14)
    table.add_column("Tolerance", justify="right", min_width=12)
    table.add_column("", min_width=10)

    for name, value in info:
        table.add_row(name, value, "—", "")
    for name, value, tol_val, passed, floor in gated:
        if not math.isfinite(tol_val):
            table.add_row(name, f"{value:.2f} %", "—", "")
            continue
        status = "[bold green]✓ PASS[/bold green]" if passed else "[bold red]✗ FAIL[/bold red]"
        bound = f"> {tol_val:.1f} %" if floor else f"{tol_val:.1f} %"
        color = "green" if passed else "red"
        table.add_row(name, f"[{color}]{value:.2f} %[/{color}]", bound, status)

    _console.print()
    _console.print(table)
    _console.print()
