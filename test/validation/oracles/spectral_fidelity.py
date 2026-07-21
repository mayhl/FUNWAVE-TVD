"""Spectral-wavemaker fidelity oracle (2D Boussinesq).

Ref-less validation postproc: measures Hm0 from station eta variance downwave of
an internal spectral (TMA/JONSWAP) source on a flat bottom and gates it against
the Hm0 the deck asked for.  The wavemaker renormalizes the sampled band to carry
the full target Hm0, so the deck value is the exact analytic target — any deficit
is generation-side (e.g. the source-box width truncating low-frequency Gaussian
tails) or absorption-side (sponge leakage), which the diagnostics separate:

  * ``hm0_error_pct`` (gated) — station-mean Hm0 vs the deck Hm0.
  * ``station_spread_pct`` (diagnostic) — (max-min)/mean Hm0 across the station
    line; a partial standing-wave pattern from east-sponge reflection shows up
    here first, which is why the stations span ~half a peak wavelength.
  * band energy fractions (diagnostic) — measured PSD energy in the low/mid/high
    thirds of [f_min, f_max] vs the analytic spectrum's fractions; low-band
    deficit is the signature of the source-width truncation (parity ledger A7c).

Note 1: the record's first third is discarded (ramp + propagation transient);
Hm0 is variance-based (4 sqrt(m0)) so it is insensitive to the deterministic
phase choice of the ZERO_PHASE parity build.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) -> SubsectionResult
(ref_dir is None in oracle mode and unused.)

Tolerance keys (under tolerances: spectral_fidelity:):
  hm0_error_pct   — max allowed % error of station-mean Hm0 vs the deck value
  band_error_pct  — optional; gates the worst band-fraction error when present
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
import yaml
from rich import box
from rich.console import Console
from rich.table import Table

from test.framework.results import MetricResult, SubsectionResult
from test.regression.postproc.utils import read_run_metadata

_console = Console()

G = 9.81  # m s-2


# ---------------------------------------------------------------------------
# Physics — analytic spectrum (mirrors the Fortran tma_density)
# ---------------------------------------------------------------------------


def _tma_density(f: np.ndarray, fm: float, h: float, gamma: float, is_jonswap: bool) -> np.ndarray:
    """TMA/JONSWAP density; the Kitaigorodskii phi factor drops for JONSWAP."""
    omega_h = 2.0 * math.pi * f * math.sqrt(h / G)
    phi = 1.0 - 0.5 * (2.0 - omega_h) ** 2
    phi = np.where(omega_h <= 1.0, 0.5 * omega_h**2, phi)
    phi = np.where(omega_h >= 2.0, 1.0, phi)
    if is_jonswap:
        phi = np.ones_like(f)
    sigma = np.where(f > fm, 0.09, 0.07)
    return (
        G**2
        * f**-5
        * (2.0 * math.pi) ** -4
        * phi
        * np.exp(-1.25 * (f / fm) ** -4)
        * gamma ** np.exp(-((f / fm - 1.0) ** 2) / (2.0 * sigma**2))
    )


def _band_fractions(f: np.ndarray, dens: np.ndarray, edges: tuple[float, float, float, float]) -> list[float]:
    """Energy fraction in each of the three sub-bands defined by edges."""
    total = np.trapezoid(dens, f)
    fracs = []
    for lo, hi in zip(edges[:-1], edges[1:]):
        m = (f >= lo) & (f < hi)
        fracs.append(float(np.trapezoid(dens[m], f[m]) / total) if m.sum() > 1 else 0.0)
    return fracs


# ---------------------------------------------------------------------------
# I/O
# ---------------------------------------------------------------------------


def _read_case(run_dir: Path) -> tuple[float, float, float, float, float, float, bool] | None:
    """Parse (hm0, fp, fmin, fmax, gamma, depth, is_jonswap) from the run YAML."""
    yaml_files = sorted(run_dir.glob("*.yaml"))
    if not yaml_files:
        return None
    with open(yaml_files[0]) as fh:
        cfg = yaml.safe_load(fh)

    spec = cfg.get("wavemaker", {}).get("spectrum", {})
    freq = spec.get("freq", {})
    if not spec or "hm0" not in spec or "peak" not in freq:
        return None

    bathy = cfg.get("grid", {}).get("bathymetry", {})
    depth = float(cfg.get("wavemaker", {}).get("source", {}).get("depth", bathy.get("depth", 0.0)))

    return (
        float(spec["hm0"]),
        float(freq["peak"]),
        float(freq.get("min", 0.0)),
        float(freq.get("max", 0.0)),
        float(spec.get("gamma", 3.3)),
        depth,
        str(spec.get("type", "tma")).lower().startswith("jon"),
    )


def _find_station_files(output_dir: Path) -> list[Path]:
    import re

    sta_re = re.compile(r"^sta_\d{4}$")
    return sorted(p for p in output_dir.iterdir() if sta_re.match(p.name))


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    output_dir = read_run_metadata(dev_dir).output_dir
    sta_files = _find_station_files(output_dir)
    if not sta_files:
        _console.print("[yellow]spectral_fidelity:[/yellow] no station files found — skipping")
        return SubsectionResult(kind="statistics", label="Spectral Fidelity", metrics=[])

    case = _read_case(dev_dir)
    if case is None:
        _console.print("[yellow]spectral_fidelity:[/yellow] wavemaker spectrum not found — skipping")
        return SubsectionResult(kind="statistics", label="Spectral Fidelity", metrics=[])
    hm0_target, fp, fmin, fmax, gamma, depth, is_jonswap = case

    # per-station variance Hm0 on the stationary window (last 2/3 of the record)
    hm0_sta: list[float] = []
    psd_sum: np.ndarray | None = None
    freq_axis: np.ndarray | None = None
    for p in sta_files:
        sta = np.loadtxt(p)
        if sta.ndim == 1 or sta.shape[0] < 16:
            continue
        t, eta = sta[:, 0], sta[:, 1]
        mask = t >= t[-1] / 3.0
        eta_s = eta[mask] - eta[mask].mean()
        hm0_sta.append(4.0 * float(np.sqrt(np.mean(eta_s**2))))

        dt = float(np.median(np.diff(t[mask])))
        freq_axis = np.fft.rfftfreq(len(eta_s), d=dt)
        psd = np.abs(np.fft.rfft(eta_s)) ** 2
        psd[0] = 0.0
        psd_sum = psd if psd_sum is None else psd_sum + psd

    if not hm0_sta:
        _console.print("[yellow]spectral_fidelity:[/yellow] station records too short — skipping")
        return SubsectionResult(kind="statistics", label="Spectral Fidelity", metrics=[])

    hm0_mean = float(np.mean(hm0_sta))
    spread_pct = (max(hm0_sta) - min(hm0_sta)) / hm0_mean * 100.0
    err_pct = abs(hm0_mean - hm0_target) / hm0_target * 100.0

    # band fractions: measured (station-mean PSD) vs the analytic density,
    # both restricted to the generated band and split into thirds
    edges = (fmin, fmin + (fmax - fmin) / 3.0, fmin + 2.0 * (fmax - fmin) / 3.0, fmax)
    band_meas = _band_fractions(freq_axis, psd_sum, edges)
    f_fine = np.linspace(max(fmin, 1e-4), fmax, 2000)
    band_theo = _band_fractions(f_fine, _tma_density(f_fine, fp, depth, gamma, is_jonswap), edges)
    band_err = max(abs(m - t) / t * 100.0 for m, t in zip(band_meas, band_theo) if t > 0.0)

    tol_pct = float(tolerances.get("hm0_error_pct", 5.0))
    passed = math.isfinite(err_pct) and err_pct < tol_pct
    band_tol = tolerances.get("band_error_pct")
    band_passed = band_err < float(band_tol) if band_tol is not None else True

    metrics = [
        MetricResult("spectral_fidelity", "hm0_target_m", hm0_target, True, math.inf),
        MetricResult("spectral_fidelity", "hm0_measured_m", hm0_mean, True, math.inf),
        MetricResult("spectral_fidelity", "station_spread_pct", spread_pct, True, math.inf),
        MetricResult("spectral_fidelity", "band_low_frac", band_meas[0], True, math.inf),
        MetricResult("spectral_fidelity", "band_low_frac_theory", band_theo[0], True, math.inf),
        MetricResult("spectral_fidelity", "hm0_error_pct", err_pct, passed, tol_pct),
    ]
    if band_tol is not None:
        metrics.append(MetricResult("spectral_fidelity", "band_error_pct", band_err, band_passed, float(band_tol)))

    if verbose or not passed or not band_passed:
        _print_table(hm0_target, hm0_mean, err_pct, tol_pct, passed, spread_pct, band_meas, band_theo, band_err, len(hm0_sta))

    return SubsectionResult(kind="statistics", label="Spectral Fidelity", metrics=metrics)


def _print_table(hm0_target, hm0_mean, err_pct, tol_pct, passed, spread_pct, band_meas, band_theo, band_err, n_sta) -> None:
    table = Table(
        box=box.SIMPLE_HEAD,
        header_style="bold cyan",
        show_edge=False,
        pad_edge=True,
        title="[bold]Spectral Wavemaker Fidelity[/bold]",
        title_justify="left",
    )
    table.add_column("Metric", min_width=22)
    table.add_column("Value", justify="right", min_width=14)
    table.add_column("Tolerance", justify="right", min_width=12)
    table.add_column("", min_width=10)

    status = "[bold green]✓ PASS[/bold green]" if passed else "[bold red]✗ FAIL[/bold red]"
    err_c = "green" if passed else "red"
    table.add_row("Hm0 target", f"{hm0_target:.4f} m", "—", "")
    table.add_row(f"Hm0 measured ({n_sta} sta)", f"{hm0_mean:.4f} m", "—", "")
    table.add_row("Hm0 error", f"[{err_c}]{err_pct:.2f} %[/{err_c}]", f"{tol_pct:.1f} %", status)
    table.add_row("Station spread", f"{spread_pct:.2f} %", "—", "")
    for name, m, t in zip(("low", "mid", "high"), band_meas, band_theo):
        table.add_row(f"Band {name} frac (meas/theo)", f"{m:.3f} / {t:.3f}", "—", "")
    table.add_row("Worst band error", f"{band_err:.1f} %", "—", "")

    _console.print()
    _console.print(table)
    _console.print()
