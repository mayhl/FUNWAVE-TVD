"""
Linear-dispersion validation oracle (2D Boussinesq).

Ref-less validation postproc: reads the standing-wave (seiche) station time
series from the dev run, measures its oscillation period, and checks it against
the model's OWN Nwogu linear dispersion relation.  A Boussinesq model reproduces
exact Airy dispersion only approximately, so the oracle is the Nwogu (Padé) form,
not ``omega^2 = g k tanh(kh)`` — Airy is reported alongside purely as a diagnostic.

Setup (INI_SINE): a closed flat basin excited in a standing mode ``(m_x, m_y)``,
``eta = A cos(m_x pi x/Lx) cos(m_y pi y/Ly)``, so ``k_x = m_x pi/Lx``,
``k_y = m_y pi/Ly`` and ``|k| = sqrt(k_x^2 + k_y^2)``.  The fundamental seiche is the
default ``(1, 0)`` (half a wavelength across the domain, ``|k| = pi/Lx``); oblique
modes ``(m_x, m_y)`` with ``m_x^2 + m_y^2`` fixed sample one ``|k|`` across a spread of
propagation angles, which is how the isotropy cases are built.  A standing wave
oscillates at one frequency everywhere, so the period is read at a single near-corner
antinode station.

Nwogu linear dispersion (``beta_ref = z_alpha / h``, ``alpha = beta^2/2 + beta``):

    C^2 / (g h) = [1 - (alpha + 1/3)(kh)^2] / [1 - alpha (kh)^2]
    omega = C k,   T = 2 pi / omega

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) -> SubsectionResult
(ref_dir is None in oracle mode and unused.)

Tolerance key in validation_config.yaml (under tolerances: dispersion:):
  period_error_pct  — max allowed % error vs Nwogu theory (default: 5)
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
BETA_REF_DEFAULT = -0.531  # Nwogu reference level z_alpha/h (registry default)


# ---------------------------------------------------------------------------
# Physics
# ---------------------------------------------------------------------------

def _boussinesq_period(h: float, lam: float, beta_ref: float) -> tuple[float, float]:
    """Return (kh, T) from the Nwogu linear dispersion relation.

    lam is the physical wavelength (= 2 L for the fundamental seiche).
    """
    k = 2.0 * math.pi / lam
    kh = k * h
    alpha = 0.5 * beta_ref * beta_ref + beta_ref
    c2_over_gh = (1.0 - (alpha + 1.0 / 3.0) * kh * kh) / (1.0 - alpha * kh * kh)
    c = math.sqrt(c2_over_gh * G * h)
    return kh, 2.0 * math.pi / (c * k)


def _airy_period(h: float, lam: float) -> float:
    """Exact linear (Airy) period, reported as a diagnostic only."""
    k = 2.0 * math.pi / lam
    sig = math.sqrt(G * k * math.tanh(k * h))
    return 2.0 * math.pi / sig


def _extract_period(sta: np.ndarray, t_start: float) -> float:
    """Wave period from eta (col 1) via upward zero-crossing timing; FFT fallback."""
    t = sta[:, 0]
    eta = sta[:, 1]
    mask = t >= t_start
    if mask.sum() < 4:
        mask = np.ones(len(t), dtype=bool)
    t_s = t[mask]
    eta_s = eta[mask] - eta[mask].mean()

    crossings = []
    for i in range(len(eta_s) - 1):
        if eta_s[i] <= 0.0 and eta_s[i + 1] > 0.0:
            frac = -eta_s[i] / (eta_s[i + 1] - eta_s[i])
            crossings.append(float(t_s[i] + frac * (t_s[i + 1] - t_s[i])))
    if len(crossings) >= 2:
        return float(np.median(np.diff(crossings)))

    dt = float(np.median(np.diff(t_s)))
    freq = np.fft.rfftfreq(len(eta_s), d=dt)
    psd = np.abs(np.fft.rfft(eta_s)) ** 2
    psd[0] = 0.0
    peak = int(np.argmax(psd))
    return float("nan") if freq[peak] == 0 else 1.0 / freq[peak]


# ---------------------------------------------------------------------------
# I/O
# ---------------------------------------------------------------------------

def _read_case(run_dir: Path) -> tuple[float | None, float, float, float, int, int]:
    """Parse (h, Lx, Ly, beta_ref, m_x, m_y) from the run YAML (Lx = Mglob*dx).

    The INI_SINE mode numbers default to the fundamental seiche (1, 0) when absent,
    so a case with no ``mode_x/mode_y`` keys reduces to the 1D standing wave.
    """
    yaml_files = sorted(run_dir.glob("*.yaml"))
    if not yaml_files:
        return None, 0.0, 0.0, BETA_REF_DEFAULT, 1, 0
    with open(yaml_files[0]) as fh:
        cfg = yaml.safe_load(fh)

    geo = cfg.get("geometry", {})
    gs = geo.get("grid_size", [1, 1])
    cs = geo.get("cell_size", [1.0, 1.0])
    lx = float(gs[0]) * float(cs[0])
    ly = float(gs[1]) * float(cs[1])

    bathy = geo.get("bathymetry", {})
    h: float | None = None
    if bathy.get("type") == "flat":
        h = float(bathy.get("depth"))
    elif bathy.get("file"):
        p = run_dir / bathy["file"]
        if p.exists():
            h = float(np.median(np.loadtxt(p).ravel()))

    wm = cfg.get("wavemaker", {})
    mode_x = int(wm.get("mode_x", 1))
    mode_y = int(wm.get("mode_y", 0))

    beta_ref = float(cfg.get("physics", {}).get("beta_ref", BETA_REF_DEFAULT))
    return h, lx, ly, beta_ref, mode_x, mode_y


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
        _console.print("[yellow]dispersion:[/yellow] no station files found — skipping")
        return SubsectionResult(kind="statistics", label="Linear Dispersion", metrics=[])

    h, lx, ly, beta_ref, mode_x, mode_y = _read_case(dev_dir)
    if h is None or lx <= 0.0:
        _console.print("[yellow]dispersion:[/yellow] depth/domain not found — skipping")
        return SubsectionResult(kind="statistics", label="Linear Dispersion", metrics=[])

    # Oblique standing mode (m_x, m_y): k_x = m_x pi/Lx, k_y = m_y pi/Ly, so half a
    # wavelength spans the basin along each axis.  The fundamental seiche is the
    # default (1, 0) -> |k| = pi/Lx -> wavelength = 2 Lx (the 1D case unchanged).
    kx = mode_x * math.pi / lx
    ky = mode_y * math.pi / ly if ly > 0.0 else 0.0
    kmag = math.hypot(kx, ky)
    lam = 2.0 * math.pi / kmag
    kh, T_bous = _boussinesq_period(h, lam, beta_ref)
    T_airy = _airy_period(h, lam)

    sta = np.loadtxt(sta_files[0])
    if sta.ndim == 1:
        sta = sta.reshape(1, -1)
    t_start = max(0.0, float(sta[-1, 0]) * 0.2)  # skip the first 20% as start-up
    T_meas = _extract_period(sta, t_start=t_start)

    err_pct = abs(T_meas - T_bous) / T_bous * 100.0 if math.isfinite(T_meas) else float("nan")
    tol_pct = float(tolerances.get("period_error_pct", 5.0))
    passed = math.isfinite(err_pct) and err_pct < tol_pct

    metrics = [
        MetricResult("dispersion", "kh", kh, True, math.inf),
        MetricResult("dispersion", "T_measured_s", T_meas, True, math.inf),
        MetricResult("dispersion", "T_nwogu_s", T_bous, True, math.inf),
        MetricResult("dispersion", "T_airy_s", T_airy, True, math.inf),
        MetricResult("dispersion", "period_err_pct", err_pct, passed, tol_pct),
    ]

    if verbose or not passed:
        _print_table(h, lx, kh, beta_ref, T_bous, T_airy, T_meas, err_pct, tol_pct, passed)

    return SubsectionResult(kind="statistics", label="Linear Dispersion", metrics=metrics)


def _print_table(h, lx, kh, beta_ref, T_bous, T_airy, T_meas, err_pct, tol_pct, passed) -> None:
    table = Table(box=box.SIMPLE_HEAD, header_style="bold cyan", show_edge=False,
                  pad_edge=True, title="[bold]Linear Dispersion (Nwogu)[/bold]", title_justify="left")
    table.add_column("Metric", min_width=22)
    table.add_column("Value", justify="right", min_width=14)
    table.add_column("Tolerance", justify="right", min_width=12)
    table.add_column("", min_width=10)

    table.add_row("Depth  h", f"{h:.2f} m", "—", "")
    table.add_row("Basin  L", f"{lx:.2f} m", "—", "")
    table.add_row("beta_ref", f"{beta_ref:.3f}", "—", "")
    table.add_row("kh", f"{kh:.4f}", "—", "")
    table.add_row("T  (Nwogu)", f"{T_bous:.4f} s", "—", "")
    table.add_row("T  (Airy, diag)", f"{T_airy:.4f} s", "—", "")
    table.add_row("T  (measured)", f"{T_meas:.4f} s", "—", "")
    if math.isfinite(err_pct):
        status = "[bold green]✓ PASS[/bold green]" if passed else "[bold red]✗ FAIL[/bold red]"
        err_c = "green" if passed else "red"
        table.add_row("Period error", f"[{err_c}]{err_pct:.2f} %[/{err_c}]", f"{tol_pct:.1f} %", status)

    _console.print()
    _console.print(table)
    _console.print()
