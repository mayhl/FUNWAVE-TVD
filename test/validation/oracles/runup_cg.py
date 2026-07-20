"""
Periodic-wave runup validation oracle (Carrier-Greenspan/Keller amplification).

Ref-less validation postproc: a monochromatic wave over the flat section shoals
onto the plane beach and forms a standing swash oscillation; for nonbreaking
waves the maximum vertical runup equals the LINEAR shallow-water amplification
of the incident amplitude (Carrier & Greenspan 1958 — the nonlinear NSWE
maximum runup coincides with the linear prediction).

To avoid Bessel-convention mistakes we do not evaluate the classical plane-
beach formula; instead the oracle integrates the linear SWE amplification
numerically over the ACTUAL model depth profile (dep.out).  In flux form,

    (g h(x) eta')' + w^2 eta = 0,      phi = g h eta',

integrated seaward by RK4 from a series start at the shoreline (the regular
J0-like branch, eta(0) = 1) to a matching point on the flat, where the
decomposition eta = A+ e^{ikx} + A- e^{-ikx} gives the normalized incident
amplitude A_n and hence the amplification 2 / (2 A_n) = 1/A_n.

The incident amplitude is measured by lock-in demodulation at the forcing
frequency over the steady window (an integer number of periods): the complex
first-harmonic field eta1(x) = (2/N) sum eta e^{i w t} rejects bound
harmonics and setup, and the directional split on the flat

    A+ = |eta1 + eta1'/(ik)| / 2,      A- = |eta1 - eta1'/(ik)| / 2

(central-difference eta1', median over the flat window) gives the incident
and reflected amplitudes exactly — no node/antinode envelope-width
requirement.  The runup is the peak front-most-wet-cell elevation (same MASK
machinery as the Synolakis oracle) over the last 5 periods.

Note 1: measurement granularity at the shoreline is ~ dx * slope (front-cell
elevation), a few % of R at this deck's resolution; the linear-vs-Boussinesq
dispersive correction at kh ~ 0.2 is < 1 %.  The gate is a physical-tolerance
tripwire, not a discretization study.

Deck params read from the run YAML: wavemaker.spectrum {period}, source
{x_center} and grid.bathymetry {depth, slope, x0}.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) ->
SubsectionResult (ref_dir is None in oracle mode and unused).

Tolerance keys in validation_config.yaml (under tolerances: runup_cg:):
  runup_amp_error_pct   (default 15 %)
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

G = 9.81


def _deck_params(run_dir: Path) -> tuple[float, float, float, float, float]:
    """Return (T, h0, slope, toe_x, src_x) from the run deck."""
    deck = sorted(run_dir.glob("*.yaml"))[0]
    with open(deck) as f:
        cfg = yaml.safe_load(f)
    bathy = cfg["grid"]["bathymetry"]
    wm = cfg["wavemaker"]
    if isinstance(wm, list):
        wm = wm[0]
    return (float(wm["spectrum"]["period"]), float(bathy["depth"]),
            float(bathy["slope"]), float(bathy["x0"]),
            float(wm["source"]["x_center"]))


def _frame_times(run_dir: Path, n: int) -> np.ndarray:
    """Frame times from time_dt.out (one 't dt' row per fired field frame)."""
    rows = np.atleast_2d(np.loadtxt(run_dir / "time_dt.out"))
    return rows[:n, 0]


def _linear_amplification(x: np.ndarray, dep: np.ndarray, omega: float,
                          h0: float, x_match: float) -> float:
    """Amplification |eta(shore)| / |A+| by RK4 over the actual depth profile.

    Integrates the flux-form linear SWE seaward from a two-term series start
    just off the interpolated shoreline to x_match on the flat, then splits
    the (real) standing solution into e^{+-ikx} components.
    """
    # interpolated still-water shoreline (last wet -> first dry crossing)
    wet = np.where(dep > 0.0)[0]
    i_s = wet.max()
    x_s = x[i_s] + dep[i_s] / (dep[i_s] - dep[i_s + 1]) * (x[i_s + 1] - x[i_s])

    # local beach slope from the profile itself (mid-slope fit)
    band = (dep > 0.1 * h0) & (dep < 0.9 * h0) & (x > x_match)
    beta = -np.polyfit(x[band], dep[band], 1)[0]

    # series start: eta = sum ((-q)^n / (n!)^2) xp^n, the regular J0 branch
    q = omega**2 / (G * beta)
    eps = 2.0 * (x[1] - x[0])
    eta = 1.0 - q * eps + (q * eps) ** 2 / 4.0 - (q * eps) ** 3 / 36.0
    detadxp = -q + q**2 * eps / 2.0 - q**3 * eps**2 / 12.0

    x_i = x_s - eps  # seaward of the shoreline by eps
    h_i = float(np.interp(x_i, x, dep))
    phi = -G * h_i * detadxp  # d/dx = -d/dxp (xp measured shoreward)

    def rhs(xx: float, y: np.ndarray) -> np.ndarray:
        h = max(float(np.interp(xx, x, dep)), 1e-6)
        return np.array([y[1] / (G * h), -omega**2 * y[0]])

    y = np.array([eta, phi])
    step = -(x[1] - x[0]) / 4.0
    n_steps = int(math.ceil((x_i - x_match) / -step))
    xx = x_i
    for _ in range(n_steps):
        hh = max(step, x_match - xx)  # last partial step lands on x_match
        k1 = rhs(xx, y)
        k2 = rhs(xx + hh / 2, y + hh / 2 * k1)
        k3 = rhs(xx + hh / 2, y + hh / 2 * k2)
        k4 = rhs(xx + hh, y + hh * k3)
        y = y + hh / 6.0 * (k1 + 2 * k2 + 2 * k3 + k4)
        xx += hh

    k = omega / math.sqrt(G * h0)
    eta_m, detadx_m = y[0], y[1] / (G * h0)
    a_norm = 0.5 * math.hypot(eta_m, detadx_m / k)  # |A+| of the eta(0)=1 solution
    return 1.0 / a_norm


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    meta = read_run_metadata(dev_dir)

    dep_files = meta.output_files("DEPTH_OUT")
    mask_files = meta.output_files("MASK")
    eta_files = meta.output_files("ETA")
    if not dep_files or len(mask_files) < 10 or len(eta_files) < 10:
        _console.print("[yellow]runup_cg:[/yellow] need dep.out + MASK + ETA frames — skipping")
        return SubsectionResult(kind="statistics", label="Runup CG", metrics=[])

    period, h0, slope, toe_x, src_x = _deck_params(dev_dir)
    omega = 2.0 * math.pi / period

    mid = meta.ny // 2
    x = (np.arange(meta.nx) + 0.5) * meta.dx
    dep = meta.read_field(dep_files[0]).astype(float)[mid]

    # flat-section measurement window: clear of the source (3 half-widths) and toe
    x_match = toe_x - 5.0
    win = (x > src_x + 12.0) & (x < toe_x - 2.0)

    amp = _linear_amplification(x, dep, omega, h0, x_match)

    # clamp to the time_dt.out row count (the forced final frame can outnumber it)
    times = _frame_times(dev_dir, min(len(eta_files), len(mask_files)))
    n = len(times)
    # steady window: exactly 5 periods of frames (integer periods for the lock-in)
    dt_f = float(np.median(np.diff(times)))
    n_win = min(5 * int(round(period / dt_f)), n)
    steady = np.zeros(n, dtype=bool)
    steady[n - n_win:] = True

    eta1 = np.zeros(meta.nx, dtype=complex)
    r_up, r_down = -np.inf, np.inf
    for i in np.where(steady)[0]:
        e = meta.read_field(eta_files[i]).astype(float)[mid]
        m = meta.read_field(mask_files[i]).astype(int)[mid]
        eta1 += e * np.exp(1j * omega * times[i])
        wet = np.where(m == 1)[0]
        if len(wet):
            r_up = max(r_up, float(e[wet.max()]))
            r_down = min(r_down, float(e[wet.max()]))
    eta1 *= 2.0 / n_win

    # directional split on the flat: eta1 = A+ e^{ikx} + A- e^{-ikx}
    k = omega / math.sqrt(G * h0)
    deta1 = np.gradient(eta1, meta.dx)
    a_plus = 0.5 * np.abs(eta1 + deta1 / (1j * k))
    a_minus = 0.5 * np.abs(eta1 - deta1 / (1j * k))
    a_inc = float(np.median(a_plus[win]))
    refl = float(np.median(a_minus[win])) / a_inc

    r_lin = amp * a_inc
    cg_break = r_lin * omega**2 / (G * slope**2)
    runup_error = abs(r_up / r_lin - 1.0) * 100.0
    tol = float(tolerances.get("runup_amp_error_pct", 15.0))
    ok = math.isfinite(runup_error) and runup_error < tol

    metrics = [
        MetricResult("runup_cg", "n_frames", float(int(steady.sum())), True, math.inf),
        MetricResult("runup_cg", "runup_amp_error_pct", runup_error, ok, tol),
    ]

    if verbose or not ok:
        table = Table(box=box.SIMPLE_HEAD, header_style="bold cyan", show_edge=False,
                      pad_edge=True, title="[bold]Periodic Runup (Carrier-Greenspan)[/bold]",
                      title_justify="left")
        table.add_column("Metric", min_width=22)
        table.add_column("Value", justify="right", min_width=14)
        table.add_column("Tolerance", justify="right", min_width=12)
        table.add_column("", min_width=10)
        table.add_row("Steady frames", f"{int(steady.sum())}", "—", "")
        table.add_row("Incident amp (flat)", f"{a_inc:.5f} m", "—", "")
        table.add_row("Reflection coeff", f"{refl:.3f}", "—", "")
        table.add_row("Amplification (LSWE)", f"{amp:.3f}", "—", "")
        table.add_row("CG breaking param", f"{cg_break:.3f}", "< 1", "")
        table.add_row("Runup (linear)", f"{r_lin:.5f} m", "—", "")
        table.add_row("Runup (model)", f"{r_up:.5f} m", "—", "")
        table.add_row("Rundown (model)", f"{r_down:.5f} m", "—", "")
        status = "[bold green]✓ PASS[/bold green]" if ok else "[bold red]✗ FAIL[/bold red]"
        c = "green" if ok else "red"
        table.add_row("Runup amp error", f"[{c}]{runup_error:.4g} %[/{c}]", f"{tol:.3g} %", status)
        _console.print(table)

    return SubsectionResult(kind="statistics", label="Runup CG", metrics=metrics)
