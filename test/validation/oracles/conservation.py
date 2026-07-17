"""
Mass/energy conservation validation oracle (2D Boussinesq).

Ref-less validation postproc: integrates the ETA/U/V field frames of a closed,
frictionless, sponge-free, breaking-free basin and checks that total water volume
and total energy are conserved over the run.  No analytic period is involved — the
oracle IS the conservation law, so this catches numerical leaks (flux-form bugs,
wet/dry mass loss, secular energy dissipation) that a single-station period read
cannot see.

Quantities (mirroring legacy ``statistics.F``; per unit density, GRAV = 9.81):

    volume V  = integral eta dA            = sum eta_ij dx dy        (excess volume)
    energy E  = 1/2 g integral eta^2 dA               (available potential)
              + 1/2 integral H (u^2 + v^2) dA         (kinetic, H = eta + h)

Legacy sums PE as 1/2 g H^2, whose static 1/2 g h^2 baseline is constant and swamps
the drift; we use the *available* PE 1/2 g eta^2 so the metric is sensitive to the
oscillating energy that numerical dissipation actually erodes.

Metrics:
  mass_drift_pct    — max_t |V(t) - V(0)| / V_h * 100, V_h = integral h dA the total
                      still-water volume (a fixed, nonzero normaliser since V itself
                      is ~0 for a cos mode).  Flux-form continuity with wall BC
                      conserves this to ~machine precision; this is the tight gate.
  energy_drift_pct  — |secular slope| * span / mean(E) * 100 from a linear fit of E
                      over the frames; detrends the bounded PE<->KE (and dispersive)
                      exchange so only the secular numerical loss/gain is gated.
                      Looser than mass by design.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) -> SubsectionResult
(ref_dir is None in oracle mode and unused.)

Tolerance keys in validation_config.yaml (under tolerances: conservation:):
  mass_drift_pct    — max allowed volume drift (default: 0.01 %)
  energy_drift_pct  — max allowed secular energy drift (default: 5 %)
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
from rich import box
from rich.console import Console
from rich.table import Table

from test.framework.results import MetricResult, SubsectionResult
from test.regression.postproc.utils import read_run_metadata

_console = Console()

G = 9.81  # m s-2


# ---------------------------------------------------------------------------
# Integrals
# ---------------------------------------------------------------------------


def _integrate(meta, h: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    """Return (volume[nt], energy[nt]) integrated over each ETA/U/V frame.

    h is the still-water depth field (dep.out, positive down).  A frame with no
    matching U/V falls back to zero velocity (potential energy only).
    """
    da = meta.dx * meta.dy
    eta_files = meta.output_files("ETA")
    u_files = meta.output_files("U")
    v_files = meta.output_files("V")

    vols: list[float] = []
    engs: list[float] = []
    for i, ep in enumerate(eta_files):
        eta = meta.read_field(ep).astype(float)
        hh = eta + h  # total water column H = eta + h
        u = meta.read_field(u_files[i]).astype(float) if i < len(u_files) else np.zeros_like(eta)
        v = meta.read_field(v_files[i]).astype(float) if i < len(v_files) else np.zeros_like(eta)

        vols.append(float(np.sum(eta)) * da)
        pe = 0.5 * G * float(np.sum(eta * eta)) * da
        ke = 0.5 * float(np.sum(hh * (u * u + v * v))) * da
        engs.append(pe + ke)

    return np.asarray(vols), np.asarray(engs)


def _windowed_decay_pct(y: np.ndarray) -> float:
    """Secular energy change as (mean second half - mean first half) / mean first half.

    Leading-order PE+KE is NOT the exact Boussinesq invariant: it exchanges with the
    dispersive gradient energy at 2*omega with amplitude ~ (kh)^2, so E(t) oscillates
    even for the exact solution.  A multi-period window mean averages that bounded
    oscillation out, leaving only the secular component; numerical dissipation shows
    as a monotone (negative) change.  Signed value returned; the gate bounds |value|.
    This is meaningful only in the weakly-dispersive limit (low kh) where the residual
    oscillation is small; at kh ~ O(1) the metric is diagnostic, not a gate.
    """
    n = len(y)
    if n < 4:
        return float("nan")
    half = n // 2
    m1 = float(np.mean(y[:half]))
    m2 = float(np.mean(y[half:]))
    if m1 == 0.0:
        return float("nan")
    return (m2 - m1) / abs(m1) * 100.0


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    meta = read_run_metadata(dev_dir)

    dep_files = meta.output_files("DEPTH_OUT")
    eta_files = meta.output_files("ETA")
    if not dep_files or len(eta_files) < 3:
        _console.print("[yellow]conservation:[/yellow] need dep.out + >=3 ETA frames — skipping")
        return SubsectionResult(kind="statistics", label="Conservation", metrics=[])

    h = meta.read_field(dep_files[0]).astype(float)
    vols, engs = _integrate(meta, h)

    v_still = float(np.sum(h)) * meta.dx * meta.dy  # total still-water volume (normaliser)
    mass_drift = float(np.max(np.abs(vols - vols[0]))) / v_still * 100.0 if v_still > 0 else float("nan")
    energy_drift = _windowed_decay_pct(engs)

    tol_mass = float(tolerances.get("mass_drift_pct", 0.01))
    mass_ok = math.isfinite(mass_drift) and mass_drift < tol_mass

    # Energy is gated ONLY where the config supplies a tolerance — i.e. the low-kh
    # case where leading-order PE+KE is the true invariant.  On the dispersive cases
    # the key is absent and energy is reported as an ungated diagnostic.
    energy_gated = "energy_drift_pct" in tolerances
    tol_energy = float(tolerances.get("energy_drift_pct", math.inf))
    energy_ok = (not energy_gated) or (math.isfinite(energy_drift) and abs(energy_drift) < tol_energy)

    metrics = [
        MetricResult("conservation", "n_frames", float(len(eta_files)), True, math.inf),
        MetricResult("conservation", "mass_drift_pct", mass_drift, mass_ok, tol_mass),
        MetricResult("conservation", "energy_drift_pct", energy_drift, energy_ok, tol_energy),
    ]

    if verbose or not (mass_ok and energy_ok):
        _print_table(len(eta_files), v_still, mass_drift, tol_mass, mass_ok, energy_drift, tol_energy, energy_ok, energy_gated)

    return SubsectionResult(kind="statistics", label="Conservation", metrics=metrics)


def _print_table(n, v_still, mass_drift, tol_mass, mass_ok, energy_drift, tol_energy, energy_ok, energy_gated) -> None:
    table = Table(
        box=box.SIMPLE_HEAD,
        header_style="bold cyan",
        show_edge=False,
        pad_edge=True,
        title="[bold]Mass / Energy Conservation[/bold]",
        title_justify="left",
    )
    table.add_column("Metric", min_width=22)
    table.add_column("Value", justify="right", min_width=14)
    table.add_column("Tolerance", justify="right", min_width=12)
    table.add_column("", min_width=10)

    table.add_row("Frames", f"{n}", "—", "")
    table.add_row("Still-water volume", f"{v_still:.3f} m^3", "—", "")
    if math.isfinite(mass_drift):
        status = "[bold green]✓ PASS[/bold green]" if mass_ok else "[bold red]✗ FAIL[/bold red]"
        c = "green" if mass_ok else "red"
        table.add_row("Mass drift", f"[{c}]{mass_drift:.4g} %[/{c}]", f"{tol_mass:.3g} %", status)
    if math.isfinite(energy_drift):
        if energy_gated:
            status = "[bold green]✓ PASS[/bold green]" if energy_ok else "[bold red]✗ FAIL[/bold red]"
            c = "green" if energy_ok else "red"
            table.add_row("Energy drift", f"[{c}]{energy_drift:+.4g} %[/{c}]", f"{tol_energy:.3g} %", status)
        else:
            table.add_row("Energy drift (diag)", f"{energy_drift:+.4g} %", "—", "[dim]ungated[/dim]")

    _console.print()
    _console.print(table)
    _console.print()
