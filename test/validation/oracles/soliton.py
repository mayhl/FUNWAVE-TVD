"""
Solitary-wave permanence validation oracle (2D Boussinesq, periodic-x).

Ref-less validation postproc: a solitary wave on a flat periodic-x flume must
propagate with permanent form — after the initial-profile adjustment sheds, a
correct dispersive/nonlinear balance keeps the crest amplitude, shape, and the
analytic celerity

    c = sqrt(g * (h + a))

indefinitely; numerical dissipation or a broken dispersive term shows as
amplitude decay, shape distortion, or a celerity bias.  The oracle tracks the
crest through the ETA field frames (frame times from time_dt.out):

  celerity_error_pct   — |c_fit - c| / c from a linear fit of the unwrapped
                         sub-cell crest trajectory, first transit excluded
                         (the adjustment transient rides there).
  amplitude_decay_pct  — |mean peak over the last transit / mean peak over the
                         second transit - 1|; permanence means ~0.
  shape_error_pct      — L2 difference between the final frame's crest profile
                         and the profile exactly one transit earlier, aligned
                         by a Fourier (sub-cell) shift, normalised by a.

Note 1: the analytic c uses the DECK amplitude; the model's own solitary form
settles ~1-2 % higher in peak, a bounded O((a/h)^2) family difference that the
transit-relative metrics are insensitive to.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) ->
SubsectionResult (ref_dir is None in oracle mode and unused).

Tolerance keys in validation_config.yaml (under tolerances: soliton:):
  celerity_error_pct    (default 3 %)
  amplitude_decay_pct   (default 2 %)
  shape_error_pct       (default 5 %)
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


def _deck_params(run_dir: Path) -> tuple[float, float, float]:
    """Return (amplitude, depth, angle_deg) from the deck's initial.solitary block."""
    deck = sorted(run_dir.glob("*.yaml"))[0]
    with open(deck) as f:
        cfg = yaml.safe_load(f)
    sol = cfg.get("initial", {}).get("solitary", {})
    return float(sol["amplitude"]), float(sol["depth"]), float(sol.get("angle", 0.0))


def _frame_times(run_dir: Path, n: int) -> np.ndarray:
    """Frame times from time_dt.out (one 't dt' row per fired field frame)."""
    rows = np.atleast_2d(np.loadtxt(run_dir / "time_dt.out"))
    return rows[:n, 0]


def _crest(row: np.ndarray, dx: float) -> tuple[float, float]:
    """Sub-cell crest (position, height) via a parabolic fit around the argmax.

    The parabola uses periodic neighbours, so a crest sitting on the wrap
    seam still refines cleanly.
    """
    n = len(row)
    i = int(np.argmax(row))
    ym, y0, yp = row[(i - 1) % n], row[i], row[(i + 1) % n]
    denom = ym - 2.0 * y0 + yp
    frac = 0.0 if denom == 0.0 else 0.5 * (ym - yp) / denom
    height = y0 - 0.25 * (ym - yp) * frac
    return (i + frac) * dx, float(height)


def _unwrap(xs: np.ndarray, span: float) -> np.ndarray:
    """Unwrap a periodic trajectory (monotone propagation assumed)."""
    out = xs.copy()
    for k in range(1, len(out)):
        while out[k] < out[k - 1] - 0.5 * span:
            out[k] += span
    return out


def _fourier_shift(row: np.ndarray, shift_cells: float) -> np.ndarray:
    """Circularly shift a periodic profile by a (fractional) cell count."""
    n = len(row)
    k = np.fft.fftfreq(n) * 2.0 * np.pi
    return np.real(np.fft.ifft(np.fft.fft(row) * np.exp(-1j * k * shift_cells)))


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    meta = read_run_metadata(dev_dir)

    eta_files = meta.output_files("ETA")
    if len(eta_files) < 10:
        _console.print("[yellow]soliton:[/yellow] need >=10 ETA frames — skipping")
        return SubsectionResult(kind="statistics", label="Soliton", metrics=[])

    a, h, angle = _deck_params(dev_dir)
    span = meta.nx * meta.dx
    # oblique tiled train: a fixed row sees the crest cross at the TRACE
    # speed c / cos(theta); angle = 0 reduces to the straight flume
    c_ana = math.sqrt(G * (h + a)) / math.cos(math.radians(angle))
    t_transit = span / c_ana
    times = _frame_times(dev_dir, len(eta_files))

    # crest trajectory from one interior row (pseudo-1D case: rows identical)
    mid = meta.ny // 2
    xs, amps, rows = [], [], []
    for p in eta_files:
        row = meta.read_field(p).astype(float)[mid]
        x, amp = _crest(row, meta.dx)
        xs.append(x)
        amps.append(amp)
        rows.append(row)
    xs = _unwrap(np.asarray(xs), span)
    amps = np.asarray(amps)

    # celerity: linear fit past the first transit (adjustment transient)
    fit_mask = times > t_transit
    coeff = np.polyfit(times[fit_mask], xs[fit_mask], 1)
    c_fit = float(coeff[0])
    celerity_error = abs(c_fit - c_ana) / c_ana * 100.0

    # amplitude decay: mean crest height, last transit vs second transit
    second = (times > t_transit) & (times <= 2.0 * t_transit)
    last = times > times[-1] - t_transit
    amp_2nd = float(np.mean(amps[second]))
    amp_last = float(np.mean(amps[last]))
    amplitude_decay = abs(amp_last / amp_2nd - 1.0) * 100.0

    # shape: final frame vs one transit earlier, Fourier-aligned by the
    # fitted crest displacement, L2 normalised by the deck amplitude
    k_ref = int(np.argmin(np.abs(times - (times[-1] - t_transit))))
    shift_cells = (xs[-1] - xs[k_ref]) / meta.dx
    aligned = _fourier_shift(rows[k_ref], -shift_cells)
    shape_error = float(np.sqrt(np.mean((rows[-1] - aligned) ** 2))) / a * 100.0

    tol_c = float(tolerances.get("celerity_error_pct", 3.0))
    tol_a = float(tolerances.get("amplitude_decay_pct", 2.0))
    tol_s = float(tolerances.get("shape_error_pct", 5.0))
    c_ok = math.isfinite(celerity_error) and celerity_error < tol_c
    a_ok = math.isfinite(amplitude_decay) and amplitude_decay < tol_a
    s_ok = math.isfinite(shape_error) and shape_error < tol_s

    metrics = [
        MetricResult("soliton", "n_frames", float(len(eta_files)), True, math.inf),
        MetricResult("soliton", "celerity_error_pct", celerity_error, c_ok, tol_c),
        MetricResult("soliton", "amplitude_decay_pct", amplitude_decay, a_ok, tol_a),
        MetricResult("soliton", "shape_error_pct", shape_error, s_ok, tol_s),
    ]

    if verbose or not (c_ok and a_ok and s_ok):
        _print_table(len(eta_files), c_ana, c_fit, celerity_error, tol_c, c_ok,
                     amp_2nd, amp_last, amplitude_decay, tol_a, a_ok,
                     shape_error, tol_s, s_ok)

    return SubsectionResult(kind="statistics", label="Soliton", metrics=metrics)


def _print_table(n, c_ana, c_fit, c_err, tol_c, c_ok, amp_2nd, amp_last,
                 a_dec, tol_a, a_ok, s_err, tol_s, s_ok) -> None:
    table = Table(
        box=box.SIMPLE_HEAD,
        header_style="bold cyan",
        show_edge=False,
        pad_edge=True,
        title="[bold]Solitary-Wave Permanence[/bold]",
        title_justify="left",
    )
    table.add_column("Metric", min_width=22)
    table.add_column("Value", justify="right", min_width=16)
    table.add_column("Tolerance", justify="right", min_width=12)
    table.add_column("", min_width=10)

    def _row(label, value, tol, ok):
        status = "[bold green]✓ PASS[/bold green]" if ok else "[bold red]✗ FAIL[/bold red]"
        c = "green" if ok else "red"
        table.add_row(label, f"[{c}]{value:.4g} %[/{c}]", f"{tol:.3g} %", status)

    table.add_row("Frames", f"{n}", "—", "")
    table.add_row("Celerity (analytic)", f"{c_ana:.4f} m/s", "—", "")
    table.add_row("Celerity (fit)", f"{c_fit:.4f} m/s", "—", "")
    _row("Celerity error", c_err, tol_c, c_ok)
    table.add_row("Crest amp 2nd/last", f"{amp_2nd:.5f} / {amp_last:.5f} m", "—", "")
    _row("Amplitude decay", a_dec, tol_a, a_ok)
    _row("Shape error (L2)", s_err, tol_s, s_ok)
    _console.print(table)
