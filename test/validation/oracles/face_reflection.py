"""Open-face reflection oracle (2D Boussinesq).

Ref-less validation postproc for a radiating face (Flather with a zero
external target, or any absorber put on that face): an interior source sends
ONE coherent wave group at the face and an alongshore station column between
source and face sees it twice -- outgoing, then whatever the face sends back.
The two passages are separated in TIME, so no incident/reflected station
separation is needed: the metric is the energy ratio of the two windows.

Windows come from the deck alone.  The spectrum_2d table gives the band and,
through its phase rows (phase = 360 f T0 in degrees), the group's centre time
T0 at the source (read from the phase slope between adjacent lines); the
linear group velocity at the band edges over the flat depth, the widest table
direction (oblique components cross the fetch at cg cos(theta)) and the
group's envelope half-width (3.5 sigma_t, sigma_t = 1 / (2 pi sigma_f) with
sigma_f the amplitude-weighted band width) bound each passage:

  incident   [T0 - tau + L1/cg_max,  T0 + tau + L1/(cg_min cos)]
  reflected  [incident end,          T0 + tau + (L1 + 2 L2)/(cg_min cos)]

with L1 = source-to-column and L2 = column-to-face.  Energy is the column-mean
eta^2 integrated over the window (pooled across the column, so the periodic
box's alongshore pattern averages out).

Metrics (only kr_pct gates; kr_min_pct turns it into a floor):
  * ``kr_pct``            -- reflection coefficient, 100 sqrt(E_ref / E_inc).
  * ``reflected_pct``     -- energy ratio 100 E_ref / E_inc.
  * ``tail_pct``          -- energy after the reflected window over E_inc; a
                             large value means the windows missed something
                             (a slow return, a west-sponge re-reflection).
  * ``incident_peak_m``   -- largest |eta| on the column in the incident window.
  * ``column_spread_pct`` -- (max - min) / mean of the per-gauge incident energy.
  * window edges (s)      -- t_center_s, incident_end_s, reflected_end_s.

Note 1: the west sponge's residual from the group's westward half arrives
inside the incident window and counts as incident; a second Flather echo
re-reflected by the sponge lands inside the reflected window at ~R times the
sponge's reflection, both far below the gate.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) -> SubsectionResult
(ref_dir is None in oracle mode and unused.)

Tolerance keys (under tolerances: face_reflection:):
  kr_pct     -- max allowed reflection coefficient in percent
  kr_min_pct -- MIN required coefficient (a wall calibration deck: the group
                must come back whole); the one inverted gate here
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
from rich import box
from rich.console import Console
from rich.table import Table

from test.framework.results import MetricResult, SubsectionResult
from test.framework.run_output import read_run_metadata
from test.framework.tolerances import check_keys
from test.validation.oracles import wave_stats
from test.validation.oracles._lab import load_deck

_console = Console()
ACCEPTED_KEYS = ("kr_pct", "kr_min_pct")

G = 9.81
DT_SAMPLE = 0.1
ENVELOPE_HALF_WIDTHS = 3.5  # tau in units of sigma_t


def _read_table(path: Path) -> tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """WK_DATA2D table -> (freqs, dirs, amp[nf, nd], phase_deg[nf, nd])."""
    tok = path.read_text().split()
    nf, nd = int(tok[0]), int(tok[1])
    vals = np.array(tok[3:], dtype=float)
    freqs = vals[:nf]
    dirs = vals[nf : nf + nd]
    amp = vals[nf + nd : nf + nd + nf * nd].reshape(nd, nf).T
    phase = vals[nf + nd + nf * nd : nf + nd + 2 * nf * nd].reshape(nd, nf).T
    keep = np.abs(dirs) < 60.0
    return freqs, dirs[keep], amp[:, keep], phase[:, keep]


def _group_velocity(f: float, h: float) -> float:
    """Linear cg at frequency f over depth h (Newton on the dispersion relation)."""
    w = 2.0 * math.pi * f
    k = w * w / G
    for _ in range(50):
        th = math.tanh(k * h)
        fk = G * k * th - w * w
        dfk = G * th + G * k * h * (1.0 - th * th)
        k -= fk / dfk
    kh = k * h
    return 0.5 * (w / k) * (1.0 + 2.0 * kh / math.sinh(2.0 * kh))


def _windows(deck: dict, run_dir: Path, x_gauge: float) -> tuple[float, float, float, float] | None:
    """(t_center, incident_end, reflected_end, incident_start) from the deck and its table."""
    freqs, dirs, amp, phase = _read_table(run_dir / deck["wavemaker"]["spectrum"]["file"])
    df = float(freqs[1] - freqs[0])
    t0 = float(((phase[1, 0] - phase[0, 0]) % 360.0) / (360.0 * df))
    a = amp[:, 0]
    f_mean = float(np.sum(a * freqs) / np.sum(a))
    sigma_f = float(math.sqrt(np.sum(a * (freqs - f_mean) ** 2) / np.sum(a)))
    tau = ENVELOPE_HALF_WIDTHS / (2.0 * math.pi * sigma_f)

    h = float(deck["grid"]["bathymetry"]["depth"])
    dx = float(deck["grid"]["cell_size"][0])
    x_face = float(deck["grid"]["n_cells"][0]) * dx
    x_src = float(deck["wavemaker"]["source"]["x_center"])
    l1, l2 = x_gauge - x_src, x_face - x_gauge
    if l1 <= 0.0 or l2 <= 0.0:
        return None
    cg_max = _group_velocity(float(freqs.min()), h)
    cg_slow = _group_velocity(float(freqs.max()), h) * math.cos(math.radians(float(np.abs(dirs).max())))
    t_inc_end = t0 + tau + l1 / cg_slow
    t_ref_end = t0 + tau + (l1 + 2.0 * l2) / cg_slow
    return t0, t_inc_end, t_ref_end, t0 - tau + l1 / cg_max


def run(ref_dir, dev_dir, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    """Oracle entry point (ref_dir is None in oracle mode and unused)."""
    dev_dir = Path(dev_dir)
    check_keys(tolerances, ACCEPTED_KEYS, "face_reflection")
    label = "Face Reflection"
    output_dir = read_run_metadata(dev_dir).output_dir
    deck = load_deck(dev_dir)

    stations = [c for c in deck.get("output", {}).get("channels", []) if str(c.get("type", "")).lower() == "station"]
    if not stations or not stations[0].get("x"):
        _console.print("[yellow]face_reflection:[/yellow] no station channel in the deck — skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    win = _windows(deck, dev_dir, float(stations[0]["x"][0]))
    if win is None:
        _console.print("[yellow]face_reflection:[/yellow] column not between source and east face — skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    t0, t_inc_end, t_ref_end, t_inc_start = win

    try:
        t_raw, eta_raw = wave_stats.read_point_channel(output_dir, "eta")
    except FileNotFoundError:
        _console.print("[yellow]face_reflection:[/yellow] station record missing — skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    keep = np.concatenate([[True], np.diff(t_raw) > 0.0])
    t_raw, eta_raw = t_raw[keep], eta_raw[keep]
    if t_raw[-1] < t_ref_end:
        _console.print(
            f"[yellow]face_reflection:[/yellow] record ends at {t_raw[-1]:.0f} s before the reflected window ({t_ref_end:.0f} s) — skipping"
        )
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    t = np.arange(0.0, t_raw[-1], DT_SAMPLE)
    eta = np.stack([np.interp(t, t_raw, eta_raw[:, g]) for g in range(eta_raw.shape[1])], axis=1)

    e2 = eta**2  # [n_t, n_gauge]
    inc = (t >= t_inc_start) & (t < t_inc_end)
    ref = (t >= t_inc_end) & (t < t_ref_end)
    tail = t >= t_ref_end
    e_inc_g = e2[inc].sum(axis=0) * DT_SAMPLE
    e_inc = float(e_inc_g.mean())
    e_ref = float(e2[ref].mean(axis=1).sum() * DT_SAMPLE)
    e_tail = float(e2[tail].mean(axis=1).sum() * DT_SAMPLE) if tail.any() else 0.0
    if e_inc <= 0.0:
        _console.print("[yellow]face_reflection:[/yellow] no incident energy on the column — skipping")
        return SubsectionResult(kind="statistics", label=label, metrics=[])
    refl_pct = 100.0 * e_ref / e_inc
    kr_pct = 100.0 * math.sqrt(e_ref / e_inc)
    tail_pct = 100.0 * e_tail / e_inc
    peak = float(np.abs(eta[inc]).max())
    spread = float((e_inc_g.max() - e_inc_g.min()) / e_inc_g.mean() * 100.0)

    kr_tol = float(tolerances["kr_pct"]) if "kr_pct" in tolerances else math.inf
    kr_floor = float(tolerances["kr_min_pct"]) if "kr_min_pct" in tolerances else -math.inf
    kr_pass = kr_pct < kr_tol and kr_pct > kr_floor
    metrics = [
        MetricResult("face_reflection", "t_center_s", t0, True, math.inf),
        MetricResult("face_reflection", "incident_end_s", t_inc_end, True, math.inf),
        MetricResult("face_reflection", "reflected_end_s", t_ref_end, True, math.inf),
        MetricResult("face_reflection", "incident_peak_m", peak, True, math.inf),
        MetricResult("face_reflection", "column_spread_pct", spread, True, math.inf),
        MetricResult("face_reflection", "reflected_pct", refl_pct, True, math.inf),
        MetricResult("face_reflection", "tail_pct", tail_pct, True, math.inf),
        MetricResult("face_reflection", "kr_pct", kr_pct, kr_pass, kr_floor if math.isfinite(kr_floor) else kr_tol),
    ]
    if verbose or not kr_pass:
        _print_table(
            [
                ("Group centre", f"{t0:.1f} s"),
                ("Incident window", f"{t_inc_start:.0f}-{t_inc_end:.0f} s"),
                ("Reflected window", f"{t_inc_end:.0f}-{t_ref_end:.0f} s"),
                ("Incident peak", f"{peak:.3f} m"),
                ("Column spread", f"{spread:.1f} %"),
                ("Reflected energy", f"{refl_pct:.2f} %"),
                ("Tail energy", f"{tail_pct:.2f} %"),
            ],
            [("Reflection coeff Kr", kr_pct, kr_floor if math.isfinite(kr_floor) else kr_tol, kr_pass, math.isfinite(kr_floor))],
        )
    return SubsectionResult(kind="statistics", label=label, metrics=metrics)


def _print_table(info: list[tuple[str, str]], gated: list[tuple[str, float, float, bool, bool]]) -> None:
    table = Table(
        box=box.SIMPLE_HEAD,
        header_style="bold cyan",
        show_edge=False,
        pad_edge=True,
        title="[bold]Face Reflection[/bold]",
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
