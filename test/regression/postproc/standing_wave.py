"""
Standing-wave physics postprocessor for the regression framework.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) -> SubsectionResult

Reads station files from the dev run output directory, extracts wave period via
FFT, computes exact linear dispersion theory metrics, and returns a
SubsectionResult with MetricResult objects and Plotly/matplotlib figures.

Station file format (full_dispersion 3D model):
  col 0     : time (s)
  col 1     : eta  (m)
  col 2..K+1: u at vertical layers 1..K
  col K+2.. : v, w at each layer (not used here)

Tolerance key in regression_config.yaml  (under tolerances: standing_wave:):
  period_error_pct  — max allowed % error vs linear theory (default: 15)
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
import plotly.graph_objects as go
from rich import box
from rich.console import Console
from rich.table import Table
import yaml

from test.framework.results import (
    FigureSpec,
    InteractiveFigure,
    MetricResult,
    SubsectionResult,
)
from test.regression.postproc.utils import read_run_metadata

_console = Console()

G = 9.81  # m s⁻²


# ---------------------------------------------------------------------------
# Physics
# ---------------------------------------------------------------------------


def _wave_number(omega: float, h: float, tol: float = 1e-10) -> float:
    """Newton-Raphson solver: ω² = g k tanh(kh) for k."""
    k = (omega**2 / G) / max(np.sqrt(np.tanh(omega**2 * h / G)), 1e-12)
    for _ in range(300):
        f = omega**2 - G * k * np.tanh(k * h)
        fp = -G * (np.tanh(k * h) + k * h / np.cosh(k * h) ** 2)
        dk = -f / fp
        k += dk
        if abs(dk) / max(abs(k), 1e-12) < tol:
            break
    return k


def _theory(h: float, lam: float) -> tuple[float, float]:
    """Return (kh, T_theory) for depth h and wavelength lam."""
    k = 2.0 * math.pi / lam
    kh = k * h
    sig = math.sqrt(G * k * math.tanh(kh))
    return kh, 2.0 * math.pi / sig


def _extract_period(sta: np.ndarray, t_start: float = 2.0) -> float:
    """Estimate wave period from eta (col 1) using upward zero-crossing timing.

    Falls back to FFT if fewer than 2 crossings are found (e.g. very short run).
    """
    t = sta[:, 0]
    eta = sta[:, 1]
    mask = t >= t_start
    if mask.sum() < 4:
        mask = np.ones(len(t), dtype=bool)
    t_s = t[mask]
    eta_s = eta[mask]

    # Remove DC offset so crossings are around the mean
    eta_s = eta_s - eta_s.mean()

    crossings = []
    for i in range(len(eta_s) - 1):
        if eta_s[i] <= 0.0 and eta_s[i + 1] > 0.0:
            frac = -eta_s[i] / (eta_s[i + 1] - eta_s[i])
            crossings.append(float(t_s[i] + frac * (t_s[i + 1] - t_s[i])))

    if len(crossings) >= 2:
        return float(np.median(np.diff(crossings)))

    # FFT fallback
    dt = float(np.median(np.diff(t_s)))
    freq = np.fft.rfftfreq(len(eta_s), d=dt)
    psd = np.abs(np.fft.rfft(eta_s)) ** 2
    psd[0] = 0.0
    peak = int(np.argmax(psd))
    if freq[peak] == 0:
        return float("nan")
    return 1.0 / freq[peak]


# ---------------------------------------------------------------------------
# I/O helpers
# ---------------------------------------------------------------------------


def _read_station(path: Path) -> np.ndarray:
    """Load station file, averaging duplicate rows (parallel artifact)."""
    raw = np.loadtxt(path)
    if raw.ndim == 1:
        return raw.reshape(1, -1)
    if raw.shape[0] >= 2 and np.isclose(raw[0, 0], raw[1, 0], rtol=1e-6):
        n = (raw.shape[0] // 2) * 2
        raw = raw[:n].reshape(-1, 2, raw.shape[1]).mean(axis=1)
    return raw


def _read_uniform_depth(depth_file: Path) -> float:
    """Read depth file and return the median value (assumes uniform depth)."""
    arr = np.loadtxt(depth_file)
    return float(np.median(arr.ravel()))


def _find_station_files(output_dir: Path) -> list[Path]:
    import re

    sta_re = re.compile(r"^sta_\d{4}$")
    return sorted(p for p in output_dir.iterdir() if sta_re.match(p.name))


def _get_depth_and_lambda(run_dir: Path) -> tuple[float | None, float]:
    """Parse depth (h) and wavelength (lambda) from the run YAML.

    Lambda = Mglob * DX (full domain = one wavelength for standing wave).
    Depth is read from the bathymetry file referenced in the YAML.
    """
    yaml_files = sorted(run_dir.glob("*.yaml"))
    if not yaml_files:
        return None, 20.0  # fallback

    with open(yaml_files[0]) as fh:
        cfg = yaml.safe_load(fh)

    geo = cfg.get("geometry", {})
    gs = geo.get("grid_size", [1, 1])
    cs = geo.get("cell_size", [1.0, 1.0])
    lam = float(gs[0]) * float(cs[0])

    bathy = geo.get("bathymetry", {})
    bathy_file = bathy.get("file")
    h: float | None = None
    if bathy_file:
        p = run_dir / bathy_file
        if p.exists():
            h = _read_uniform_depth(p)

    return h, lam


# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------


def run(
    ref_dir: str | Path,
    dev_dir: str | Path,
    tolerances: dict,
    plots_dir: Path,
    verbose: bool = False,
) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    dev_meta = read_run_metadata(dev_dir)

    output_dir = dev_meta.output_dir
    sta_files = _find_station_files(output_dir)

    if not sta_files:
        _console.print("[yellow]standing_wave:[/yellow] no station files found — skipping")
        return SubsectionResult(kind="statistics", label="Standing Wave Physics", metrics=[])

    # Read station data
    sta1 = _read_station(sta_files[0])
    sta2 = _read_station(sta_files[1]) if len(sta_files) > 1 else sta1

    # Get physical parameters
    h, lam = _get_depth_and_lambda(dev_dir)
    if h is None:
        _console.print("[yellow]standing_wave:[/yellow] depth not found — dispersion metrics skipped")
        return SubsectionResult(kind="statistics", label="Standing Wave Physics", metrics=[])

    # Compute metrics
    kh, T_theo = _theory(h, lam)

    t_start = max(0.0, float(sta1[-1, 0]) * 0.4)  # skip first 40% as ramp
    T_meas = _extract_period(sta1, t_start=t_start)
    err_pct = abs(T_meas - T_theo) / T_theo * 100.0 if math.isfinite(T_meas) else float("nan")
    tol_pct = float(tolerances.get("period_error_pct", 15.0))
    passed = math.isfinite(err_pct) and err_pct < tol_pct

    metrics = [
        MetricResult("wave_period", "T_measured_s", T_meas, True, math.inf),
        MetricResult("wave_period", "T_theory_s", T_theo, True, math.inf),
        MetricResult("wave_period", "kh", kh, True, math.inf),
        MetricResult("wave_period", "period_err_pct", err_pct, passed, tol_pct),
    ]

    if verbose or not passed:
        _print_table(h, lam, kh, T_theo, T_meas, err_pct, tol_pct, passed)

    # Figures
    figures: list[FigureSpec] = []
    figures.append(_make_timeseries_figure(sta1, sta2, sta_files, plots_dir))
    figures.append(_make_dispersion_figure(h, lam, kh, T_theo, T_meas, plots_dir))

    return SubsectionResult(
        kind="statistics",
        label="Standing Wave Physics",
        metrics=metrics,
        figures=figures,
    )


# ---------------------------------------------------------------------------
# Rich table
# ---------------------------------------------------------------------------


def _print_table(
    h: float,
    lam: float,
    kh: float,
    T_theo: float,
    T_meas: float,
    err_pct: float,
    tol_pct: float,
    passed: bool,
) -> None:
    table = Table(
        box=box.SIMPLE_HEAD,
        header_style="bold cyan",
        show_edge=False,
        pad_edge=True,
        title="[bold]Standing Wave Physics[/bold]",
        title_justify="left",
    )
    table.add_column("Metric", min_width=22)
    table.add_column("Value", justify="right", min_width=14)
    table.add_column("Tolerance", justify="right", min_width=12)
    table.add_column("", min_width=10)

    def _row(label: str, value: str, tol: str = "—", status: str = "") -> None:
        table.add_row(label, value, tol, status)

    _row("Depth  h", f"{h:.1f} m")
    _row("Wavelength  λ", f"{lam:.1f} m")
    _row("kh  (k = 2π/λ)", f"{kh:.4f}")
    _row("T  (theory)", f"{T_theo:.4f} s")
    _row("T  (measured)", f"{T_meas:.4f} s")

    if math.isfinite(err_pct):
        status = "[bold green]✓ PASS[/bold green]" if passed else "[bold red]✗ FAIL[/bold red]"
        err_c = "green" if passed else "red"
        _row("Period error", f"[{err_c}]{err_pct:.2f} %[/{err_c}]", f"{tol_pct:.1f} %", status)

    _console.print()
    _console.print(table)
    _console.print()


# ---------------------------------------------------------------------------
# Figure helpers
# ---------------------------------------------------------------------------

_DARK = dict(
    template="plotly_dark",
    paper_bgcolor="#161b27",
    plot_bgcolor="#0f1117",
    font=dict(family="SF Mono, Menlo, Consolas, monospace", size=12, color="#94a3b8"),
)
_LITE = dict(
    template="plotly_white",
    paper_bgcolor="#ffffff",
    plot_bgcolor="#f8fafc",
    font=dict(family="SF Mono, Menlo, Consolas, monospace", size=12, color="#334155"),
)


def _make_timeseries_figure(
    sta1: np.ndarray,
    sta2: np.ndarray,
    sta_files: list[Path],
    plots_dir: Path,
) -> FigureSpec:
    label1 = f"Station 1  ({sta_files[0].name})"
    label2 = f"Station 2  ({sta_files[1].name})" if len(sta_files) > 1 else label1

    def _build(dark: bool) -> go.Figure:
        c1 = "#60a5fa" if dark else "#2563eb"
        c2 = "#fb923c" if dark else "#ea580c"
        fig = go.Figure()
        fig.add_trace(
            go.Scatter(x=sta1[:, 0].tolist(), y=sta1[:, 1].tolist(), mode="lines", name=label1, line=dict(color=c1, width=1.5))
        )
        fig.add_trace(
            go.Scatter(
                x=sta2[:, 0].tolist(), y=sta2[:, 1].tolist(), mode="lines", name=label2, line=dict(color=c2, width=1.5, dash="dash")
            )
        )
        theme = _DARK if dark else _LITE
        grid_c = "#1e293b" if dark else "#e2e8f0"
        fig.update_layout(
            **theme,
            xaxis=dict(title="Time (s)", gridcolor=grid_c, zerolinecolor=grid_c),
            yaxis=dict(title="η (m)", gridcolor=grid_c, zerolinecolor=grid_c),
            legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1),
            margin=dict(l=60, r=20, t=40, b=50),
            height=320,
        )
        return fig

    fig_dark = _build(dark=True)
    fig_light = _build(dark=False)

    png_path = plots_dir / "sw_timeseries.png"
    fig_light.write_image(str(png_path), width=900, height=320, scale=2)

    return FigureSpec(
        title="Surface elevation timeseries",
        png_path=png_path,
        interactive=InteractiveFigure(
            kind="plotly",
            json_str=fig_dark.to_json(),
            alt_json_str=fig_light.to_json(),
        ),
    )


def _make_dispersion_figure(
    h: float,
    lam: float,
    kh_meas: float,
    T_theo: float,
    T_meas: float,
    plots_dir: Path,
) -> FigureSpec:
    # Exact theory curve
    h_arr = np.linspace(max(h * 0.05, 0.5), h * 4.0, 400)
    k = 2.0 * math.pi / lam
    kh_arr = k * h_arr
    sig_arr = np.sqrt(G * k * np.tanh(kh_arr))
    T_arr = 2.0 * math.pi / sig_arr

    def _build(dark: bool) -> go.Figure:
        th_c = "#94a3b8" if dark else "#475569"
        pt_c = "#4ade80" if dark else "#16a34a"
        me_c = "#fb923c" if dark else "#ea580c"
        grid_c = "#1e293b" if dark else "#e2e8f0"

        fig = go.Figure()
        fig.add_trace(
            go.Scatter(
                x=kh_arr.tolist(),
                y=T_arr.tolist(),
                mode="lines",
                name="Linear theory",
                line=dict(color=th_c, width=2),
            )
        )
        fig.add_trace(
            go.Scatter(
                x=[kh_meas],
                y=[T_theo],
                mode="markers",
                name=f"Theory  T={T_theo:.3f}s",
                marker=dict(color=pt_c, size=10, symbol="circle"),
            )
        )
        if math.isfinite(T_meas):
            fig.add_trace(
                go.Scatter(
                    x=[kh_meas],
                    y=[T_meas],
                    mode="markers",
                    name=f"Measured  T={T_meas:.3f}s",
                    marker=dict(color=me_c, size=10, symbol="x"),
                )
            )
        # kh = π reference line
        fig.add_vline(
            x=math.pi,
            line_dash="dot",
            line_color=th_c,
            opacity=0.5,
            annotation_text="kh=π",
            annotation_font_size=11,
            annotation_font_color=th_c,
        )

        theme = _DARK if dark else _LITE
        fig.update_layout(
            **theme,
            xaxis=dict(title="kh", gridcolor=grid_c, zerolinecolor=grid_c, range=[0, None]),
            yaxis=dict(title="T (s)", gridcolor=grid_c, zerolinecolor=grid_c),
            legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1),
            margin=dict(l=60, r=20, t=40, b=50),
            height=320,
        )
        return fig

    fig_dark = _build(dark=True)
    fig_light = _build(dark=False)

    png_path = plots_dir / "sw_dispersion.png"
    fig_light.write_image(str(png_path), width=700, height=320, scale=2)

    return FigureSpec(
        title="Dispersion relation",
        png_path=png_path,
        interactive=InteractiveFigure(
            kind="plotly",
            json_str=fig_dark.to_json(),
            alt_json_str=fig_light.to_json(),
        ),
    )
