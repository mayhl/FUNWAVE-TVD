from __future__ import annotations
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from rich import box
from rich.console import Console
from rich.progress import BarColumn, MofNCompleteColumn, Progress, SpinnerColumn, TextColumn, TimeElapsedColumn
from rich.table import Table

from test.framework.results import SubsectionResult, MetricResult, FigureSpec, InteractiveFigure
from test.regression.postproc.utils import (
    RunMetadata,
    VariableInfo,
    read_run_metadata,
    get_output_variables,
    compute_metric_series,
    metric_stat_name,
    MASK_PREFIXES,
)

_console = Console()


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
    ref_dir = Path(ref_dir)
    dev_dir = Path(dev_dir)

    ref_meta = read_run_metadata(ref_dir)
    dev_meta = read_run_metadata(dev_dir)

    ref_vars = get_output_variables(ref_meta.output_dir, kind="field")
    dev_vars = get_output_variables(dev_meta.output_dir, kind="field")

    ref_map = {v.prefix: v for v in ref_vars}
    dev_map = {v.prefix: v for v in dev_vars}
    common  = sorted(ref_map.keys() & dev_map.keys())
    missing = sorted((ref_map.keys() | dev_map.keys()) - ref_map.keys() & dev_map.keys())

    all_output = ref_map.keys() | dev_map.keys()
    for pfx in sorted(tolerances):
        if pfx != "default" and pfx not in all_output:
            _console.print(f"[yellow]WARN:[/yellow] tolerance specified for '{pfx}' but no output files found")

    metrics = []
    rows: list[_Row] = []
    series_data: list[tuple] = []  # (prefix, l2_series, idx_first, idx_last, tol, passed)

    for prefix in common:
        rv = ref_map[prefix]
        dv = dev_map[prefix]

        if rv.unstable or dv.unstable:
            rows.append(_Row(prefix, rv, dv, l2_mean=None, l2_max=None, note="unstable"))
            continue

        # Compare over shared index range
        idx_first = max(rv.first, dv.first)
        idx_last  = min(rv.last,  dv.last)
        if idx_first > idx_last:
            rows.append(_Row(prefix, rv, dv, l2_mean=None, l2_max=None, note="no overlap"))
            continue

        metric_series = compute_metric_series(
            ref_meta, dev_meta, prefix, idx_first, idx_last
        )

        err_mean = float(np.mean(metric_series))
        err_max  = float(np.max(metric_series))
        tol      = tolerances.get(prefix, tolerances.get("default", np.inf))
        passed   = err_mean < tol

        metrics.append(MetricResult(
            variable=prefix,
            stat=metric_stat_name(prefix, "mean"),
            value=err_mean,
            passed=passed,
            tolerance=tol,
        ))
        metrics.append(MetricResult(
            variable=prefix,
            stat=metric_stat_name(prefix, "max"),
            value=err_max,
            passed=True,
            tolerance=float("inf"),
        ))
        rows.append(_Row(prefix, rv, dv, l2_mean=err_mean, l2_max=err_max,
                         tol=tol, passed=passed))
        series_data.append((prefix, metric_series, idx_first, idx_last, tol, passed))

    for prefix in missing:
        rv = ref_map.get(prefix)
        dv = dev_map.get(prefix)
        rows.append(_Row(prefix, rv, dv, l2_mean=None, l2_max=None, note="missing in one"))

    any_failed = any(not r.passed for r in rows if r.note == "" and np.isfinite(r.tol))
    if verbose or any_failed:
        _print_table(ref_meta, dev_meta, rows)

    result = SubsectionResult(kind="field", label="Field Data", metrics=metrics)
    fig = _make_figure(series_data, plots_dir)
    if fig is not None:
        result.figures.append(fig)

    failing = [
        (pfx, l2, i0, il, tol, p)
        for pfx, l2, i0, il, tol, p in series_data
        if np.isfinite(tol) and not p
    ]
    # TODO: gate this block on --no-auto-report (passed through as plots=True/False);
    #       _make_failure_figures writes spatial PNGs for every failing variable and
    #       is the main source of slowness when tests fail during a dev iteration.
    if failing:
        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            BarColumn(bar_width=20),
            MofNCompleteColumn(),
            TimeElapsedColumn(),
            console=_console,
            transient=True,
        ) as progress:
            task = progress.add_task("  Generating subreport", total=len(failing))
            for prefix, l2_per_step, idx_first, idx_last, tol, passed in failing:
                progress.update(task, description=f"  Generating subreport  [dim]{prefix}[/dim]")
                result.figures.extend(
                    _make_failure_figures(ref_meta, dev_meta, prefix, l2_per_step, idx_first, tol, plots_dir)
                )
                progress.advance(task)

    return result


# ---------------------------------------------------------------------------
# Figure generation
# ---------------------------------------------------------------------------

_DARK_PASS  = "#4ade80"
_DARK_FAIL  = "#f87171"
_DARK_NOTOL = "#94a3b8"
_LITE_PASS  = "#16a34a"
_LITE_FAIL  = "#dc2626"
_LITE_NOTOL = "#64748b"


def _build_fig(series_data: list[tuple], dark: bool) -> go.Figure:
    pass_c  = _DARK_PASS  if dark else _LITE_PASS
    fail_c  = _DARK_FAIL  if dark else _LITE_FAIL
    notol_c = _DARK_NOTOL if dark else _LITE_NOTOL

    fig = go.Figure()
    for prefix, l2_series, idx_first, idx_last, tol, passed in series_data:
        xs = list(range(idx_first, idx_first + len(l2_series)))
        finite_tol = np.isfinite(tol)
        color = (pass_c if passed else fail_c) if finite_tol else notol_c

        fig.add_trace(go.Scatter(
            x=xs, y=l2_series.tolist(),
            mode="lines",
            name=prefix,
            line=dict(color=color, width=1.5),
        ))
        if finite_tol:
            fig.add_hline(
                y=tol, line_dash="dot", line_color=color, opacity=0.5,
                annotation_text=f"{prefix} tol",
                annotation_font_size=11,
                annotation_font_color=color,
            )

    if dark:
        fig.update_layout(
            template="plotly_dark",
            paper_bgcolor="#161b27",
            plot_bgcolor="#0f1117",
            font=dict(family="SF Mono, Menlo, Consolas, Liberation Mono, monospace", size=12, color="#94a3b8"),
            xaxis=dict(gridcolor="#1e293b", zerolinecolor="#1e293b"),
            yaxis=dict(gridcolor="#1e293b", zerolinecolor="#1e293b"),
        )
    else:
        fig.update_layout(
            template="plotly_white",
            paper_bgcolor="#ffffff",
            plot_bgcolor="#f8fafc",
            font=dict(family="SF Mono, Menlo, Consolas, Liberation Mono, monospace", size=12, color="#334155"),
            xaxis=dict(gridcolor="#e2e8f0", zerolinecolor="#e2e8f0", color="#475569"),
            yaxis=dict(gridcolor="#e2e8f0", zerolinecolor="#e2e8f0", color="#475569"),
        )

    fig.update_layout(
        margin=dict(l=60, r=20, t=20, b=50),
        xaxis_title="Output step",
        yaxis_title="L2 error",
        yaxis_type="log",
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1,
                    font=dict(size=12)),
        height=320,
    )
    return fig


def _make_figure(series_data: list[tuple], plots_dir: Path) -> FigureSpec | None:
    """Build L2-vs-step figure for tolerated variables only; returns None if none exist."""
    tolerated = [s for s in series_data if np.isfinite(s[4])]  # s[4] = tol
    if not tolerated:
        return None
    fig_dark  = _build_fig(tolerated, dark=True)
    fig_light = _build_fig(tolerated, dark=False)

    png_path = plots_dir / "field_l2.png"
    fig_light.write_image(str(png_path), width=900, height=320, scale=2)

    return FigureSpec(
        title="L2 error vs output step",
        png_path=png_path,
        interactive=InteractiveFigure(
            kind="plotly",
            json_str=fig_dark.to_json(),
            alt_json_str=fig_light.to_json(),
        ),
    )


# ---------------------------------------------------------------------------
# Failure diagnostic figures
# ---------------------------------------------------------------------------

def _select_timesteps(l2_per_step: np.ndarray, idx_first: int, tol: float) -> list[int]:
    """Return up to 7 deduplicated sorted step indices for failure diagnostics."""
    n = len(l2_per_step)
    candidates: set[int] = set()
    for frac in (0.0, 0.25, 0.50, 0.75, 1.0):
        candidates.add(idx_first + round(frac * (n - 1)))
    candidates.add(idx_first + int(np.argmax(l2_per_step)))
    if np.isfinite(tol):
        exceeds = np.where(l2_per_step >= tol)[0]
        if len(exceeds):
            candidates.add(idx_first + int(exceeds[0]))
    return sorted(candidates)


def _is_1d_mode(ref_meta: RunMetadata) -> bool:
    if ref_meta.is_3d:
        return False
    return ref_meta.ny <= max(5, ref_meta.nx // 20)



def _load_mask(meta: RunMetadata, idx: int) -> np.ndarray | None:
    """Return boolean (ny, nx) wet mask for step idx, or None if unavailable."""
    p = meta.field_path("mask", idx)
    if not p.exists():
        return None
    arr = meta.read_field(p)
    if arr.ndim == 3:
        arr = arr[arr.shape[0] // 2]  # midplane for 3D masks
    return arr > 0


def _build_time_map(output_dir: Path, run_dir: Path) -> dict[int, float]:
    """Parse time_dt.out -> {step_index (0-based): simulation_time}.

    Returns empty dict if absent (e.g. refactored dev version without this file yet).
    """
    for p in [output_dir / "time_dt.out", run_dir / "time_dt.out"]:
        if p.exists():
            result: dict[int, float] = {}
            for i, line in enumerate(p.read_text().splitlines()):
                parts = line.split()
                if parts:
                    try:
                        result[i] = float(parts[0])
                    except ValueError:
                        pass
            return result
    return {}


def _categorize_steps(
    l2_per_step: np.ndarray,
    idx_first: int,
    tol: float,
) -> dict[int, list[str]]:
    """Return {step_idx: [label, ...]} describing why each step was selected."""
    n = len(l2_per_step)
    cats: dict[int, list[str]] = {}

    def _add(step: int, label: str) -> None:
        lst = cats.setdefault(step, [])
        if label not in lst:
            lst.append(label)

    for frac, label in [(0.0, "0%"), (0.25, "25%"), (0.50, "50%"), (0.75, "75%"), (1.0, "100%")]:
        _add(idx_first + round(frac * (n - 1)), label)
    if np.isfinite(tol):
        exceeds = np.where(l2_per_step >= tol)[0]
        if len(exceeds):
            _add(idx_first + int(exceeds[0]), "First ≥ tol")
    _add(idx_first + int(np.argmax(l2_per_step)), "Max L2")
    return cats


def _order_steps(
    steps: list[int],
    l2_per_step: np.ndarray,
    idx_first: int,
    tol: float,
) -> list[int]:
    """Reorder steps: First-exceeds-tol first, Max-L2 second, rest chronologically."""
    first_idx: int | None = None
    if np.isfinite(tol):
        exceeds = np.where(l2_per_step >= tol)[0]
        if len(exceeds):
            first_idx = idx_first + int(exceeds[0])
    max_idx = idx_first + int(np.argmax(l2_per_step))

    head: list[int] = []
    if first_idx is not None and first_idx in steps:
        head.append(first_idx)
    if max_idx in steps and max_idx not in head:
        head.append(max_idx)
    tail = [s for s in steps if s not in head]
    return head + tail


def _step_title(step: int, time_map: dict[int, float], cats: dict[int, list[str]]) -> str:
    t_str   = f"T={time_map[step]:.1f}s" if step in time_map else f"step {step}"
    cat_str = "  |  ".join(cats.get(step, []))
    return f"{t_str}  —  {cat_str}" if cat_str else t_str


def _theme_base(dark: bool) -> dict:
    if dark:
        return dict(
            template="plotly_dark",
            paper_bgcolor="#161b27",
            plot_bgcolor="#0f1117",
            font=dict(family="SF Mono, Menlo, Consolas, Liberation Mono, monospace", size=12, color="#94a3b8"),
        )
    return dict(
        template="plotly_white",
        paper_bgcolor="#ffffff",
        plot_bgcolor="#f8fafc",
        font=dict(family="SF Mono, Menlo, Consolas, Liberation Mono, monospace", size=12, color="#334155"),
    )



def _make_1d_step_fig(
    ref_meta: RunMetadata,
    ref_arr: np.ndarray,
    dev_arr: np.ndarray,
    prefix: str,
    title: str,
    dark: bool,
) -> go.Figure:
    """Build 1D overlay+diff figure for a single timestep.

    ref_arr / dev_arr must be pre-masked float arrays (NaN = dry).
    """
    mid  = ref_arr.shape[0] // 2
    xs   = (np.arange(ref_arr.shape[1]) * ref_meta.dx).tolist()
    diff = (dev_arr[mid, :] - ref_arr[mid, :]).tolist()

    ref_c  = "#60a5fa" if dark else "#2563eb"
    dev_c  = "#fb923c" if dark else "#ea580c"
    diff_c = "#f87171" if dark else "#dc2626"

    fig = make_subplots(rows=1, cols=1, specs=[[{"secondary_y": True}]])
    fig.add_trace(go.Scatter(x=xs, y=ref_arr[mid, :].tolist(), mode="lines", name="ref",
        line=dict(color=ref_c, width=1.5)), row=1, col=1, secondary_y=False)
    fig.add_trace(go.Scatter(x=xs, y=dev_arr[mid, :].tolist(), mode="lines", name="dev",
        line=dict(color=dev_c, width=1.5)), row=1, col=1, secondary_y=False)
    fig.add_trace(go.Scatter(x=xs, y=diff, mode="lines", name="dev−ref",
        line=dict(color=diff_c, width=1.0, dash="dash")),
        row=1, col=1, secondary_y=True)
    fig.update_yaxes(zeroline=False)
    fig.update_yaxes(title_text=prefix,          title_font_size=11, secondary_y=False)
    fig.update_yaxes(title_text="dev−ref",  title_font_size=11, secondary_y=True)
    fig.update_layout(
        **_theme_base(dark),
        title=dict(text=title, font=dict(size=13), x=0.5, xanchor="center"),
        height=360,
        margin=dict(l=60, r=80, t=60, b=50),
        xaxis_title="x (m)",
        legend=dict(orientation="h", yanchor="bottom", y=1.08, xanchor="right", x=1),
    )
    return fig


def _make_2d_step_fig(
    ref_arr: np.ndarray,
    dev_arr: np.ndarray,
    prefix: str,
    title: str,
    vmin: float,
    vmax: float,
    amax: float,
    dark: bool,
) -> go.Figure:
    """Build 2D heatmap figure (ref / dev / diff) for a single timestep.

    ref_arr / dev_arr must be pre-masked float arrays (NaN = dry).
    """
    diff_arr = dev_arr - ref_arr
    fig = make_subplots(
        rows=3, cols=1,
        specs=[[{"type": "heatmap"}]] * 3,
        row_titles=["ref", "dev", "dev−ref"],
        shared_xaxes=True,
        shared_yaxes=True,
        vertical_spacing=0.05,
    )
    fig.add_trace(go.Heatmap(z=ref_arr.tolist(),  coloraxis="coloraxis"),  row=1, col=1)
    fig.add_trace(go.Heatmap(z=dev_arr.tolist(),  coloraxis="coloraxis"),  row=2, col=1)
    fig.add_trace(go.Heatmap(z=diff_arr.tolist(), coloraxis="coloraxis2"), row=3, col=1)
    fig.update_layout(
        **_theme_base(dark),
        title=dict(text=title, font=dict(size=13), x=0.5, xanchor="center"),
        height=560,
        margin=dict(l=60, r=120, t=60, b=50),
        coloraxis=dict(
            colorscale="Viridis",
            cmin=vmin, cmax=vmax,
            colorbar=dict(x=1.02, y=0.67, len=0.60, yanchor="middle", thickness=12,
                          title=dict(text=prefix, font=dict(size=10))),
        ),
        coloraxis2=dict(
            colorscale="RdBu",
            cmid=0, cmin=-amax, cmax=amax,
            colorbar=dict(x=1.02, y=0.17, len=0.28, yanchor="middle", thickness=12,
                          title=dict(text="dev−ref", font=dict(size=10))),
        ),
    )
    return fig


def _make_failure_figures(
    ref_meta: RunMetadata,
    dev_meta: RunMetadata,
    prefix: str,
    l2_per_step: np.ndarray,
    idx_first: int,
    tol: float,
    plots_dir: Path,
) -> list[FigureSpec]:
    """Return one FigureSpec per selected timestep for a failing variable."""
    steps    = _select_timesteps(l2_per_step, idx_first, tol)
    steps    = _order_steps(steps, l2_per_step, idx_first, tol)
    cats     = _categorize_steps(l2_per_step, idx_first, tol)
    time_map = _build_time_map(ref_meta.output_dir, ref_meta.run_dir)
    mode_1d  = _is_1d_mode(ref_meta)

    # Read and pre-mask all selected steps.
    # For 3D volumetric fields take the midplane k-slice for visualization.
    pairs: list[tuple[np.ndarray, np.ndarray]] = []
    is_3d_field = False
    mid_k: int = 0
    for step in steps:
        ref_arr = ref_meta.read_field(ref_meta.field_path(prefix, step)).astype(float)
        dev_arr = dev_meta.read_field(dev_meta.field_path(prefix, step)).astype(float)
        if ref_arr.ndim == 3:
            is_3d_field = True
            mid_k = ref_arr.shape[0] // 2
            ref_arr = ref_arr[mid_k]
            dev_arr = dev_arr[mid_k]
        ref_mask = _load_mask(ref_meta, step)
        dev_mask = _load_mask(dev_meta, step)
        if ref_mask is not None:
            ref_arr[~ref_mask] = np.nan
        if dev_mask is not None:
            dev_arr[~dev_mask] = np.nan
        pairs.append((ref_arr, dev_arr))

    # Compute global 2D color ranges once across all steps
    if not mode_1d:
        all_vals = np.concatenate([arr.ravel() for pair in pairs for arr in pair])
        vmin = float(np.nanmin(all_vals))
        vmax = float(np.nanmax(all_vals))
        amax_raw = max(float(np.nanmax(np.abs(d - r))) for r, d in pairs)
        amax = amax_raw if amax_raw > 0 else 1.0

    slice_note = f"  [k={mid_k}]" if is_3d_field else ""
    result: list[FigureSpec] = []
    for step, (ref_arr, dev_arr) in zip(steps, pairs):
        title    = _step_title(step, time_map, cats) + slice_note
        step_fmt = f"{step:04d}" if ref_meta.is_3d else f"{step:05d}"
        png_path = plots_dir / f"diag_{prefix}_{step_fmt}.png"

        if mode_1d:
            fig_dark  = _make_1d_step_fig(ref_meta, ref_arr, dev_arr, prefix, title, dark=True)
            fig_light = _make_1d_step_fig(ref_meta, ref_arr, dev_arr, prefix, title, dark=False)
            fig_light.write_image(str(png_path), width=900, height=360, scale=2)
        else:
            fig_dark  = _make_2d_step_fig(ref_arr, dev_arr, prefix, title, vmin, vmax, amax, dark=True)
            fig_light = _make_2d_step_fig(ref_arr, dev_arr, prefix, title, vmin, vmax, amax, dark=False)
            fig_light.write_image(str(png_path), width=900, height=560, scale=2)

        result.append(FigureSpec(
            title=f"{prefix}{slice_note}  —  {title}",
            png_path=png_path,
            interactive=InteractiveFigure(
                kind="plotly",
                json_str=fig_dark.to_json(),
                alt_json_str=fig_light.to_json(),
            ),
        ))
    return result


# ---------------------------------------------------------------------------
# Rich display
# ---------------------------------------------------------------------------

@dataclass
class _Row:
    prefix:  str
    ref_var: VariableInfo | None
    dev_var: VariableInfo | None
    l2_mean: float | None
    l2_max:  float | None
    tol:     float = np.inf
    passed:  bool  = True
    note:    str   = ""


def _print_table(ref_meta: RunMetadata, dev_meta: RunMetadata, rows: list[_Row]) -> None:
    dims = f"{ref_meta.nx}×{ref_meta.ny}×{ref_meta.nz}" if ref_meta.is_3d else f"{ref_meta.nx}×{ref_meta.ny}"
    fmt  = "binary" if ref_meta.binary else "ASCII"
    meta = f"[dim]{dims}[/dim]  [dim]{fmt}[/dim]"
    table = Table(
        box=box.SIMPLE_HEAD,
        header_style="bold cyan",
        show_edge=False,
        pad_edge=True,
        title=f"[bold]Field Comparison[/bold]  ·  {meta}",
        title_justify="left",
    )
    table.add_column("Variable", min_width=10)
    table.add_column("Steps",    justify="right", min_width=6)
    table.add_column("Mean err", justify="right", min_width=12)
    table.add_column("Max err",  justify="right", min_width=12)
    table.add_column("Tol",      justify="right", min_width=10)
    table.add_column("",         min_width=8)

    for r in rows:
        if r.note:
            table.add_row(
                f"[dim]{r.prefix}[/dim]", "—", "—", "—", "—",
                f"[yellow]{r.note}[/yellow]",
            )
            continue

        steps = str(r.ref_var.count) if r.ref_var else "—"
        l2m   = f"{r.l2_mean:.3e}" if r.l2_mean is not None else "—"
        l2x   = f"{r.l2_max:.3e}"  if r.l2_max  is not None else "—"
        tol   = f"{r.tol:.1e}"     if np.isfinite(r.tol)    else "—"

        if np.isinf(r.tol):
            status = "[dim]○[/dim]"
            l2m_fmt = f"[dim]{l2m}[/dim]"
        elif r.passed:
            status  = "[bold green]✓ PASS[/bold green]"
            l2m_fmt = f"[green]{l2m}[/green]"
        else:
            status  = "[bold red]✗ FAIL[/bold red]"
            l2m_fmt = f"[red]{l2m}[/red]"

        table.add_row(r.prefix, steps, l2m_fmt, l2x, tol, status)

    _console.print()
    _console.print(table)
    _console.print()


if __name__ == "__main__":
    import sys

    if len(sys.argv) < 3:
        print("Usage: python -m test.regression.postproc.field_compare <ref_dir> <dev_dir>")
        sys.exit(1)

    run(sys.argv[1], sys.argv[2], tolerances={}, plots_dir=Path("."))
