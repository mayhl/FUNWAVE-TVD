"""Generate a self-contained HTML regression report from SimResult objects."""

from __future__ import annotations

import itertools
import math
from collections import OrderedDict
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

from test.framework.results import SimResult, SubsectionResult, MetricResult, FigureSpec


@dataclass
class ReportMeta:
    ref_branch: str
    dev_branch: str
    ref_hash: str = ""
    dev_hash: str = ""
    generated_at: str = ""

    def __post_init__(self):
        if not self.generated_at:
            self.generated_at = datetime.now().strftime("%Y-%m-%d %H:%M:%S")


# ---------------------------------------------------------------------------
# CSS  (uses CSS variables for dark/light theming)
# ---------------------------------------------------------------------------

_CSS = """
*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

:root {
    --bg-page:         #0f1117;
    --bg-card:         #161b27;
    --bg-header:       #1e293b;
    --bg-code:         #1e293b;
    --border:          #1e293b;
    --text:            #e2e8f0;
    --text-h1:         #f8fafc;
    --text-h2:         #94a3b8;
    --text-meta:       #64748b;
    --text-dim:        #475569;
    --color-pass:      #4ade80;
    --color-fail:      #f87171;
    --color-warn:      #fbbf24;
    --badge-pass-bg:   #14532d;
    --badge-pass-fg:   #4ade80;
    --badge-fail-bg:   #450a0a;
    --badge-fail-fg:   #f87171;
    --badge-warn-bg:   #451a03;
    --badge-warn-fg:   #fbbf24;
    --badge-dim-bg:    #1e293b;
    --badge-dim-fg:    #64748b;
    --btn-bg:          #1e293b;
    --btn-fg:          #94a3b8;
    --btn-hover-bg:    #293548;
}

[data-theme="light"] {
    --bg-page:         #f8fafc;
    --bg-card:         #ffffff;
    --bg-header:       #f1f5f9;
    --bg-code:         #f1f5f9;
    --border:          #e2e8f0;
    --text:            #334155;
    --text-h1:         #0f172a;
    --text-h2:         #475569;
    --text-meta:       #64748b;
    --text-dim:        #94a3b8;
    --color-pass:      #16a34a;
    --color-fail:      #dc2626;
    --color-warn:      #d97706;
    --badge-pass-bg:   #dcfce7;
    --badge-pass-fg:   #16a34a;
    --badge-fail-bg:   #fee2e2;
    --badge-fail-fg:   #dc2626;
    --badge-warn-bg:   #fef3c7;
    --badge-warn-fg:   #d97706;
    --badge-dim-bg:    #f1f5f9;
    --badge-dim-fg:    #64748b;
    --btn-bg:          #e2e8f0;
    --btn-fg:          #475569;
    --btn-hover-bg:    #cbd5e1;
}

body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    font-size: 14px;
    background: var(--bg-page);
    color: var(--text);
    padding: 2rem;
    line-height: 1.5;
    transition: background 0.15s, color 0.15s;
}

.page-header { display: flex; align-items: flex-start; justify-content: space-between;
               margin-bottom: 0.25rem; }

h1 { font-size: 1.4rem; font-weight: 600; color: var(--text-h1); }
h2 { font-size: 1rem; font-weight: 600; color: var(--text-h2); text-transform: uppercase;
     letter-spacing: 0.05em; margin: 2rem 0 0.75rem; }
h3 { font-size: 0.95rem; font-weight: 600; color: var(--text); margin: 1.5rem 0 0.5rem; }

.meta { font-size: 0.8rem; color: var(--text-meta); margin-bottom: 2rem; }
.meta span { margin-right: 1.5rem; }
.meta code { font-family: "SF Mono", "Fira Code", monospace; font-size: 0.78rem;
              background: var(--bg-code); padding: 0.1rem 0.35rem; border-radius: 3px; }

.theme-toggle {
    flex-shrink: 0;
    background: var(--btn-bg);
    color: var(--btn-fg);
    border: none;
    border-radius: 5px;
    padding: 0.3rem 0.75rem;
    font-size: 0.78rem;
    font-weight: 600;
    cursor: pointer;
    letter-spacing: 0.04em;
}
.theme-toggle:hover { background: var(--btn-hover-bg); }

table { border-collapse: collapse; width: 100%; }

.summary-table th,
.summary-table td { padding: 0.5rem 1rem; border-bottom: 1px solid var(--border); }
.summary-table th { background: var(--bg-header); color: var(--text-h2); font-size: 0.78rem;
                     text-transform: uppercase; letter-spacing: 0.06em; text-align: left; }
.summary-table tr:last-child td { border-bottom: none; }
.summary-table td.num   { text-align: right; font-family: "SF Mono","Fira Code",monospace;
                           font-size: 0.85rem; }
.summary-table td.center { text-align: center; }

.detail-table th,
.detail-table td { padding: 0.4rem 0.8rem; border-bottom: 1px solid var(--border); }
.detail-table th { background: var(--bg-header); color: var(--text-h2); font-size: 0.75rem;
                    text-transform: uppercase; letter-spacing: 0.06em; text-align: left; }
.detail-table td { font-family: "SF Mono", "Fira Code", monospace; font-size: 0.82rem; }
.detail-table td.right { text-align: right; }
.detail-table tr:last-child td { border-bottom: none; }

.card { background: var(--bg-card); border: 1px solid var(--border); border-radius: 8px;
         margin-bottom: 1.5rem; overflow: hidden; }
.card-header { background: var(--bg-header); padding: 0.6rem 1rem; display: flex;
                align-items: center; gap: 0.75rem; }
.card-header .sim-name { font-weight: 600; color: var(--text-h1); font-size: 0.95rem; }
.card-header .sim-status { font-size: 0.8rem; }
.card-body { padding: 1rem; }

.pass   { color: var(--color-pass); }
.fail   { color: var(--color-fail); }
.warn   { color: var(--color-warn); }
.dim    { color: var(--text-dim); }
.mono   { font-family: "SF Mono", "Fira Code", monospace; font-size: 0.82rem; }

.badge { display: inline-block; padding: 0.15rem 0.5rem; border-radius: 4px;
          font-size: 0.75rem; font-weight: 600; }
.badge-pass  { background: var(--badge-pass-bg); color: var(--badge-pass-fg); }
.badge-fail  { background: var(--badge-fail-bg); color: var(--badge-fail-fg); }
.badge-warn  { background: var(--badge-warn-bg); color: var(--badge-warn-fg); }
.badge-dim   { background: var(--badge-dim-bg);  color: var(--badge-dim-fg); }

.section-label { font-size: 0.8rem; font-weight: 600; color: var(--text-h2);
                  text-transform: uppercase; letter-spacing: 0.05em; margin: 1rem 0 0.4rem; }

.fig-wrap { margin-top: 1rem; }
.fig-interactive { width: 100%; }
.fig-print { display: none; max-width: 100%; }

@media print {
    :root {
        --bg-page:       #ffffff;
        --bg-card:       #ffffff;
        --bg-header:     #f1f5f9;
        --bg-code:       #f1f5f9;
        --border:        #e2e8f0;
        --text:          #334155;
        --text-h1:       #0f172a;
        --text-h2:       #475569;
        --text-meta:     #64748b;
        --text-dim:      #94a3b8;
        --color-pass:    #16a34a;
        --color-fail:    #dc2626;
        --color-warn:    #d97706;
        --badge-pass-bg: #dcfce7; --badge-pass-fg: #16a34a;
        --badge-fail-bg: #fee2e2; --badge-fail-fg: #dc2626;
        --badge-warn-bg: #fef3c7; --badge-warn-fg: #d97706;
        --badge-dim-bg:  #f1f5f9; --badge-dim-fg:  #64748b;
    }
    body { padding: 0; font-size: 11pt; }
    table { page-break-inside: avoid; }
    .card { page-break-inside: avoid; }
    .fig-interactive { display: none; }
    .fig-print { display: block; max-width: 100%; }
    .theme-toggle { display: none; }
}
"""

_PLOTLY_CONFIG = "{responsive: true, displayModeBar: 'hover', displaylogo: false, modeBarButtonsToRemove: ['sendDataToCloud', 'lasso2d', 'select2d'], toImageButtonOptions: {format: 'png', filename: 'funwave_l2', width: 1200, height: 400, scale: 2}}"

_TOGGLE_JS = (
    """
function _applyPlotlyTheme(theme) {
    document.querySelectorAll('.plotly-switchable').forEach(function(el) {
        var jsonEl = document.getElementById(el.id + '-' + theme);
        if (!jsonEl) return;
        var spec = JSON.parse(jsonEl.textContent);
        Plotly.react(el.id, spec.data, spec.layout, """
    + _PLOTLY_CONFIG
    + """);
    });
}

function toggleTheme() {
    var body    = document.body;
    var btn     = document.getElementById('theme-toggle');
    var isLight = body.getAttribute('data-theme') === 'light';
    var next    = isLight ? 'dark' : 'light';
    if (next === 'light') {
        body.setAttribute('data-theme', 'light');
        btn.textContent = 'Dark';
    } else {
        body.removeAttribute('data-theme');
        btn.textContent = 'Light';
    }
    localStorage.setItem('fw-report-theme', next);
    _applyPlotlyTheme(next);
}

(function() {
    var saved = localStorage.getItem('fw-report-theme');
    var btn   = document.getElementById('theme-toggle');
    if (saved === 'light') {
        document.body.setAttribute('data-theme', 'light');
        if (btn) btn.textContent = 'Dark';
        _applyPlotlyTheme('light');
    }
})();
"""
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _status_badge(status: str) -> str:
    mapping = {
        "PASS": '<span class="badge badge-pass">✓ PASS</span>',
        "FAIL": '<span class="badge badge-fail">✗ FAIL</span>',
        "XFAIL": '<span class="badge badge-warn">⚠ XFAIL</span>',
        "XPASS": '<span class="badge badge-fail">✗ XPASS</span>',
        "SIM_FAILED": '<span class="badge badge-fail">✗ SIM FAILED</span>',
        "POSTPROCESS_ERROR": '<span class="badge badge-warn">⚠ ERROR</span>',
        "COMPLETED": '<span class="badge badge-dim">COMPLETED</span>',
    }
    return mapping.get(status, f'<span class="badge badge-dim">{status}</span>')


def _metric_cell(value: float, passed: bool, tol: float) -> str:
    fmt = f"{value:.3e}"
    if math.isinf(tol):
        return f'<span class="dim">{fmt}</span>'
    cls = "pass" if passed else "fail"
    return f'<span class="{cls}">{fmt}</span>'


def _subsection_summary(sub: SubsectionResult | None) -> str:
    if sub is None:
        return '<span class="dim">—</span>'
    if not sub.metrics:
        return '<span class="dim">—</span>'
    p, t = sub.n_passed, sub.n_total
    if t == 0:
        return '<span class="dim">—</span>'
    cls = "pass" if p == t else "fail"
    return f'<span class="{cls}">{p}/{t}</span>'


# ---------------------------------------------------------------------------
# Section renderers
# ---------------------------------------------------------------------------

_fig_counter = itertools.count()


def _render_figure(spec: FigureSpec) -> str:
    parts = []
    if spec.interactive and spec.interactive.kind == "plotly":
        uid = f"plotly-fig-{next(_fig_counter)}"
        has_alt = bool(spec.interactive.alt_json_str)
        switchable = " plotly-switchable" if has_alt else ""

        parts.append('    <div class="fig-wrap">')

        # Embed both theme JSONs in non-executing script tags (safe, no escaping needed)
        parts.append(f'      <script type="application/json" id="{uid}-dark">{spec.interactive.json_str}</script>')
        if has_alt:
            parts.append(f'      <script type="application/json" id="{uid}-light">{spec.interactive.alt_json_str}</script>')

        parts.append(f'      <div id="{uid}" class="fig-interactive{switchable}"></div>')
        parts.append(f"""      <script>
        (function(){{
          var el = document.getElementById('{uid}-dark');
          var spec = JSON.parse(el.textContent);
          Plotly.newPlot('{uid}', spec.data, spec.layout, {_PLOTLY_CONFIG});
        }})();
      </script>""")

        if spec.png_path and Path(spec.png_path).exists():
            parts.append(f'      <img class="fig-print" src="{spec.png_path}" alt="{spec.title}">')

        parts.append("    </div>")

    elif spec.png_path and Path(spec.png_path).exists():
        parts.append(f'    <div class="fig-wrap"><img style="max-width:100%" src="{spec.png_path}" alt="{spec.title}"></div>')

    return "\n".join(parts)


def _render_field_section(sub: SubsectionResult) -> str:
    # Group metrics by variable (preserving insertion order)
    by_var: dict[str, dict[str, MetricResult]] = OrderedDict()
    for m in sub.metrics:
        by_var.setdefault(m.variable, {})[m.stat] = m

    rows_html = ""
    for var, stats in by_var.items():
        mean_m = next((v for k, v in stats.items() if k.endswith("_mean")), None)
        max_m = next((v for k, v in stats.items() if k.endswith("_max")), None)
        if mean_m is None:
            continue

        tol_finite = math.isfinite(mean_m.tolerance)
        mean_cell = _metric_cell(mean_m.value, mean_m.passed, mean_m.tolerance)
        max_cell = f'<span class="dim">{max_m.value:.3e}</span>' if max_m else "—"
        tol_fmt = f"{mean_m.tolerance:.1e}" if tol_finite else "—"

        if not tol_finite:
            status_cell = '<span class="dim">○</span>'
        elif mean_m.passed:
            status_cell = '<span class="pass">✓ PASS</span>'
        else:
            status_cell = '<span class="fail">✗ FAIL</span>'

        rows_html += f"""
        <tr>
          <td>{var}</td>
          <td class="right">{mean_cell}</td>
          <td class="right">{max_cell}</td>
          <td class="right dim">{tol_fmt}</td>
          <td class="right">{status_cell}</td>
        </tr>"""

    figures_html = "\n".join(_render_figure(f) for f in sub.figures)

    return f"""
    <div class="section-label">{sub.label}</div>
    <table class="detail-table">
      <thead>
        <tr>
          <th>Variable</th>
          <th class="right">Mean L2</th>
          <th class="right">Max L2</th>
          <th class="right">Tolerance</th>
          <th class="right">Status</th>
        </tr>
      </thead>
      <tbody>{rows_html}
      </tbody>
    </table>
    {figures_html}"""


def _render_statistics_section(sub: SubsectionResult) -> str:
    """Generic renderer for statistics/physics metric subsections."""
    rows_html = ""
    for m in sub.metrics:
        tol_finite = math.isfinite(m.tolerance)
        val_str = f"{m.value:.4f}" if math.isfinite(m.value) else "—"
        tol_str = f"{m.tolerance:.2g}" if tol_finite else "—"

        if not tol_finite:
            status_cell = '<span class="dim">○</span>'
            val_cell = f'<span class="dim">{val_str}</span>'
        elif m.passed:
            status_cell = '<span class="pass">✓ PASS</span>'
            val_cell = f'<span class="pass">{val_str}</span>'
        else:
            status_cell = '<span class="fail">✗ FAIL</span>'
            val_cell = f'<span class="fail">{val_str}</span>'

        rows_html += f"""
        <tr>
          <td>{m.stat}</td>
          <td class="right">{val_cell}</td>
          <td class="right dim">{tol_str}</td>
          <td class="right">{status_cell}</td>
        </tr>"""

    figures_html = "\n".join(_render_figure(f) for f in sub.figures)

    return f"""
    <div class="section-label">{sub.label}</div>
    <table class="detail-table">
      <thead>
        <tr>
          <th>Metric</th>
          <th class="right">Value</th>
          <th class="right">Tolerance</th>
          <th class="right">Status</th>
        </tr>
      </thead>
      <tbody>{rows_html}
      </tbody>
    </table>
    {figures_html}"""


def _render_sim_card(result: SimResult) -> str:
    header = f"""
    <div class="card-header">
      <span class="sim-name">{result.name}</span>
      <span class="sim-status">{_status_badge(result.status)}</span>
    </div>"""

    body_parts = []
    for sub in result.subsections:
        if sub.kind == "field":
            body_parts.append(_render_field_section(sub))
        elif sub.kind in ("statistics", "station"):
            body_parts.append(_render_statistics_section(sub))
        else:
            body_parts.append(f'<div class="section-label">{sub.label}</div><p class="dim">No detail available.</p>')

    if result.notes:
        body_parts.append(f'<pre class="dim" style="margin-top:0.5rem;font-size:0.8rem">{result.notes.strip()}</pre>')

    body = "".join(body_parts) or '<p class="dim">No postprocess results.</p>'
    return f'<div class="card">{header}<div class="card-body">{body}</div></div>'


# ---------------------------------------------------------------------------
# Summary table
# ---------------------------------------------------------------------------


def _render_summary(results: list[SimResult]) -> str:
    kinds = [k for k in ("field", "station", "statistics") if any(r.subsection(k) for r in results)]
    kind_labels = {"field": "Field", "station": "Station", "statistics": "Statistics"}

    extra_headers = "".join(f"<th>{kind_labels[k]}</th>" for k in kinds)
    rows_html = ""
    for r in results:
        extra_cells = "".join(f'<td class="center">{_subsection_summary(r.subsection(k))}</td>' for k in kinds)
        rows_html += f"""
        <tr>
          <td class="mono">{r.name}</td>
          <td>{_status_badge(r.status)}</td>
          {extra_cells}
        </tr>"""

    return f"""
    <h2>Summary</h2>
    <table class="summary-table">
      <thead>
        <tr>
          <th>Simulation</th>
          <th>Status</th>
          {extra_headers}
        </tr>
      </thead>
      <tbody>{rows_html}
      </tbody>
    </table>"""


# ---------------------------------------------------------------------------
# Public entry points
# ---------------------------------------------------------------------------


def generate(results: list[SimResult], meta: ReportMeta, output_path: Path) -> Path:
    """Write a self-contained HTML report and return the path."""
    ref_tag = f"{meta.ref_branch}@{meta.ref_hash[:8]}" if meta.ref_hash else meta.ref_branch
    dev_tag = f"{meta.dev_branch}@{meta.dev_hash[:8]}" if meta.dev_hash else meta.dev_branch

    sim_cards = "\n".join(_render_sim_card(r) for r in results)

    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>FUNWAVE Regression Report</title>
  <style>{_CSS}</style>
  <script src="https://cdn.plot.ly/plotly-2.35.2.min.js" charset="utf-8"></script>
</head>
<body>
  <div class="page-header">
    <h1>FUNWAVE Regression Report</h1>
    <button class="theme-toggle" id="theme-toggle" onclick="toggleTheme()">Light</button>
  </div>
  <div class="meta">
    <span>ref: <code>{ref_tag}</code></span>
    <span>dev: <code>{dev_tag}</code></span>
    <span>generated: <code>{meta.generated_at}</code></span>
  </div>

  {_render_summary(results)}

  <h2>Details</h2>
  {sim_cards}

  <script>{_TOGGLE_JS}</script>
</body>
</html>"""

    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(html, encoding="utf-8")
    return output_path


def generate_pdf(results: list[SimResult], meta: ReportMeta, output_path: Path) -> Path:
    """Write a PDF report (via WeasyPrint) and return the path."""
    import weasyprint

    html_path = generate(results, meta, Path(output_path).with_suffix(".html"))
    pdf_path = Path(output_path)
    pdf_path.parent.mkdir(parents=True, exist_ok=True)
    weasyprint.HTML(filename=str(html_path)).write_pdf(str(pdf_path))
    return pdf_path
