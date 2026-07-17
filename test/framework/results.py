from __future__ import annotations
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Literal


@dataclass
class MetricResult:
    variable: str
    stat: str  # "L2_mean", "L2_max", "rmse", "max_abs"
    value: float
    passed: bool
    tolerance: float


@dataclass
class InteractiveFigure:
    # Serialized figure for HTML embedding.
    # plotly: fig.to_json()
    # bokeh:  json.dumps(bokeh.embed.json_item(plot.state))  — also covers HoloViews/Bokeh backend
    kind: Literal["plotly", "bokeh"]
    json_str: str  # default (dark) theme JSON
    alt_json_str: str = ""  # light theme JSON; enables theme switching when set


@dataclass
class FigureSpec:
    title: str = ""
    png_path: Path | None = None  # PDF path (matplotlib or kaleido raster)
    interactive: InteractiveFigure | None = None  # HTML embed


@dataclass
class SubsectionResult:
    kind: Literal["field", "station", "statistics"]
    label: str
    metrics: list[MetricResult] = field(default_factory=list)
    figures: list[FigureSpec] = field(default_factory=list)

    @property
    def n_passed(self) -> int:
        return sum(1 for m in self.metrics if math.isfinite(m.tolerance) and m.passed)

    @property
    def n_total(self) -> int:
        return sum(1 for m in self.metrics if math.isfinite(m.tolerance))

    @property
    def summary(self) -> str:
        """Rich-markup string for the summary table cell (e.g. '[green]3/3[/green]')."""
        if not self.metrics:
            return "—"
        color = "green" if self.n_passed == self.n_total else "red"
        return f"[{color}]{self.n_passed}/{self.n_total}[/{color}]"


@dataclass
class SimResult:
    name: str
    # PASS | FAIL | XFAIL | XPASS | SIM_FAILED | POSTPROCESS_ERROR | COMPLETED
    status: str
    subsections: list[SubsectionResult] = field(default_factory=list)
    notes: str = ""
    ref_dir: str = ""
    dev_dir: str = ""

    def subsection(self, kind: str) -> SubsectionResult | None:
        return next((s for s in self.subsections if s.kind == kind), None)
