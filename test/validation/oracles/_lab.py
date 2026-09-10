"""Shared plumbing for the lab-case checks under test/validation/cases/.

Each case's check.py is a model-vs-truth postproc parameterized by its deck
plus the measured-data files beside it; the per-case binding rides in the
suite's ``tolerances:`` block (the only per-sim channel the postproc
contract provides), so every check shares the same small needs collected
here: deck access, measured-table reading, error norms, field-frame timing
from the per-channel t.out index, and matplotlib figure emission for the
board report.

Note 1: config keys inside a tolerances block that do not end in ``_pct``
are check parameters (file names, alignment options, windows), not gates;
the runner only treats emitted MetricResult tolerances as gates, so the
mixing is safe and keeps each case's config entry self-contained.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import yaml

from test.framework.results import FigureSpec

# ---------------------------------------------------------------------------
# Deck access
# ---------------------------------------------------------------------------


def load_deck(run_dir: Path) -> dict:
    """Return the parsed run deck (the single *.yaml staged in the run dir)."""
    decks = sorted(Path(run_dir).glob("*.yaml"))
    if not decks:
        raise FileNotFoundError(f"no run deck (*.yaml) in {run_dir}")
    with open(decks[0]) as f:
        return yaml.safe_load(f)


# ---------------------------------------------------------------------------
# Error norms
# ---------------------------------------------------------------------------


# SI factor per declared unit; dimensionless and time pass through
_UNIT_FACTORS = {"m": 1.0, "s": 1.0, "-": 1.0, "cm": 0.01, "mm": 0.001}


def read_table(path: Path) -> tuple[np.ndarray, dict[str, str]]:
    """Read a self-describing measured-data file: numeric body plus `# key: value` header.

    The first header line is the dataset title and carries no key; every later
    `# key: value` line lands in the returned dict.  Columns and units are
    declared there, so the caller needs no external schema; a `units:` line
    converts the body to SI on load (cm columns arrive as m).
    """
    head: dict[str, str] = {}
    for line in path.read_text().splitlines():
        if not line.startswith("#"):
            break
        if ":" in line:
            key, _, val = line[1:].partition(":")
            head[key.strip()] = val.strip()
        else:
            head.setdefault("title", line[1:].strip())
    data = np.loadtxt(path, comments="#")
    if "units" in head:
        data = data * np.array([_UNIT_FACTORS[u] for u in head["units"].split()])
    return data, head


def nrmse_pct(measured: np.ndarray, model: np.ndarray, norm: str = "max") -> float:
    """Return 100 * rms(model - measured) / N.

    ``norm`` picks N: "max" (peak |measured|, the published-profile
    convention), "rms" (measured rms, shape-sensitive), or "range".
    """
    m = np.asarray(measured, dtype=float)
    d = np.asarray(model, dtype=float)
    err = float(np.sqrt(np.mean((d - m) ** 2)))
    if norm == "rms":
        ref = float(np.sqrt(np.mean(m**2)))
    elif norm == "range":
        ref = float(m.max() - m.min())
    else:
        ref = float(np.max(np.abs(m)))
    return 100.0 * err / ref if ref > 0.0 else float("inf")


# ---------------------------------------------------------------------------
# Field-frame timing (per-channel t.out: rows of "frame  t  dt")
# ---------------------------------------------------------------------------


def frame_times(files: list[Path]) -> np.ndarray:
    """Return the output time of each field frame in ``files``.

    Field channels write frames <var>_NNNNN next to a t.out index; we map
    each file's NNNNN through its folder's t.out.  Frames without an index
    row fail loud — silent time guessing would corrupt every lab metric.
    """
    if not files:
        return np.empty(0)
    index: dict[Path, dict[int, float]] = {}
    times = []
    for f in files:
        folder = f.parent
        if folder not in index:
            tout = folder / "t.out"
            if not tout.exists():
                raise FileNotFoundError(f"no t.out frame index beside {f}")
            rows = np.loadtxt(tout, ndmin=2)
            index[folder] = {int(r[0]): float(r[1]) for r in rows}
        frame = int(f.name.rsplit("_", 1)[-1])
        if frame not in index[folder]:
            raise KeyError(f"frame {frame} of {f.name} missing from {folder / 't.out'}")
        times.append(index[folder][frame])
    return np.array(times)


# ---------------------------------------------------------------------------
# Figures (matplotlib Agg -> FigureSpec for the HTML/PDF board report)
# ---------------------------------------------------------------------------


def save_figure(fig, plots_dir: Path, name: str, title: str) -> FigureSpec:
    """Rasterize a matplotlib figure into plots_dir and wrap it as a FigureSpec."""
    plots_dir = Path(plots_dir)
    plots_dir.mkdir(exist_ok=True)
    png_path = plots_dir / f"{name}.png"
    fig.savefig(png_path, dpi=150, bbox_inches="tight")
    import matplotlib.pyplot as plt

    plt.close(fig)
    return FigureSpec(title=title, png_path=png_path)


def new_figure(nrows: int = 1, ncols: int = 1, height_per_row: float = 2.2):
    """Return (fig, axes list) on the Agg backend, board-report sized."""
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    fig, axes = plt.subplots(nrows, ncols, figsize=(9.0, max(2.5, height_per_row * nrows)), squeeze=False)
    return fig, [ax for row in axes for ax in row]
