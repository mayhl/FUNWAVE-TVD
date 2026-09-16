"""
Shared closed-basin seiche run harness for the dispersion / isotropy validators.

Both validators excite an INI_SINE standing wave in a closed flat basin, read the
single near-corner station time series, and measure its oscillation period.  The
only per-validator differences are the basin shape (1D strip vs square) and the
mode numbers (mode_x, mode_y), so the deck template, the ~12-period run-length
rule, the run/extract mechanics, and the Nwogu/Airy dispersion curves all
live here.  Decks are authored directly in the YAML schema (input.txt is
retired for 2D).
"""

from __future__ import annotations

import math
import shutil
import subprocess
import sys
from pathlib import Path

import numpy as np

from test.validation.oracles import wave_stats
from test.validation.oracles.dispersion import (
    BETA_REF_DEFAULT,
    _boussinesq_period,
    _extract_period,
)

REPO_ROOT = Path(__file__).resolve().parents[2]
BINARY = REPO_ROOT / "workspaces" / "dev" / "validation-2d" / "funwave"


# ---------------------------------------------------------------------------
# Dispersion curves (C / sqrt(g h) as a function of kh)
# ---------------------------------------------------------------------------


def _cg_nwogu(kh):
    """Nwogu C/sqrt(gh) as a function of kh (scalar or array)."""
    alpha = 0.5 * BETA_REF_DEFAULT**2 + BETA_REF_DEFAULT
    return np.sqrt((1.0 - (alpha + 1.0 / 3.0) * kh**2) / (1.0 - alpha * kh**2))


def _cg_airy(kh):
    """Exact linear (Airy) C/sqrt(gh) = sqrt(tanh(kh)/kh)."""
    return np.sqrt(np.tanh(kh) / kh)


# ---------------------------------------------------------------------------
# Run harness
# ---------------------------------------------------------------------------

_DECK = """\
# Auto-generated seiche validation case; do not edit by hand.
grid:
  cell_size: [{dx:.6f}, {dy:.6f}]
  n_cells: [{mglob}, {nglob}]
  n_procs: [1, 1]
  bathymetry:
    type: flat
    depth: {h:.6f}
simulation:
  title: {title}
  total_time: {total_time:.4f}
  screen_interval: {total_time:.4f}
initial:
  sine_mode:
    amplitude: 0.01
    mode_x: {mode_x}
    mode_y: {mode_y}
numerics:
  cfl: 0.5
  froude_cap: 3.0
  min_depth: 0.01
breaking:
  model: shock_capturing
output:
  format: ascii
  channels:
    - name: sta
      type: station
      x: [{sta_x:.6f}]
      y: [{sta_y:.6f}]
      variables: [eta]
      interval: {dt_sta:.6f}
  depth_out: true
"""


def run_length(h: float, k: float) -> tuple[float, float]:
    """(total_time, dt_sta): ~12 Nwogu periods, station sampled ~60x per period."""
    _, t_nwogu = _boussinesq_period(h, 2.0 * math.pi / k, BETA_REF_DEFAULT)
    return max(40.0, 12.0 * t_nwogu), t_nwogu / 60.0


def run_seiche(
    run_dir: Path,
    *,
    title: str,
    h: float,
    mglob: int,
    nglob: int,
    dx: float,
    dy: float,
    mode_x: int,
    mode_y: int,
    total_time: float,
    dt_sta: float,
    station: str = "2 2",
    verbose: bool = False,
) -> float | None:
    """Write the YAML deck, run (1 rank), read station 1; return its period (s).

    Returns None on a run failure or an unmeasurable series.
    """
    if run_dir.exists():
        shutil.rmtree(run_dir)
    out_dir = run_dir / "output"
    out_dir.mkdir(parents=True)

    # legacy station strings are 1-based grid indices "i j"
    i_sta, j_sta = (int(v) for v in station.split())
    (run_dir / "input.yaml").write_text(
        _DECK.format(
            title=title,
            h=h,
            mglob=mglob,
            nglob=nglob,
            dx=dx,
            dy=dy,
            mode_x=mode_x,
            mode_y=mode_y,
            total_time=total_time,
            dt_sta=dt_sta,
            sta_x=(i_sta - 1) * dx,
            sta_y=(j_sta - 1) * dy,
        )
    )

    run = subprocess.run(
        ["mpirun", "-np", "1", str(BINARY), "input.yaml"],
        cwd=run_dir,
        capture_output=True,
        text=True,
    )
    try:
        t, v = wave_stats.read_point_channel(out_dir, "eta", channel="sta")
    except FileNotFoundError:
        t = None
    if run.returncode != 0 or t is None:
        if verbose:
            print(f"  [{title}] run failed (rc={run.returncode}):\n{run.stdout[-400:]}\n{run.stderr[-400:]}", file=sys.stderr)
        return None

    arr = np.column_stack([t, v])
    t_start = max(0.0, float(arr[-1, 0]) * 0.2)  # drop the first 20 % as start-up
    return _extract_period(arr, t_start=t_start)
