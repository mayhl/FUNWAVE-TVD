"""
Shared closed-basin seiche run harness for the dispersion / isotropy validators.

Both validators excite an INI_SINE standing wave in a closed flat basin, read the
single near-corner station time series, and measure its oscillation period.  The
only per-validator differences are the basin shape (1D strip vs square) and the
mode numbers (mode_x, mode_y), so the deck template, the ~12-period run-length
rule, the run/convert/extract mechanics, and the Nwogu/Airy dispersion curves all
live here.
"""

from __future__ import annotations

import math
import shutil
import subprocess
import sys
from pathlib import Path

import numpy as np

from test.validation.oracles.dispersion import (
    BETA_REF_DEFAULT,
    _boussinesq_period,
    _extract_period,
)

REPO_ROOT = Path(__file__).resolve().parents[2]
BINARY = REPO_ROOT / "workspaces" / "dev" / "validation-2d" / "funwave"
CONVERT = REPO_ROOT / "scripts" / "convert_input.py"


# ---------------------------------------------------------------------------
# Dispersion curves (C / sqrt(g h) as a function of kh)
# ---------------------------------------------------------------------------

def _cg_nwogu(kh):
    """Nwogu C/sqrt(gh) as a function of kh (scalar or array)."""
    alpha = 0.5 * BETA_REF_DEFAULT ** 2 + BETA_REF_DEFAULT
    return np.sqrt((1.0 - (alpha + 1.0 / 3.0) * kh**2) / (1.0 - alpha * kh**2))


def _cg_airy(kh):
    """Exact linear (Airy) C/sqrt(gh) = sqrt(tanh(kh)/kh)."""
    return np.sqrt(np.tanh(kh) / kh)


# ---------------------------------------------------------------------------
# Run harness
# ---------------------------------------------------------------------------

_DECK = """\
! Auto-generated seiche validation case; do not edit by hand.
TITLE = {title}

PX = 1
PY = 1

DEPTH_TYPE = FLAT
DEPTH_FLAT = {h:.6f}

Mglob = {mglob}
Nglob = {nglob}
DX = {dx:.6f}
DY = {dy:.6f}

TOTAL_TIME = {total_time:.4f}
PLOT_INTV = {total_time:.4f}
SCREEN_INTV = {total_time:.4f}

WAVEMAKER = INI_SINE
AMP = 0.01
mode_x = {mode_x}
mode_y = {mode_y}

PERIODIC = F

DIFFUSION_SPONGE = F
FRICTION_SPONGE = F
DIRECT_SPONGE = F

Cd = 0.0
VISCOSITY_BREAKING = F

CFL = 0.5
FroudeCap = 3.0
MinDepth = 0.01

NumberStations = 1
STATIONS_FILE = stations.txt
PLOT_INTV_STATION = {dt_sta:.6f}

DEPTH_OUT = T
ETA = T
"""


def run_length(h: float, k: float) -> tuple[float, float]:
    """(total_time, dt_sta): ~12 Nwogu periods, station sampled ~60x per period."""
    _, t_nwogu = _boussinesq_period(h, 2.0 * math.pi / k, BETA_REF_DEFAULT)
    return max(40.0, 12.0 * t_nwogu), t_nwogu / 60.0


def run_seiche(run_dir: Path, *, title: str, h: float, mglob: int, nglob: int,
               dx: float, dy: float, mode_x: int, mode_y: int,
               total_time: float, dt_sta: float, station: str = "2 2",
               verbose: bool = False) -> float | None:
    """Write the deck, convert, run (1 rank), read station 1; return its period (s).

    Returns None on convert/run failure or an unmeasurable series.
    """
    if run_dir.exists():
        shutil.rmtree(run_dir)
    out_dir = run_dir / "output"
    out_dir.mkdir(parents=True)

    (run_dir / "input.txt").write_text(
        _DECK.format(title=title, h=h, mglob=mglob, nglob=nglob, dx=dx, dy=dy,
                     mode_x=mode_x, mode_y=mode_y, total_time=total_time, dt_sta=dt_sta)
    )
    (run_dir / "stations.txt").write_text(station + "\n")

    conv = subprocess.run(
        ["uv", "run", "python", str(CONVERT), "input.txt", "input.yaml"],
        cwd=run_dir, capture_output=True, text=True,
    )
    if conv.returncode != 0:
        if verbose:
            print(f"  [{title}] convert failed:\n{conv.stderr}", file=sys.stderr)
        return None

    run = subprocess.run(
        ["mpirun", "-np", "1", str(BINARY), "input.yaml"],
        cwd=run_dir, capture_output=True, text=True,
    )
    sta = out_dir / "sta_0001"
    if run.returncode != 0 or not sta.exists():
        if verbose:
            print(f"  [{title}] run failed (rc={run.returncode}):\n"
                  f"{run.stdout[-400:]}\n{run.stderr[-400:]}", file=sys.stderr)
        return None

    arr = np.loadtxt(sta)
    if arr.ndim == 1:
        arr = arr.reshape(1, -1)
    t_start = max(0.0, float(arr[-1, 0]) * 0.2)  # drop the first 20 % as start-up
    return _extract_period(arr, t_start=t_start)
