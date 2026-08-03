"""
Channel-statistics oracle: no-reference postprocessor recomputing the
windowed channel products from the snapshot stream.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) -> SubsectionResult

The deck runs fixed dt with the fields channel's snapshot interval EQUAL
to dt, so the snapshot frames are exactly the sample set the statistics
accumulators ingested.  Each stat window (interval T, edges at t_start +
k*T) covers the samples in (edge_{k-1}, edge_k]; with uniform dt the
dt-weighted mean is the plain average and std is about the window mean —
recomputed here in float64 and asserted against the engine's shifted-
moment float32 products at roundoff-level tolerance.  hsig checks
4.004 * std(eta) (the Rayleigh H_1/3 constant).  ref_dir is unused
(oracle mode).

Tolerance keys (tolerances: channel_stats:):
  rtol — relative tolerance on each product (default 1e-4)
"""

from __future__ import annotations

from pathlib import Path

import numpy as np

from test.framework.results import MetricResult, SubsectionResult
from test.regression.postproc.utils import read_run_metadata

# deck constants (keep in lockstep with channel_stats.yaml)
DT = 0.02
WIN = 1.0
STATS_VARS = ("eta", "u")


def run(
    ref_dir: str | Path | None,
    dev_dir: str | Path,
    tolerances: dict,
    plots_dir: Path,
    verbose: bool = False,
) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    tol = tolerances.get("channel_stats", {}) if tolerances else {}
    rtol = float(tol.get("rtol", 1e-4))

    meta = read_run_metadata(dev_dir)
    metrics: list[MetricResult] = []
    per_win = round(WIN / DT)

    fields = {v: meta.output_files(v) for v in STATS_VARS}
    for var in STATS_VARS:
        snaps = fields[var]
        if not snaps:
            metrics.append(MetricResult(variable=var, stat="snapshots", value=0.0,
                                        passed=False, tolerance=1.0))
            continue
        # frame k sits at t = k*DT (counter base _00000 = t 0); window j
        # covers frames (j-1)*per_win+1 .. j*per_win inclusive
        data = np.stack([meta.read_field(p).astype(np.float64) for p in snaps])
        for stat in ("mean", "std"):
            prods = meta.output_files(f"{var}_{stat}")
            for w, prod in enumerate(prods, start=1):
                # field channels count from the legacy _00000 base and the
                # degenerate first flush is dropped, so _0000w closes window w
                # spanning frames (w-1)*per_win+1 .. w*per_win
                lo, hi = (w - 1) * per_win + 1, w * per_win
                if hi >= len(data):
                    break
                sample = data[lo:hi + 1]
                want = sample.mean(axis=0) if stat == "mean" else sample.std(axis=0)
                got = meta.read_field(prod).astype(np.float64)
                denom = max(np.abs(want).max(), 1e-12)
                rel = float(np.abs(got - want).max() / denom)
                metrics.append(MetricResult(variable=f"{var}_{stat}", stat=f"w{w}",
                                            value=rel, passed=rel <= rtol,
                                            tolerance=rtol))

    # hsig = 4.004 std(eta) per window
    for w, prod in enumerate(meta.output_files("hsig"), start=1):
        lo, hi = (w - 1) * per_win + 1, w * per_win
        data = np.stack([meta.read_field(p).astype(np.float64)
                         for p in fields["eta"]])
        if hi >= len(data):
            break
        want = 4.004 * data[lo:hi + 1].std(axis=0)
        got = meta.read_field(prod).astype(np.float64)
        denom = max(np.abs(want).max(), 1e-12)
        rel = float(np.abs(got - want).max() / denom)
        metrics.append(MetricResult(variable="hsig", stat=f"w{w}", value=rel,
                                    passed=rel <= rtol, tolerance=rtol))

    if not metrics:
        metrics.append(MetricResult(variable="channels", stat="found", value=0.0,
                                    passed=False, tolerance=1.0))
    return SubsectionResult(kind="statistics", label="channel-stats oracle", metrics=metrics)
