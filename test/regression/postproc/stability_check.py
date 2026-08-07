"""
Stability-sentinel postprocessor: no-reference oracle asserting the run
stayed on the stable branch.

Entry point: run(ref_dir, dev_dir, tolerances, plots_dir, verbose) -> SubsectionResult

To detect a blow-up we use the adaptive-dt tell established by the 2026-07
damping mission: a stable run of the sentinel deck holds dt at the fixed
value (or its routine CFL halvings), while an instability cascades the step
below 1e-2 within a few seconds of onset; we additionally require the final
eta field to be finite and bounded.  ref_dir is unused (oracle mode — the
sim's exe entry carries no ref branch).

Tolerance keys in regression_config.yaml (under tolerances: stability:):
  min_dt      — floor the observed dt must never cross (default: 0.011)
  max_abs_eta — bound on |eta| in the final frame, metres (default: 30.0)
  max_abs_vel — optional bound on |u|/|v| in the final frame, m/s; checked
                only when the key is present.  Quiescence gates (lake-at-rest
                decks) need it — a bounded self-excited velocity mode can sit
                well under any eta cap while the dt never dips.
"""

from __future__ import annotations

import math
import re
from pathlib import Path

import numpy as np

from test.framework.results import MetricResult, SubsectionResult
from test.regression.postproc.utils import read_run_metadata

_DT_RE = re.compile(r"dt =\s*([0-9.Ee+-]+)")


def run(
    ref_dir: str | Path | None,
    dev_dir: str | Path,
    tolerances: dict,
    plots_dir: Path,
    verbose: bool = False,
) -> SubsectionResult:
    dev_dir = Path(dev_dir)
    # The runner hands over the per-kind dict (tolerances["stability"]) already;
    # tolerate the outer-dict shape too so direct invocations keep working.
    tol = tolerances or {}
    if "stability" in tol and isinstance(tol["stability"], dict):
        tol = tol["stability"]
    min_dt_floor = float(tol.get("min_dt", 0.011))
    eta_cap = float(tol.get("max_abs_eta", 30.0))

    metrics: list[MetricResult] = []

    # ---- dt floor from the run log ----------------------------------
    log = dev_dir / "funwave.log"
    dts = []
    if log.exists():
        for line in log.read_text(errors="replace").splitlines():
            m = _DT_RE.search(line)
            if m:
                try:
                    dts.append(float(m.group(1)))
                except ValueError:
                    pass
    min_dt = min(dts) if dts else float("nan")
    metrics.append(
        MetricResult(
            variable="dt",
            stat="min",
            value=min_dt,
            passed=bool(dts) and min_dt >= min_dt_floor,
            tolerance=min_dt_floor,
        )
    )

    # ---- final eta finite and bounded -------------------------------
    meta = read_run_metadata(dev_dir)
    eta_files = meta.output_files("eta")
    if eta_files:
        eta = meta.read_field(eta_files[-1])
        finite = bool(np.isfinite(eta).all())
        max_abs = float(np.abs(eta[np.isfinite(eta)]).max()) if finite or np.isfinite(eta).any() else math.inf
        metrics.append(
            MetricResult(
                variable="eta",
                stat="max_abs",
                value=max_abs,
                passed=finite and max_abs <= eta_cap,
                tolerance=eta_cap,
            )
        )
    else:
        metrics.append(
            MetricResult(variable="eta", stat="max_abs", value=math.inf, passed=False, tolerance=eta_cap)
        )

    # ---- optional velocity quiescence (final u/v frames) -------------
    if "max_abs_vel" in tol:
        vel_cap = float(tol["max_abs_vel"])
        for var in ("u", "v"):
            files = meta.output_files(var)
            if files:
                fld = meta.read_field(files[-1])
                finite = bool(np.isfinite(fld).all())
                max_abs = float(np.abs(fld[np.isfinite(fld)]).max()) if np.isfinite(fld).any() else math.inf
                metrics.append(
                    MetricResult(
                        variable=var,
                        stat="max_abs",
                        value=max_abs,
                        passed=finite and max_abs <= vel_cap,
                        tolerance=vel_cap,
                    )
                )
            else:
                metrics.append(
                    MetricResult(variable=var, stat="max_abs", value=math.inf, passed=False, tolerance=vel_cap)
                )

    return SubsectionResult(kind="statistics", label="stability sentinel", metrics=metrics)
