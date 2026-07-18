"""Hotstart self-consistency comparator.

Compares the core.bin a continuous run leaves against the one a
checkpoint+restart pair leaves at the same simulation time.  Unlike the
field/statistics post-processors this is NOT a dev-vs-ref comparison — both
core.bin files come from the SAME (dev) build; ref_dir holds the continuous
leg, dev_dir holds the restart leg.  A per-field max-abs difference under
tolerance is a PASS (0 == bitwise, which the NSWE case must hit).

core.bin layout (see model_checkpoint_mod):
    header : ver(int32) M(int32) N(int32) time(SP)
    body   : eta p q u v hu hv mask mask9 pflux qflux   (each M*N, SP)
SP (float32/float64, set by USE_DOUBLE) is inferred from the file size so the
reader does not need to know the build precision.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np

from test.framework.results import SubsectionResult, MetricResult

FIELDS = ["eta", "p", "q", "u", "v", "hu", "hv", "mask", "mask9", "pflux", "qflux"]
NFIELD = len(FIELDS)


def _read_core_bin(path: Path) -> tuple[float, dict[str, np.ndarray]]:
    raw = path.read_bytes()
    ver, m, n = np.frombuffer(raw[:12], np.int32)
    cells = int(m) * int(n)
    # total = 12 (3 int32) + sp*(1 time + NFIELD*cells); solve for sp bytes
    sp = (len(raw) - 12) // (1 + NFIELD * cells)
    if sp not in (4, 8):
        raise ValueError(f"{path}: cannot infer SP precision (got {sp} bytes/word)")
    ftype = np.float32 if sp == 4 else np.float64
    time = float(np.frombuffer(raw[12:12 + sp], ftype)[0])
    body = np.frombuffer(raw[12 + sp:], ftype)
    return time, {f: body[i * cells:(i + 1) * cells] for i, f in enumerate(FIELDS)}


def run(ref_dir, dev_dir, tolerances: dict, plots_dir=None, verbose: bool = False) -> SubsectionResult:
    # `chk` is the harness convention for the checkpoint dir inside each leg's
    # run dir; a `checkpoint_subdir` tolerance key overrides it.
    subdir = tolerances.get("checkpoint_subdir", "chk")
    ref_bin = Path(ref_dir) / subdir / "core.bin"
    dev_bin = Path(dev_dir) / subdir / "core.bin"

    sub = SubsectionResult(kind="field", label="hotstart round-trip")

    t_ref, a = _read_core_bin(ref_bin)
    t_dev, b = _read_core_bin(dev_bin)

    # time must match exactly — a mismatch means the legs stopped at different
    # steps and the field comparison would be meaningless
    sub.metrics.append(MetricResult(
        variable="time", stat="max_abs", value=abs(t_ref - t_dev),
        passed=(t_ref == t_dev), tolerance=0.0,
    ))

    default_tol = tolerances.get("default", 1.0e-5)
    for f in FIELDS:
        d = float(np.max(np.abs(a[f] - b[f]))) if a[f].size else 0.0
        tol = tolerances.get(f, default_tol)
        sub.metrics.append(MetricResult(
            variable=f, stat="max_abs", value=d, passed=(d <= tol), tolerance=tol,
        ))
    return sub
