"""Statistics-channel comparison: field_compare over the <var>_<stat> prefixes."""

from __future__ import annotations

from pathlib import Path

from test.framework.results import SubsectionResult
from test.regression.postproc.field_compare import run as _run


def run(ref_dir: str | Path, dev_dir: str | Path, tolerances: dict, plots_dir: Path, verbose: bool = False) -> SubsectionResult:
    """Compare the statistics-channel products of the two runs."""
    return _run(ref_dir, dev_dir, tolerances, plots_dir, verbose, kind="statistics")
