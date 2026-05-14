from __future__ import annotations
from collections import defaultdict
from dataclasses import dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING, Literal
import re
import yaml

if TYPE_CHECKING:
    import numpy as np


# ---------------------------------------------------------------------------
# Output file prefix catalogues (sourced from src/old/io.F)
# All 2D snapshot files use prefix_%05d naming.
# Station files use sta_%04d.
# dep.out is the only static exception (written once at init for DEPTH_OUT).
# ---------------------------------------------------------------------------

# Prefixes written at each PLOT_INTV (time-evolving field snapshots).
# Includes sediment transport outputs — same %05d indexing, no reason to separate.
FIELD_PREFIXES: frozenset[str] = frozenset([
    "eta", "etasrn",
    "u", "v",
    "mask", "mask9",
    "hmax", "hmin", "umax", "MFmax", "VORmax",
    "p", "q",
    "age",
    "roller", "U_undertow", "V_undertow", "nubrk",
    "FrcInsX", "FrcInsY", "BrkSrcX", "BrkSrcY",
    "time",
    "Pstorm", "Ustorm", "Vstorm",
    "Fves", "Pves", "VesUp", "VesVp",
    "tmp",
    "Ax", "Ay", "Bx", "By",
    # sediment
    "dep",
    "C", "Pick", "Depo", "Pavg", "Davg",
    "DchgS", "DchgB", "BedFx", "BedFy", "BedStr",
    "Aval", "AvalAc", "Cb", "Ca", "Redu", "TauEx", "Hpo",
    "FoamEta",
])

# Binary mask fields — use mismatch fraction instead of normalized L2.
MASK_PREFIXES: frozenset[str] = frozenset({"mask", "mask9"})

# Absolute floor on the reference L2 norm to prevent division by near-zero.
# When max_t ||ref(t)|| < DEFAULT_FLOOR the metric falls back to absolute L2.
# Matched to the default tolerance so the fallback pass condition is
# ||diff|| < floor * tol = 1e-4 * 1e-4 = 1e-8, consistent with relative intent.
DEFAULT_FLOOR: float = 1e-4

# Prefixes written at T_INTV_mean after STEADY_TIME (wave-averaged statistics).
STATS_PREFIXES: frozenset[str] = frozenset([
    "umean", "vmean", "etamean",
    "ulagm", "vlagm",
    "Hrms", "Havg", "Hsig",
    "Sxx", "Sxy", "Syy",
    "DxSxx", "DySxy", "DySyy", "DxSxy",
    "PgrdX", "PgrdY",
    "DxUUH", "DyUVH", "DyVVH", "DxUVH",
    "FRCX", "FRCY",
    "BrkDissX", "BrkDissY",
])

# Canonical variable name (from input flags) → primary output file prefix.
# WaveHeight produces three files (Hrms, Havg, Hsig); Hrms is listed as primary.
# DEPTH_OUT → dep.out (static, not a %05d series).
VAR_TO_PREFIX: dict[str, str] = {
    "ETA":        "eta",
    "ETAscreen":  "etasrn",
    "U":          "u",
    "V":          "v",
    "Umean":      "umean",
    "Vmean":      "vmean",
    "ETAmean":    "etamean",
    "MASK":       "mask",
    "MASK9":      "mask9",
    "Hmax":       "hmax",
    "Hmin":       "hmin",
    "MFmax":      "MFmax",
    "Umax":       "umax",
    "VORmax":     "VORmax",
    "WaveHeight": "Hrms",    # also produces Havg, Hsig
    "P":          "p",
    "Q":          "q",
}

# Variables that produce a static file rather than a %05d series.
STATIC_FILES: dict[str, str] = {
    "DEPTH_OUT": "dep.out",
}

# All txt input flag names recognised as output variables.
_TXT_VAR_FLAGS = [
    "DEPTH_OUT", "U", "V", "ETA", "ETAscreen",
    "Hmax", "Hmin", "MFmax", "Umax", "VORmax",
    "Umean", "Vmean", "ETAmean",
    "MASK", "MASK9",
    "SXL", "SXR", "SYL", "SYR",
    "SourceX", "SourceY",
    "P", "Q", "Fx", "Fy", "Gx", "Gy",
    "AGE", "TMP", "WaveHeight",
]

_TSERIES_RE = re.compile(r"^(.+)_(\d{5})$")
_STATION_RE = re.compile(r"^sta_(\d{4})$")


# ---------------------------------------------------------------------------
# VariableInfo — prefix + timestep range discovered from output directory
# ---------------------------------------------------------------------------

@dataclass
class VariableInfo:
    prefix: str
    first: int        # first valid timestep index
    last: int         # last valid timestep index (99999 excluded)
    unstable: bool    # True if a prefix_99999 file exists (blow-up sentinel)

    @property
    def count(self) -> int:
        """Expected number of files assuming continuous indexing first..last."""
        return self.last - self.first + 1


# ---------------------------------------------------------------------------
# RunMetadata
# ---------------------------------------------------------------------------

@dataclass
class RunMetadata:
    run_dir: Path
    output_dir: Path
    nx: int           # Mglob / grid_size[0]
    ny: int           # Nglob / grid_size[1]
    dx: float
    dy: float
    binary: bool = False      # True if FIELD_IO_TYPE = BINARY; default is ASCII
    output_res: int = 1       # OUTPUT_RES stride (ASCII only; binary is always full res)
    variables: list[str] = field(default_factory=list)  # canonical names from input

    def output_files(self, variable: str) -> list[Path]:
        """Sorted list of output files for a variable.

        For DEPTH_OUT returns [dep.out] if present.
        For all others globs prefix_%05d files.
        """
        static = STATIC_FILES.get(variable)
        if static:
            p = self.output_dir / static
            return [p] if p.exists() else []
        prefix = VAR_TO_PREFIX.get(variable, variable)
        return sorted(self.output_dir.glob(f"{prefix}_[0-9][0-9][0-9][0-9][0-9]"))

    def read_field(self, path: Path) -> "np.ndarray":
        """Read a 2D field file and return an (ny, nx) float32 array.

        Binary: raw float32, Fortran column-major (MPI_ORDER_FORTRAN), no record
                markers — must reshape (nx, ny) then transpose to get (ny, nx).
        ASCII:  one row per J, Mglob values per row — np.loadtxt infers shape.
        """
        import numpy as np
        if self.binary:
            n = self.nx * self.ny
            dtype = np.float64 if path.stat().st_size == n * 8 else np.float32
            return (
                np.fromfile(path, dtype=dtype, count=n)
                .reshape((self.nx, self.ny), order="F")
                .T
            )
        return np.loadtxt(path, dtype=np.float32)


# ---------------------------------------------------------------------------
# Variable discovery from output directory
# ---------------------------------------------------------------------------

OutputKind = Literal["field", "statistics", "station"]


def get_output_variables(
    output_dir: str | Path,
    kind: OutputKind | None = None,
) -> list[VariableInfo]:
    """Return sorted list of VariableInfo for each prefix found in output_dir.

    Each VariableInfo carries the prefix, first/last valid timestep index, and
    an unstable flag set independently when a prefix_99999 file is present.
    The range first..last is expected to be continuous; only the bounds are reported.

    Args:
        output_dir: Path to the simulation output directory.
        kind: Filter by output category.
              "field"      – time-evolving snapshots, including sediment (FIELD_PREFIXES)
              "statistics" – wave-averaged mean fields (STATS_PREFIXES)
              "station"    – sta_%04d files only
              None         – all prefix_%05d files, unfiltered
    """
    output_dir = Path(output_dir)

    if kind == "station":
        suffixes = [
            int(m.group(1))
            for f in output_dir.iterdir()
            if (m := _STATION_RE.match(f.name))
        ]
        if not suffixes:
            return []
        return [VariableInfo("sta", min(suffixes), max(suffixes))]

    _KIND_MAP: dict[str, frozenset[str]] = {
        "field":      FIELD_PREFIXES,
        "statistics": STATS_PREFIXES,
    }
    allowed = _KIND_MAP.get(kind) if kind else None

    prefix_suffixes: dict[str, list[int]] = defaultdict(list)
    for f in output_dir.iterdir():
        m = _TSERIES_RE.match(f.name)
        if not m:
            continue
        prefix = m.group(1)
        if allowed is None or prefix in allowed:
            prefix_suffixes[prefix].append(int(m.group(2)))

    result = []
    for prefix, s in sorted(prefix_suffixes.items()):
        unstable = 99999 in s
        valid = [x for x in s if x != 99999]
        if valid:
            result.append(VariableInfo(prefix, min(valid), max(valid), unstable=unstable))
        else:
            # blow-up on first output step — no valid files at all
            result.append(VariableInfo(prefix, 99999, 99999, unstable=True))
    return result


# ---------------------------------------------------------------------------
# Input file parsers
# ---------------------------------------------------------------------------

def read_run_metadata(run_dir: str | Path) -> RunMetadata:
    """Parse run metadata from the first yaml or txt input file found in run_dir."""
    run_dir = Path(run_dir)

    yaml_files = sorted(run_dir.glob("*.yaml"))
    txt_files  = sorted(p for p in run_dir.glob("*.txt") if p.name != "LOG.txt")

    if yaml_files:
        return _from_yaml(run_dir, yaml_files[0])
    if txt_files:
        return _from_txt(run_dir, txt_files[0])
    raise FileNotFoundError(f"No .yaml or .txt input file found in {run_dir}")


def _from_yaml(run_dir: Path, path: Path) -> RunMetadata:
    with open(path) as f:
        cfg = yaml.safe_load(f)

    geo = cfg.get("geometry", {})
    out = cfg.get("output", {})

    grid_size = geo.get("grid_size", [0, 0])    # [nx, ny]
    cell_size = geo.get("cell_size", [1.0, 1.0]) # [dx, dy]

    result_folder = out.get("result_folder", "output").rstrip("/")
    output_dir = (run_dir / result_folder).resolve()

    variables = list(out.get("variables", []))
    if out.get("depth_out", False):
        variables = ["DEPTH_OUT"] + variables

    binary     = out.get("field_io_type", "ASCII").upper() == "BINARY"
    output_res = int(out.get("output_res", 1))

    return RunMetadata(
        run_dir=run_dir,
        output_dir=output_dir,
        nx=int(grid_size[0]),
        ny=int(grid_size[1]),
        dx=float(cell_size[0]),
        dy=float(cell_size[1]),
        binary=binary,
        output_res=output_res,
        variables=variables,
    )


def _from_txt(run_dir: Path, path: Path) -> RunMetadata:
    kv = _parse_txt(path)

    result_folder = kv.get("RESULT_FOLDER", "output").rstrip("/")
    output_dir = (run_dir / result_folder).resolve()

    variables = [
        v for v in _TXT_VAR_FLAGS
        if kv.get(v, "F").upper() in ("T", ".TRUE.", "TRUE")
    ]

    binary     = kv.get("FIELD_IO_TYPE", "ASCII").upper() == "BINARY"
    output_res = int(kv.get("OUTPUT_RES", 1))

    return RunMetadata(
        run_dir=run_dir,
        output_dir=output_dir,
        nx=int(kv.get("Mglob", 0)),
        ny=int(kv.get("Nglob", 0)),
        dx=float(kv.get("DX", 1.0)),
        dy=float(kv.get("DY", 1.0)),
        binary=binary,
        output_res=output_res,
        variables=variables,
    )


_KV_RE = re.compile(r"(\w+)\s*=\s*([^\s!,:;]+)")


# ---------------------------------------------------------------------------
# Per-step error metric computation
# ---------------------------------------------------------------------------

def compute_metric_series(
    ref_meta: RunMetadata,
    dev_meta: RunMetadata,
    prefix: str,
    idx_first: int,
    idx_last: int,
    floor: float = DEFAULT_FLOOR,
) -> "np.ndarray":
    """Compute a per-timestep error metric between ref and dev output fields.

    For mask fields (MASK_PREFIXES): returns mismatch fraction in [0, 1].
    For all others: returns normalised L2,
        ||dev - ref||₂ / max(max_t ||ref(t)||₂, floor),
    where the denominator is the maximum reference norm seen across *all*
    timesteps.  Using the per-step norm instead would blow up for fields that
    start near zero (e.g. hmax before waves arrive).
    """
    import numpy as np
    is_mask = prefix in MASK_PREFIXES
    indices = list(range(idx_first, idx_last + 1))

    if is_mask:
        vals: list[float] = []
        for idx in indices:
            ref_arr = ref_meta.read_field(ref_meta.output_dir / f"{prefix}_{idx:05d}").astype(float)
            dev_arr = dev_meta.read_field(dev_meta.output_dir / f"{prefix}_{idx:05d}").astype(float)
            vals.append(float(np.mean(ref_arr != dev_arr)))
        return np.array(vals)

    # Pass 1: find max reference L2 norm across all timesteps.
    max_norm_ref = 0.0
    for idx in indices:
        ref_arr = ref_meta.read_field(ref_meta.output_dir / f"{prefix}_{idx:05d}").astype(float)
        max_norm_ref = max(max_norm_ref, float(np.sqrt(np.sum(ref_arr ** 2))))
    denom = max(max_norm_ref, floor)

    # Pass 2: compute per-step normalised L2.
    vals = []
    for idx in indices:
        ref_arr = ref_meta.read_field(ref_meta.output_dir / f"{prefix}_{idx:05d}").astype(float)
        dev_arr = dev_meta.read_field(dev_meta.output_dir / f"{prefix}_{idx:05d}").astype(float)
        diff = dev_arr - ref_arr
        vals.append(float(np.sqrt(np.sum(diff ** 2))) / denom)
    return np.array(vals)


def metric_stat_name(prefix: str, agg: str) -> str:
    """Return the MetricResult.stat name for a given prefix and aggregation."""
    kind = "mismatch" if prefix in MASK_PREFIXES else "relL2"
    return f"{kind}_{agg}"


def _parse_txt(path: Path) -> dict[str, str]:
    """Parse FUNWAVE legacy key = value input file.

    Handles multiple key=value pairs on one line and strips ! comments.
    """
    kv: dict[str, str] = {}
    for line in path.read_text().splitlines():
        line = line.split("!")[0]
        for m in _KV_RE.finditer(line):
            kv[m.group(1)] = m.group(2)
    return kv
