"""Run-output access shared by the comparators and the validation oracles.

RunMetadata resolves a run directory (its deck or legacy input.txt) to the
output folder, grid size and frame format; the readers below list and load
frame series and the prefix catalogues say which prefixes belong to which
comparison kind.
"""

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
# Output file prefix catalogues (registry-derived; see FIELD_PREFIXES)
# All 2D snapshot files use prefix_%05d naming.
# Station files: the 3D vendor ref writes sta_%04d; 2D point channels write <chan>/<var>.dat.
# dep.out is the only static exception (written once at init for DEPTH_OUT).
# ---------------------------------------------------------------------------

# Output prefixes the comparators recognise, derived from the registry
# (src/model/registry.yaml `variables:`) so a new engine field needs no
# edit here: a snapshot writes <name>_NNNNN, a statistics channel writes
# <name>_<stat>_NNNNN.  The extremes and event statistics ride with the
# snapshots (running-envelope channels); the moments are the statistics
# kind.  Threshold-tagged event names (<name>_<stat>_<value>) are not
# listed -- a tolerance on one still fails loudly as "in neither run".
_REGISTRY = Path(__file__).resolve().parents[2] / "src" / "model" / "registry.yaml"


def _registry_names() -> list[str]:
    with open(_REGISTRY) as f:
        return [v["name"] for v in yaml.safe_load(f)["variables"]]


_MOMENT_STATS = ("mean", "rms", "std")
_EXTREME_STATS = ("max", "min", "max_time", "first_time", "last_time", "duration", "duration_max", "count")
# sediment fields the stepper registers without registry metadata, and the
# 3D vendor ref's volumetric prefixes (vendor/3d-fd writes _%04d frames)
_SEDIMENT_PREFIXES = (
    "sediment_c",
    "sediment_pickup",
    "sediment_depo",
    "sediment_bedfx",
    "sediment_bedfy",
    "sediment_bedstr",
    "sediment_pavg",
    "sediment_davg",
    "sediment_dchgs",
    "sediment_dchgb",
    "sediment_aval",
    "sediment_avalac",
)
_VENDOR_3D_PREFIXES = ("w", "tke", "eps", "prod", "mu", "upwp", "sali", "temp", "rho", "b")

_NAMES = _registry_names()
FIELD_PREFIXES: frozenset[str] = frozenset(
    [*_NAMES, *(f"{n}_{st}" for n in _NAMES for st in _EXTREME_STATS), *_SEDIMENT_PREFIXES, *_VENDOR_3D_PREFIXES]
)

# Binary mask fields — use mismatch fraction instead of normalized L2.
MASK_PREFIXES: frozenset[str] = frozenset({"mask", "mask9"})

# Absolute floor on the reference L2 norm to prevent division by near-zero.
# When max_t ||ref(t)|| < DEFAULT_FLOOR the metric falls back to absolute L2.
# Matched to the default tolerance so the fallback pass condition is
# ||diff|| < floor * tol = 1e-4 * 1e-4 = 1e-8, consistent with relative intent.
DEFAULT_FLOOR: float = 1e-4

# Statistics-channel prefixes: the moments of every registry name plus the
# product-derived hsig.
STATS_PREFIXES: frozenset[str] = frozenset([*(f"{n}_{st}" for n in _NAMES for st in _MOMENT_STATS), "hsig"])

# Legacy flag spelling -> output prefix, for the oracles that still ask by
# flag name (ETA/U/V/MASK/P/Q).
VAR_TO_PREFIX: dict[str, str] = {
    "ETA": "eta",
    "U": "u",
    "V": "v",
    "MASK": "mask",
    "MASK9": "mask9",
    "P": "p",
    "Q": "q",
}

# Variables that produce a static file rather than a %05d series.
STATIC_FILES: dict[str, str] = {
    "DEPTH_OUT": "dep.out",
}


_TSERIES_RE = re.compile(r"^(.+)_(\d{4,5})$")  # 5-digit (2D) or 4-digit (3D)
_STATION_RE = re.compile(r"^sta_(\d{4})$")


# ---------------------------------------------------------------------------
# VariableInfo — prefix + timestep range discovered from output directory
# ---------------------------------------------------------------------------


@dataclass
class VariableInfo:
    prefix: str
    first: int  # first valid timestep index
    last: int  # last valid timestep index (99999 excluded)
    unstable: bool  # True if a prefix_99999 file exists (blow-up sentinel)

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
    nx: int  # Mglob / grid_size[0]
    ny: int  # Nglob / grid_size[1]
    dx: float
    dy: float
    binary: bool = False  # True if output.format is binary; default is ASCII
    nz: int = 0  # Kglob for 3D runs; 0 for 2D runs
    # per-prefix parent dir cache for field_path (flat vs channel subfolder)
    _prefix_dirs: dict = field(default_factory=dict)

    @property
    def is_3d(self) -> bool:
        return self.nz > 1

    def field_path(self, prefix: str, idx: int) -> Path:
        """Return the path for a time-series output file.

        3D uses 4-digit suffixes (_%04d); 2D uses 5-digit (_%05d).
        Channels own subfolders since the flags-list retirement — resolve
        the prefix's parent once (flat first, then one channel level) and
        cache it.
        """
        digits = 4 if self.is_3d else 5
        name = f"{prefix}_{idx:0{digits}d}"
        parent = self._prefix_dirs.get(prefix)
        if parent is None:
            parent = self.output_dir
            if not (parent / name).exists():
                hit = next(self.output_dir.glob(f"*/{name}"), None)
                if hit is not None:
                    parent = hit.parent
            self._prefix_dirs[prefix] = parent
        return parent / name

    def output_files(self, variable: str) -> list[Path]:
        """Sorted list of output files for a variable.

        For DEPTH_OUT returns [dep.out] if present.
        For all others globs prefix_%05d (2D) or prefix_%04d (3D) files.
        """
        static = STATIC_FILES.get(variable)
        if static:
            p = self.output_dir / static
            return [p] if p.exists() else []
        prefix = VAR_TO_PREFIX.get(variable, variable)
        pattern = f"{prefix}_[0-9][0-9][0-9][0-9]" if self.is_3d else f"{prefix}_[0-9][0-9][0-9][0-9][0-9]"
        # channels own subfolders since board 3; the flat glob keeps old
        # ref trees readable
        return sorted(self.output_dir.glob(pattern)) + sorted(self.output_dir.glob(f"*/{pattern}"))

    def read_field(self, path: Path) -> "np.ndarray":
        """Read a field file and return a float32 numpy array.

        2D runs return (ny, nx).
        3D runs return (ny, nx) for 2D fields (e.g. eta) and (nz, ny, nx) for
        volumetric fields (e.g. u, v, w, p) — detected by row count.

        Binary: raw float32, Fortran column-major, no record markers.
        ASCII:  rows written by the Fortran output routines:
          putfile2D — Nglob rows, Mglob values each → (ny, nx)
          putfile3D — Kglob*Nglob rows, Mglob values each → reshaped to (nz, ny, nx)
        """
        import numpy as np

        if self.binary:
            n = self.nx * self.ny
            dtype = np.float64 if path.stat().st_size == n * 8 else np.float32
            return np.fromfile(path, dtype=dtype, count=n).reshape((self.nx, self.ny), order="F").T
        arr = np.loadtxt(path, dtype=np.float32)
        if self.nz > 1 and arr.ndim == 2 and arr.shape[0] == self.nz * self.ny:
            arr = arr.reshape(self.nz, self.ny, self.nx)
        return arr


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
        suffixes = [int(m.group(1)) for f in output_dir.iterdir() if (m := _STATION_RE.match(f.name))]
        if not suffixes:
            return []
        # stations carry no 99999 sentinel -- that convention is field-file only
        return [VariableInfo("sta", min(suffixes), max(suffixes), unstable=False)]

    _KIND_MAP: dict[str, frozenset[str]] = {
        "field": FIELD_PREFIXES,
        "statistics": STATS_PREFIXES,
    }
    allowed = _KIND_MAP.get(kind) if kind else None

    prefix_suffixes: dict[str, list[int]] = defaultdict(list)
    # channels own subfolders since board 3 (one level); flat entries keep
    # old ref trees readable.  A prefix appearing in two channel folders
    # merges -- acceptable while board decks keep variables unique per
    # channel kind.
    entries = list(output_dir.iterdir())
    entries += [f for d in output_dir.iterdir() if d.is_dir() for f in d.iterdir()]
    for f in entries:
        if f.is_dir():
            continue
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
    txt_files = sorted(p for p in run_dir.glob("*.txt") if p.name != "LOG.txt")

    if yaml_files:
        return _from_yaml(run_dir, yaml_files[0])
    if txt_files:
        return _from_txt(run_dir, txt_files[0])
    raise FileNotFoundError(f"No .yaml or .txt input file found in {run_dir}")


def _from_yaml(run_dir: Path, path: Path) -> RunMetadata:
    with open(path) as f:
        cfg = yaml.safe_load(f)

    # 2-D schema renamed geometry: -> grid: (config reorg); the ref side and
    # 3-D configs still say geometry:
    geo = cfg.get("grid", cfg.get("geometry", {}))
    out = cfg.get("output", {})

    # n_cells (2-D schema) with grid_size fallback (ref side + 3-D geometry:)
    grid_size = geo.get("n_cells", geo.get("grid_size", [0, 0]))  # [nx, ny] or [nx, ny, nz]
    cell_size = geo.get("cell_size", [1.0, 1.0])  # [dx, dy]

    result_folder = out.get("result_folder", "output").rstrip("/")
    output_dir = (run_dir / result_folder).resolve()

    binary = str(out.get("format", "binary")).upper() == "BINARY"

    return RunMetadata(
        run_dir=run_dir,
        output_dir=output_dir,
        nx=int(grid_size[0]),
        ny=int(grid_size[1]),
        nz=int(grid_size[2]) if len(grid_size) > 2 else 0,
        dx=float(cell_size[0]),
        dy=float(cell_size[1]),
        binary=binary,
    )


def _from_txt(run_dir: Path, path: Path) -> RunMetadata:
    kv = _parse_txt(path)

    result_folder = kv.get("RESULT_FOLDER", "output").rstrip("/")
    output_dir = (run_dir / result_folder).resolve()

    binary = kv.get("FIELD_IO_TYPE", "ASCII").upper() == "BINARY"

    return RunMetadata(
        run_dir=run_dir,
        output_dir=output_dir,
        nx=int(kv.get("Mglob", 0)),
        ny=int(kv.get("Nglob", 0)),
        nz=int(kv.get("Kglob", 0)),
        dx=float(kv.get("DX", 1.0)),
        dy=float(kv.get("DY", 1.0)),
        binary=binary,
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
            ref_arr = ref_meta.read_field(ref_meta.field_path(prefix, idx)).astype(float)
            dev_arr = dev_meta.read_field(dev_meta.field_path(prefix, idx)).astype(float)
            vals.append(float(np.mean(ref_arr != dev_arr)))
        return np.array(vals)

    # TODO: check error scaling — current metric is ||dev-ref||₂ / max_t(||ref||₂),
    #       which grows like √N with grid size and makes tolerances grid-dependent.
    #       Consider switching to RMS form: √mean((dev-ref)²) / √mean(ref²),
    #       where the N factors cancel and tolerances transfer across resolutions.
    #       Also revisit DEFAULT_FLOOR if the denominator changes.

    # Pass 1: find max reference L2 norm across all timesteps.
    max_norm_ref = 0.0
    for idx in indices:
        ref_arr = ref_meta.read_field(ref_meta.field_path(prefix, idx)).astype(float)
        max_norm_ref = max(max_norm_ref, float(np.sqrt(np.sum(ref_arr**2))))
    denom = max(max_norm_ref, floor)

    # Pass 2: compute per-step normalised L2.
    vals = []
    for idx in indices:
        ref_arr = ref_meta.read_field(ref_meta.field_path(prefix, idx)).astype(float)
        dev_arr = dev_meta.read_field(dev_meta.field_path(prefix, idx)).astype(float)
        diff = dev_arr - ref_arr
        vals.append(float(np.sqrt(np.sum(diff**2))) / denom)
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
