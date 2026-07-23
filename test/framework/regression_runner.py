import math
import platform
import re
import time
import os
import shutil
import subprocess
import importlib
import copy
from dataclasses import dataclass
from pathlib import Path
import yaml

_STRICT_STRIP_RE = re.compile(r"^\s*DT_fixed\s*=", re.IGNORECASE)
# Grid-size keys in a legacy input.txt; Kglob absent -> 2D (treated as 1 layer).
_GRID_RE = {k: re.compile(rf"^\s*{k}\s*=\s*(\d+)", re.IGNORECASE | re.MULTILINE) for k in ("Mglob", "Nglob", "Kglob")}
# Rewrite the hardwired PX/PY so mpirun -np matches the auto-sized decomposition.
_PX_RE = re.compile(r"(?im)^(\s*PX\s*=\s*)\d+")
_PY_RE = re.compile(r"(?im)^(\s*PY\s*=\s*)\d+")
# Default rank-sizing dial: np = round(sqrt(cells) / K). K=13 ~ 60% eff (debug,
# max node usage); K=25 ~ 85% eff (production). Measured on wheat (92-core, shm).
_DEFAULT_NP_K = 13.0
# Halo (Nghost=3) needs a few interior cells; floor each subdomain axis here.
_MIN_SUBDOMAIN = 4
from rich import box
from rich.progress import Progress, SpinnerColumn, TextColumn, TimeElapsedColumn
from rich.table import Table
from test.framework.base_runner import BaseRunner
from test.framework.workspace_utils import get_build_path
from test.framework.results import MetricResult, SimResult, SubsectionResult
from test.framework.html_report import generate as generate_html_report, generate_pdf as generate_pdf_report, ReportMeta

STAMP_FILE = ".build_stamp"
CONFIG_PATH = os.path.join(os.path.dirname(__file__), "..", "regression", "regression_config.yaml")


@dataclass
class _SimTask:
    """One scheduled simulation: a dev run plus (unless cached/oracle) a ref run.

    ref_state fixes the plan at prep time ("needs_run" | "cached" | "oracle");
    ref_status / dev_status carry the runtime outcome as the scheduler drains
    the pool. eff_np is the rank count actually launched (the auto-sized declared_np
    capped to the budget, factored into decomp=(px,py)); ref+dev of one task both
    run at eff_np/decomp so their comparison stays valid — and bitwise — regardless
    of the machine's rank budget.
    """

    sim: dict
    index: int
    ref_build_dir: str | None
    curr_build_dir: str
    oracle_mode: bool
    curr_run_dir: str
    ref_run_dir: str | None
    ref_out: str | None
    sim_stamp: str | None
    curr_input: str
    ref_input: str | None
    eff_np: int
    declared_np: int
    decomp: tuple[int, int]
    dt_mode: str
    ref_state: str
    ref_status: str = "pending"
    dev_status: str = "pending"
    ref_elapsed: float = 0.0
    dev_elapsed: float = 0.0
    ref_stderr: str = ""
    dev_stderr: str = ""


class RegressionRunner(BaseRunner):
    def __init__(self, reporter, provider):
        super().__init__(reporter)
        self.provider = provider
        self.repo_root = os.environ.get("FUNWAVE_SRC_ROOT", os.getcwd())

        config_path = os.environ.get("FUNWAVE_REGRESSION_CONFIG", CONFIG_PATH)
        with open(config_path) as f:
            config = yaml.safe_load(f)
        self._refs = config.get("refs", {})
        self.executables = self._normalize_executables(config["executables"])
        self.simulations = config["simulations"]
        # Auto rank-sizing dial (see _auto_np); config field, default debug (K=13).
        self._np_K = float(config.get("np_sizing", {}).get("K") or _DEFAULT_NP_K)

    def _normalize_executables(self, raw):
        result = {}
        for name, spec in raw.items():
            if isinstance(spec, list):
                result[name] = {"cmake_flags": spec, "ref_branch": None}
            else:
                ref_key = spec.get("ref")
                ref_cfg = self._refs.get(ref_key, {}) if ref_key else {}
                exe_flags = spec.get("cmake_flags", [])
                ref_flags = ref_cfg.get("cmake_flags", [])
                result[name] = {
                    "cmake_flags": exe_flags + ref_flags,
                    "ref_branch": ref_cfg.get("branch") if ref_key else None,
                }
        return result

    def _worktree_path(self, branch):
        return os.path.join(self.repo_root, "test", "regression", "worktrees", branch)

    def _ensure_worktree(self, branch):
        path = self._worktree_path(branch)
        if not os.path.exists(path):
            self.reporter.step(f"Creating worktree: {branch}")
            subprocess.run(["git", "worktree", "add", path, branch], check=True, capture_output=True)
        return path

    def _exe_build_dir(self, workspace, exe_type):
        return os.path.join(get_build_path(workspace), exe_type)

    def _git_hash(self, source_dir):
        hash_ = subprocess.check_output(["git", "-C", source_dir, "rev-parse", "HEAD"], stderr=subprocess.DEVNULL).decode().strip()
        dirty = subprocess.call(["git", "-C", source_dir, "diff", "--quiet"], stderr=subprocess.DEVNULL) != 0
        return f"{hash_}-dirty" if dirty else hash_

    def _is_build_current(self, build_dir, source_dir, binary_path):
        if not os.path.exists(binary_path):
            return False
        stamp_path = os.path.join(build_dir, STAMP_FILE)
        if not os.path.exists(stamp_path):
            return False
        try:
            with open(stamp_path) as f:
                return f.read().strip() == self._git_hash(source_dir)
        except Exception:
            return False

    def _write_stamp(self, build_dir, source_dir):
        try:
            with open(os.path.join(build_dir, STAMP_FILE), "w") as f:
                f.write(self._git_hash(source_dir))
        except Exception:
            pass

    def _build(self, build_dir, source_dir, cmake_flags=None, binary_path=None, force=False, label=None):
        """Build and return (full_hash, rebuilt: bool)."""
        full_hash = self._git_hash(source_dir)
        if not force and binary_path and self._is_build_current(build_dir, source_dir, binary_path):
            return full_hash, False

        tag = label or os.path.basename(build_dir)
        os.makedirs(build_dir, exist_ok=True)
        if force:
            cache = os.path.join(build_dir, "CMakeCache.txt")
            if os.path.exists(cache):
                os.remove(cache)

        # Regression execs funwave directly (no ctest) -> build the exe only,
        # no unit-test scaffolding (ENABLE_UNIT_TESTING stays OFF).
        # Compiler selection is CMake auto-config from the environment (batch
        # scripts export FC: mpifort on wheat, ftn on Cray PE).  On macOS
        # FindMPI misdetects under bare gfortran, so default FC to the OpenMPI
        # wrapper there -- an explicit FC always wins.
        env = os.environ.copy()
        if platform.system() == "Darwin":
            env.setdefault("FC", "mpif90")
        cmake_cmd = ["cmake", "-S", source_dir, "-B", build_dir]
        cmake_cmd += [f"-D{flag}" for flag in (cmake_flags or [])]
        # BUILD_TYPE opt-in mirrors runners.py; NOTE: also retypes ref builds,
        # so leave unset on regression boards (refs are pinned RelWithDebInfo)
        if os.environ.get("BUILD_TYPE"):
            cmake_cmd += [f"-DCMAKE_BUILD_TYPE={os.environ['BUILD_TYPE']}"]

        with Progress(SpinnerColumn(), TextColumn("[progress.description]{task.description}"), transient=True) as progress:
            config_task = progress.add_task(f"cmake  {tag}  configuring...", total=None)
            try:
                subprocess.run(cmake_cmd, check=True, capture_output=True, env=env)
            except subprocess.CalledProcessError as exc:
                # captured output is invisible on a headless board otherwise
                print(f"cmake configure failed for {tag}:\n{exc.stdout}\n{exc.stderr}")
                raise
            progress.remove_task(config_task)

            build_task = progress.add_task(f"make   {tag}  compiling...  [dim]0%[/dim]", total=100)
            make_proc = subprocess.Popen(
                ["make", "-C", build_dir, "-j8"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
            )

            while make_proc.poll() is None:
                line = make_proc.stdout.readline()
                if "[" in line and "%" in line:
                    try:
                        percent = int(line.split("[")[1].split("%")[0].strip())
                        progress.update(
                            build_task, completed=percent, description=f"make   {tag}  compiling...  [dim]{percent}%[/dim]"
                        )
                    except:
                        pass

            if make_proc.returncode != 0:
                # captured stderr is invisible on a headless board otherwise
                err = make_proc.stderr.read() if make_proc.stderr else ""
                print(f"make failed for {tag}:\n{err[-3000:]}")
                raise subprocess.CalledProcessError(make_proc.returncode, "make")

        self._write_stamp(build_dir, source_dir)
        return full_hash, True

    def _expand_simulations(self, simulations):
        expanded = []
        for sim in simulations:
            if "input_files" in sim:
                for ifile in sim["input_files"]:
                    stem = Path(ifile).stem
                    s = {k: v for k, v in sim.items() if k != "input_files"}
                    s["input_file"] = ifile
                    s["curr_input"] = f"{stem}.yaml"
                    s["name"] = f"{sim['name']}_{stem}"
                    expanded.append(s)
            else:
                expanded.append(sim)
        # decomp sweep: one variant per rank count; the group is aggregated
        # post-run into a spread-across-np invariance gate (_sweep_results)
        swept = []
        for sim in expanded:
            if "np_sweep" not in sim:
                swept.append(sim)
                continue
            for np_want in sim["np_sweep"]:
                s = {k: v for k, v in sim.items() if k != "np_sweep"}
                s["np_pin"] = int(np_want)
                s["sweep_group"] = sim["name"]
                s["name"] = f"{sim['name']}_np{np_want}"
                swept.append(s)
        return swept

    def _setup_run_dir(self, sim, run_dir, fixed_dt=False, decomp=None):
        os.makedirs(run_dir, exist_ok=True)
        if "output_dir" in sim:
            os.makedirs(os.path.join(run_dir, sim["output_dir"]), exist_ok=True)
        input_dir = os.path.join(self.repo_root, sim["input"])
        for item in os.listdir(input_dir):
            src = os.path.join(input_dir, item)
            # native-YAML deck stages verbatim (no legacy DT-strip / PX-PY
            # rewrite — new-schema decks omit decomposition: for auto).
            # ONLY the sim's own deck: metadata readers and oracles resolve
            # the run deck as the single *.yaml in the run dir.
            if os.path.isfile(src) and item.endswith(".yaml"):
                if item == sim.get("input_file"):
                    shutil.copy2(src, os.path.join(run_dir, item))
                continue
            if os.path.isfile(src) and item.endswith(".txt"):
                dst = os.path.join(run_dir, item)
                edit_pxpy = decomp is not None and item == sim["input_file"]
                if fixed_dt and not edit_pxpy:
                    shutil.copy2(src, dst)
                    continue
                with open(src) as f:
                    text = f.read()
                if not fixed_dt:
                    text = "".join(l for l in text.splitlines(keepends=True) if not _STRICT_STRIP_RE.match(l))
                if edit_pxpy:
                    text = self._rewrite_pxpy(text, decomp[0], decomp[1])
                with open(dst, "w") as f:
                    f.write(text)
        data_src = os.path.join(input_dir, "data")
        if os.path.isdir(data_src):
            data_dst = os.path.join(run_dir, "data")
            if os.path.exists(data_dst):
                shutil.rmtree(data_dst)
            shutil.copytree(data_src, data_dst)

    def _preprocess(self, sim, run_dir):
        curr_input = sim.get("curr_input", sim["input_file"])

        def subst(arg):
            return (
                arg.replace("{input_file}", sim["input_file"])
                .replace("{curr_input}", curr_input)
                .replace("{repo_root}", self.repo_root)
                .replace("{run_dir}", run_dir)
            )

        cmd = [subst(a) for a in sim["preprocess"]]
        try:
            subprocess.run(cmd, cwd=run_dir, check=True, capture_output=True)
        except subprocess.CalledProcessError as e:
            # captured stderr is invisible on a headless board otherwise
            err = (e.stderr or b"").decode(errors="replace")
            print(f"preprocess failed: {' '.join(cmd)}\n{err[-2000:]}")
            raise

    def _resolve_cmake_flags(self, flags):
        return [f.replace("{repo_root}", self.repo_root) for f in flags]

    def _run_postprocess(self, sim, ref_run_dir, curr_run_dir, ref_status, curr_status, verbose: bool = False) -> SimResult:
        """Run configured post-processors after a simulation pair and return a SimResult."""
        run_ok = ref_status in ("COMPLETED", "cached", "oracle") and curr_status == "COMPLETED"
        result = SimResult(
            name=sim["name"],
            status="SIM_FAILED" if not run_ok else "COMPLETED",
            ref_dir=ref_run_dir,
            dev_dir=curr_run_dir,
        )
        if not run_ok or "postprocess" not in sim:
            return result

        tolerances = sim.get("tolerances", {})
        plots_dir = Path(curr_run_dir) / "plots"
        plots_dir.mkdir(exist_ok=True)

        for kind, module_path in sim["postprocess"].items():
            try:
                mod = importlib.import_module(module_path)
                sub = mod.run(
                    ref_dir=ref_run_dir,
                    dev_dir=curr_run_dir,
                    tolerances=tolerances.get(kind, {}),
                    plots_dir=plots_dir,
                    verbose=verbose,
                )
                result.subsections.append(sub)
            except Exception as exc:
                result.status = "POSTPROCESS_ERROR"
                result.notes += f"\n[{kind}] {type(exc).__name__}: {exc}"
                return result

        gated = [m for s in result.subsections for m in s.metrics if math.isfinite(m.tolerance)]
        # zero gated metrics = a silently-skipping postproc (e.g. an oracle that
        # no longer recognises the deck schema), never a vacuous pass
        if not gated:
            result.status = "POSTPROCESS_ERROR"
            result.notes += "\npostprocess produced no gated metrics (oracle skipped?)"
            return result
        result.status = "PASS" if all(m.passed for m in gated) else "FAIL"

        # known_fail remaps comparison outcomes only — SIM_FAILED/POSTPROCESS_ERROR
        # stay loud (a crash is never the documented ledger gap)
        known_fail = sim.get("known_fail")
        if known_fail:
            if result.status == "FAIL":
                result.status = "XFAIL"
                result.notes += f"\nknown failure: {known_fail}"
            else:
                result.status = "XPASS"
                result.notes += f"\nunexpected pass — remove known_fail: {known_fail}"
        return result

    def _print_summary(self, results: list[SimResult]) -> None:
        """Render a Rich summary table of all SimResult objects."""
        KINDS = ["field", "station", "statistics"]
        LABELS = {"field": "Field", "station": "Station", "statistics": "Statistics"}
        STATUS_FMT = {
            "PASS": "[bold green]✓ PASS[/bold green]",
            "FAIL": "[bold red]✗ FAIL[/bold red]",
            "XFAIL": "[yellow]⚠ XFAIL[/yellow]",
            "XPASS": "[bold red]✗ XPASS[/bold red]",
            "SIM_FAILED": "[bold red]✗ SIM_FAILED[/bold red]",
            "POSTPROCESS_ERROR": "[yellow]⚠ POSTPROCESS_ERROR[/yellow]",
            "COMPLETED": "[dim]COMPLETED[/dim]",
        }

        active_kinds = [k for k in KINDS if any(r.subsection(k) for r in results)]

        table = Table(
            box=box.SIMPLE_HEAD,
            header_style="bold cyan",
            show_edge=False,
            pad_edge=True,
        )
        table.add_column("Simulation", min_width=28)
        table.add_column("Status", min_width=12)
        for k in active_kinds:
            table.add_column(LABELS[k], justify="center", min_width=10)

        for r in results:
            row = [r.name, STATUS_FMT.get(r.status, r.status)]
            for k in active_kinds:
                sub = r.subsection(k)
                row.append(sub.summary if sub else "[dim]—[/dim]")
            table.add_row(*row)

        self.reporter.console.print()
        self.reporter.console.print(table)
        self.reporter.console.print()

    @staticmethod
    def _rank_budget(ranks) -> tuple[int, str]:
        """Resolve the total ranks to pack into, and where the number came from.

        Precedence: explicit -j, then FUNWAVE_TEST_RANKS, then the batch
        allocation (SLURM/PBS) so an offloaded run on a compute node saturates
        the node without a manual flag, then the local CPU count.
        """
        if ranks:
            return ranks, "-j"
        env = os.environ
        for var in ("FUNWAVE_TEST_RANKS", "SLURM_CPUS_ON_NODE", "SLURM_NTASKS", "PBS_NCPUS"):
            val = env.get(var)
            if val and val.isdigit() and int(val) > 0:
                return int(val), var
        nodefile = env.get("PBS_NODEFILE")
        if nodefile and os.path.exists(nodefile):
            try:
                n = sum(1 for line in open(nodefile) if line.strip())
                if n > 0:
                    return n, "PBS_NODEFILE"
            except OSError:
                pass
        return os.cpu_count() or 1, "cpu_count"

    @staticmethod
    def _grid_cells(input_path) -> tuple[int, int, int] | None:
        """Parse (Mglob, Nglob, Kglob) from a legacy .txt or a native YAML deck; Kglob=1 if absent (2D)."""
        if str(input_path).endswith(".yaml"):
            try:
                with open(input_path) as f:
                    cfg = yaml.safe_load(f)
                gs = cfg["grid"]["grid_size"]
            except (OSError, KeyError, TypeError, yaml.YAMLError):
                return None
            return int(gs[0]), int(gs[1]), int(gs[2]) if len(gs) > 2 else 1
        try:
            text = open(input_path).read()
        except OSError:
            return None
        m = _GRID_RE["Mglob"].search(text)
        n = _GRID_RE["Nglob"].search(text)
        if not m or not n:
            return None
        k = _GRID_RE["Kglob"].search(text)
        return int(m.group(1)), int(n.group(1)), int(k.group(1)) if k else 1

    @staticmethod
    def _factor_decomp(np_want, m, n, min_cells=_MIN_SUBDOMAIN) -> tuple[int, int]:
        """Aspect-aware PX*PY <= np_want, each subdomain axis >= min_cells.

        Mirrors the binary's compute_optimal_grid_size: among factor pairs whose
        product is the largest achievable <= np_want, pick the one whose PX/PY
        aspect best matches M/N (log-ratio) so halos (Nghost=3) always fit and the
        subdomains stay square-ish. A grid too small to split just returns (1, 1).
        """
        px_max = max(1, m // min_cells)
        py_max = max(1, n // min_cells)
        target = math.log((m / n) if n else 1.0)
        for total in range(min(np_want, px_max * py_max), 0, -1):
            best = None
            for px in range(1, total + 1):
                if total % px or px > px_max:
                    continue
                py = total // px
                if py > py_max:
                    continue
                score = abs(math.log(px / py) - target)
                if best is None or score < best[0]:
                    best = (score, px, py)
            if best:
                return best[1], best[2]
        return 1, 1

    def _auto_np(self, sim, budget) -> tuple[int, int, tuple[int, int]]:
        """Size ranks per test from the horizontal footprint: np = round(sqrt(Mglob*Nglob) / K).

        Returns (eff_np, target_np, (px, py)). target_np is the pre-budget wish;
        eff_np = px*py is what launches (feasible factorization capped to budget).
        Only the horizontal plane decomposes (nx*ny), so Kglob (vertical layers) is
        NOT in the count: a 2026-07-18 wheat scan of the 3D standing wave showed its
        HYPRE Poisson solve does not strong-scale (np=1 fastest, np=16 slower than
        serial), so counting K would over-provision. M*N → the 3D case sizes to np=1,
        matching the data; every 2D case is unchanged (K=1). 3D stays uncalibrated
        beyond "don't parallelize this tiny domain"; a larger horizontal 3D grid may
        scale and would want its own K — and the legacy 3D path needs PX|Mglob,
        PY|Nglob (np=7 crashed rc=24), unlike the modern 2D path which tolerates
        remainder cells.

        A test may pin `decomp: [px, py]` to force a specific decomposition when its
        coverage intent depends on it (e.g. flume_2d_irr exercises periodic-Y across
        ranks, which aspect-sizing would collapse to py=1). The pin must fit the
        budget. FUTURE: a square periodic domain would keep that coverage under pure
        auto-sizing and let the pin go.
        """
        override = sim.get("decomp")
        if override:
            px, py = int(override[0]), int(override[1])
            # A pin fixes both np and decomposition (mpirun -np must equal px*py), so
            # it can't be budget-capped like an auto size; fall back if it won't fit.
            if px * py <= budget:
                return px * py, px * py, (px, py)
            self.reporter.warn(f"{sim['name']}: decomp pin {px}x{py} exceeds rank budget {budget}; auto-sizing instead")
        input_path = os.path.join(self.repo_root, sim["input"], sim["input_file"])
        grid = self._grid_cells(input_path)
        if grid is None:
            return 1, 1, (1, 1)
        m, n, _k = grid
        # sweep variants pin the rank count (not the decomposition — the
        # factorization stays aspect-optimal so it matches what a production
        # run at that -np would use); collapse below the pin surfaces via the
        # capped-np warning, declared_np = the pin
        pin = sim.get("np_pin")
        target_np = int(pin) if pin else max(1, round(math.sqrt(m * n) / self._np_K))
        px, py = self._factor_decomp(min(target_np, budget), m, n)
        return px * py, target_np, (px, py)

    @staticmethod
    def _rewrite_pxpy(text, px, py) -> str:
        """Set PX/PY in a legacy input.txt to the auto-sized decomposition."""
        text = _PX_RE.sub(rf"\g<1>{px}", text)
        return _PY_RE.sub(rf"\g<1>{py}", text)

    def _prepare_task(self, sim, index, exe_dirs, fixed_dt, force, budget) -> _SimTask:
        """Resolve run dirs, the ref cache state, and the effective np for one sim.

        Effective np is the declared np capped to the rank budget so a single
        run always fits the pool. The ref cache is keyed on (dt_mode, eff_np):
        a stamp written under a different rank budget is stale and re-runs.
        """
        exe_type = sim["exe_type"]
        ref_build_dir, curr_build_dir, _ref_branch = exe_dirs[exe_type]
        if sim.get("test_type") in ("self_consistency", "reproducibility"):
            return self._prepare_selfcompare_task(sim, index, curr_build_dir, fixed_dt, force, budget)
        oracle_mode = ref_build_dir is None
        curr_input = sim.get("curr_input", sim["input_file"])

        curr_run_dir = os.path.join(curr_build_dir, "runs", sim["name"])
        ref_run_dir = os.path.join(ref_build_dir, "runs", sim["name"]) if not oracle_mode else None
        ref_out = os.path.join(ref_run_dir, sim["output_dir"]) if not oracle_mode else None
        sim_stamp = os.path.join(ref_out, ".sim_complete") if not oracle_mode else None

        eff_np, declared_np, decomp = self._auto_np(sim, budget)
        dt_mode = "fixed_dt" if fixed_dt else "adaptive"

        if force:
            if sim_stamp and os.path.exists(sim_stamp):
                os.remove(sim_stamp)
            for d in (ref_run_dir, curr_run_dir):
                if d and os.path.exists(d):
                    shutil.rmtree(d, ignore_errors=True)

        ref_state = "oracle" if oracle_mode else "needs_run"
        ref_input = None if oracle_mode else sim["input_file"]
        if not oracle_mode:
            stamp_val = f"{dt_mode} np={eff_np}"
            cached = False
            if os.path.exists(sim_stamp):
                try:
                    cached = open(sim_stamp).read().strip() == stamp_val
                except OSError:
                    cached = False
            if cached:
                ref_state = "cached"
            else:
                # stale ref: clear both run dirs so ref+dev regenerate in lockstep
                for d in (ref_run_dir, curr_run_dir):
                    if os.path.exists(d):
                        shutil.rmtree(d, ignore_errors=True)
                if "preprocess" in sim and sim.get("preprocess_ref", False):
                    ref_input = curr_input

        return _SimTask(
            sim=sim,
            index=index,
            ref_build_dir=ref_build_dir,
            curr_build_dir=curr_build_dir,
            oracle_mode=oracle_mode,
            curr_run_dir=curr_run_dir,
            ref_run_dir=ref_run_dir,
            ref_out=ref_out,
            sim_stamp=sim_stamp,
            curr_input=curr_input,
            ref_input=ref_input,
            eff_np=eff_np,
            declared_np=declared_np,
            decomp=decomp,
            dt_mode=dt_mode,
            ref_state=ref_state,
            ref_status="pending" if ref_state == "needs_run" else ref_state,
        )

    def _prepare_selfcompare_task(self, sim, index, build_dir, fixed_dt, force, budget) -> _SimTask:
        """Two DEV-binary runs whose end-of-run core.bin files are self-compared
        (no ref branch). Covers two test types:
          self_consistency: leg A (continuous) vs leg B (checkpoint+restart)
          reproducibility:  two identical runs (guards deterministic phase seeding)
        ref_out/sim_stamp are None (the ref slot runs the dev binary, which changes
        every rebuild, so it never caches).
        """
        base = os.path.join(build_dir, "runs", sim["name"])
        ref_run_dir = os.path.join(base, "A")  # self_consistency: continuous 0 -> T; reproducibility: run 1
        curr_run_dir = os.path.join(base, "B")  # self_consistency: checkpoint+restart; reproducibility: run 2
        if force and os.path.exists(base):
            shutil.rmtree(base, ignore_errors=True)
        eff_np, declared_np, decomp = self._auto_np(sim, budget)  # decomp pin
        return _SimTask(
            sim=sim,
            index=index,
            ref_build_dir=build_dir,
            curr_build_dir=build_dir,
            oracle_mode=False,
            curr_run_dir=curr_run_dir,
            ref_run_dir=ref_run_dir,
            ref_out=None,
            sim_stamp=None,
            curr_input="run.yaml",
            ref_input="run.yaml",
            eff_np=eff_np,
            declared_np=declared_np,
            decomp=decomp,
            dt_mode="fixed_dt" if fixed_dt else "adaptive",
            ref_state="needs_run",
            ref_status="pending",
        )

    def _hotstart_base(self, sim) -> dict:
        """Load the base hotstart deck (a native simulation:/output: yaml, not legacy txt)."""
        path = os.path.join(self.repo_root, sim["input"], sim["input_file"])
        with open(path) as f:
            return yaml.safe_load(f)

    @staticmethod
    def _hotstart_legs(base: dict) -> tuple[dict, dict, dict]:
        """Derive the three legs from one base deck (T = base total_time):
            A  : continuous 0 -> T,     checkpoint at end
            B1 : first half 0 -> T/2,   checkpoint at end
            B2 : restart    T/2 -> T,   restarts from B1's checkpoint, checkpoint at end
        All write ./chk (relative to the run dir); B2 restarts from ./chk, which B1
        laid down in the same dir. A/chk and B/chk are then compared at t = T.
        """
        t_full = float(base["simulation"]["total_time"])
        t_chk = t_full / 2.0

        def _leg(total, restart=False):
            d = copy.deepcopy(base)
            d["simulation"]["total_time"] = total
            d.setdefault("output", {})["checkpoint"] = "./chk"
            if restart:
                d.setdefault("hot_start", {})["checkpoint"] = "./chk"
            return d

        return _leg(t_full), _leg(t_chk), _leg(t_full, restart=True)

    def _write_leg(self, deck: dict, run_dir: str, name: str) -> None:
        os.makedirs(run_dir, exist_ok=True)
        rf = (deck.get("output") or {}).get("result_folder")
        if rf:
            os.makedirs(os.path.join(run_dir, rf), exist_ok=True)
        with open(os.path.join(run_dir, name), "w") as f:
            yaml.safe_dump(deck, f, sort_keys=False)

    def _run_blocking(self, binary, input_file, run_dir, np) -> str:
        """Run one leg to completion inline (used for leg B1, which must finish
        before B2 can restart). Cheap here — B1 is np=1 and a few hundred steps."""
        jid = self.provider.submit(binary, input_file, run_dir, np=np)
        while self.provider.get_status(jid) == "RUNNING":
            time.sleep(0.2)
        return self.provider.get_status(jid)

    def _launch_run(self, task: _SimTask, kind: str, fixed_dt: bool) -> str:
        """Set up the run dir, preprocess, and submit the ref or dev run.

        A self_consistency task runs the dev binary for both slots: ref = leg A
        (continuous); dev = leg B1 (blocking, lays down the checkpoint) then B2
        (restart, whose async job id drives the slot). If B1 fails, B2 is still
        submitted so the outcome surfaces as a loud SIM_FAILED, not a silent skip.

        A reproducibility task runs the SAME deck (with a ./chk checkpoint) in both
        slots; the two end-of-run core.bin files must be bitwise identical.
        """
        sim = task.sim
        ttype = sim.get("test_type")
        if ttype == "self_consistency":
            binary = os.path.join(task.curr_build_dir, sim["binary"])
            a_deck, b1_deck, b2_deck = self._hotstart_legs(self._hotstart_base(sim))
            if kind == "ref":
                self._write_leg(a_deck, task.ref_run_dir, "A.yaml")
                return self.provider.submit(binary, "A.yaml", task.ref_run_dir, np=task.eff_np)
            # dev: B1 lays down the checkpoint that B2 restarts from
            self._write_leg(b1_deck, task.curr_run_dir, "B1.yaml")
            self._write_leg(b2_deck, task.curr_run_dir, "B2.yaml")
            st = self._run_blocking(binary, "B1.yaml", task.curr_run_dir, task.eff_np)
            if st != "COMPLETED":
                self.reporter.warn(f"{sim['name']}: checkpoint leg B1 {st}; restart will fail")
            return self.provider.submit(binary, "B2.yaml", task.curr_run_dir, np=task.eff_np)
        if ttype == "reproducibility":
            binary = os.path.join(task.curr_build_dir, sim["binary"])
            deck = self._hotstart_base(sim)
            deck.setdefault("output", {})["checkpoint"] = "./chk"
            run_dir = task.ref_run_dir if kind == "ref" else task.curr_run_dir
            self._write_leg(deck, run_dir, "run.yaml")
            return self.provider.submit(binary, "run.yaml", run_dir, np=task.eff_np)
        if kind == "ref":
            run_dir, binary, input_file = task.ref_run_dir, os.path.join(task.ref_build_dir, sim["binary"]), task.ref_input
            self._setup_run_dir(sim, run_dir, fixed_dt=fixed_dt, decomp=task.decomp)
            if "preprocess" in sim and sim.get("preprocess_ref", False):
                self._preprocess(sim, run_dir)
        else:
            run_dir, binary, input_file = task.curr_run_dir, os.path.join(task.curr_build_dir, sim["binary"]), task.curr_input
            self._setup_run_dir(sim, run_dir, fixed_dt=fixed_dt, decomp=task.decomp)
            if "preprocess" in sim:
                self._preprocess(sim, run_dir)
        return self.provider.submit(binary, input_file, run_dir, np=task.eff_np)

    def _write_sim_stamp(self, task: _SimTask) -> None:
        """Record (dt_mode, eff_np) so a later run under a different rank budget re-runs the ref."""
        if task.sim_stamp is None:  # self_consistency: leg A never caches
            return
        try:
            os.makedirs(task.ref_out, exist_ok=True)
            with open(task.sim_stamp, "w") as f:
                f.write(f"{task.dt_mode} np={task.eff_np}\n")
        except Exception:
            pass

    @staticmethod
    def _sched_desc(running, used, budget, queued) -> str:
        names = ", ".join(f"{r['task'].sim['name']}/{r['kind']}" for r in running.values()) or "—"
        return f"running {len(running)} [{used}/{budget} ranks]  queued {queued}  |  {names}"

    def _run_scheduler(self, tasks, budget, fixed_dt, verbose, stop_on_pass) -> list[SimResult]:
        """Bin-pack ref/dev runs into the rank budget, draining the pool as jobs finish.

        Each run costs eff_np ranks; a run launches only when eff_np <= free
        ranks (first-fit-decreasing, largest first). A task's postprocess fires
        once both its ref and dev runs have completed. stop_on_pass halts new
        launches after the first PASS; in-flight runs still finalize.
        """
        queue = []
        remaining = {}
        for t in tasks:
            n = 1  # dev always runs
            if t.ref_state == "needs_run":
                queue.append((t, "ref"))
                n += 1
            queue.append((t, "dev"))
            remaining[t.index] = n
        queue.sort(key=lambda tk: tk[0].eff_np, reverse=True)

        capped = [t for t in tasks if t.eff_np < t.declared_np]
        if capped:
            names = ", ".join(f"{t.sim['name']} ({t.declared_np}->{t.eff_np})" for t in capped)
            self.reporter.warn(
                f"rank budget {budget}: sized-down np for {names} — grid wants more ranks than the pool (ref+dev still matched)"
            )

        results: dict[int, SimResult] = {}
        running: dict[str, dict] = {}
        used = 0
        stop = False

        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            TimeElapsedColumn(),
            console=self.reporter.console,
            transient=True,
        ) as progress:
            pt = progress.add_task("scheduling", total=None)
            while queue or running:
                launched = True
                while launched and not stop:
                    launched = False
                    for i, (t, kind) in enumerate(queue):
                        if t.eff_np <= budget - used:
                            jid = self._launch_run(t, kind, fixed_dt)
                            running[jid] = {"task": t, "kind": kind, "np": t.eff_np, "t0": time.time()}
                            used += t.eff_np
                            queue.pop(i)
                            launched = True
                            break
                progress.update(pt, description=self._sched_desc(running, used, budget, len(queue)))
                if not running:
                    break
                time.sleep(1)

                done = []
                for jid, r in running.items():
                    st = self.provider.get_status(jid)
                    if st in ("COMPLETED", "FAILED"):
                        r["status"] = st
                        r["elapsed"] = time.time() - r["t0"]
                        done.append(jid)
                for jid in done:
                    r = running.pop(jid)
                    used -= r["np"]
                    t, kind = r["task"], r["kind"]
                    if kind == "ref":
                        t.ref_status, t.ref_elapsed = r["status"], r["elapsed"]
                        if r["status"] == "COMPLETED":
                            self._write_sim_stamp(t)
                        else:
                            _, t.ref_stderr = self.provider.get_output(jid)
                    else:
                        t.dev_status, t.dev_elapsed = r["status"], r["elapsed"]
                        if r["status"] == "FAILED":
                            _, t.dev_stderr = self.provider.get_output(jid)
                    remaining[t.index] -= 1
                    if remaining[t.index] == 0:
                        res = self._finalize_and_emit(t, verbose)
                        results[t.index] = res
                        if stop_on_pass and res.status == "PASS":
                            self.reporter.info("  [dim]--stop-on-pass: first passing test found, stopping.[/dim]")
                            stop = True
                if stop:
                    queue.clear()

        return [results[i] for i in sorted(results)]

    def _finalize_and_emit(self, task: _SimTask, verbose: bool) -> SimResult:
        """Postprocess a completed task and print its run + result lines."""
        sim = task.sim
        result = self._run_postprocess(sim, task.ref_run_dir, task.curr_run_dir, task.ref_status, task.dev_status, verbose=verbose)

        def _fmt_run(s, elapsed=0.0):
            if s == "cached":
                return "[dim]cached[/dim]"
            if s == "oracle":
                return "[dim]oracle[/dim]"
            if s == "COMPLETED":
                return f"[green]ran {elapsed:.0f}s[/green]"
            return f"[red]{s}[/red]"

        np_hint = f"np={task.eff_np}" if task.eff_np == task.declared_np else f"np={task.eff_np}(capped)"
        run_line = (
            f"  \\[{sim['name']}]  {np_hint}  "
            f"ref: {_fmt_run(task.ref_status, task.ref_elapsed)}  dev: {_fmt_run(task.dev_status, task.dev_elapsed)}"
        )

        STATUS_ICON = {
            "PASS": "[bold green]✓ PASS[/bold green]",
            "FAIL": "[bold red]✗ FAIL[/bold red]",
            "XFAIL": "[yellow]⚠ XFAIL[/yellow]",
            "XPASS": "[bold red]✗ XPASS[/bold red]",
            "SIM_FAILED": "[bold red]✗ SIM FAILED[/bold red]",
            "POSTPROCESS_ERROR": "[yellow]⚠ ERROR[/yellow]",
            "COMPLETED": "[dim]no comparison[/dim]",
        }
        sub_summary = "  ".join(f"{s.kind}: {s.summary}" for s in result.subsections)
        result_icon = STATUS_ICON.get(result.status, result.status)
        if result.status in ("XFAIL", "XPASS"):
            result_icon += f" [dim]({sim.get('known_fail')})[/dim]"
        result_line = f"  \\[{sim['name']}]  {result_icon}" + (f"  [dim]{sub_summary}[/dim]" if sub_summary else "")

        self.reporter.info(run_line)
        if task.ref_status == "FAILED" or task.dev_status == "FAILED":
            for label, err in [("ref", task.ref_stderr), ("dev", task.dev_stderr)]:
                if err:
                    self.reporter.info(f"    {label} stderr: {err[:400]}")

        if result.status == "PASS":
            self.reporter.success(result_line)
        elif result.status == "XPASS":
            self.reporter.error(result_line)
            self.reporter.error(f"    unexpected pass — remove known_fail: {sim.get('known_fail')} from regression_config.yaml")
        elif result.status in ("SIM_FAILED", "POSTPROCESS_ERROR"):
            self.reporter.error(result_line)
        else:
            self.reporter.warn(result_line)
        return result

    def _sweep_results(self, simulations, sim_results, verbose: bool) -> list[SimResult]:
        """Aggregate np_sweep variant groups into decomp-invariance results.

        For each sweep group, every oracle metric gated in ALL variants is
        compared across rank counts: the spread max-min must sit inside the
        sim's `sweep_tolerances: {stat: tol}` entry (same units as the metric,
        usually percentage points). Metrics without a sweep tolerance are
        reported as ungated diagnostics. This gates decomp-INVARIANCE of the
        physics, not run-to-run bitwise — the proven bug class is a rank seam
        shifting a physical metric, not roundoff.
        """
        groups: dict[str, list[dict]] = {}
        for sim in simulations:
            if sim.get("sweep_group"):
                groups.setdefault(sim["sweep_group"], []).append(sim)
        by_name = {r.name: r for r in sim_results}

        out = []
        for group, sims in groups.items():
            sweep_tol = sims[0].get("sweep_tolerances", {})
            per_np: list[tuple[int, SimResult]] = []
            broken = []
            for s in sims:
                r = by_name.get(s["name"])
                if r is None or r.status in ("SIM_FAILED", "POSTPROCESS_ERROR"):
                    broken.append(s["name"])
                else:
                    per_np.append((s["np_pin"], r))
            name = f"{group}_sweep"
            if broken or len(per_np) < 2:
                out.append(SimResult(name=name, status="SIM_FAILED",
                                     notes=f"sweep incomplete: {', '.join(broken) or 'fewer than 2 variants'}"))
                self.reporter.error(f"  \\[{name}]  [bold red]✗ SWEEP INCOMPLETE[/bold red]")
                continue

            # values per (variable, stat) across np, gated metrics only
            series: dict[tuple[str, str], dict[int, float]] = {}
            for np_val, r in per_np:
                for sub in r.subsections:
                    for m in sub.metrics:
                        if math.isfinite(m.tolerance):
                            series.setdefault((m.variable, m.stat), {})[np_val] = m.value

            nps = sorted(np for np, _ in per_np)
            metrics = []
            rows = []
            for (var, stat), vals in sorted(series.items()):
                if len(vals) != len(nps):
                    continue  # metric missing in some variant (e.g. skipped frames)
                spread = max(vals.values()) - min(vals.values())
                tol = sweep_tol.get(stat, math.inf)
                ok = spread <= tol
                metrics.append(MetricResult(var, f"{stat}_spread", spread, ok, tol))
                rows.append((stat, vals, spread, tol, ok))

            sub = SubsectionResult(kind="statistics", label="Decomp sweep", metrics=metrics)
            status = "PASS" if all(m.passed for m in metrics if math.isfinite(m.tolerance)) else "FAIL"
            res = SimResult(name=name, status=status, subsections=[sub])
            out.append(res)

            if verbose or status == "FAIL":
                table = Table(box=box.SIMPLE_HEAD, header_style="bold cyan", show_edge=False,
                              pad_edge=True, title=f"[bold]Decomp Sweep ({group})[/bold]",
                              title_justify="left")
                table.add_column("Metric", min_width=20)
                for np_val in nps:
                    table.add_column(f"np={np_val}", justify="right")
                table.add_column("Spread", justify="right")
                table.add_column("Tol", justify="right")
                table.add_column("", min_width=8)
                for stat, vals, spread, tol, ok in rows:
                    icon = "[green]✓[/green]" if ok else "[bold red]✗[/bold red]"
                    if not math.isfinite(tol):
                        icon, tol_s = "[dim]—[/dim]", "[dim]—[/dim]"
                    else:
                        tol_s = f"{tol:.4g}"
                    table.add_row(stat, *[f"{vals[np_val]:.5g}" for np_val in nps],
                                  f"{spread:.4g}", tol_s, icon)
                self.reporter.console.print(table)

            icon = "[bold green]✓ PASS[/bold green]" if status == "PASS" else "[bold red]✗ FAIL[/bold red]"
            line = f"  \\[{name}]  {icon}  [dim]statistics: {sub.summary}[/dim]"
            (self.reporter.success if status == "PASS" else self.reporter.warn)(line)
        return out

    def run(
        self,
        filter_tags=None,
        force=False,
        report: bool = False,
        pdf: bool = False,
        verbose: bool = False,
        stop_on_pass: bool = False,
        fixed_dt: bool = False,
        ranks: int | None = None,
    ):
        try:
            current_branch = subprocess.check_output(["git", "rev-parse", "--abbrev-ref", "HEAD"]).decode().strip()
        except Exception:
            current_branch = "dev"

        self.reporter.step(f"Running Regression Tests (dev: {current_branch})")

        simulations = self._expand_simulations(self.simulations)
        if filter_tags:
            tag_set = set(filter_tags)
            simulations = [s for s in simulations if tag_set & set(s.get("tags", []))]
        if not simulations:
            self.reporter.info("No tests matched the requested tags.")
            return
        tag_hint = f"  ({', '.join(sorted(filter_tags))})" if filter_tags else ""
        self.reporter.info(f"{len(simulations)} test(s){tag_hint}")

        # Build one binary per exe_type needed. Store dirs so the sim loop doesn't recompute.
        exe_dirs = {}  # exe_type -> (ref_build_dir, curr_build_dir, ref_branch)
        ref_hashes = {}
        curr_hash = ""

        def _tag(branch, h, rebuilt):
            label = f"{branch}@{h[:8]}"
            status = "[green]built[/green]" if rebuilt else "[dim]cached[/dim]"
            return f"{label}  {status}"

        for exe_type in {s["exe_type"] for s in simulations}:
            spec = self.executables[exe_type]
            cmake_flags = self._resolve_cmake_flags(spec["cmake_flags"])
            ref_branch = spec["ref_branch"]
            oracle_mode = ref_branch is None  # no ref repo — an analytic oracle stands in

            curr_build_dir = self._exe_build_dir("dev", exe_type)
            first_sim = next(s for s in simulations if s["exe_type"] == exe_type)
            curr_bin = os.path.join(curr_build_dir, first_sim["binary"])
            curr_hash, curr_rebuilt = self._build(
                curr_build_dir,
                self.repo_root,
                cmake_flags=cmake_flags,
                binary_path=curr_bin,
                force=force,
                label=f"dev/{exe_type}  ({current_branch})",
            )

            # validation exe — one build; the postproc compares against theory, not a ref run
            if oracle_mode:
                exe_dirs[exe_type] = (None, curr_build_dir, None)
                self.reporter.info(
                    f"build \\[{exe_type}]  ref: [dim]oracle (no ref)[/dim]  dev: {_tag(current_branch, curr_hash, curr_rebuilt)}"
                )
                continue

            ref_source = self._ensure_worktree(ref_branch)
            ref_build_dir = self._exe_build_dir(ref_branch, exe_type)
            ref_bin = os.path.join(ref_build_dir, first_sim["binary"])
            ref_hash, ref_rebuilt = self._build(
                ref_build_dir,
                ref_source,
                cmake_flags=cmake_flags,
                binary_path=ref_bin,
                force=force,
                label=f"ref/{exe_type}  ({ref_branch})",
            )
            ref_hashes[exe_type] = ref_hash
            exe_dirs[exe_type] = (ref_build_dir, curr_build_dir, ref_branch)
            self.reporter.info(
                f"build \\[{exe_type}]  ref: {_tag(ref_branch, ref_hash, ref_rebuilt)}  dev: {_tag(current_branch, curr_hash, curr_rebuilt)}"
            )

        budget, budget_src = self._rank_budget(ranks)
        # a sweep variant capped below its pin would duplicate a smaller
        # variant's decomposition — drop it instead (the HPC board with the
        # full rank pool runs the complete sweep)
        skipped = [s["name"] for s in simulations if s.get("np_pin", 0) > budget]
        if skipped:
            self.reporter.info(f"sweep variant(s) beyond the {budget}-rank budget skipped: {', '.join(skipped)}")
            simulations = [s for s in simulations if s.get("np_pin", 0) <= budget]
        self.reporter.info(f"scheduling {len(simulations)} test(s) across {budget} ranks ({budget_src})")

        tasks = [self._prepare_task(sim, i, exe_dirs, fixed_dt, force, budget) for i, sim in enumerate(simulations)]
        sim_results = self._run_scheduler(tasks, budget, fixed_dt, verbose, stop_on_pass)
        sim_results += self._sweep_results(simulations, sim_results, verbose)

        self._print_summary(sim_results)

        any_failed = any(r.status not in ("PASS", "XFAIL", "COMPLETED") for r in sim_results)
        # TODO: honour --no-auto-report: skip this block when any_failed but flag is set
        if report or pdf or any_failed:
            # Oracle exes have no ref repo (ref_branch / ref_hash are None / absent);
            # substitute "oracle" so a failing run renders a report instead of
            # crashing on a None in the join.
            unique_refs = sorted({exe_dirs[s["exe_type"]][2] or "oracle" for s in simulations})
            meta = ReportMeta(
                ref_branch=", ".join(unique_refs),
                dev_branch=current_branch,
                ref_hash=", ".join((ref_hashes.get(t) or "oracle")[:8] for t in sorted(exe_dirs)),
                dev_hash=curr_hash,
            )
            base = Path(self.repo_root) / "workspaces" / "regression_report"
            want_pdf = pdf or any_failed
            pdf_path = None
            with Progress(
                SpinnerColumn(), TextColumn("[progress.description]{task.description}"), TimeElapsedColumn(), transient=True
            ) as progress:
                progress.add_task("  Generating report...", total=None)
                html_path = generate_html_report(sim_results, meta, base.with_suffix(".html"))
                if want_pdf:
                    try:
                        pdf_path = generate_pdf_report(sim_results, meta, base.with_suffix(".pdf"))
                    except Exception as exc:
                        # PDF rendering (WeasyPrint) is best-effort: headless HPC nodes
                        # lack a compatible libpango, so a render failure must degrade
                        # to "no PDF" instead of crashing the board (which -- since a
                        # failing test forces want_pdf -- would bury the results under a
                        # traceback).  HTML report and pass/fail are unaffected.
                        self.reporter.info(f"PDF report skipped (render failed: {exc})")
            self.reporter.info(f"HTML report: file://{html_path}")
            if pdf_path is not None:
                self.reporter.info(f"PDF  report: {pdf_path}")

        return any_failed
