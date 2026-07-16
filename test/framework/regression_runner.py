import math
import re
import time
import os
import shutil
import subprocess
import importlib
from pathlib import Path
import yaml

_STRICT_STRIP_RE = re.compile(r'^\s*DT_fixed\s*=', re.IGNORECASE)
from rich import box
from rich.progress import Progress, SpinnerColumn, TextColumn, TimeElapsedColumn
from rich.table import Table
from test.framework.base_runner import BaseRunner
from test.framework.workspace_utils import get_build_path
from test.framework.results import SimResult, SubsectionResult
from test.framework.html_report import generate as generate_html_report, generate_pdf as generate_pdf_report, ReportMeta

STAMP_FILE = ".build_stamp"
CONFIG_PATH = os.path.join(os.path.dirname(__file__), "..", "regression", "regression_config.yaml")

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

    def _normalize_executables(self, raw):
        result = {}
        for name, spec in raw.items():
            if isinstance(spec, list):
                result[name] = {"cmake_flags": spec, "ref_branch": None}
            else:
                ref_key   = spec.get("ref")
                ref_cfg   = self._refs.get(ref_key, {}) if ref_key else {}
                exe_flags = spec.get("cmake_flags", [])
                ref_flags = ref_cfg.get("cmake_flags", [])
                result[name] = {
                    "cmake_flags": exe_flags + ref_flags,
                    "ref_branch":  ref_cfg.get("branch") if ref_key else None,
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
        hash_ = subprocess.check_output(
            ["git", "-C", source_dir, "rev-parse", "HEAD"],
            stderr=subprocess.DEVNULL
        ).decode().strip()
        dirty = subprocess.call(
            ["git", "-C", source_dir, "diff", "--quiet"],
            stderr=subprocess.DEVNULL
        ) != 0
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

        toolchain_path = os.path.join(self.repo_root, "cmake", "toolchains", "macos_mpi.cmake")
        pfunit_glob = os.path.join(self.repo_root, "extern", "pfunit", "installed", "PFUNIT-*", "cmake")
        import glob as _glob
        pfunit_dirs = _glob.glob(pfunit_glob)
        pfunit_dir = sorted(pfunit_dirs)[-1] if pfunit_dirs else None
        cmake_cmd = ["cmake", "-S", source_dir, "-B", build_dir,
                     f"-DCMAKE_TOOLCHAIN_FILE={toolchain_path}", "-DENABLE_TESTING=ON"]
        if pfunit_dir:
            cmake_cmd.append(f"-DPFUNIT_DIR={pfunit_dir}")
        cmake_cmd += [f"-D{flag}" for flag in (cmake_flags or [])]

        with Progress(SpinnerColumn(), TextColumn("[progress.description]{task.description}"), transient=True) as progress:
            config_task = progress.add_task(f"cmake  {tag}  configuring...", total=None)
            subprocess.run(cmake_cmd, check=True, capture_output=True)
            progress.remove_task(config_task)

            build_task = progress.add_task(f"make   {tag}  compiling...  [dim]0%[/dim]", total=100)
            make_proc = subprocess.Popen(["make", "-C", build_dir, "-j8"],
                                         stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)

            while make_proc.poll() is None:
                line = make_proc.stdout.readline()
                if "[" in line and "%" in line:
                    try:
                        percent = int(line.split("[")[1].split("%")[0].strip())
                        progress.update(build_task, completed=percent,
                                        description=f"make   {tag}  compiling...  [dim]{percent}%[/dim]")
                    except:
                        pass

            if make_proc.returncode != 0:
                raise subprocess.CalledProcessError(make_proc.returncode, "make")

        self._write_stamp(build_dir, source_dir)
        return full_hash, True

    def _expand_simulations(self, simulations):
        expanded = []
        for sim in simulations:
            if 'input_files' in sim:
                for ifile in sim['input_files']:
                    stem = Path(ifile).stem
                    s = {k: v for k, v in sim.items() if k != 'input_files'}
                    s['input_file'] = ifile
                    s['curr_input'] = f"{stem}.yaml"
                    s['name'] = f"{sim['name']}_{stem}"
                    expanded.append(s)
            else:
                expanded.append(sim)
        return expanded

    def _setup_run_dir(self, sim, run_dir, fixed_dt=False):
        os.makedirs(run_dir, exist_ok=True)
        if "output_dir" in sim:
            os.makedirs(os.path.join(run_dir, sim["output_dir"]), exist_ok=True)
        input_dir = os.path.join(self.repo_root, sim["input"])
        for item in os.listdir(input_dir):
            src = os.path.join(input_dir, item)
            if os.path.isfile(src) and item.endswith('.txt'):
                dst = os.path.join(run_dir, item)
                if fixed_dt:
                    shutil.copy2(src, dst)
                else:
                    with open(src) as f:
                        lines = f.readlines()
                    with open(dst, 'w') as f:
                        f.writelines(l for l in lines if not _STRICT_STRIP_RE.match(l))
        data_src = os.path.join(input_dir, 'data')
        if os.path.isdir(data_src):
            data_dst = os.path.join(run_dir, 'data')
            if os.path.exists(data_dst):
                shutil.rmtree(data_dst)
            shutil.copytree(data_src, data_dst)

    def _preprocess(self, sim, run_dir):
        curr_input = sim.get("curr_input", sim["input_file"])
        def subst(arg):
            return (arg
                    .replace("{input_file}", sim["input_file"])
                    .replace("{curr_input}", curr_input)
                    .replace("{repo_root}", self.repo_root)
                    .replace("{run_dir}", run_dir))
        cmd = [subst(a) for a in sim["preprocess"]]
        subprocess.run(cmd, cwd=run_dir, check=True, capture_output=True)

    def _resolve_cmake_flags(self, flags):
        return [f.replace("{repo_root}", self.repo_root) for f in flags]

    def _run_postprocess(self, sim, ref_run_dir, curr_run_dir, ref_status, curr_status,
                         verbose: bool = False) -> SimResult:
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

        all_passed = all(m.passed for s in result.subsections for m in s.metrics
                         if math.isfinite(m.tolerance))
        result.status = "PASS" if all_passed else "FAIL"

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
            "PASS":              "[bold green]✓ PASS[/bold green]",
            "FAIL":              "[bold red]✗ FAIL[/bold red]",
            "XFAIL":             "[yellow]⚠ XFAIL[/yellow]",
            "XPASS":             "[bold red]✗ XPASS[/bold red]",
            "SIM_FAILED":        "[bold red]✗ SIM_FAILED[/bold red]",
            "POSTPROCESS_ERROR": "[yellow]⚠ POSTPROCESS_ERROR[/yellow]",
            "COMPLETED":         "[dim]COMPLETED[/dim]",
        }

        active_kinds = [k for k in KINDS if any(r.subsection(k) for r in results)]

        table = Table(
            box=box.SIMPLE_HEAD,
            header_style="bold cyan",
            show_edge=False,
            pad_edge=True,
        )
        table.add_column("Simulation", min_width=28)
        table.add_column("Status",     min_width=12)
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

    def run(self, filter_tags=None, force=False, report: bool = False, pdf: bool = False,
            verbose: bool = False, stop_on_pass: bool = False, fixed_dt: bool = False):
        try:
            current_branch = subprocess.check_output(
                ["git", "rev-parse", "--abbrev-ref", "HEAD"]).decode().strip()
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
        exe_dirs = {}    # exe_type -> (ref_build_dir, curr_build_dir, ref_branch)
        ref_hashes = {}
        curr_hash = ""
        def _tag(branch, h, rebuilt):
            label = f"{branch}@{h[:8]}"
            status = "[green]built[/green]" if rebuilt else "[dim]cached[/dim]"
            return f"{label}  {status}"

        for exe_type in {s["exe_type"] for s in simulations}:
            spec = self.executables[exe_type]
            cmake_flags = self._resolve_cmake_flags(spec["cmake_flags"])
            ref_branch  = spec["ref_branch"]
            oracle_mode = ref_branch is None   # no ref repo — an analytic oracle stands in

            curr_build_dir = self._exe_build_dir("dev", exe_type)
            first_sim = next(s for s in simulations if s["exe_type"] == exe_type)
            curr_bin  = os.path.join(curr_build_dir, first_sim["binary"])
            curr_hash, curr_rebuilt = self._build(curr_build_dir, self.repo_root, cmake_flags=cmake_flags, binary_path=curr_bin, force=force, label=f"dev/{exe_type}  ({current_branch})")

            # validation exe — one build; the postproc compares against theory, not a ref run
            if oracle_mode:
                exe_dirs[exe_type] = (None, curr_build_dir, None)
                self.reporter.info(f"build \\[{exe_type}]  ref: [dim]oracle (no ref)[/dim]  dev: {_tag(current_branch, curr_hash, curr_rebuilt)}")
                continue

            ref_source    = self._ensure_worktree(ref_branch)
            ref_build_dir = self._exe_build_dir(ref_branch, exe_type)
            ref_bin       = os.path.join(ref_build_dir, first_sim["binary"])
            ref_hash, ref_rebuilt = self._build(ref_build_dir, ref_source, cmake_flags=cmake_flags, binary_path=ref_bin, force=force, label=f"ref/{exe_type}  ({ref_branch})")
            ref_hashes[exe_type] = ref_hash
            exe_dirs[exe_type] = (ref_build_dir, curr_build_dir, ref_branch)
            self.reporter.info(f"build \\[{exe_type}]  ref: {_tag(ref_branch, ref_hash, ref_rebuilt)}  dev: {_tag(current_branch, curr_hash, curr_rebuilt)}")

        def _run_with_spinner(job_id, description):
            t0 = time.time()
            with Progress(SpinnerColumn(), TextColumn("[progress.description]{task.description}"),
                          TimeElapsedColumn(), transient=True) as progress:
                progress.add_task(description, total=None)
                while self.provider.get_status(job_id) not in ["COMPLETED", "FAILED"]:
                    time.sleep(2)
            return self.provider.get_status(job_id), time.time() - t0

        all_passed = True
        sim_results: list[SimResult] = []
        for sim in simulations:
            exe_type = sim["exe_type"]
            ref_build_dir, curr_build_dir, ref_branch = exe_dirs[exe_type]
            curr_input = sim.get("curr_input", sim["input_file"])
            oracle_mode = ref_build_dir is None   # validation — no ref run, compare vs theory

            curr_run_dir = os.path.join(curr_build_dir, "runs", sim["name"])
            ref_run_dir  = os.path.join(ref_build_dir, "runs", sim["name"]) if not oracle_mode else None
            ref_out      = os.path.join(ref_run_dir, sim["output_dir"])   if not oracle_mode else None
            sim_stamp    = os.path.join(ref_out, ".sim_complete")         if not oracle_mode else None

            if force:
                if sim_stamp and os.path.exists(sim_stamp):
                    os.remove(sim_stamp)
                for d in (ref_run_dir, curr_run_dir):
                    if d and os.path.exists(d):
                        shutil.rmtree(d, ignore_errors=True)

            # --- ref (skipped in oracle mode: theory is the reference) ---
            # Stamp records the dt mode that generated the cached ref; a
            # mismatch (or a legacy empty stamp) invalidates it — adaptive
            # refs are not comparable against fixed-dt dev runs or vice versa
            ref_status  = "oracle"
            ref_stderr  = ""
            ref_elapsed = 0.0
            if not oracle_mode:
                dt_mode = "fixed_dt" if fixed_dt else "adaptive"
                stamp_mode = None
                if os.path.exists(sim_stamp):
                    try:
                        stamp_mode = open(sim_stamp).read().strip() or None
                    except OSError:
                        pass
                if stamp_mode == dt_mode:
                    ref_status = "cached"
                else:
                    for d in (ref_run_dir, curr_run_dir):
                        if os.path.exists(d):
                            shutil.rmtree(d, ignore_errors=True)
                    self._setup_run_dir(sim, ref_run_dir, fixed_dt=fixed_dt)
                    ref_input = sim["input_file"]
                    if "preprocess" in sim and sim.get("preprocess_ref", False):
                        self._preprocess(sim, ref_run_dir)
                        ref_input = curr_input
                    ref_id = self.provider.submit(
                        os.path.join(ref_build_dir, sim["binary"]), ref_input, ref_run_dir,
                        np=sim.get("np", 1))
                    ref_status, ref_elapsed = _run_with_spinner(ref_id, f"  \\[{sim['name']}]  ref  running  (np={sim.get('np', 1)})")
                    if ref_status == "COMPLETED":
                        try:
                            os.makedirs(ref_out, exist_ok=True)
                            with open(os.path.join(ref_out, ".sim_complete"), "w") as f:
                                f.write(dt_mode + "\n")
                        except Exception:
                            pass
                    else:
                        all_passed = False
                        _, ref_stderr = self.provider.get_output(ref_id)

            # --- dev ---
            self._setup_run_dir(sim, curr_run_dir, fixed_dt=fixed_dt)
            if "preprocess" in sim:
                self._preprocess(sim, curr_run_dir)
            curr_id = self.provider.submit(
                os.path.join(curr_build_dir, sim["binary"]), curr_input, curr_run_dir,
                np=sim.get("np", 1))
            curr_status, curr_elapsed = _run_with_spinner(curr_id, f"  \\[{sim['name']}]  dev  running  (np={sim.get('np', 1)})")
            dev_stderr = ""
            if curr_status != "COMPLETED":
                all_passed = False
                _, dev_stderr = self.provider.get_output(curr_id)

            # --- execution result line ---
            def _fmt_run(s, elapsed=0.0):
                if s == "cached":    return "[dim]cached[/dim]"
                if s == "oracle":    return "[dim]oracle[/dim]"
                if s == "COMPLETED": return f"[green]ran {elapsed:.0f}s[/green]"
                return f"[red]{s}[/red]"
            run_line = f"  \\[{sim['name']}]  ref: {_fmt_run(ref_status, ref_elapsed)}  dev: {_fmt_run(curr_status, curr_elapsed)}"
            sim_result = self._run_postprocess(sim, ref_run_dir, curr_run_dir, ref_status, curr_status,
                                               verbose=verbose)

            # --- pass/fail result line ---
            STATUS_ICON = {
                "PASS":              "[bold green]✓ PASS[/bold green]",
                "FAIL":              "[bold red]✗ FAIL[/bold red]",
                "XFAIL":             "[yellow]⚠ XFAIL[/yellow]",
                "XPASS":             "[bold red]✗ XPASS[/bold red]",
                "SIM_FAILED":        "[bold red]✗ SIM FAILED[/bold red]",
                "POSTPROCESS_ERROR": "[yellow]⚠ ERROR[/yellow]",
                "COMPLETED":         "[dim]no comparison[/dim]",
            }
            sub_summary = "  ".join(
                f"{s.kind}: {s.summary}" for s in sim_result.subsections
            )
            result_icon = STATUS_ICON.get(sim_result.status, sim_result.status)
            if sim_result.status in ("XFAIL", "XPASS"):
                result_icon += f" [dim]({sim.get('known_fail')})[/dim]"
            result_line = f"  \\[{sim['name']}]  {result_icon}" + (f"  [dim]{sub_summary}[/dim]" if sub_summary else "")

            self.reporter.info(run_line)
            if ref_status == "FAILED" or curr_status == "FAILED":
                for label, err in [("ref", ref_stderr), ("dev", dev_stderr)]:
                    if err:
                        self.reporter.info(f"    {label} stderr: {err[:400]}")

            if sim_result.status == "PASS":
                self.reporter.success(result_line)
            elif sim_result.status == "XPASS":
                self.reporter.error(result_line)
                self.reporter.error(f"    unexpected pass — remove known_fail: {sim.get('known_fail')} from regression_config.yaml")
            elif sim_result.status in ("SIM_FAILED", "POSTPROCESS_ERROR"):
                self.reporter.error(result_line)
            else:
                self.reporter.warn(result_line)
            sim_results.append(sim_result)

            if stop_on_pass and sim_result.status == "PASS":
                self.reporter.info("  [dim]--stop-on-pass: first passing test found, stopping.[/dim]")
                break

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
            with Progress(SpinnerColumn(), TextColumn("[progress.description]{task.description}"),
                          TimeElapsedColumn(), transient=True) as progress:
                progress.add_task("  Generating report...", total=None)
                html_path = generate_html_report(sim_results, meta, base.with_suffix(".html"))
                if want_pdf:
                    pdf_path = generate_pdf_report(sim_results, meta, base.with_suffix(".pdf"))
            self.reporter.info(f"HTML report: file://{html_path}")
            if want_pdf:
                self.reporter.info(f"PDF  report: {pdf_path}")
