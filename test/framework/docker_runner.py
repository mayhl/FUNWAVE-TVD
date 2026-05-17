"""Docker build + test runner — mirrors the CI workflow locally.

For each compiler under docker/<name>/Dockerfile:
  1. docker build   (tagged funwave-<name>:test)
  2. docker run     single container — cmake build then ctest

Runs sequentially; streams output in verbose mode, captures otherwise.
"""
from __future__ import annotations

import re
import subprocess
import time
from dataclasses import dataclass, field
from itertools import product
from pathlib import Path

from rich import box
from rich.console import Console
from rich.panel import Panel
from rich.progress import Progress, SpinnerColumn, TextColumn, TimeElapsedColumn
from rich.table import Table
from rich.text import Text

DOCKER_DIR = Path(__file__).parent.parent.parent / "docker"

_console = Console()

_DISPLAY_NAME: dict[str, str] = {
    "gnu":   "GNU",
    "intel": "Intel",
    "llvm":  "LLVM",
    "nvhpc": "NVHPC",
    "local": "Local",
}

def _display(name: str, build_type: str | None = None) -> str:
    base = _DISPLAY_NAME.get(name, name.upper())
    return f"{base} ({build_type})" if build_type else base

# cmake --build:  "[ 42%] Building Fortran object ..."
_CMAKE_PCT_RE = re.compile(r"\[\s*(\d+)%\]")
# ctest:          "  3/12 Test #3: test_name ..."
_CTEST_RE     = re.compile(r"^\s*(\d+)/(\d+)\s+Test")


DEFAULT_BUILD_TYPES = ["RelWithDebInfo"]

@dataclass
class ImageResult:
    name:           str
    build_type:     str   = "Release"
    build_status:   str   = "pending"   # pending | ok | failed
    compile_status: str   = "pending"
    test_status:    str   = "pending"
    build_time:     float = 0.0
    compile_time:   float = 0.0
    test_time:      float = 0.0
    build_log:      str   = ""
    compile_log:    str   = ""
    test_log:       str   = ""


def discover(filter_names: list[str] | None = None) -> list[str]:
    """Return compiler names.

    Docker-based compilers come from docker/*/Dockerfile.  The special
    name ``"local"`` skips Docker entirely and runs cmake+ctest in the
    host environment; it is included only when explicitly requested via
    ``filter_names``.
    """
    docker_names = sorted(
        d.name for d in DOCKER_DIR.iterdir()
        if d.is_dir() and (d / "Dockerfile").exists()
    )
    if filter_names:
        names = [n for n in docker_names if n in filter_names]
        if "local" in filter_names:
            names.insert(0, "local")
    else:
        names = ["local"] + docker_names
    return names


def _popen(cmd: list[str]) -> subprocess.Popen:
    return subprocess.Popen(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True,
    )


def _log(level: str, name: str, msg: str) -> None:
    style = {"INFO": "dim", "PASS": "bold green", "FAIL": "bold red"}[level]
    _console.print(f"[{style}]{level}:[/{style}]  {name}  {msg}")


# ---------------------------------------------------------------------------
# Phase runners
# ---------------------------------------------------------------------------

def _run_build(cmd: list[str], verbose: bool, no_cache: bool) -> tuple[int, str]:
    full_cmd = list(cmd)
    if no_cache:
        full_cmd += ["--no-cache"]
    proc = _popen(full_cmd)
    assert proc.stdout is not None
    lines: list[str] = []
    for line in proc.stdout:
        lines.append(line)
        if verbose:
            _console.print(f"[dim]  {line.rstrip()}[/dim]")
    proc.wait()
    return proc.returncode, "".join(lines)


def _ccache_mount(name: str) -> list[str]:
    """Return docker run args to mount a per-compiler ccache volume."""
    import os
    cache_dir = Path.home() / ".cache" / "funwave-ccache" / name
    cache_dir.mkdir(parents=True, exist_ok=True)
    return ["-v", f"{cache_dir}:/ccache"]


def _run_compile_and_test(tag: str, name: str, build_type: str, verbose: bool,
                          prog: Progress | None = None,
                          task_id: int | None = None) -> tuple[int, str, int, str, float, float]:
    """Single docker run — cmake build then ctest, both phases in one container."""
    cmd = ["docker", "run", "--rm", "-e", f"BUILD_TYPE={build_type}",
           *_ccache_mount(name), tag]
    proc = _popen(cmd)
    assert proc.stdout is not None

    compile_lines: list[str] = []
    test_lines:    list[str] = []
    phase      = "compile"
    t_start    = time.monotonic()
    t_split    = 0.0
    cmake_done = False

    for line in proc.stdout:
        if verbose:
            _console.print(f"[dim]  {line.rstrip()}[/dim]")

        if phase == "compile":
            compile_lines.append(line)
            m = _CMAKE_PCT_RE.search(line)
            if m:
                if int(m.group(1)) == 100:
                    cmake_done = True
            elif _CTEST_RE.search(line) or "Test project" in line \
                    or line.strip() == "=== FUNWAVE TESTS ===" \
                    or (cmake_done and line.strip()):
                phase   = "test"
                t_split = time.monotonic()
                if prog is not None and task_id is not None:
                    prog.update(task_id, description=f"  {_display(name, build_type)}  testing…")
                test_lines.append(line)
        else:
            test_lines.append(line)

    proc.wait()
    t_end = time.monotonic()

    if phase == "compile":
        compile_rc   = proc.returncode
        test_rc      = 0
        compile_secs = t_end - t_start
        test_secs    = 0.0
    else:
        compile_rc   = 0
        test_rc      = proc.returncode
        compile_secs = (t_split - t_start) if t_split else (t_end - t_start)
        test_secs    = (t_end - t_split)   if t_split else 0.0

    return compile_rc, "".join(compile_lines), test_rc, "".join(test_lines), compile_secs, test_secs


def _run_local(build_type: str, verbose: bool,
               prog: Progress | None = None,
               task_id: int | None = None) -> tuple[int, str, int, str, float, float]:
    """Run cmake build then ctest in the local host environment (no Docker).

    Uses a dedicated build dir (``build/local-<build_type>``) so it never
    clobbers the developer's normal ``build/`` directory.
    """
    import os
    repo_root = DOCKER_DIR.parent
    env = os.environ.copy()
    env["BUILD_TYPE"] = build_type
    env["FUNWAVE_BUILD_DIR"] = str(repo_root / "build" / f"local-{build_type.lower()}")

    cmd = [str(repo_root / "bin" / "fun-dev"), "unit", "--mode", "ci"]
    proc = subprocess.Popen(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True,
        env=env, cwd=str(repo_root),
    )
    assert proc.stdout is not None

    compile_lines: list[str] = []
    test_lines:    list[str] = []
    phase      = "compile"
    t_start    = time.monotonic()
    t_split    = 0.0
    cmake_done = False

    for line in proc.stdout:
        if verbose:
            _console.print(f"[dim]  {line.rstrip()}[/dim]")
        if phase == "compile":
            compile_lines.append(line)
            m = _CMAKE_PCT_RE.search(line)
            if m:
                if int(m.group(1)) == 100:
                    cmake_done = True
            elif _CTEST_RE.search(line) or "Test project" in line \
                    or line.strip() == "=== FUNWAVE TESTS ===" \
                    or (cmake_done and line.strip()):
                phase   = "test"
                t_split = time.monotonic()
                if prog is not None and task_id is not None:
                    prog.update(task_id, description=f"  {_display('local', build_type)}  testing…")
                test_lines.append(line)
        else:
            test_lines.append(line)

    proc.wait()
    t_end = time.monotonic()

    if phase == "compile":
        compile_rc   = proc.returncode
        test_rc      = 0
        compile_secs = t_end - t_start
        test_secs    = 0.0
    else:
        compile_rc   = 0
        test_rc      = proc.returncode
        compile_secs = (t_split - t_start) if t_split else (t_end - t_start)
        test_secs    = (t_end - t_split)   if t_split else 0.0

    return compile_rc, "".join(compile_lines), test_rc, "".join(test_lines), compile_secs, test_secs


# ---------------------------------------------------------------------------
# Display helpers
# ---------------------------------------------------------------------------

def _status_cell(status: str) -> Text:
    if status == "ok":
        return Text("✓ PASS", style="bold green")
    if status == "failed":
        return Text("✗ FAIL", style="bold red")
    return Text("—", style="dim")


def _summary_table(results: list[ImageResult]) -> Table:
    show_image  = any(r.name != "local" for r in results)
    table = Table(
        box=box.SIMPLE_HEAD,
        header_style="bold cyan",
        show_edge=False,
        pad_edge=True,
        title="[bold]Docker CI Results[/bold]",
        title_justify="left",
    )
    table.add_column("Compiler",    min_width=12)
    table.add_column("Config",      min_width=14)
    if show_image:
        table.add_column("Image",       min_width=8)
    table.add_column("Compile",     min_width=8)
    table.add_column("Tests",       min_width=8)
    if show_image:
        table.add_column("Image (s)",   justify="right", min_width=10)
    table.add_column("Compile (s)", justify="right", min_width=11)
    table.add_column("Test (s)",    justify="right", min_width=10)

    for r in results:
        row = [_display(r.name), r.build_type]
        if show_image:
            row.append(_status_cell(r.build_status))
        row += [
            _status_cell(r.compile_status),
            _status_cell(r.test_status),
        ]
        if show_image:
            row.append(f"[dim]{r.build_time:.1f}[/dim]" if r.build_time else "[dim]—[/dim]")
        row += [
            f"[dim]{r.compile_time:.1f}[/dim]" if r.compile_time else "[dim]—[/dim]",
            f"[dim]{r.test_time:.1f}[/dim]"    if r.test_time    else "[dim]—[/dim]",
        ]
        table.add_row(*row)
    return table


def _show_log(log: str, title: str) -> None:
    _console.print(Panel(log[-3000:], title=title, border_style="red", expand=False))


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def run(
    filter_names: list[str] | None = None,
    build_types:  list[str] | None = None,
    no_build: bool = False,
    no_cache: bool = False,
    verbose: bool = False,
) -> list[ImageResult]:
    names = discover(filter_names)
    build_types = build_types or DEFAULT_BUILD_TYPES
    if not names:
        _console.print("[yellow]No Dockerfiles found matching filter.[/yellow]")
        return []

    _console.rule("[bold cyan]Docker Testing Suite[/bold cyan]")
    results = [ImageResult(name=n, build_type=bt) for n, bt in product(names, build_types)]
    repo_root = DOCKER_DIR.parent

    built: set[str] = set()

    for r in results:
        tag        = f"funwave-{r.name}:test"
        dockerfile = str(DOCKER_DIR / r.name / "Dockerfile")
        dn         = _display(r.name)

        _log("INFO", f"{dn} ({r.build_type})", "starting…")

        with Progress(SpinnerColumn(), TextColumn("{task.description}"),
                      TimeElapsedColumn(),
                      console=_console, transient=True) as prog:
            first_desc = (
                f"  {dn}  building…"
                if (r.name != "local" and not no_build and r.name not in built)
                else f"  {dn} ({r.build_type})  compiling…"
            )
            tid = prog.add_task(first_desc, total=None)

            if r.name == "local":
                # ── Local: skip Docker entirely ────────────────────────
                r.build_status = "n/a"
                compile_rc, compile_log, test_rc, test_log, compile_secs, test_secs = \
                    _run_local(r.build_type, verbose, prog, tid)
                r.compile_log    = compile_log
                r.compile_time   = compile_secs
                r.compile_status = "ok" if compile_rc == 0 else "failed"
                r.test_log       = test_log
                r.test_time      = test_secs
                r.test_status    = "ok" if test_rc == 0 else "failed"
            else:
                # ── 1. Build image (once per compiler) ─────────────────
                if not no_build and r.name not in built:
                    build_cmd = ["docker", "build", "--file", dockerfile,
                                 "--tag", tag, str(repo_root)]
                    t0 = time.monotonic()
                    rc, log = _run_build(build_cmd, verbose, no_cache)
                    r.build_time   = time.monotonic() - t0
                    r.build_log    = log
                    r.build_status = "ok" if rc == 0 else "failed"
                    if r.build_status == "ok":
                        built.add(r.name)
                        prog.update(tid, description=f"  {dn} ({r.build_type})  compiling…")
                else:
                    r.build_status = "ok"

                # ── 2 & 3. Compile + Test ──────────────────────────────
                if r.build_status == "ok":
                    compile_rc, compile_log, test_rc, test_log, compile_secs, test_secs = \
                        _run_compile_and_test(tag, r.name, r.build_type, verbose, prog, tid)
                    r.compile_log    = compile_log
                    r.compile_time   = compile_secs
                    r.compile_status = "ok" if compile_rc == 0 else "failed"
                    r.test_log       = test_log
                    r.test_time      = test_secs
                    r.test_status    = "ok" if test_rc == 0 else "failed"
                else:
                    r.compile_status = r.test_status = "failed"

        # ── Result ─────────────────────────────────────────────────────
        label = f"{dn} ({r.build_type})"
        if r.build_status == "failed":
            _log("FAIL", label, "build failed")
            if not verbose:
                _show_log(r.build_log, "Build log (tail)")
        elif r.compile_status == "failed":
            _log("FAIL", label, "compile failed")
            if not verbose:
                _show_log(r.compile_log, "Compile log (tail)")
        elif r.test_status == "failed":
            _log("FAIL", label, "tests failed")
            if not verbose:
                _show_log(r.test_log, "Test log (tail)")
        else:
            _log("PASS", label, "all tests passed")

    _console.print()
    _console.print(_summary_table(results))
    return results
