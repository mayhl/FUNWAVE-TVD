import os
import subprocess
import time

import yaml
from rich.console import Console, Group
from rich.live import Live
from rich.panel import Panel
from rich.progress import BarColumn, Progress, TaskProgressColumn, TextColumn, TimeElapsedColumn
from rich.table import Table

from test.framework.base_runner import BaseRunner


class UnitTestRunner(BaseRunner):
    """The pFUnit tier: build the test binaries, run them, tabulate."""

    def __init__(self, reporter, mode="dev", build_dir=None, compile_only=False):
        super().__init__(reporter)
        self.mode = mode
        self.build_dir = build_dir or os.environ.get("FUNWAVE_BUILD_DIR", "build")
        self.test_bin_dir = os.path.join(self.build_dir, "test", "core")
        self.compile_only = compile_only

    def _pretty_print_error(self, log):
        repo_root = os.environ.get("FUNWAVE_SRC_ROOT", os.getcwd())
        log = log.replace(repo_root, ".")

        lines = log.splitlines()
        formatted = []

        for line in lines:
            if "At line" in line and "of file" in line:
                formatted.append(f"[yellow]{line}[/yellow]")
            elif "runtime error" in line:
                formatted.append(f"[bold red]{line}[/bold red]")
            elif "#0" in line:
                formatted.append("\n[dim]... [stack trace hidden] ...[/dim]")
                break
            else:
                formatted.append(line)

        return "\n".join(formatted)

    def run(self):
        """Run the unit tests in dev or ci mode; True when all pass."""
        self.reporter.step(f"Running Unit Tests ({self.mode.upper()} mode)")
        self.reporter.step("")
        if self.mode == "ci":
            self.reporter.info("CI Mode: Performing full build...")
            build_type = os.environ.get("BUILD_TYPE", "RelWithDebInfo")
            import platform

            testing = "OFF" if self.compile_only else "ON"
            cmake_args = [
                "cmake",
                "-S",
                ".",
                "-B",
                self.build_dir,
                f"-DENABLE_UNIT_TESTING={testing}",
                "-DENABLE_DEV_MODE=ON",
                f"-DCMAKE_BUILD_TYPE={build_type}",
            ]
            # macOS FindMPI misdetects under bare gfortran -> default FC to
            # the OpenMPI wrapper (explicit FC always wins)
            env = os.environ.copy()
            if platform.system() == "Darwin":
                env.setdefault("FC", "mpif90")
            subprocess.run(cmake_args, check=True, env=env)
            nproc = os.cpu_count() or 4
            subprocess.run(["cmake", "--build", self.build_dir, f"-j{nproc}"], check=True)
            if not self.compile_only:
                print("=== FUNWAVE TESTS ===", flush=True)

        if self.compile_only:
            self.reporter.success("Compile-only mode: build finished, tests skipped.")
            return True

        # Load test groups from YAML configuration
        config_path = os.path.join(os.environ.get("FUNWAVE_SRC_ROOT", os.getcwd()), "test/unit/test_config.yaml")
        with open(config_path) as f:
            config = yaml.safe_load(f)
            groups_from_yaml = {g["name"]: g["tests"] for g in config["groups"]}
            all_defined_tests = [t for tests in groups_from_yaml.values() for t in tests]

        # Discover all available tests from ctest
        ctest_list_proc = subprocess.run(["ctest", "-N"], cwd=self.build_dir, capture_output=True, text=True)
        available_tests = [line.strip() for line in ctest_list_proc.stdout.splitlines() if "Test #" in line]
        available_test_names = [line.split(":", 1)[1].split()[0].strip() for line in available_tests if ":" in line]

        # Orphans: a suite ctest knows but test_config.yaml does not never
        # runs -- three suites sat there silently (audit 2026-08-22), so CI
        # mode fails on them rather than warning
        orphans = [t for t in available_test_names if t not in all_defined_tests]
        for test in orphans:
            self.reporter.warn(f"Test '{test}' discovered by ctest is missing from test_config.yaml")
        if orphans and self.mode == "ci":
            self.reporter.error(f"{len(orphans)} registered suite(s) not in test_config.yaml")
            return False

        groups = groups_from_yaml

        results = []
        common_columns = (
            TextColumn("[progress.description]{task.description:<10}"),
            BarColumn(),
            TaskProgressColumn(),
            TextColumn("([green]P:[/green]{task.fields[passed]} [red]F:[/red]{task.fields[failed]})"),
            TimeElapsedColumn(),
        )

        progress = Progress(*common_columns)
        tasks = {name: progress.add_task(name, total=len(files), passed=0, failed=0) for name, files in groups.items()}

        summary_progress = Progress(*common_columns)
        summary_task = summary_progress.add_task("Total", total=sum(len(f) for f in groups.values()), passed=0, failed=0)

        dashboard = Group(progress, summary_progress)

        with Live(dashboard, refresh_per_second=10):
            for group, files in groups.items():
                for test_name in files:
                    start = time.time()

                    # Use ctest to execute the test, leveraging CMake's CTestTestfile configuration
                    proc = subprocess.run(
                        # a name that matches nothing must not pass (ctest -R exits 0)
                        ["ctest", "-R", f"^{test_name}$", "--no-tests=error", "--output-on-failure"],
                        cwd=self.build_dir,
                        capture_output=True,
                        text=True,
                    )

                    duration = time.time() - start
                    passed = proc.returncode == 0
                    log = proc.stdout if not passed else ""
                    results.append(
                        {"name": test_name, "status": "Pass" if passed else "Fail", "time": f"{duration:.2f}s", "log": log}
                    )

                    task_id = tasks[group]
                    progress.update(
                        task_id,
                        advance=1,
                        passed=progress.tasks[task_id].fields["passed"] + (1 if passed else 0),
                        failed=progress.tasks[task_id].fields["failed"] + (0 if passed else 1),
                    )

                    summary = summary_progress.tasks[summary_task].fields
                    summary_progress.update(
                        summary_task,
                        advance=1,
                        passed=summary["passed"] + (1 if passed else 0),
                        failed=summary["failed"] + (0 if passed else 1),
                    )

        Console().print("")
        self.display_results_table(results)

        # Display logs for failures
        for res in results:
            if res["status"] == "Fail" and res["log"]:
                formatted_log = self._pretty_print_error(res["log"])
                Console().print(Panel(formatted_log, title=f"Error Log: {res['name']}", border_style="red"))

        self.reporter.success("Unit tests execution finished.")
        return all(r["status"] == "Pass" for r in results)

    def display_results_table(self, results):
        """Print the per-test status table."""
        table = Table(title="Test Execution Summary")
        table.add_column("Test File", style="cyan", no_wrap=True)
        table.add_column("Status", style="magenta")
        table.add_column("Execution Time", justify="right", style="green")

        for res in results:
            status_color = "green" if res["status"] == "Pass" else "red"
            table.add_row(res["name"], f"[{status_color}]{res['status']}[/{status_color}]", res["time"])

        Console().print(table)
