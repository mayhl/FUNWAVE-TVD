import glob
import typer
import subprocess
import os

from test.framework.runners import UnitTestRunner
from test.framework.regression_runner import RegressionRunner
from test.framework.docker_runner import run as docker_run, DEFAULT_BUILD_TYPES
from test.framework.reporters import ConsoleReporter
from test.framework.providers.base import LocalProvider
from test.framework.workspace_utils import get_build_path, setup_workspace

PROJ_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

app = typer.Typer(
    help="FUNWAVE Test Orchestration Engine.",
    context_settings={"help_option_names": ["-h", "--help"]}
)

@app.callback()
def main():
    """Unified CLI for FUNWAVE build and test lifecycle."""
    pass

@app.command()
def unit(
    mode: str = typer.Option("dev", "--mode", "-m", help="Execution mode (ci/dev)"),
    build_dir: str = typer.Option(None, "--build-dir", "-b", help="Override FUNWAVE_BUILD_DIR")
):
    """Run Unit Tests (pFUnit) with a live dashboard."""
    reporter = ConsoleReporter()
    runner = UnitTestRunner(reporter, mode=mode, build_dir=build_dir)
    passed = runner.run()
    if not passed:
        raise typer.Exit(1)

@app.command()
def regression(
    tags: list[str] = typer.Option(None, "--tag", "-t", help="Filter tests by tag (repeat for multiple)"),
    force: bool = typer.Option(False, "--force", "-f", help="Force rebuild even if binaries are up to date"),
    report: bool = typer.Option(False, "--report", "-r", help="Generate HTML report after run"),
    pdf: bool = typer.Option(False, "--pdf", help="Generate PDF report after run (implies --report)"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Show detail tables for all tests (default: only on failure)"),
    stop_on_pass: bool = typer.Option(False, "--stop-on-pass", "-1", help="Stop after the first passing test"),
):
    """Run Regression Tests."""
    reporter = ConsoleReporter()
    provider = LocalProvider()
    runner = RegressionRunner(reporter, provider)
    runner.run(filter_tags=tags or None, force=force, report=report, pdf=pdf, verbose=verbose,
               stop_on_pass=stop_on_pass)

@app.command()
def suite(
    filter_names: list[str] = typer.Option(None, "--filter", "-f", help="Compiler names to run (repeat for multiple; use 'local' to run in host environment instead of Docker)"),
    build_types:  list[str] = typer.Option(None, "--build-type", "-b", help="CMake build types to test (repeat for multiple; default: RelWithDebInfo)"),
    no_build: bool = typer.Option(False, "--no-build", help="Skip image build, run existing images"),
    no_cache: bool = typer.Option(False, "--no-cache", help="Force fresh build, ignoring Docker layer cache"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Stream full build/test output"),
):
    """Build and test across compiler environments (mirrors CI workflow).

    By default runs all Docker compiler images.  Use ``--filter local``
    to run cmake+ctest in the host environment without Docker.  Docker
    and local targets can be mixed freely.
    """
    results = docker_run(
        filter_names=filter_names or None,
        build_types=build_types or None,
        no_build=no_build,
        no_cache=no_cache,
        verbose=verbose,
    )
    any_failed = any(r.build_status == "failed" or r.test_status == "failed" for r in results)
    raise typer.Exit(1 if any_failed else 0)


@app.command()
def install():
    """Install all build dependencies (uv Python env + pFUnit + HYPRE)."""
    typer.echo("Syncing Python environment...")
    subprocess.run(["uv", "sync", "-q"], cwd=PROJ_ROOT, check=True)

    pfunit_done = bool(glob.glob(os.path.join(PROJ_ROOT, "extern", "pfunit", "installed", "PFUNIT-*")))
    hypre_done  = os.path.isdir(os.path.join(PROJ_ROOT, "extern", "hypre", "installed", "lib"))

    if pfunit_done and hypre_done:
        typer.echo("pFUnit and HYPRE already installed — skipping.")
    else:
        if pfunit_done:
            typer.echo("pFUnit already installed — skipping.")
        if hypre_done:
            typer.echo("HYPRE already installed — skipping.")
        script = os.path.join(PROJ_ROOT, "scripts", "install_deps.sh")
        subprocess.run(["bash", script], cwd=PROJ_ROOT, check=True)

    typer.echo("All dependencies ready.")

@app.command()
def setup(workspace: str = typer.Argument("dev", help="Workspace name to initialize")):
    """Initialize a workspace directory structure."""
    path = setup_workspace(workspace)
    typer.echo(f"Workspace '{workspace}' initialized at: {path}")

if __name__ == "__main__":
    app()
