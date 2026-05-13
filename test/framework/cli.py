import glob
import typer
import subprocess
import os
import sys

from test.framework.runners import UnitTestRunner
from test.framework.regression_runner import RegressionRunner
from test.framework.reporters import ConsoleReporter
from test.framework.providers.base import LocalProvider
from test.framework.dev_tool import app as dev_app

PROJ_ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

app = typer.Typer(
    help="""
    FUNWAVE Test Orchestration Engine.
    """,
    context_settings={"help_option_names": ["-h", "--help"]}
)

app.add_typer(dev_app, name="dev", help="Development and refactoring workflow tools.")

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
    runner.run()

@app.command()
def regression(
    branch: str = typer.Option("feat/cmake", "--branch", "-b", help="Reference branch to compare against")
):
    """Run Regression Tests."""
    reporter = ConsoleReporter()
    provider = LocalProvider()
    runner = RegressionRunner(reporter, provider, ref_branch=branch)
    runner.run()

from test.framework.workspace_utils import get_build_path, setup_workspace

@app.command()
def install():
    """Install all build dependencies (uv Python env + pFUnit)."""
    typer.echo("Syncing Python environment...")
    subprocess.run(["uv", "sync", "-q"], cwd=PROJ_ROOT, check=True)

    pfunit_pattern = os.path.join(PROJ_ROOT, "extern", "pfunit", "installed", "PFUNIT-*")
    if glob.glob(pfunit_pattern):
        typer.echo("pFUnit already installed — skipping.")
    else:
        typer.echo("Building pFUnit...")
        script = os.path.join(PROJ_ROOT, "scripts", "install_deps.sh")
        subprocess.run(["bash", script], cwd=PROJ_ROOT, check=True)

    typer.echo("All dependencies ready.")

@app.command()
def setup(workspace: str = typer.Argument("dev", help="Workspace name to initialize")):
    """Initialize a workspace directory structure."""
    path = setup_workspace(workspace)
    typer.echo(f"Workspace '{workspace}' initialized at: {path}")

@app.command(context_settings={"allow_extra_args": True, "ignore_unknown_options": True})
def build(
    ctx: typer.Context,
    workspace: str = typer.Option("dev", "--workspace", "-w", help="Workspace name to build in")
):
    """Wraps 'cmake' and 'make' for building."""
    build_dir = get_build_path(workspace)
    src_root = os.environ.get("FUNWAVE_SRC_ROOT", os.getcwd())
    
    # Ensure workspace exists
    os.makedirs(build_dir, exist_ok=True)
    
    # Configure command (simplified)
    config_cmd = ["cmake", "-S", src_root, "-B", build_dir] + ctx.args
    print(f"Configuring workspace '{workspace}': {' '.join(config_cmd)}")
    subprocess.run(config_cmd)
    
    # Build command
    build_cmd = ["make", "-C", build_dir, "-j8"]
    print(f"Building workspace '{workspace}': {' '.join(build_cmd)}")
    subprocess.run(build_cmd)

@app.command(context_settings={"allow_extra_args": True, "ignore_unknown_options": True})
def clean(ctx: typer.Context):
    """Wraps standard workspace cleanup."""
    command = ["rm"] + ctx.args
    print(f"Cleaning: {' '.join(command)}")
    subprocess.run(command)

if __name__ == "__main__":
    app()
