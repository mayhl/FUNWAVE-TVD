import typer
import subprocess
import os
import sys

# Since 'test' is now a proper package via pyproject.toml, 
# imports should be absolute from the project root.
from test.framework.runners import UnitTestRunner
from test.framework.regression_runner import RegressionRunner
from test.framework.reporters import ConsoleReporter
from test.framework.providers.base import LocalProvider

app = typer.Typer(
    help="""
    FUNWAVE Test Orchestration Engine.
    
    This CLI provides a unified interface for building the FUNWAVE-TVD model
    and orchestrating various test tiers including unit (pFUnit) and
    regression testing.
    """,
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

@app.command(context_settings={"allow_extra_args": True, "ignore_unknown_options": True})
def build(ctx: typer.Context):
    """Wraps 'make' for building."""
    command = ["make"] + ctx.args
    print(f"Executing build: {' '.join(command)}")
    subprocess.run(command)

@app.command(context_settings={"allow_extra_args": True, "ignore_unknown_options": True})
def clean(ctx: typer.Context):
    """Wraps standard workspace cleanup."""
    command = ["rm"] + ctx.args
    print(f"Cleaning: {' '.join(command)}")
    subprocess.run(command)

if __name__ == "__main__":
    app()
