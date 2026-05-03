import os
import shutil
import typer
import subprocess
from test.framework.workspace_utils import get_build_path

app = typer.Typer()

@app.command()
def setup(branch: str = typer.Argument(..., help="Branch to set in legacy workspace")):
    """Set up the legacy workspace with the specified branch."""
    legacy_path = get_build_path("legacy")
    if os.path.exists(legacy_path):
        shutil.rmtree(legacy_path)
    os.makedirs(legacy_path)
    
    # Initialize as a worktree
    subprocess.run(["git", "worktree", "add", legacy_path, branch], check=True)
    typer.echo(f"Legacy workspace initialized with branch: {branch}")

@app.command()
def test():
    """Run dev/legacy comparison workflow."""
    dev_build = get_build_path("dev")
    legacy_build = get_build_path("legacy")
    
    # 1. Incremental build
    typer.echo("Building workspaces...")
    # ... trigger builds ...
    
    # 2. Preprocess
    typer.echo("Preprocessing inputs...")
    
    # 3. Run simulations
    typer.echo("Running simulations...")
    
    # 4. Metrics
    typer.echo("Comparing metrics via probes...")

@app.command()
def clean():
    """Clean legacy and dev workspaces."""
    for ws in ["dev", "legacy"]:
        path = get_build_path(ws)
        if os.path.exists(path):
            shutil.rmtree(path)
            typer.echo(f"Cleaned {ws} workspace.")
