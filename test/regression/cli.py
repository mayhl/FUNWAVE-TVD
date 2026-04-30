import typer
import subprocess
import os
import shutil
from pathlib import Path

app = typer.Typer()

@app.command()
def refresh():
    """Refresh the reference build environment."""
    typer.echo("Refreshing reference environment...")
    base_dir = Path("../regression_ref/src/funwave_reference")
    build_dir = Path("../regression_ref/funwave_reference-build")
    
    subprocess.run(["git", "fetch", "origin"], cwd=base_dir, check=True)
    subprocess.run(["git", "checkout", "master"], cwd=base_dir, check=True)
    subprocess.run(["git", "pull", "origin", "master"], cwd=base_dir, check=True)
    
    subprocess.run(["cmake", "."], cwd=build_dir, check=True)
    typer.echo("Refresh complete.")

@app.command()
def run(file_to_move: Path, test_input: Path):
    """Move file to src and run a single test."""
    base_dir = Path("../regression_ref/src/funwave_reference")
    build_dir = Path("../regression_ref/funwave_reference-build")
    
    file_name = file_to_move.name
    dest = base_dir / "src" / file_name
    
    typer.echo(f"Moving {file_to_move} to {dest}...")
    shutil.move(str(file_to_move), str(dest))
    
    typer.echo("Building...")
    subprocess.run(["make", "-j4"], cwd=build_dir, check=True)
    
    typer.echo("Running test...")
    # Adjust executable path and args as needed
    subprocess.run(["./funwave", str(test_input), "./outputs/new"], cwd=build_dir, check=True)
    typer.echo("Execution complete.")

if __name__ == "__main__":
    app()
