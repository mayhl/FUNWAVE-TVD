#!/usr/bin/env python3
import subprocess
import sys
import os

# This acts as the "intermediate layer".
# It delegates to the Typer CLI while being an executable itself.
# It uses 'uv' to ensure the correct environment and dependencies.
CLI_PATH = os.path.join(os.getcwd(), "test", "framework", "cli.py")

def main():
    # Construct the command: uv run python3 test/framework/cli.py [args...]
    # sys.argv[1:] passes all arguments provided to 'fun' to the CLI
    cmd = ["uv", "run", "python3", CLI_PATH] + sys.argv[1:]
    
    # Run the command and pass the return code back to the shell
    try:
        result = subprocess.run(cmd)
        sys.exit(result.returncode)
    except FileNotFoundError:
        print("Error: 'uv' not found. Please ensure it is installed.")
        sys.exit(1)

if __name__ == "__main__":
    main()
