#!/usr/bin/env bash
# setup_dev.sh: Compiles and initializes all development-related dependencies.

set -e

echo "--- FUNWAVE Dev Mode Setup ---"

# 1. Initialize/Sync Python CLI and development tools via setup_cli
echo "Initializing Python environment..."
bash scripts/setup_cli.sh

# 2. Compile pFUnit for unit testing
if [ -f "scripts/install_deps.sh" ]; then
	echo "Compiling pFUnit..."
	bash scripts/install_deps.sh
else
	echo "Warning: scripts/install_deps.sh not found. Skipping pFUnit compilation."
fi

# 3. Finalize setup
echo "--------------------------------------------------------"
echo "Dev Mode setup complete."
echo "Note: If you plan to enable documentation (ENABLE_DOCS=ON),"
echo "ensure 'doxygen' and 'graphviz' are installed on your system."
echo "--------------------------------------------------------"
