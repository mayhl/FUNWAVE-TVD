#!/usr/bin/env bash
# setup_cli.sh: Configures the FUNWAVE CLI environment.

set -e

# 1. Install/Verify uv
if ! command -v uv &>/dev/null; then
  echo ""
  echo "Installing uv..."
  curl -LsSf https://astral.sh/uv/install.sh | sh

  if [ -f "$HOME/.cargo/env" ]; then
    echo "Sourcing .cargo/env to update PATH..."
    source "$HOME/.cargo/env"
  else
    echo "Note: uv installed, but ~/.cargo/env not found."
  fi
fi

# Ensure uv is in PATH before proceeding
if ! command -v uv &>/dev/null; then
  SHELL_NAME=$(basename "$SHELL")
  CONFIG_FILE=""
  case "$SHELL_NAME" in
  zsh) CONFIG_FILE="~/.zshrc" ;;
  bash) CONFIG_FILE="~/.bashrc" ;;
  *) CONFIG_FILE="your shell profile" ;;
  esac
  echo ""
  echo "--------------------------------------------------------"
  echo "ERROR: 'uv' is not found in your current PATH."
  echo "Please run: source $CONFIG_FILE"
  echo "Then, rerun 'scripts/setup_cli.sh'."
  echo "--------------------------------------------------------"
  exit 1
fi

# 2. Setup sync
echo ""
echo "Syncing Python environment..."
uv sync -q

# 3. Create system variable FUNWAVE_SRC_ROOT
# We use the current directory as the anchor
export FUNWAVE_SRC_ROOT=$(pwd)
echo ""
echo "Setting FUNWAVE_SRC_ROOT to $FUNWAVE_SRC_ROOT"

# 4. Configure Environment
echo "How would you like to persist the FUNWAVE CLI environment?"
echo "1) Automatic: Add to ~/.zshrc or ~/.bashrc"
echo "2) Manual: Create 'fun.env' to source when needed"
read -p "Select [1 or 2]: " choice

# Get absolute path to project root
SCRIPT_DIR="${BASH_SOURCE[0]:-%x}"
SCRIPT_DIR="$(cd "$(dirname "$SCRIPT_DIR")" && pwd)"
export FUNWAVE_SRC_ROOT="$(dirname "$SCRIPT_DIR")"

if [ "$choice" == "1" ]; then
  SHELL_CONFIG=""
  [ -f "$HOME/.zshrc" ] && SHELL_CONFIG="$HOME/.zshrc"
  [ -z "$SHELL_CONFIG" ] && [ -f "$HOME/.bashrc" ] && SHELL_CONFIG="$HOME/.bashrc"

  if [ -n "$SHELL_CONFIG" ]; then
    if ! grep -q "FUNWAVE_SRC_ROOT" "$SHELL_CONFIG"; then
      echo ""
      echo "export FUNWAVE_SRC_ROOT=\"$FUNWAVE_SRC_ROOT\"" >>"$SHELL_CONFIG"
      echo "export PATH=\"\$PATH:\$FUNWAVE_SRC_ROOT/bin\"" >>"$SHELL_CONFIG"
      echo "Added environment variables to $SHELL_CONFIG"
    fi
  fi
else
  cat <<EOF >"$FUNWAVE_SRC_ROOT/fun.env"
export FUNWAVE_SRC_ROOT="$FUNWAVE_SRC_ROOT"
export PATH="\$PATH:\$FUNWAVE_SRC_ROOT/bin"
EOF
  echo ""
  echo "Created '$FUNWAVE_SRC_ROOT/fun.env'. Run 'source fun.env' to activate."
fi

# 5. Install Typer autocompletion (User will need to re-source or restart shell)
echo ""
echo "Note: Typer autocompletion can be installed by running './bin/fun --install-completion'"

echo ""
echo "CLI setup complete. Please restart your terminal or source your shell config."
