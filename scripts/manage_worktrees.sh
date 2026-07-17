#!/usr/bin/env bash
# scripts/manage_worktrees.sh

# Get the directory of the script to anchor paths correctly
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
WORKTREES_DIR="$ROOT_DIR/test/regression/worktrees"

if [ ! -d "$WORKTREES_DIR" ]; then
	echo "No regression worktrees directory found."
	exit 0
fi

# List them for the user first
echo "Current managed worktrees:"
git worktree list | grep "test/regression/worktrees" || echo "None found."

echo ""
read -p "Are you sure you want to force-remove all regression worktrees? [y/N]: " confirm
if [[ $confirm == [yY] ]]; then
	# Identify and remove
	worktrees=$(git worktree list | grep "test/regression/worktrees" | awk '{print $1}')

	for wt in $worktrees; do
		echo "Removing worktree: $wt"
		git worktree remove --force "$wt"
	done

	# Clean up directories
	rm -rf "$WORKTREES_DIR"/*
	echo "Cleanup complete."
else
	echo "Cleanup aborted."
fi
