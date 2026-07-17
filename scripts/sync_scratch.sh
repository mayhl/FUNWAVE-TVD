#!/bin/sh
# Mirror the working tree (incl. git history) onto a mounted HPC scratch
# copy, so the regression suite can build + run natively on a compute node.
#
# Usage: scripts/sync_scratch.sh <mounted-scratch-path>/mayhlFUNWAVE
#
# NOTE 1: --delete keeps the mirror honest (case-renames like logging.f90 ->
#   logging.F90 otherwise leave BOTH files on case-sensitive Linux); excluded
#   dirs are protected from deletion, so HPC-side builds survive resyncs.
# NOTE 2: git worktree registrations carry absolute local paths — excluded;
#   run `git worktree prune` once on the HPC side, the regression runner
#   recreates its ref worktrees on demand.
# NOTE 3: .private_docs / .claude stay local by policy.
set -eu

dst=${1:?usage: sync_scratch.sh <mounted-scratch-path>}
src=$(cd "$(dirname "$0")/.." && pwd)

rsync -a --delete --info=stats1 \
	--exclude='/build*/' \
	--exclude='/workspaces/' \
	--exclude='/.parity/' \
	--exclude='/.refs/' \
	--exclude='/.private_docs/' \
	--exclude='/.claude/' \
	--exclude='/sol_leg/' \
	--exclude='/examples_FD/' \
	--exclude='/test/regression/worktrees/' \
	--exclude='/.git/worktrees/' \
	--exclude='__pycache__' \
	--exclude='.venv' \
	--exclude='.DS_Store' \
	"$src/" "$dst/"

printf 'synced %s -> %s\n' "$src" "$dst"
