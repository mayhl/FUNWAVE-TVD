#!/bin/bash

MPIRUN="mpirun -n"

N_PROCS=$1
EXEC=$2
INPUT=$3
OUT_DPATH=$4

if [ ! -f "$EXEC" ]; then
  echo "Executable not found."
  echo "PATH: $EXEC"
  exit 1
fi

if [ ! -f "$INPUT" ]; then
  echo "Input file not found."
  echo "PATH: $INPUT"
  exit 1
fi

rm -r "${OUT_DPATH:?}"
mkdir -p "$OUT_DPATH"

spinner() {
  local PID=$!
  local delay=0.75
  local spinstr='|/-\'
  printf "Running: "
  while kill -0 $PID 2>/dev/null; do
    local temp=${spinstr#?}
    printf " [%c]  " "$spinstr"
    local spinstr=$temp${spinstr%"$temp"}
    sleep $delay
    printf "\b\b\b\b\b\b"
  done
  printf "    \b\b\b\b COMPLETED\n"

}

EXEC=$(realpath "$EXEC")
INPUT=$(realpath "$INPUT")
CWD=$(pwd)
cd "$OUT_DPATH" || return 1

(mpirun -np $N_PROCS $EXEC $INPUT 2>err.out 1>std.out) &
spinner

cd "$CWD" || return 1
# TODO: Add finished check
exit 0
