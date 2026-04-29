#!/bin/bash
# Usage: ./run_test.sh [branch_name]

TARGET_BRANCH=${1:-master}
echo "Regression testing against branch: $TARGET_BRANCH"

# Navigate to the reference directory and checkout the branch
cd ../regression_ref/src/funwave_reference || exit
git fetch origin
git checkout "$TARGET_BRANCH"
git pull origin "$TARGET_BRANCH"

# Build the reference executable (if needed)
cd ../funwave_reference-build || exit
cmake .
make -j4

# Paths for comparison
O_EPATH=./funwave
N_EPATH=../../../exe_funwave

# Execution
#./exec_mpi.sh 4 $O_EPATH ./inputs/beach_2d.txt ./outputs/old
#./exec_mpi.sh 4 $N_EPATH ./inputs/beach_2d.txt ./outputs/new
