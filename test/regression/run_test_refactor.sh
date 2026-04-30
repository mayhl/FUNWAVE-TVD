#!/bin/bash
# Usage: ./run_test_refactor.sh [branch_name] [file_to_move]
# Example: ./run_test_refactor.sh master src/old/io.F

TARGET_BRANCH=${1:-master}
FILE_TO_MOVE=${2} # e.g., src/old/io.F

if [ -z "$FILE_TO_MOVE" ]; then
    echo "Usage: $0 [branch_name] [file_to_move]"
    exit 1
fi

echo "Regression testing (refactor mode) against branch: $TARGET_BRANCH"
echo "Moving $FILE_TO_MOVE to src/"

# Navigate to the reference worktree/repo
# Adjust path based on where run_test.sh points
cd ../regression_ref/src/funwave_reference || exit
git fetch origin
git checkout "$TARGET_BRANCH"
git pull origin "$TARGET_BRANCH"

# Move the file and patch the build system (simple example)
# Assuming file name is the same
FILE_NAME=$(basename "$FILE_TO_MOVE")
mv "$FILE_TO_MOVE" "src/$FILE_NAME"

# Note: You may need to update src/CMakeLists.txt and src/old/CMakeLists.txt
# to reflect the move. For a temporary refactor, you can use sed:
# sed -i "/$FILE_NAME/d" src/old/CMakeLists.txt
# echo "  $FILE_NAME" >> src/CMakeLists.txt

# Build the reference executable
cd ../funwave_reference-build || exit
cmake .
make -j4

# Execute and compare...
echo "Refactor build complete. Ready for comparison."
