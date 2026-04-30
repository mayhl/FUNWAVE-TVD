#!/bin/bash
# Usage: ./manage_refactor.sh refresh | run [file_to_move] [test_input]

CMD=$1

if [ "$CMD" == "refresh" ]; then
    echo "Refreshing reference environment..."
    cd ../regression_ref/src/funwave_reference || exit
    git fetch origin
    git checkout master # or target branch
    git pull origin master
    cd ../funwave_reference-build || exit
    cmake .
    echo "Refresh complete."

elif [ "$CMD" == "run" ]; then
    FILE_TO_MOVE=$2
    TEST_INPUT=$3
    
    if [ -z "$FILE_TO_MOVE" ] || [ -z "$TEST_INPUT" ]; then
        echo "Usage: $0 run [file_to_move] [test_input]"
        exit 1
    fi

    echo "Running instrumented test..."
    cd ../regression_ref/src/funwave_reference || exit
    FILE_NAME=$(basename "$FILE_TO_MOVE")
    
    # Ensure file is in src/
    if [ -f "$FILE_TO_MOVE" ]; then
        mv "$FILE_TO_MOVE" "src/$FILE_NAME"
    fi
    
    cd ../funwave_reference-build || exit
    make -j4
    
    # Run singular test (adjust path to executable as needed)
    ./funwave "$TEST_INPUT" ./outputs/new
    echo "Execution complete."
else
    echo "Unknown command. Use 'refresh' or 'run'."
    exit 1
fi
