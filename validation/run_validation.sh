#!/bin/bash

# This runs the full experiment as described in the paper.

# First, generate the sample data, if it does not already exists.

if [ ! -d "data/gen_small_1" ]; then
    cd ..
    sbt "runMain org.softlang.s2s.gen"
    cd validation || exit
fi

# Activate the python environment.
# shellcheck disable=SC1091
source venv/bin/activate 

# Launch processes for running validation.

mkdir -p results

(
    trap 'kill 0' EXIT

    SCRIPT="validate.py"
    declare -a COMMANDS=(
        '-d data/gen_small_1 -o results/gen_small_1.csv'
        '-d data/gen_small_2 -o results/gen_small_2.csv'
        '-d data/gen_wide_1 -o results/gen_wide_1.csv'
        '-d data/gen_wide_2 -o results/gen_wide_2.csv'
        '-d data/gen_deep_1 -o results/gen_deep_1.csv'
        '-d data/gen_deep_2 -o results/gen_deep_2.csv'
        '-d data/gen_large_1 -o results/gen_large_1.csv'
        '-d data/gen_large_2 -o results/gen_large_2.csv'
    )

    for cmd in "${COMMANDS[@]}"; do
        # shellcheck disable=SC2086
        python3 $SCRIPT $cmd &
        pids+=($!)
    done

    wait

    ./evaluate_validation.sh
)

