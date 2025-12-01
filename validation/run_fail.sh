#!/bin/bash

# This runs the validation experiment.

# Activate the python environment.
# shellcheck disable=SC1091
source venv/bin/activate 

# Launch processes for running validation.

(
    trap 'kill 0' EXIT

    SCRIPT="validate.py"
    declare -a COMMANDS=(
        '-d data/gen_fail_1 -o gen_fail_1.csv'
        '-d data/gen_fail_2 -o gen_fail_2.csv'
        '-d data/gen_fail_3 -o gen_fail_3.csv'
        '-d data/gen_fail_4 -o gen_fail_4.csv'
    )

    for cmd in "${COMMANDS[@]}"; do
        # shellcheck disable=SC2086
        python3 $SCRIPT $cmd &
        pids+=($!)
    done

    wait
)

