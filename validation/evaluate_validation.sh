#!/bin/bash

cat results/metadata

./evaluate_one.sh small
./evaluate_one.sh wide
./evaluate_one.sh deep
./evaluate_one.sh large
