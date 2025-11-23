#!/usr/bin/env python3

import argparse

from csvout import csv_gather, csv_header
from gconfig import Args
from runner import run_all, run_dir, run_one


def main():
    """Command line interface for S2S validation."""
    # Define the argument parser.
    parser = argparse.ArgumentParser(
        prog="s2s-validation",
        description="Validation tools for S2S.",
        epilog="see https://github.com/softlang/s2s",
    )

    parser = argparse.ArgumentParser(prog="PROG")
    mode = parser.add_mutually_exclusive_group(required=True)

    # Execution mode.
    _ = mode.add_argument(
        "-a",
        "--run-all",
        action="store_true",
        help="run all known validation cases in 'data'",
    )
    _ = mode.add_argument("-s", "--single", help="single test case folder")
    _ = mode.add_argument(
        "-d", "--directory", help="multiple test case folders in directory"
    )
    _ = mode.add_argument(
        "-g",
        "--gather-results",
        action="store_true",
        help="show available results in 'data'",
    )

    # Global options.
    _ = parser.add_argument(
        "-r",
        "--render",
        action="store_true",
        help="generate .png files (requires graphviz)",
    )
    _ = parser.add_argument(
        "-n",
        "--number-of-triples",
        action="store",
        default=100,
        type=int,
        help="Number of triples (default: 100)",
    )
    _ = parser.add_argument(
        "-x",
        "--repetitions",
        action="store",
        default=1,
        type=int,
        help="Number of repititions per sample (default: 1)",
    )

    # Parse CLI
    args = parser.parse_args(namespace=Args())

    # In any case, print CSV header for output.
    print(csv_header())

    # Execute the required mode (based on the 'mode' arguments).
    if args.single:
        run_one(args.single, args)
    elif args.directory:
        run_dir(args.directory, args)
    elif args.run_all:
        run_all(args)
    elif args.gather_results:
        csv_gather()
    else:
        pass  # Mode is required, this is never reached.


if __name__ == "__main__":
    main()
