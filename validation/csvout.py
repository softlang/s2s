import os

from data import Data


def csv_header() -> str:
    """Print csv header."""
    return (
        "test case,input triples,result triples,"
        "missing targets,total targets,"
        "evaluation time,error,valid"
    )


def csv_row(data: Data, path: str | None = None) -> None:
    """Print CSV data to stdout and path-based result file."""
    csv = ("{},{},{},{},{},{},{},{}").format(
        data.path,
        data.insize(),
        data.outsize(),
        data.out_missing_targets,
        data.out_total_targets,
        data.time,
        data.error,
        data.validity,
    )
    if path:
        with open(path, mode="w", encoding="utf-8") as f:
            _ = f.write(csv)
    print(csv)


def csv_gather() -> None:
    """Print all csv results, including header."""
    for subdir in os.listdir("data"):
        for test in os.listdir(os.path.join("data", subdir)):
            path = os.path.join("data", subdir, test, "out", "result.csv")
            try:
                with open(path, encoding="utf-8") as f:
                    print(f.read())
            except IOError:
                pass
