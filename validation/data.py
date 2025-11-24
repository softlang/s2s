from dataclasses import dataclass
from enum import Enum

from rdflib import Graph


class Status(Enum):
    OK = 0
    MISSING_TARGETS = 1
    MISSING_ALL_TARGETS = 2
    GRAPH_GENERATION_TIMEOUT = 3
    EMPTY_OUTPUT = 4
    EMPTY_INPUT = 5
    MISSING_TARGETS_OUT = 6
    MISSING_ALL_TARGETS_OUT = 7


@dataclass
class Data:
    """Single result entry, for a single validation case."""

    path: str | None = None

    def insize(self) -> int:
        return len(self.in_graph) if self.in_graph else -1

    def outsize(self) -> int:
        return len(self.out_graph) if self.out_graph else -1

    out_missing_targets: int | None = None
    out_total_targets: int | None = None
    in_missing_targets: int | None = None
    in_total_targets: int | None = None

    in_graph: Graph | None = None
    out_graph: Graph | None = None
    broken_graph: Graph | None = None

    out_graph_file: str | None = None
    in_graph_file: str | None = None

    time: float | None = None

    error: Status | None = None
    validity: bool | None = None
