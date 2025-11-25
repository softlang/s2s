import random
from argparse import Namespace
from dataclasses import dataclass

from vocabulary import Vocabulary


class Args(Namespace):
    run_all: bool = False
    single: str = ""
    directory: str = ""
    gather_results: bool = False
    render: bool = False
    number_of_triples: int = 100
    repetitions: int = 1
    tries: int = 100
    increase: int = 0
    outfile: str | None = None


@dataclass
class Config:
    """A configuration for generating and pruning graphs."""

    # The vocabulary from which to generate the graph.
    voc: Vocabulary

    # Target number of triples/statements in graphs.
    # This scales the entire graph.
    triples_baseline: int
    triples_increase: int

    # Tuple, consisting of:
    # - Ratio values between 0 and 1
    # - randomization factor applied as follows:
    #   x - x * rf to x + x * rf
    # for x in the ratios listed below.
    node_to_triple_ratio: tuple[float, float]
    concept_property_ratio: tuple[float, float]
    property_label_ratio: tuple[float, float]

    # Enable property graph mode if True.
    property_mode: bool

    # Prefix for generated nodes.
    gen_node_prefix: str = "https://github.com/softlang/s2s/gen/node"

    # Prefix for generated edges.
    gen_edge_prefix: str = "https://github.com/softlang/s2s/gen/edge"

    def number_of_triples(self, it: int) -> int:
        """The number of triples, based on the current iteration."""
        return it * self.triples_increase + self.triples_baseline

    def rnd_number_of_nodes(self, it: int) -> int:
        """Return a random number of nodes."""
        return int(self._rnd(self.node_to_triple_ratio) * self.number_of_triples(it))

    def rnd_concept_property_ratio(self) -> float:
        """Ratio of element elements of the generated graph.

        - Ratio of concepts to properties (RDF) or
        - Ratio of nodes to edges (property graph).
        """
        return self._rnd(self.concept_property_ratio)

    def rnd_property_label_ratio(self) -> float:
        """For property graphs, ratio of labels to key-value pairs."""
        return self._rnd(self.property_label_ratio)

    def _rnd(self, xf: tuple[float, float]) -> float:
        """Randomize x with randomization facor f."""
        x, f = xf
        return random.uniform(x - x * f, x + x * f)
