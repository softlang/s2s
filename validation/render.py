"""Rendering of (small) RDF graphs."""

# from rdflib.extras.external_graph_libs import rdflib_to_networkx_multidigraph
# import networkx as nx
# import matplotlib.pyplot as plt

import io
from pathlib import Path

import pydotplus
from rdflib import Graph
from rdflib.tools.rdf2dot import rdf2dot


def render(graph: Graph, store_path: str):
    """Take an rdflib graph, and render it."""
    stream = io.StringIO()
    rdf2dot(graph, stream)
    dg = pydotplus.graph_from_dot_data(stream.getvalue())
    dg.write_png(store_path)
