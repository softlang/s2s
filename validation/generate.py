
"""Generation of random RDF graphs."""

import random
from dataclasses import dataclass

from rdflib import Graph
from rdflib.term import URIRef
from pyshacl import validate

from vocabulary import Vocabulary


@dataclass
class Config:
    """A configuration for generating and pruning graphs."""

    # The vocabulary from which to generate the graph.
    voc: Vocabulary

    # The total number of triples in the graph.
    # List with choices, chooses one randomly.
    number_of_triples: list[int]

    # The number of nodes (< number_of_triples).
    # List with choices, chooses one randomly.
    number_of_nodes: list[int]

    # Ratio of concepts to properties (RDF)
    # or nodes to edges (property graph).
    concept_property_ratio: list[float]

    # In property_mode, the ratio of labels to
    # key-value (properties).
    property_label_ratio: list[float]

    # Multiplicators for the number_of_triples and
    # number_of_nodes settings. Sampled and applied to both.
    multiplicator: list[float]

    # Enable property graph mode if True.
    property_mode: bool

    # Prefix for generated nodes.
    gen_node_prefix = "https://github.com/softlang/s2s/gen/node"

    # Prefix for generated edges.
    gen_edge_prefix = "https://github.com/softlang/s2s/gen/edge"


# Function: 'generate' graphs and helper functions.


def _gen_property_nodes(config, cpr, non):
    """Generate only ratio*nodes nodes, for actual nodes (excl. edges)."""
    reduced = int(cpr * non)
    return [
        URIRef("{}{}".format(config.gen_node_prefix, i))
        for i in range(0, reduced)
    ]


def _gen_rdf_nodes(config, non):
    """Generate the non required nodes."""
    return list(set(
        URIRef("{}{}".format(config.gen_node_prefix, i))
        for i in range(0, non)
    ).union(config.voc.nominals))


def _gen_property_edge(config, i):
    """Generate a new edge from some unique identifier."""
    edge = URIRef("{}{}".format(config.gen_edge_prefix, i))
    return edge


def _initial_graph(config, mult, cpr):
    """Initialize a graph, before generating its triples."""
    # In property mapping mode, generate the basic structure here.
    g = Graph()
    non = int(mult * random.choice(config.number_of_nodes))
    if config.property_mode:
        nodes = _gen_property_nodes(config, cpr, non)
        edges = []
        rest = int(
            (1.0 - cpr) * non)

        for i in range(0, rest):
            # Sample two nodes, generate a fresh edge.
            node1 = random.choice(nodes)
            node2 = random.choice(nodes)
            edge = _gen_property_edge(config, i)
            edges.append(edge)
            # Add n1-nte->e and e-etn->n2 triples to the graph.
            g.add((node1, config.voc.meta_nte, edge))
            g.add((edge, config.voc.meta_etn, node2))
            # TODO add meta edges node/edge
            g.add((node1, config.voc.rdf_type, config.voc.meta_node))
            g.add((node2, config.voc.rdf_type, config.voc.meta_node))
            g.add((edge, config.voc.rdf_type, config.voc.meta_edge))
    # In standard RDF mode, just generate the nodes.
    else:
        nodes = _gen_rdf_nodes(config, non)
        edges = None
    return (g, nodes, edges)


def _random_prop_triple(config, nodes, edges, cpr, plr):
    """Return a random property-graph triple, or None."""
    # Generate a node-triple if true, or edge-triple otherwise.
    rnd_do_node = random.random() > cpr
    # Generate a key-value pair if true, or label otherwise.
    rnd_do_prop = random.random() > plr

    if rnd_do_node:
        node = random.choice(nodes)
        if config.voc.node_properties and rnd_do_prop:
            key = random.choice(config.voc.node_properties)
            value = random.choice(config.voc.values)
            return (node, key, value)
        elif config.voc.node_labels:
            label = random.choice(config.voc.node_labels)
            return (node, config.voc.rdf_type, label)
        else:
            return None
    else:
        edge = random.choice(edges)
        if config.voc.edge_properties and rnd_do_prop:
            key = random.choice(config.voc.edge_properties)
            value = random.choice(config.voc.values)
            return (node, key, value)
        elif config.voc.edge_labels:
            label = random.choice(config.voc.edge_labels)
            return (edge, config.voc.rdf_type, label)
        else:
            return None


def _random_rdf_triple(config, nodes, cpr):
    """Return a random triple, or None if no triples are possible."""
    draw = random.random() > cpr
    if config.voc.properties and draw:
        # Generate a property triple.
        node1 = random.choice(nodes)
        prop = random.choice(config.voc.properties)
        node2 = random.choice(nodes)
        return (node1, prop, node2)
    elif config.voc.concepts:
        # Generate a concept triples.
        node = random.choice(nodes)
        concept = random.choice(config.voc.concepts)
        return (node, config.voc.rdf_type, concept)
    else:
        return None


def generate(config):
    """Generate a new graph with the given settings."""
    cpr = random.choice(config.concept_property_ratio)
    lpr = random.choice(config.property_label_ratio)
    mult = random.choice(config.multiplicator)
    g, nodes, edges = _initial_graph(config, mult, cpr)

    # Generate the required number of triples.
    for i in range(0, int(mult * random.choice(config.number_of_triples))):
        if config.property_mode:
            draw = _random_prop_triple(config, nodes, edges, cpr, lpr)
        else:
            draw = _random_rdf_triple(config, nodes, cpr)
        if draw:
            g.add(draw)
    return g


# Function: 'count_targets' and helper functions.


def _ask(graph, q):
    """Execute ask query against the graph."""
    r = graph.query(q)
    for ri in r:
        if ri:
            return True
        else:
            return False


def _has_class_target(graph, c):
    """Test, whether the graph has a specific class target."""
    return _ask(graph, """
        ASK {{
            ?t1 a <{}>
        }}""".format(c))


def _has_property_target(graph, p):
    """Test, whether the graph has a specific class target."""
    return _ask(graph, """
        ASK {{
            ?t1 <{}> ?t2
        }}""".format(p))


def count_targets(shapes, graph):
    """
    Test, whether a given graph satisfies all targets of all shapes.

    Returns a tuple of the number of targets that occur n or more
    times, and the total count of identified target queries.
    """
    # Obtain all class targets.
    target_classes = shapes.query("""
        SELECT DISTINCT ?c WHERE {
            ?s <http://www.w3.org/ns/shacl#targetClass> ?c
        }""")

    # Obtain all property (subject/object) targets.
    target_subjects = shapes.query("""
        SELECT DISTINCT ?p WHERE {
            ?s <http://www.w3.org/ns/shacl#targetSubjectsOf> ?p
        }""")
    target_objects = shapes.query("""
        SELECT DISTINCT ?p WHERE {
            ?s <http://www.w3.org/ns/shacl#targetObjectsOf> ?p
        }""")

    # For all these cases, verify that there are targets
    # in the current iteration of the pruned graph.

    ct = [_has_class_target(graph, t["c"]) for t in target_classes]
    st = [_has_property_target(graph, t["p"]) for t in target_subjects]
    ot = [_has_property_target(graph, t["p"]) for t in target_objects]

    total = 0
    for c in target_classes:
        total += 1
    for p in target_subjects:
        total += 1
    for p in target_objects:
        total += 1

    return ((ct + st + ot).count(False), total)


# Function: 'prune' and helper functions.


def _validation_report(shapes, graph):
    """Produce a SHACL validation report."""
    is_valid, report, _ = validate(
        data_graph=graph,
        shacl_graph=shapes)
    return is_valid, report


def _violating_nodes(report):
    """Return the nodes violating input shapes."""
    return report.query("""
        SELECT ?n ?s ?c WHERE {
            ?x <http://www.w3.org/ns/shacl#focusNode> ?n .
            ?x <http://www.w3.org/ns/shacl#sourceShape> ?s .
            ?x <http://www.w3.org/ns/shacl#sourceConstraintComponent> ?c .
        }
    """)


def _all_subject_triples(graph, node):
    """Return all triples for a given node that must be removed."""
    q = """
        SELECT ?p ?o WHERE {{
            <{n}> ?p ?o
        }}
    """.format(n=node)
    return graph.query(q)


def _all_object_triples(graph, node):
    """Return all triples for a given node that must be removed."""
    q = """
        SELECT ?s ?p WHERE {{
            ?s ?p <{n}>
        }}
    """.format(n=node)
    return graph.query(q)


def _dangling_edges(graph, node, voc):
    """Return all dangling edge triples for some node."""
    q = """
        SELECT ?e WHERE {{
            {{ <{n}> <{nte}> ?e }}
            UNION
            {{ ?e <{etn}> <{n}> }}
        }}
    """.format(n=node,
               nte=voc.meta_nte_raw,
               etn=voc.meta_etn_raw)
    return graph.query(q)


def _purge_violations(graph, report, property_mode, voc):
    """Remove nodes that violate any shapes according to 'report'."""
    violations = _violating_nodes(report)
    for node, shape, constraint in violations:

        # All triples where node is subject and object.
        subjects = _all_subject_triples(graph, node)
        objects = _all_object_triples(graph, node)

        # Edges that are now 'dangling', since one node is missing.
        # Careful: Need to get this /before/ removing the nodes!
        dangling = _dangling_edges(graph, node, voc)

        for p, o in subjects:
            graph.remove((node, p, o))

        for s, p in objects:
            graph.remove((s, p, node))

        if "node" in node and property_mode:
            # Remove all edges for removed reified edges (property graphs).
            for e, in dangling:
                for p, o in _all_subject_triples(graph, e):
                    graph.remove((e, p, o))
                for s, p in _all_object_triples(graph, e):
                    graph.remove((s, p, e))


def prune(config, shapes, graph, max_iterations=10):
    """Prune a graph with a shapes graph, removing violations."""
    for i in range(0, max_iterations):
        is_valid, report = _validation_report(shapes, graph)
        (missing_targets, total_targets) = count_targets(shapes, graph)

        # If the report comes back clean, break, indicating validity.
        if is_valid and missing_targets == 0:
            return "ok"
        elif missing_targets > 0:
            return "missing targets {}/{}".format(
                missing_targets, total_targets)

        # Otherwise, prune and continue checking.
        _purge_violations(graph, report, config.property_mode, config.voc)

    # If we reach max_iterations, return False.
    return "failed to prune in {} steps".format(max_iterations)
