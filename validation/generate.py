"""Generation of random RDF graphs."""

import random

from data import Status
from gconfig import Config
from pyshacl import validate  # pyright: ignore[reportUnknownVariableType]
from rdflib import Graph
from rdflib.query import Result
from rdflib.term import URIRef
from vocabulary import Vocabulary

# Function: 'generate' graphs and helper functions.


def _gen_property_nodes(config: Config, cpr: float, non: int):
    """Generate only ratio*nodes nodes, for actual nodes (excl. edges)."""
    reduced = int(cpr * non)
    return [URIRef("{}{}".format(config.gen_node_prefix, i)) for i in range(0, reduced)]


def _gen_rdf_nodes(config: Config, non: int):
    """Generate the non required nodes."""
    return list(
        set(
            URIRef("{}{}".format(config.gen_node_prefix, i)) for i in range(0, non)
        ).union(config.voc.nominals)
    )


def _gen_property_edge(config: Config, i: int):
    """Generate a new edge from some unique identifier."""
    edge = URIRef("{}{}".format(config.gen_edge_prefix, i))
    return edge


def _initial_graph(
    config: Config, cpr: float, it: int
) -> tuple[Graph, list[URIRef], list[URIRef]]:
    """Initialize a graph, before generating its triples."""
    # In property mapping mode, generate the basic structure here.
    g = Graph()
    non = config.rnd_number_of_nodes(it)
    if config.property_mode:
        nodes = _gen_property_nodes(config, cpr, non)
        edges: list[URIRef] = []
        rest = int((1.0 - cpr) * non)

        # Sample edges.
        for i in range(0, rest):
            # Sample two nodes, generate a fresh edge.
            node1 = random.choice(nodes)
            node2 = random.choice(nodes)
            edge = _gen_property_edge(config, i)
            edges.append(edge)
            # Add n1-nte->e and e-etn->n2 triples to the graph.
            _ = g.add((node1, config.voc.meta_nte, edge))
            _ = g.add((edge, config.voc.meta_etn, node2))
            _ = g.add((edge, config.voc.rdf_type, config.voc.meta_edge))

        # Add meta node type to all nodes.
        for n in nodes:
            _ = g.add((n, config.voc.rdf_type, config.voc.meta_node))

    # In standard RDF mode, just generate the nodes.
    else:
        nodes = _gen_rdf_nodes(config, non)
        edges = []
    return (g, nodes, edges)


def _random_prop_triple(
    config: Config, nodes: list[URIRef], edges: list[URIRef], cpr: float, plr: float
):
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
            return (edge, key, value)
        elif config.voc.edge_labels:
            label = random.choice(config.voc.edge_labels)
            return (edge, config.voc.rdf_type, label)
        else:
            return None


def _random_rdf_triple(config: Config, nodes: list[URIRef], cpr: float):
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


def generate(config: Config, it: int):
    """Generate a new graph with the given settings."""
    cpr = config.rnd_concept_property_ratio()
    lpr = config.rnd_property_label_ratio()
    g, nodes, edges = _initial_graph(config, cpr, it)

    # Generate the required number of triples.
    for _ in range(0, config.number_of_triples(it)):
        if config.property_mode:
            draw = _random_prop_triple(config, nodes, edges, cpr, lpr)
        else:
            draw = _random_rdf_triple(config, nodes, cpr)
        if draw:
            _ = g.add(draw)
    return g


# Function: 'count_targets' and helper functions.


def _ask(graph: Graph, q: str):
    """Execute ask query against the graph."""
    r = graph.query(q)
    for ri in r:
        if ri:
            return True
        else:
            return False


def _has_class_target(graph: Graph, c: URIRef):
    """Test, whether the graph has a specific class target."""
    return _ask(
        graph,
        """
        ASK {{
            ?t1 a <{}>
        }}""".format(
            c
        ),
    )


def _has_property_target(graph: Graph, p: URIRef):
    """Test, whether the graph has a specific class target."""
    return _ask(
        graph,
        """
        ASK {{
            ?t1 <{}> ?t2
        }}""".format(
            p
        ),
    )


def count_targets(shapes: Graph, graph: Graph):
    """
    Test, whether a given graph satisfies all targets of all shapes.

    Returns a tuple of the number of targets that occur n or more
    times, and the total count of identified target queries.
    """
    # Obtain all class targets.
    target_classes = shapes.query(
        """
        SELECT DISTINCT ?c WHERE {
            ?s <http://www.w3.org/ns/shacl#targetClass> ?c
        }"""
    )

    # Obtain all property (subject/object) targets.
    target_subjects = shapes.query(
        """
        SELECT DISTINCT ?p WHERE {
            ?s <http://www.w3.org/ns/shacl#targetSubjectsOf> ?p
        }"""
    )
    target_objects = shapes.query(
        """
        SELECT DISTINCT ?p WHERE {
            ?s <http://www.w3.org/ns/shacl#targetObjectsOf> ?p
        }"""
    )

    # For all these cases, verify that there are targets
    # in the current iteration of the pruned graph.

    ct = [
        _has_class_target(
            graph,
            t[  # pyright: ignore[reportCallIssue, reportArgumentType, reportIndexIssue]
                "c"
            ],
        )
        for t in target_classes
    ]

    st = [
        _has_property_target(
            graph,
            t[  # pyright: ignore[reportCallIssue, reportArgumentType, reportIndexIssue]
                "p"
            ],
        )
        for t in target_subjects
    ]
    ot = [
        _has_property_target(
            graph,
            t[  # pyright: ignore[reportCallIssue, reportArgumentType, reportIndexIssue]
                "p"
            ],
        )
        for t in target_objects
    ]

    total = 0
    for _ in target_classes:
        total += 1
    for _ in target_subjects:
        total += 1
    for _ in target_objects:
        total += 1

    return ((ct + st + ot).count(False), total)


# Function: 'prune' and helper functions.


def _validation_report(shapes: Graph, graph: Graph) -> tuple[bool, Graph]:
    """Produce a SHACL validation report."""
    is_valid, report, _ = validate(  # pyright: ignore[reportUnknownVariableType]
        data_graph=graph, shacl_graph=shapes
    )
    return (
        is_valid,
        report,  # pyright: ignore[reportUnknownVariableType]
    )


def _violating_nodes(report: Graph) -> Result:
    """Return the nodes violating input shapes."""
    return report.query(
        """
        SELECT ?n ?s ?c WHERE {
            ?x <http://www.w3.org/ns/shacl#focusNode> ?n .
            ?x <http://www.w3.org/ns/shacl#sourceShape> ?s .
            ?x <http://www.w3.org/ns/shacl#sourceConstraintComponent> ?c .
        }
    """
    )


def _all_subject_triples(graph: Graph, node: URIRef):
    """Return all triples for a given node that must be removed."""
    q = """
        SELECT ?p ?o WHERE {{
            <{n}> ?p ?o
        }}
    """.format(
        n=node
    )
    return graph.query(q)


def _all_object_triples(graph: Graph, node: URIRef):
    """Return all triples for a given node that must be removed."""
    q = """
        SELECT ?s ?p WHERE {{
            ?s ?p <{n}>
        }}
    """.format(
        n=node
    )
    return graph.query(q)


def _dangling_edges(graph: Graph, node: URIRef, voc: Vocabulary):
    """Return all dangling edge triples for some node."""
    q = """
        SELECT ?e WHERE {{
            {{ <{n}> <{nte}> ?e }}
            UNION
            {{ ?e <{etn}> <{n}> }}
        }}
    """.format(
        n=node, nte=voc.meta_nte_raw, etn=voc.meta_etn_raw
    )
    return graph.query(q)


def _purge_violations(
    graph: Graph, report: Graph, property_mode: bool, voc: Vocabulary
):
    """Remove nodes that violate any shapes according to 'report'."""
    violations = _violating_nodes(report)
    for (
        node,  # pyright: ignore[reportGeneralTypeIssues, reportUnknownVariableType]
        _,  # pyright: ignore[reportUnknownVariableType]
        _,  # pyright: ignore[reportUnknownVariableType]
    ) in violations:

        # All triples where node is subject and object.
        subjects = _all_subject_triples(
            graph, node  # pyright: ignore[reportArgumentType]
        )
        objects = _all_object_triples(
            graph, node  # pyright: ignore[reportArgumentType]
        )

        # Edges that are now 'dangling', since one node is missing.
        # Careful: Need to get this /before/ removing the nodes!
        dangling = _dangling_edges(
            graph, node, voc  # pyright: ignore[reportArgumentType]
        )

        for (
            p,  # pyright: ignore[reportGeneralTypeIssues, reportAssignmentType, reportUnknownVariableType]
            o,  # pyright: ignore[reportUnknownVariableType]
        ) in subjects:
            _ = graph.remove((node, p, o))  # pyright: ignore[reportUnknownArgumentType]

        for (
            s,  # pyright: ignore[reportGeneralTypeIssues, reportAssignmentType, reportUnknownVariableType]
            p,  # pyright: ignore[reportUnknownVariableType]
        ) in objects:
            graph.remove(
                (
                    s,
                    p,
                    node,
                )  # pyright: ignore[reportUnusedCallResult, reportUnknownArgumentType]
            )

        if ("node" in node) and property_mode:  # pyright: ignore[reportOperatorIssue]
            # Remove all edges for removed reified edges (property graphs).
            for (
                e,  # pyright: ignore[reportGeneralTypeIssues, reportAssignmentType, reportUnknownVariableType]
            ) in dangling:
                for (
                    p,  # pyright: ignore[reportGeneralTypeIssues, reportAssignmentType, reportUnknownVariableType]
                    o,  # pyright: ignore[reportUnknownVariableType]
                ) in _all_subject_triples(
                    graph, e  # pyright: ignore[reportArgumentType]
                ):
                    _ = graph.remove(
                        (e, p, o)  # pyright: ignore[reportUnknownArgumentType]
                    )
                for (
                    s,  # pyright: ignore[reportGeneralTypeIssues, reportAssignmentType, reportUnknownVariableType]
                    p,  # pyright: ignore[reportUnknownVariableType]
                ) in _all_object_triples(
                    graph, e  # pyright: ignore[reportArgumentType]
                ):
                    _ = graph.remove(
                        (s, p, e)  # pyright: ignore[reportUnknownArgumentType]
                    )


def prune(
    config: Config, shapes: Graph, graph: Graph, max_iterations: int = 10
) -> Status:
    """Prune a graph with a shapes graph, removing violations."""
    global total
    for _ in range(0, max_iterations):
        is_valid, report = _validation_report(shapes, graph)
        (missing_targets, total_targets) = count_targets(shapes, graph)

        # If the report comes back clean, break, indicating validity.
        if is_valid and missing_targets == 0:
            return Status.OK
        elif is_valid and missing_targets == total_targets:
            return Status.MISSING_ALL_TARGETS
        elif is_valid and missing_targets > 0:
            return Status.MISSING_TARGETS
        # Otherwise, prune and continue checking.
        _purge_violations(graph, report, config.property_mode, config.voc)

    # If we reach max_iterations, return False.
    return Status.GRAPH_GENERATION_TIMEOUT
