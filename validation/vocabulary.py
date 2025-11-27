"""A vocabulary for RDF graph generation."""

from rdflib.term import URIRef


class Vocabulary:
    """
    A vocabulary for RDF graph generation.

    A vocabular contains only fully extended IRIs of
    concept names (classes), role names (properties), and individual
    names (nominals), that ought to appear in a generated RDF graph.
    """

    meta_etn_raw: str = "https://github.com/softlang/s2s/meta_etn"
    meta_nte_raw: str = "https://github.com/softlang/s2s/meta_nte"
    meta_etn: URIRef = URIRef(meta_etn_raw)
    meta_nte: URIRef = URIRef(meta_nte_raw)

    meta_node_raw: str = "https://github.com/softlang/s2s/meta_node"
    meta_edge_raw: str = "https://github.com/softlang/s2s/meta_edge"
    meta_node: URIRef = URIRef(meta_node_raw)
    meta_edge: URIRef = URIRef(meta_edge_raw)

    otherval_raw: str = "https://github.com/softlang/s2s/gentype-sumin"
    otherval: URIRef = URIRef(otherval_raw)

    epp_raw: str = "https://github.com/softlang/s2s/ekey"
    npp_raw: str = "https://github.com/softlang/s2s/nkey"
    elp_raw: str = "https://github.com/softlang/s2s/elabel"
    nlp_raw: str = "https://github.com/softlang/s2s/nlabel"

    # Concepts and properties that are always defined.
    a_ekey: str = "https://github.com/softlang/s2s/ekey/genkey-trolle"
    a_nkey: URIRef = URIRef("https://github.com/softlang/s2s/nkey/genkey-trolln")
    a_elab: URIRef = URIRef("https://github.com/softlang/s2s/elabel/genlab-trolle")
    a_nlab: URIRef = URIRef("https://github.com/softlang/s2s/nlabel/genlab-trolln")

    # Basic built-in uris.
    rdf_type_raw: str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
    rdf_type: URIRef = URIRef(rdf_type_raw)

    def __init__(self, concepts: str, properties: str, nominals: str):
        """Initialize a vocabulary from its components, parsed from files."""
        # Load the raw data from files.
        self.raw_concepts: list[str] = self._load_dim(concepts)
        self.raw_properties: list[str] = self._load_dim(properties)
        self.raw_nominals: list[str] = self._load_dim(nominals)

        # For RDF graphs, directly.

        self.concepts: list[URIRef] = [URIRef(c) for c in self.raw_concepts]
        self.properties: list[URIRef] = [URIRef(p) for p in self.raw_properties]
        self.nominals: list[URIRef] = [URIRef(n) for n in self.raw_nominals]

        # For property-graphs.

        self.values: list[URIRef] = [self.otherval] + self.nominals

        self.edge_properties: list[URIRef] = [
            URIRef(p) for p in self.raw_properties if self.epp_raw in p
        ]
        #self.edge_properties.append(URIRef(self.a_ekey))

        self.node_properties: list[URIRef] = [
            URIRef(p) for p in self.raw_properties if self.npp_raw in p
        ]
        #self.node_properties.append(URIRef(self.a_nkey))

        self.edge_labels: list[URIRef] = [
            URIRef(c) for c in self.raw_concepts if self.elp_raw in c
        ]
        #self.edge_labels.append(URIRef(self.a_elab))

        self.node_labels: list[URIRef] = [
            URIRef(c) for c in self.raw_concepts if self.nlp_raw in c
        ]
        #self.node_labels.append(URIRef(self.a_nlab))

    def _load_dim(self, path: str) -> list[str]:
        """Load one thing from a file; might be empty."""
        try:
            with open(path, encoding="utf-8") as f:
                return [line.rstrip() for line in f]
        except OSError:
            return []
