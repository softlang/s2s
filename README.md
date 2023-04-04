# From Shapes to Shapes: Inferring SHACL Shapes for SPARQL Data Pipelines

## Requirements

Building Shapes2Shapes requires the Scala build tool [SBT](https://www.scala-sbt.org/) and Java.

## Quick Start

0. Install the requirements.
1. Clone or download this repository.
2. Run an example (building automatically via SBT)
    * Windows: `.\s2s.bat --debug .\paper_examples\example_2.sparql .\paper_examples\example_2.shacl`
    * GNU/Linux or macOS: `./s2s --debug paper_examples/example_2.sparql paper_examples/example_2.shacl`
    * From a `sbt` session: `runMain org.softlang.s2s.s2s --debug paper_examples/example_2.sparql paper_examples/example_2.shacl`

## Usage

The following usage examples assume usage of the `s2s` utility script; see the quick start section for system specific information, or for information on how to run the program from a SBT session, directly.

### Command Line Interface

Basic usage is as follows: `s2s [OPTIONS] ... [query-file] [shapes-file?]`. Full documentation of the command line interface is accessible with `s2s --help`. The primary input for Shapes2Shapes is a (SCCQ) SPARQL query (`query-file`) and an optional set of Simple SHACL input shapes (`shapes-file`). The query is encoded via a (subset) of standard SPARQL syntax, e.g.:

```sparql
CONSTRUCT {
    ?x a :C . ?y a :D
} WHERE { 
    ?x a :A . ?y a :B
}
```

Prefix definitions are allowed. By default, the prefix `:` is defined for examples and bound to a system specific IRI. Various standard prefixes, such as RDF, are predefined. The standard prefix (internal IRI) can be redefined via `--prefix <prefix>`, such that, for example, `:` can be rebound with another standard prefix definition.

Simple SHACL shapes are encoded as description logic axioms (one per line), where the target is the left-hand-side of a subsumption axiom, and the constraint is on the right-hand-side:

```
:B ⊑ :A
:A ⊑ :B
```

More examples are available in the [examples](examples/) or [paper_examples](paper_examples/) folder.

Another relevant command-line option is `--debug`. While by default, the program outputs only the result shapes, including the `--debug` option will print detailed information about the internals of the method, including input, output, vocabulary, all inferred axioms, as well as all candidate shapes.

## Further Reading

Philipp Seifer, Daniel Hernandez, Ralf Lämmel, and Steffen Staab. From Shapes to Shapes: Inferring SHACL Shapes for SPARQL Data Pipelines. Publication. Year. URL.

```BibTeX
TBD: bibtex-citation
```
