# From Shapes to Shapes: Inferring SHACL Shapes for Results of SPARQL CONSTRUCT Queries

## Requirements

Building Shapes2Shapes requires the Scala build tool [SBT](https://www.scala-sbt.org/), as well as Java.

## Quick Start

0. Install the requirements.
1. Clone this repository.
2. Run an example (building automatically via SBT)
    * Windows: `.\s2s.bat .\paper_examples\q1.sparql .\paper_examples\S1.shacl`
    * GNU/Linux or macOS: `./s2s paper_examples/q1.sparql paper_examples/S1.shacl`
    * From a `sbt` session: `runMain org.softlang.s2s.s2s paper_examples/q1.sparql paper_examples/S1.shacl`

Note, that running from a SBT session is much faster for multiple executions, due to SBT startup time.

## Usage

The following usage examples assume usage of the `s2s` utility script; see the quick start section for system specific information, or for information on how to run the program from a SBT session, directly.

Note: On Windows, you may want or need to set the active code page to UTF-8 via command [`chcp 65001`](https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/chcp). The included `s2s.bat` script does so, automatically.

### Command Line Interface

Basic usage is as follows: `s2s [OPTIONS] ... [query-file] [shapes-file?]`. Full documentation of the command line interface is accessible with `s2s --help`. The primary input for Shapes2Shapes is a (SCCQ) SPARQL query (`query-file`) and an optional set of Simple SHACL input shapes (`shapes-file`). The query is encoded via a (subset) of standard SPARQL syntax, e.g.:

```sparql
CONSTRUCT {
    ?y a :E .
    ?z a :B .
    ?y :p ?z
} WHERE {
    ?w :p ?y .
    ?x :p ?z .
    ?y a :B .
    ?z a :E
}
```

Prefix definitions are allowed. By default, the prefix `:` is defined (for usage in examples) and bound to a S2S specific IRI. Various standard prefixes, such as RDF, are predefined. The standard prefix can be redefined via `--prefix <prefix>`, such that, for example, `:` can be rebound for a required domain (without requiring explicit prefix definitions in each query file).

Simple SHACL shapes are encoded as description logic axioms (one per line), where the target is the left-hand-side of a subsumption axiom, and the constraint is on the right-hand-side:

```
:A ⊑ ∃:p.:B
∃:r.⊤ ⊑ :B
:B ⊑ :E
```

More examples are available in the [paper_examples](paper_examples/) folder, or in test cases.

Another relevant command-line option is `--debug`. While by default, the program outputs only the result shapes, including the `--debug` option will print detailed information about the internals of our method, including the input, output, vocabulary, all inferred axioms (annotated with the respective step of the method specification in the paper), as well as all generated candidate shapes.

## Further Reading

Philipp Seifer, Daniel Hernandez, Ralf Lämmel, and Steffen Staab. From Shapes to Shapes: Inferring SHACL Shapes for Results of SPARQL CONSTRUCT Queries. Publication. Year. URL.

```BibTeX
TBD: bibtex-citation
```
