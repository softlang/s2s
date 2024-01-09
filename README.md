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

Please also consider the full usage [tutorial](tutorial/README.md), which includes intuitive examples for the method itself, as well as the implementation.

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

Alternatively, JSON-LD syntax can be used; the equivalent shape specification to the aforementioned axioms is given below. More examples are available in the [paper_examples](paper_examples/) folder, or in test cases. The syntax is determined by file extension, using `json` for JSON-LD and `shacl` for formal DL notation. 

```json
{
  "@context": {
    "s2s": "https://github.com/softlang/s2s/",
    "sh": "http://www.w3.org/ns/shacl#"
  },
  "@graph": [
    {
      "@id": "s2s:s1",
      "@type": "sh:NodeShape",
      "sh:targetClass": {
        "@id": "s2s:A"
      },
      "sh:property": {
        "sh:path": {
          "@id": "s2s:p"
        },
        "sh:qualifiedValueShape": {
          "sh:class": {
            "@id": "s2s:B"
          }
        },
        "sh:qualifiedMinCount": 1
      }
    },
    {
      "@id": "s2s:s2",
      "@type": "sh:NodeShape",
      "sh:targetSubjectsOf": {
        "@id": "s2s:r"
      },
      "sh:class": {
        "@id": "s2s:B"
      }
    },
    {
      "@id": "s2s:s3",
      "@type": "sh:NodeShape",
      "sh:targetClass": {
        "@id": "s2s:B"
      },
      "sh:class": {
        "@id": "s2s:E"
      }
    }
  ]
}
```

Another relevant command-line option is `--debug`. While by default, the program outputs only the result shapes, including the `--debug` option will print detailed information about the internals of our method, including the input, output, vocabulary, all inferred axioms (annotated with the respective step of the method specification in the paper), as well as all generated candidate shapes.

## Further Reading

The [tutorial](tutorial/README.md), here in the repository.

The full paper: Philipp Seifer, Daniel Hernandez, Ralf Lämmel, and Steffen Staab. From Shapes to Shapes: Inferring SHACL Shapes for Results of SPARQL CONSTRUCT Queries. Publication. Year. URL.

```BibTeX
TBD: bibtex-citation
```
