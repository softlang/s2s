# From Shapes to Shapes

- [Quick Start](#quick-start)
- [Usage](#usage)
   - [Command Line Interface](#command-line-interface)
   - [Application Programming Interface](#application-programming-interface)
- [Further Reading](#further-reading)
  - [Full Tutorial](docs/README.md)
  - [Paper](https://google.de)

## Quick Start

This section introduces two ways of using Shapes2Shapes (S2S).

### Release Version

Running the release version of S2S requires Java.

0. Download ...
1. Unzip to chosen location.
3. Run an example, using the included launcher script:
    * GNU/Linux or macOS: `./s2s docs/examples/q1.sparql docs/examples/S1.shacl`
    * Windows: `.\bin\s2s.bat .\docs\examples\q1.sparql .\docs\examples\S1.shacl`

In summary, assuming a *nix environment:

```sh
curl ...
unzip s2s-0.0.1.zip
cd s2s-0.0.1

./s2s docs/examples/q1.sparql docs/examples/S1.shacl
```

### Building from Source

Building S2S from source requires the Scala build tool [SBT](https://www.scala-sbt.org/), as well as Java.

0. Install the requirements.
1. Clone this repository.
2. Run `sbt stage`.
3. Run an example, using the local launcher script:
    * GNU/Linux or macOS: `./s2s docs/examples/q1.sparql docs/examples/S1.shacl`
    * Windows: `.\s2s.bat .\docs\examples\q1.sparql .\docs\examples\S1.shacl`

In summary, assuming a *nix environment:

```sh
git clone https://github.com/softlang/s2s
cd s2s
sbt stage

./s2s docs/examples/q1.sparql docs/examples/S1.shacl
```

## Usage

Please also consider reading the full [tutorial](docs/README.md), which includes more intuitive examples for the method itself. The following examples always refer to simply `s2s` as an executable; see the quick start section for system specific information, or for information on how to run the program from a SBT session, directly. Note: On Windows, you may want to set the active code page to UTF-8 via command [`chcp 65001`](https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/chcp). This enables correct display of unicode symbols in output generated by `s2s`.

### Command Line Interface

Basic usage is as follows: `s2s [OPTIONS] ... [query-file] [shapes-file?]`. Full documentation of the command line interface is accessible with `s2s --help`. The primary input for S2S is a (SCCQ) SPARQL query (`query-file`) and an optional set of SHACL input shapes (`shapes-file`). The query is encoded via a (subset) of standard SPARQL syntax, e.g.:

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

Prefix definitions are allowed. By default, the prefix `:` is defined (for usage in examples) and bound to a S2S specific IRI. Various standard prefixes (e.g., RDF) are predefined. The default prefix can be redefined via `--prefix <prefix>`, such that `:` can be rebound for a required domain, without requiring explicit prefix definitions in each query.

Simple SHACL shapes are encoded as description logic axioms (one per line), where the target is the left-hand-side of a subsumption axiom, and the constraint is on the right-hand-side:

```
:A ⊑ ∃:p.:B
∃:r.⊤ ⊑ :B
:B ⊑ :E
```

Alternatively, JSON-LD syntax can be used; the equivalent shape specification to the aforementioned axioms is given below. More examples are available in the [docs/examples](docs/examples/) folder. The syntax is determined by file extension, using `.json` for JSON-LD and `.shacl` for formal DL notation. Note, that JSON-LD support is still experimental.

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

Another relevant command-line option is `--debug`. While by default, the program outputs only the result shapes, including the `--debug` option will print detailed information about the internals of the S2S method, including the input, output, vocabulary, all inferred axioms (annotated with the respective step of the method specification in the paper), as well as all generated candidate shapes.

### Application Programming Interface

While the CLI application is useful for interacting with the S2S method, S2S can also be accessed through its internal API. Currently, we lack a full documentation or example application for the API, which will be provided in the future. The main entry point for programmatic access to S2S is `org.softlang.s2s.infer.Shapes2Shapes`, with an usage example (the CLI itself) being available in `org.softlang.s2s.main.S2S`. A minimal usage example is given below, using String encoded inputs for query and shapes. Both `constructShapes` and `constructAxioms` can be invoked with formal input as well. To this end, consider `org.softlang.s2s.infer.AlgorithmInput`.

```scala
package org.softlang.s2s.example

import org.softlang.s2s.infer.Shapes2Shapes

/** A minimal example for using the s2s API. */
object Example:

    val q: String = """
        CONSTRUCT {
            ?x a :Person .
            ?y a :Agent
        } WHERE {
            ?x a :Person .
            ?y a :Agent
        }
    """

    val s: Set[String] = Set(":Person ⊑ :Agent")

    // Instantiate shapes 2 shapes with default configuration. 
    val s2s = Shapes2Shapes()

    // Run with side-effects, i.e., printing the results.
    s2s.run(q, s)

    // Return the set of output shapes (and a log).
    val (shapes, log) = s2s.constructShapes(q, s)

    // Return the set of constructed axioms (and a log).
    val (axioms, log) = s2s.constructAxioms(q, s)

```

## Further Reading

The [tutorial](docs/README.md), here in the repository.

The full paper: Philipp Seifer, Daniel Hernandez, Ralf Lämmel, and Steffen Staab. From Shapes to Shapes: Inferring SHACL Shapes for Results of SPARQL CONSTRUCT Queries. Publication. Year. URL.

An extended version is available on [arXiv](https://google.de).

```BibTeX
TBD: bibtex-citation
```
