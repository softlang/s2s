# From Shapes to Shapes: Inferring SHACL Shapes for SPARQL Data Pipelines

### Requirements

Building Shapes2Shapes requires the Scala build tool [SBT](https://www.scala-sbt.org/) and Java.

### Building, Setup and Execution

Essentially, building and running Shapes2Shapes can be handled via `sbt` directly, or by building a fat jar.

##### Building (and running) Shapes2Shapes as a fat JAR

In order to build the project execute `make` (or, manually, `sbt assembly`). Shapes2Shapes can be executed via the included `s2s <args>` script on UNIX systems or `s2s.bat <args>` on Windows. All options for `<args>` are defined in [Command Line Interface](#command-line-interface) below. Alternatively, manual execution is possible via `java -jar target/scala-3.2.0/s2s.jar <args>`. 

##### Running Shapes2Shapes via SBT

In order to run Shapes2Shapes directly from SBT, launch SBT via `sbt` and execute `run <args>` . Alternatively, run `sbt "run <args>"` directly from the command line. Note, however, that this is rather slow due to the startup time of SBT and only recommended for development.

### Usage

The following usage examples assume usage of the `s2s` utility (see also the previous section for other ways of running Shapes2Shapes).

#### Command Line Interface

Basic usage is as follows: `s2s [OPTIONS] ... [query-file] [shapes-file?]`. Full documentation of the command line interface is accessible with `s2s --help`. The primary input for Shapes2Shapes is a (SCCQ) SPARQL query (`query-file`) and an optional set of Simple SHACL input shapes (`shapes-file`). The query is encoded via a (subset) of SPARQL, e.g.:

```sparql
CONSTRUCT {
    ?x a :C . ?y a :D
} WHERE { 
    ?x a :A . ?y a :B
}
```

Prefix definitions are allowed. By default, the prefix `:` is defined for examples and bound to a system specific IRI. Various standard prefixes, such as RDF, are predefined. The standard prefix (internal IRI) can be redefined via `--prefix <prefix>`, such that, for example, `:` can be rebound with a standard prefix definition.

Simple SHACL shapes are encoded as description logic axioms (one per line), where the target is the left-hand-side of a subsumption axiom, and the constraint is on the right-hand-side:

```
:B ⊑ :A
:A ⊑ :B
```

More examples are available in the [examples](examples/) folder.

Further relevant command-line options are `--debug` for extended output, `--nolog` for no output (also in combination with `--output` to print only the output shapes) and `--help` for full documentation. Example for executing Shapes2Shapes with the example query and shapes above, while obtaining a full, formal output log with internal debugging information:

```sh
./s2s --debug examples/q1.sparql examples/s1.shacl
```

Usage example, for running the above example while only obtaining the list of output shapes:

```sh
./s2s --nolog --output examples/q1.sparql examples/s1.shacl
```

Finally, in standard mode, the formal input and output are printed as follows:

```sh
./s2s examples/q1.sparql examples/s1.shacl
```

### Further Reading

Philipp Seifer, Daniel Hernandez, Ralf Lämmel, and Steffen Staab. From Shapes to Shapes: Inferring SHACL Shapes for SPARQL Data Pipelines. Publication. Year. URL.

```BibTeX
TBD: bibtex-citation
```
