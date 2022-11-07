# From Shapes to Shapes: Inferring SHACL Shapes for SPARQL Data Pipelines

### Requirements

Running Shapes2Shapes requires [SBT](https://www.scala-sbt.org/), the Scala build tool (and Java).

### Usage

Consider the file [Main.scala](src/main/scala/org.softlang.s2s/Main.scala).
It demonstrates a basic example of using S2S for inferring result shapes and printing a full debug log to standard output.
The `Configuration` provides various settings (see constructor for documentation).

Input are a SCCQ query using standard SPARQL syntax, shapes use standard DL syntax.
The default prefix `:` is configured for examples, and required for queries and shapes.
(The prefix is removed in (formal) output, as well as the internal prefix used for variables; there is a setting for disabling that).
Any additional prefix can be defined via SPARQL prefix declarations.
Currently, automatic renaming between Template and Pattern is not supported.
To this end, the implementation rejects queries, where common concept and property names are used.

Execute examples via `sbt` and then `run`.